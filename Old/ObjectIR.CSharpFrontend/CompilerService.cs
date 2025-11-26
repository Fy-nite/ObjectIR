using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.Json;
using ObjectIR.Core.IR;
using ObjectIR.Core.Serialization;

namespace ObjectIR.CSharpFrontend;

/// <summary>
/// High-level compiler service that handles end-to-end C# to ObjectIR compilation
/// </summary>
public class CompilerService
{
    private readonly CompilerOptions _options;
    private List<string> _errors = new();
    private List<string> _warnings = new();

    public CompilerService(CompilerOptions options)
    {
        _options = options ?? throw new ArgumentNullException(nameof(options));
    }

    /// <summary>
    /// Compiles all input files and generates ObjectIR modules
    /// </summary>
    public bool Compile()
    {
        if (_options.Verbosity >= VerbosityLevel.Verbose)
            Console.WriteLine($"Compiling {_options.InputFiles.Count} file(s) to module '{_options.ModuleName}'...\n");

        try
        {
            // Read all input files
            var sourceCode = ReadInputFiles();
            if (sourceCode == null)
                return false;

            // Compile using Roslyn
            var compiler = new RoslynCompiler(sourceCode, _options.ModuleName ?? "module.cs");
            
            // Enable aggressive bootstrap mode if requested (fuck off mode)
            RoslynCompiler.AggressiveBootstrapMode = _options.AggressiveBootstrapMode;
            
            if (_options.Verbosity >= VerbosityLevel.Debug)
                Console.WriteLine($"[DEBUG] Parsing C# source ({sourceCode.Length} characters)...");

            var compilationInfo = compiler.Compile();
            if (compilationInfo == null)
            {
                ReportCompilationErrors(compiler.GetErrors());
                return false;
            }

            if (_options.Verbosity >= VerbosityLevel.Verbose)
                Console.WriteLine("✓ Roslyn compilation successful");

            // Emit ObjectIR
            if (_options.Verbosity >= VerbosityLevel.Debug)
                Console.WriteLine($"[DEBUG] Emitting ObjectIR module...");

            Module? irModule = null;
            try
            {
                irModule = compiler.EmitIR(_options.ModuleName ?? "module");
            }
            catch (Exception emitEx)
            {
                // In aggressive bootstrap mode, some emissions errors are expected
                if (_options.AggressiveBootstrapMode)
                {
                    if (_options.Verbosity >= VerbosityLevel.Verbose)
                        Console.WriteLine($"⚠ IR emission warning (bootstrap mode): {emitEx.Message}");
                    
                    if (_options.Verbosity >= VerbosityLevel.Debug)
                    {
                        Console.WriteLine($"[DEBUG] Exception details: {emitEx}");
                        if (emitEx.InnerException != null)
                            Console.WriteLine($"[DEBUG] Inner exception: {emitEx.InnerException}");
                    }
                    // Continue with best-effort IR - this allows bootstrap compilation to proceed
                    // even if source location extraction fails
                }
                else
                {
                    throw;
                }
            }

            if (irModule == null)
            {
                if (!_options.AggressiveBootstrapMode)
                {
                    ReportCompilationErrors(compiler.GetErrors());
                    return false;
                }
                // In bootstrap mode, we'll try to continue with empty module
                if (_options.Verbosity >= VerbosityLevel.Normal)
                    Console.WriteLine("⚠ No IR module generated, creating placeholder...");
                irModule = new Module(_options.ModuleName ?? "module");
            }

            if (_options.Verbosity >= VerbosityLevel.Verbose)
                Console.WriteLine("✓ ObjectIR emission successful");

            // Apply optimizations if requested
            if (_options.OptimizeIR)
            {
                if (_options.Verbosity >= VerbosityLevel.Debug)
                    Console.WriteLine($"[DEBUG] Applying IR optimizations...");

                ApplyOptimizations(irModule);

                if (_options.Verbosity >= VerbosityLevel.Verbose)
                    Console.WriteLine("✓ Optimizations applied");
            }

            // Write output
            if (_options.Verbosity >= VerbosityLevel.Debug)
                Console.WriteLine($"[DEBUG] Writing output to {_options.OutputDirectory}...");

            if (!WriteOutput(irModule))
                return false;

            if (_options.Verbosity >= VerbosityLevel.Normal)
                PrintCompilationSummary(irModule);

            return true;
        }
        catch (Exception ex)
        {
            _errors.Add($"Internal compiler error: {ex.Message}");
            if (_options.Verbosity >= VerbosityLevel.Debug)
                _errors.Add(ex.StackTrace ?? "");
            ReportErrors();
            return false;
        }
    }

    /// <summary>
    /// Reads and concatenates all input files, organizing them correctly
    /// </summary>
    private string? ReadInputFiles()
    {
        var sources = new List<string>();

        foreach (var file in _options.InputFiles)
        {
            try
            {
                if (!File.Exists(file))
                {
                    _errors.Add($"Input file not found: {file}");
                    continue;
                }

                var content = File.ReadAllText(file);
                sources.Add(content);

                if (_options.Verbosity >= VerbosityLevel.Debug)
                    Console.WriteLine($"[DEBUG] Loaded {file} ({content.Length} bytes)");
            }
            catch (Exception ex)
            {
                _errors.Add($"Error reading {file}: {ex.Message}");
            }
        }

        if (_errors.Count > 0)
        {
            ReportErrors();
            return null;
        }

        // If single file, use as-is
        if (sources.Count == 1)
            return sources[0];

        // For multiple files, organize correctly:
        // Extract usings from all files and strip namespaces
        var usings = new HashSet<string>();
        var allTypeLines = new List<string>();

        foreach (var source in sources)
        {
            var lines = source.Split(new[] { "\r\n", "\r", "\n" }, StringSplitOptions.None);

            bool inUsingSection = true;
            bool inNamespaceDecl = false;

            foreach (var line in lines)
            {
                var trimmed = line.Trim();

                // Skip empty lines and comments while in using section
                if (string.IsNullOrWhiteSpace(trimmed) || trimmed.StartsWith("//"))
                {
                    if (!inUsingSection && !inNamespaceDecl)
                        allTypeLines.Add(line);
                    continue;
                }

                // Collect using statements
                if (trimmed.StartsWith("using ") && inUsingSection)
                {
                    usings.Add(trimmed);
                    continue;
                }

                // Handle namespace declarations
                if (trimmed.StartsWith("namespace "))
                {
                    inUsingSection = false;
                    if (trimmed.EndsWith(";"))
                    {
                        // File-scoped namespace, skip the declaration but continue processing content
                        continue;
                    }
                    else
                    {
                        // Block namespace opening, we'll skip until matching close brace
                        inNamespaceDecl = true;
                        continue;
                    }
                }

                // Track namespace braces
                if (inNamespaceDecl)
                {
                    if (trimmed == "}")
                    {
                        inNamespaceDecl = false;
                        continue;
                    }
                    // Line is inside block namespace, keep but remove one level of indentation
                    if (line.StartsWith("    "))
                        allTypeLines.Add(line.Substring(4));
                    else
                        allTypeLines.Add(line);
                }
                else
                {
                    inUsingSection = false;
                    allTypeLines.Add(line);
                }
            }
        }

        // Rebuild: usings first, then types globally (no wrapping namespace)
        var result = new StringBuilder();
        foreach (var u in usings.OrderBy(x => x))
        {
            result.AppendLine(u);
        }

        if (usings.Count > 0)
            result.AppendLine();

        foreach (var line in allTypeLines)
        {
            result.AppendLine(line);
        }

        var finalResult = result.ToString();
        
        if (_options.Verbosity >= VerbosityLevel.Debug)
        {
            Console.WriteLine($"[DEBUG] Combined source: {allTypeLines.Count} type lines, {usings.Count} using statements");
            var typeDeclarations = allTypeLines.Where(l => l.Trim().StartsWith("public class ") || 
                                                            l.Trim().StartsWith("public interface ") || 
                                                            l.Trim().StartsWith("public struct ") ||
                                                            l.Trim().StartsWith("public enum ")).ToList();
            Console.WriteLine($"[DEBUG] Found {typeDeclarations.Count} top-level type declarations");
        }

        return finalResult;
    }

    /// <summary>
    /// Applies IR-level optimizations
    /// </summary>
    private void ApplyOptimizations(Module module)
    {
        // Placeholder for optimization passes
        // Could integrate with SemanticAnalyzer for pattern-based optimizations
        
        foreach (var type in module.Types)
        {
            if (type is ClassDefinition classDef)
            {
                // Example: could remove empty methods, inline trivial getters, etc.
                // For now, this is a placeholder for future optimization
            }
        }
    }

    /// <summary>
    /// Writes the ObjectIR module to disk in the specified format
    /// </summary>
    private bool WriteOutput(Module module)
    {
        try
        {
            var fileName = Path.Combine(
                _options.OutputDirectory,
                _options.ModuleName ?? "module"
            );

            var outputFile = _options.OutputFormat switch
            {
                OutputFormat.Binary => WriteModuleBinary(module, fileName),
                OutputFormat.Text => WriteModuleText(module, fileName),
                OutputFormat.Json => WriteModuleJson(module, fileName),
                _ => null
            };

            if (outputFile != null && _options.Verbosity >= VerbosityLevel.Verbose)
                Console.WriteLine($"✓ Output written to {outputFile}");

            return outputFile != null;
        }
        catch (Exception ex)
        {
            _errors.Add($"Error writing output: {ex.Message}");
            ReportErrors();
            return false;
        }
    }

    /// <summary>
    /// Writes module in binary format (placeholder)
    /// </summary>
    private string? WriteModuleBinary(Module module, string basePath)
    {
        var path = $"{basePath}.ir";
        // TODO: Implement binary serialization
        // For now, write a marker file
        File.WriteAllText(path, $"ObjectIR Binary Module: {module.Name} v{module.Version}\n");
        return path;
    }

    /// <summary>
    /// Writes module in human-readable text format
    /// </summary>
    private string? WriteModuleText(Module module, string basePath)
    {
        var path = $"{basePath}.ir.txt";
        using var writer = new StreamWriter(path);

        writer.WriteLine($"ObjectIR Module: {module.Name}");
        writer.WriteLine($"Version: {module.Version}");
        writer.WriteLine($"Types: {module.Types.Count}");
        writer.WriteLine();

        foreach (var type in module.Types)
        {
            WriteTypeDefinition(writer, type);
            writer.WriteLine();
        }

        return path;
    }

    /// <summary>
    /// Writes module in JSON format
    /// </summary>
    private string? WriteModuleJson(Module module, string basePath)
    {
        var path = $"{basePath}.ir.json";
        
        // Use ModuleSerializer to generate JSON with camelCase naming policy
        var json = module.DumpJson(indented: true);
        File.WriteAllText(path, json);

        // Validate the generated JSON
        var validationResult = JsonValidator.Validate(json);
        if (!validationResult.IsValid)
        {
            _errors.Add("Generated JSON failed validation:");
            _errors.Add($"  {validationResult.ErrorMessage}");
            ReportErrors();
            return null;
        }

        if (_options.Verbosity >= VerbosityLevel.Debug)
            Console.WriteLine($"[DEBUG] JSON validation passed");

        return path;
    }

    private object SerializeType(TypeDefinition type)
    {
        var baseData = new
        {
            type.Name,
            type.Namespace,
            Kind = type.Kind.ToString(),
            Access = type.Access.ToString()
        };

        // Add type-specific details
        if (type is ClassDefinition classDef)
        {
            return new
            {
                baseData.Name,
                baseData.Namespace,
                baseData.Kind,
                baseData.Access,
                FieldCount = classDef.Fields.Count,
                MethodCount = classDef.Methods.Count,
                Fields = classDef.Fields.Select(f => SerializeField(f)).ToList(),
                Methods = classDef.Methods.Select(m => SerializeMethod(m)).ToList()
            };
        }
        else if (type is StructDefinition structDef)
        {
            return new
            {
                baseData.Name,
                baseData.Namespace,
                baseData.Kind,
                baseData.Access,
                FieldCount = structDef.Fields.Count,
                MethodCount = structDef.Methods.Count,
                Fields = structDef.Fields.Select(f => SerializeField(f)).ToList(),
                Methods = structDef.Methods.Select(m => SerializeMethod(m)).ToList()
            };
        }

        return baseData;
    }

    private object SerializeMethod(MethodDefinition method)
    {
        return new
        {
            method.Name,
            ReturnType = method.ReturnType.GetQualifiedName(),
            Access = method.Access.ToString(),
            ParameterCount = method.Parameters.Count,
            Parameters = method.Parameters.Select(p => new { p.Name, Type = p.Type.GetQualifiedName() }).ToList(),
            LocalVariables = method.Locals.Select(l => new { l.Name, Type = l.Type.GetQualifiedName() }).ToList(),
            method.IsStatic,
            method.IsVirtual,
            method.IsAbstract,
            InstructionCount = method.Instructions.Count,
            Instructions = SerializeInstructions(method.Instructions)
        };
    }

    private object SerializeField(FieldDefinition field)
    {
        return new
        {
            field.Name,
            Type = field.Type.GetQualifiedName(),
            Access = field.Access.ToString(),
            field.IsStatic,
            field.IsReadOnly
        };
    }

    private object[] SerializeInstructions(InstructionList instructions)
    {
        var result = new List<object>();
        
        foreach (var instr in instructions)
        {
            result.Add(SerializeInstruction(instr));
        }
        
        return result.ToArray();
    }

    private object SerializeInstruction(Instruction instr)
    {
        return instr switch
        {
            LoadArgInstruction li => new { opCode = "ldarg", operand = new { argumentName = li.ArgumentName } },
            LoadLocalInstruction li => new { opCode = "ldloc", operand = new { localName = li.LocalName } },
            LoadConstantInstruction li => new { opCode = "ldc", operand = new { value = li.Value?.ToString() ?? "null", type = li.Type.GetQualifiedName() } },
            LoadNullInstruction => new { opCode = "ldnull" },
            StoreArgInstruction si => new { opCode = "starg", operand = new { argumentName = si.ArgumentName } },
            StoreLocalInstruction si => new { opCode = "stloc", operand = new { localName = si.LocalName } },
            ArithmeticInstruction ai => new { opCode = MapArithmeticOp(ai.Operation) },
            ComparisonInstruction ci => new { opCode = MapComparisonOp(ci.Operation) },
            CallInstruction ci => new { opCode = "call", operand = new { method = new { declaringType = ci.Method.DeclaringType.GetQualifiedName(), name = ci.Method.Name, returnType = ci.Method.ReturnType.GetQualifiedName(), parameterTypes = ci.Method.ParameterTypes.Select(p => p.GetQualifiedName()).ToArray() } } },
            CallVirtualInstruction ci => new { opCode = "callvirt", operand = new { method = new { declaringType = ci.Method.DeclaringType.GetQualifiedName(), name = ci.Method.Name, returnType = ci.Method.ReturnType.GetQualifiedName(), parameterTypes = ci.Method.ParameterTypes.Select(p => p.GetQualifiedName()).ToArray() } } },
            NewObjectInstruction ni => new { opCode = "newobj", operand = new { type = ni.Type.GetQualifiedName() } },
            NewArrayInstruction ni => new { opCode = "newarr", operand = new { elementType = ni.ElementType.GetQualifiedName() } },
            CastInstruction ci => new { opCode = "cast", operand = new { targetType = ci.TargetType.GetQualifiedName() } },
            ReturnInstruction => new { opCode = "ret" },
            IfInstruction ii => new { opCode = "if", operand = new { thenBlock = SerializeInstructions(ii.ThenBlock), elseBlock = ii.ElseBlock != null ? SerializeInstructions(ii.ElseBlock) : null } },
            WhileInstruction wi => new { opCode = "while", operand = new { condition = "present", body = SerializeInstructions(wi.Body) } },
            TryInstruction ti => new { opCode = "try", operand = new { tryBlock = SerializeInstructions(ti.TryBlock), catchClauses = ti.CatchClauses.Select(c => new { exceptionType = c.ExceptionType?.GetQualifiedName(), body = SerializeInstructions(c.Body) }).ToArray(), finallyBlock = ti.FinallyBlock != null ? SerializeInstructions(ti.FinallyBlock) : null } },
            ThrowInstruction => new { opCode = "throw" },
            LoadElementInstruction => new { opCode = "ldelem" },
            StoreElementInstruction => new { opCode = "stelem" },
            UnaryNegateInstruction => new { opCode = "neg" },
            UnaryNotInstruction => new { opCode = "not" },
            DupInstruction => new { opCode = "dup" },
            PopInstruction => new { opCode = "pop" },
            LoadFieldInstruction lfi => new { opCode = "ldfld", operand = new { field = new { declaringType = lfi.Field.DeclaringType.GetQualifiedName(), name = lfi.Field.Name, type = lfi.Field.FieldType.GetQualifiedName() } } },
            LoadStaticFieldInstruction lsfi => new { opCode = "ldsfld", operand = new { field = new { declaringType = lsfi.Field.DeclaringType.GetQualifiedName(), name = lsfi.Field.Name, type = lsfi.Field.FieldType.GetQualifiedName() } } },
            StoreFieldInstruction sfi => new { opCode = "stfld", operand = new { field = new { declaringType = sfi.Field.DeclaringType.GetQualifiedName(), name = sfi.Field.Name, type = sfi.Field.FieldType.GetQualifiedName() } } },
            StoreStaticFieldInstruction ssfi => new { opCode = "stsfld", operand = new { field = new { declaringType = ssfi.Field.DeclaringType.GetQualifiedName(), name = ssfi.Field.Name, type = ssfi.Field.FieldType.GetQualifiedName() } } },
            _ => new { opCode = instr.GetType().Name.Replace("Instruction", "").ToLowerInvariant() }
        };
    }

    private string MapArithmeticOp(ArithmeticOp op) => op switch
    {
        ArithmeticOp.Add => "add",
        ArithmeticOp.Sub => "sub",
        ArithmeticOp.Mul => "mul",
        ArithmeticOp.Div => "div",
        ArithmeticOp.Rem => "rem",
        _ => throw new ArgumentException($"Unknown arithmetic op: {op}")
    };

    private string MapComparisonOp(ComparisonOp op) => op switch
    {
        ComparisonOp.Equal => "ceq",
        ComparisonOp.NotEqual => "cne",
        ComparisonOp.Greater => "cgt",
        ComparisonOp.GreaterOrEqual => "cge",
        ComparisonOp.Less => "clt",
        ComparisonOp.LessOrEqual => "cle",
        _ => throw new ArgumentException($"Unknown comparison op: {op}")
    };

    /// <summary>
    /// Writes a type definition to the text output
    /// </summary>
    private void WriteTypeDefinition(StreamWriter writer, TypeDefinition type)
    {
        writer.WriteLine($"  {type.Kind}: {type.GetQualifiedName()}");
        writer.WriteLine($"    Access: {type.Access}");

        if (type is ClassDefinition classDef)
        {
            if (classDef.BaseType != null)
                writer.WriteLine($"    Base: {classDef.BaseType.GetQualifiedName()}");

            if (classDef.Interfaces.Count > 0)
                writer.WriteLine($"    Interfaces: {string.Join(", ", classDef.Interfaces.Select(i => i.GetQualifiedName()))}");

            if (classDef.Fields.Count > 0)
            {
                writer.WriteLine($"    Fields ({classDef.Fields.Count}):");
                foreach (var field in classDef.Fields)
                    writer.WriteLine($"      {field.Access} {field.Type.GetQualifiedName()} {field.Name}");
            }

            if (classDef.Methods.Count > 0)
            {
                writer.WriteLine($"    Methods ({classDef.Methods.Count}):");
                foreach (var method in classDef.Methods)
                {
                    var paramList = string.Join(", ", method.Parameters.Select(p => $"{p.Type.GetQualifiedName()} {p.Name}"));
                    writer.WriteLine($"      {method.ReturnType.GetQualifiedName()} {method.Name}({paramList})");
                }
            }

            if (classDef.Properties.Count > 0)
            {
                writer.WriteLine($"    Properties ({classDef.Properties.Count}):");
                foreach (var prop in classDef.Properties)
                    writer.WriteLine($"      {prop.Type.GetQualifiedName()} {prop.Name}");
            }
        }
        else if (type is InterfaceDefinition interfaceDef)
        {
            if (interfaceDef.BaseInterfaces.Count > 0)
                writer.WriteLine($"    Base Interfaces: {string.Join(", ", interfaceDef.BaseInterfaces.Select(i => i.GetQualifiedName()))}");

            if (interfaceDef.Methods.Count > 0)
            {
                writer.WriteLine($"    Methods ({interfaceDef.Methods.Count}):");
                foreach (var method in interfaceDef.Methods)
                {
                    var paramList = string.Join(", ", method.Parameters.Select(p => $"{p.Type.GetQualifiedName()} {p.Name}"));
                    writer.WriteLine($"      {method.ReturnType.GetQualifiedName()} {method.Name}({paramList})");
                }
            }
        }
    }

    /// <summary>
    /// Reports compilation errors
    /// </summary>
    private void ReportCompilationErrors(IReadOnlyList<CompilationError> errors)
    {
        foreach (var error in errors)
        {
            _errors.Add(error.ToString());
        }
        ReportErrors();
    }

    /// <summary>
    /// Reports all accumulated errors and warnings
    /// </summary>
    private void ReportErrors()
    {
        foreach (var error in _errors)
        {
            Console.Error.WriteLine($"error: {error}");
        }

        foreach (var warning in _warnings)
        {
            if (_options.Verbosity >= VerbosityLevel.Normal)
                Console.WriteLine($"warning: {warning}");
        }

        _options.ErrorCount = _errors.Count;
        _options.WarningCount = _warnings.Count;
    }

    /// <summary>
    /// Prints compilation summary
    /// </summary>
    private void PrintCompilationSummary(Module module)
    {
        Console.WriteLine();
        Console.WriteLine("Compilation Summary:");
        Console.WriteLine($"  Module: {module.Name} v{module.Version}");
        Console.WriteLine($"  Types: {module.Types.Count}");
        Console.WriteLine($"  Output: {_options.OutputFormat}");

        if (_options.Verbosity >= VerbosityLevel.Verbose)
        {
            var totalMembers = module.Types.OfType<ClassDefinition>().Sum(c => c.Fields.Count + c.Methods.Count);
            Console.WriteLine($"  Total Members: {totalMembers}");
        }

        Console.WriteLine($"  Status: {(_errors.Count == 0 ? "✓ Success" : "✗ Failed")}");

        if (_errors.Count > 0)
            Console.WriteLine($"  Errors: {_errors.Count}");
        if (_warnings.Count > 0)
            Console.WriteLine($"  Warnings: {_warnings.Count}");
    }

    /// <summary>
    /// Gets the final exit code
    /// </summary>
    public int GetExitCode()
    {
        if (_errors.Count > 0)
            return 1;
        if (_warnings.Count > 0 && _options.WarningsAsErrors)
            return 2;
        return 0;
    }
}
