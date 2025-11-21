using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using ObjectIR.Core.IR;
using ObjectIR.Core.Serialization;
using ObjectIR.Core.Builder;
using ObjectIR.CSharpBackend;
using ObjectIR.Examples;
using ObjectIR.Runtime.Wrapper;

namespace ObjectIR.UnifiedRuntime;

/// <summary>
/// Unified ObjectIR runtime that combines:
/// 1. Construct compiler (C# backend code generation)
/// 2. ObjectIR Module compilation
/// 3. C++ Runtime execution
/// 
/// This tool provides an end-to-end pipeline from high-level ObjectIR definitions
/// to execution within the C++ runtime.
/// </summary>
class Program
{
    public static bool Debug = false;

    public static void Log(string message)
    {
        if (Debug)
        {
            Console.WriteLine($"[ObjectIR-CSharp] {message}");
        }
    }

    static void Main(string[] args)
    {
        try
        {
            if (args.Length == 0)
            {
                PrintBanner();
                ShowUsage();
                ShowExamples();
                return;
            }

            // Global flags
            if (args.Contains("--help") || args.Contains("-h"))
            {
                PrintBanner();
                ShowUsage();
                ShowExamples();
                return;
            }
            if (args.Contains("--version"))
            {
                PrintBanner();
                Console.WriteLine("ObjectIR Unified Runtime v1.0.0");
                return;
            }

            string command = args[0].ToLower();

            switch (command)
            {
                case "debug":
                    Debug = true;
                    Console.WriteLine("Debug mode enabled.");
                    break;

                case "run":
                    if (args.Length < 2)
                    {
                        Console.ForegroundColor = ConsoleColor.Red;
                        Console.Error.WriteLine("Error: 'run' command requires a module file (.json or .fob).");
                        Console.ResetColor();
                        ShowUsage();
                        return;
                    }
                    var invocationArgs = args.Length > 2 ? args[2..] : Array.Empty<string>();
                    RunModuleInCppRuntime(args[1], invocationArgs);
                    break;

                case "compile":
                    if (args.Length < 2)
                    {
                        Console.ForegroundColor = ConsoleColor.Red;
                        Console.Error.WriteLine("Error: 'compile' command requires a module JSON file.");
                        Console.ResetColor();
                        ShowUsage();
                        return;
                    }
                    CompileModuleToCSharp(args[1]);
                    break;

                case "build-and-run":
                    if (args.Length < 2)
                    {
                        Console.ForegroundColor = ConsoleColor.Red;
                        Console.Error.WriteLine("Error: 'build-and-run' command requires an example name.");
                        Console.ResetColor();
                        ShowUsage();
                        ShowExamples();
                        return;
                    }
                    BuildExampleAndRun(args[1]);
                    break;

                case "build":
                    if (args.Length < 2)
                    {
                        Console.ForegroundColor = ConsoleColor.Red;
                        Console.Error.WriteLine("Error: 'build' command requires an example name.");
                        Console.ResetColor();
                        ShowUsage();
                        ShowExamples();
                        return;
                    }
                    BuildExampleToJson(args[1]);
                    break;

                case "pipeline":
                    if (args.Length < 2)
                    {
                        Console.ForegroundColor = ConsoleColor.Red;
                        Console.Error.WriteLine("Error: 'pipeline' command requires an example name.");
                        Console.ResetColor();
                        ShowUsage();
                        ShowExamples();
                        return;
                    }
                    RunFullPipeline(args[1]);
                    break;

                case "help":
                case "-h":
                case "--help":
                    PrintBanner();
                    ShowUsage();
                    ShowExamples();
                    break;

                default:
                    Console.ForegroundColor = ConsoleColor.Yellow;
                    Console.Error.WriteLine($"Unknown command: {command}");
                    Console.ResetColor();
                    ShowUsage();
                    ShowExamples();
                    SuggestClosestCommand(command);
                    break;
            }
        }
        catch (Exception ex)
        {
            Console.ForegroundColor = ConsoleColor.Red;
            Console.Error.WriteLine($"Error: {ex.Message}");
            Console.ResetColor();
            if (args.Contains("--verbose"))
            {
                Console.Error.WriteLine(ex.StackTrace);
            }
            Environment.Exit(1);
        }
    }

    static void PrintBanner()
    {
        Console.ForegroundColor = ConsoleColor.Cyan;
        Console.WriteLine("\n=== ObjectIR Unified Runtime ===\n");
        Console.ResetColor();
    }

    static void SuggestClosestCommand(string input)
    {
        var commands = new[] { "run", "compile", "build", "build-and-run", "pipeline", "help" };
        string? suggestion = null;
        int minDistance = int.MaxValue;
        foreach (var cmd in commands)
        {
            int dist = LevenshteinDistance(input, cmd);
            if (dist < minDistance)
            {
                minDistance = dist;
                suggestion = cmd;
            }
        }
        if (minDistance <= 3 && suggestion != null)
        {
            Console.ForegroundColor = ConsoleColor.Green;
            Console.WriteLine($"Did you mean '{suggestion}'?");
            Console.ResetColor();
        }
    }

    static int LevenshteinDistance(string a, string b)
    {
        if (string.IsNullOrEmpty(a)) return b.Length;
        if (string.IsNullOrEmpty(b)) return a.Length;
        int[,] d = new int[a.Length + 1, b.Length + 1];
        for (int i = 0; i <= a.Length; i++) d[i, 0] = i;
        for (int j = 0; j <= b.Length; j++) d[0, j] = j;
        for (int i = 1; i <= a.Length; i++)
        {
            for (int j = 1; j <= b.Length; j++)
            {
                int cost = a[i - 1] == b[j - 1] ? 0 : 1;
                d[i, j] = Math.Min(
                    Math.Min(d[i - 1, j] + 1, d[i, j - 1] + 1),
                    d[i - 1, j - 1] + cost);
            }
        }
        return d[a.Length, b.Length];
    }

    /// <summary>
    /// Run a compiled module directly in the C++ runtime
    /// </summary>
    static void RunModuleInCppRuntime(string modulePath, IReadOnlyList<string>? invocationArgs = null)
    {
        Log($"[ObjectIR] Loading module from: {modulePath}");

        if (!File.Exists(modulePath))
            throw new FileNotFoundException($"Module file not found: {modulePath}");

        // Check if this is a FOB file
        if (IsFOBFile(modulePath))
        {
            ExecuteFOBModule(modulePath, invocationArgs ?? Array.Empty<string>());
        }
        else
        {
            // Assume JSON format
            string jsonContent = File.ReadAllText(modulePath);
            Module module = ModuleSerializer.LoadFromJson(jsonContent);
            ExecuteModule(module, jsonContent, invocationArgs ?? Array.Empty<string>());
        }
    }

    /// <summary>
    /// Check if a file is in FOB format by reading the first 3 bytes
    /// </summary>
    static bool IsFOBFile(string filePath)
    {
        try
        {
            using var stream = File.OpenRead(filePath);
            if (stream.Length < 3) return false;
            
            byte[] magic = new byte[3];
            int bytesRead = stream.Read(magic, 0, 3);
            if (bytesRead < 3) return false;
            
            return magic[0] == 'F' && magic[1] == 'O' && magic[2] == 'B';
        }
        catch
        {
            return false;
        }
    }

    /// <summary>
    /// Execute a FOB module directly in the C++ runtime
    /// </summary>
    static void ExecuteFOBModule(string fobPath, IReadOnlyList<string> invocationArgs)
    {
        Log($"[ObjectIR-CSharp] Reading FOB module: {fobPath}");
        Log($"[ObjectIR-CSharp] Initializing C++ runtime...");
        using var runtime = new RuntimeWrapper();
        Log($"[ObjectIR-CSharp] Loading FOB module into C++ runtime");
        
        try
        {
            var (entryClassName, entryMethodName) = runtime.LoadFOBModuleFromFile(fobPath);
            
            if (string.IsNullOrEmpty(entryClassName) || string.IsNullOrEmpty(entryMethodName))
            {
                Console.WriteLine("[ObjectIR] FOB module loaded but no valid entry point found in header.");
                PrintRuntimeInfo(Path.GetFileNameWithoutExtension(fobPath), (int)new FileInfo(fobPath).Length, "Loaded");
                return;
            }

            Log($"[ObjectIR-CSharp] FOB module loaded successfully");
            Log($"[ObjectIR-CSharp] Entry point: {entryClassName}.{entryMethodName}");

            // Execute the entry point method
            try
            {
                runtime.InvokeMethod(entryClassName, entryMethodName, null);
                Log($"[ObjectIR-CSharp] Executed {entryClassName}.{entryMethodName} successfully");
                PrintRuntimeInfo(Path.GetFileNameWithoutExtension(fobPath), (int)new FileInfo(fobPath).Length, "Completed");
            }
            catch (Exception ex)
            {
                Console.WriteLine($"[ERROR] Failed to execute entry point {entryClassName}.{entryMethodName}: {ex.Message}");
                PrintRuntimeInfo(Path.GetFileNameWithoutExtension(fobPath), (int)new FileInfo(fobPath).Length, "Failed");
                throw;
            }
        }
        catch (Exception ex)
        {
            Console.WriteLine($"[ERROR] Failed to load FOB module: {ex.Message}");
            PrintRuntimeInfo(Path.GetFileNameWithoutExtension(fobPath), (int)new FileInfo(fobPath).Length, "Failed");
            throw;
        }
    }

    /// <summary>
    /// Compile a module to C# code
    /// </summary>
    static void CompileModuleToCSharp(string modulePath)
    {
        Log($"[ObjectIR] Compiling module to C# from: {modulePath}");

        if (!File.Exists(modulePath))
            throw new FileNotFoundException($"Module file not found: {modulePath}");

        if (IsFOBFile(modulePath))
        {
            throw new InvalidOperationException("Cannot compile FOB files to C#. FOB files are already compiled binary modules.");
        }

        string json = File.ReadAllText(modulePath);
        Module module = ModuleSerializer.LoadFromJson(json);

        CSharpCodeGenerator generator = new CSharpCodeGenerator();
        string code = generator.Generate(module);

        string outputFile = $"{module.Name}.cs";
        File.WriteAllText(outputFile, code);

        Log($"[ObjectIR] Generated C# code written to: {outputFile}");
        Log($"[ObjectIR] File size: {new FileInfo(outputFile).Length} bytes");
    }

    /// <summary>
    /// Build an example and run it in the C++ runtime
    /// </summary>
    static void BuildExampleAndRun(string exampleName)
    {
        Log($"[ObjectIR-CSharp] Building example: {exampleName}");

        Module module = BuildExample(exampleName);
        string moduleName = module.Name;

        // Serialize to FOB
        Log($"[ObjectIR-CSharp] Serializing module to FOB");
        byte[] fobData = module.DumpFob();
        string fobFile = $"{moduleName}.fob";
        File.WriteAllBytes(fobFile, fobData);

        Log($"[ObjectIR-CSharp] Module serialized: {fobFile} ({new FileInfo(fobFile).Length} bytes)");

        // Run in C++ runtime
        Log($"[ObjectIR-CSharp] Executing in C++ runtime...\n");
        RunModuleInCppRuntime(fobFile, Array.Empty<string>());
    }

    /// <summary>
    /// Build an example to FOB (without execution)
    /// </summary>
    static void BuildExampleToJson(string exampleName)
    {
        Log($"[ObjectIR-CSharp] Building example: {exampleName}");

        Module module = BuildExample(exampleName);
        string moduleName = module.Name;

        Log($"[ObjectIR-CSharp] Serializing module to FOB");
        byte[] fobData = module.DumpFob();
        string fobFile = $"{moduleName}.fob";
        File.WriteAllBytes(fobFile, fobData);

        Log($"[ObjectIR-CSharp] Module saved: {fobFile} ({new FileInfo(fobFile).Length} bytes)");
        Log($"[ObjectIR-CSharp] Module '{moduleName}' is ready for compilation or execution");
    }

    /// <summary>
    /// Run the complete pipeline: build -> serialize -> codegen -> execute
    /// </summary>
    static void RunFullPipeline(string exampleName)
    {
        Console.WriteLine($"\n{'='} ObjectIR Unified Pipeline {'='}\n");

        // Step 1: Build
        Console.WriteLine($"[Step 1/4] Building example: {exampleName}");
        Module module = BuildExample(exampleName);
        string moduleName = module.Name;
        Console.WriteLine($"  ✓ Module built with {module.Types.Count} types");

        // Step 2: Serialize
        Console.WriteLine($"\n[Step 2/4] Serializing to FOB");
        byte[] fobData = module.DumpFob();
        string fobFile = $"{moduleName}.fob";
        File.WriteAllBytes(fobFile, fobData);
        Console.WriteLine($"  ✓ Serialized: {fobFile} ({new FileInfo(fobFile).Length} bytes)");

        // Step 3: Code Generation
        Console.WriteLine($"\n[Step 3/4] Generating C# code");
        CSharpCodeGenerator generator = new CSharpCodeGenerator();
        string code = generator.Generate(module);
        string csFile = $"{moduleName}.generated.cs";
        File.WriteAllText(csFile, code);
        Console.WriteLine($"  ✓ Generated: {csFile} ({new FileInfo(csFile).Length} bytes)");

        // Step 4: Execute in C++ Runtime
        Console.WriteLine($"\n[Step 4/4] Executing in C++ runtime");
        RunModuleInCppRuntime(fobFile, Array.Empty<string>());

        Console.WriteLine($"\n{'='} Pipeline Complete {'='}\n");
        Console.WriteLine($"Output files:");
        Console.WriteLine($"  - {fobFile} (ObjectIR FOB)");
        Console.WriteLine($"  - {csFile} (Generated C# code)");
    }

    /// <summary>
    /// Build a specific example
    /// </summary>
    static Module BuildExample(string exampleName)
    {
        return exampleName.ToLower() switch
        {
            "calculator" => new CalculatorExampleBuilder().Build(),
            "todoapp" => new TodoAppExampleBuilder().Build(),
            "modulecomposition" => new ModuleCompositionExampleBuilder().Build(),
            "moduleloader" => new ModuleLoaderExampleBuilder().Build(),
            _ => throw new ArgumentException($"Unknown example: {exampleName}\nAvailable examples: calculator, todoapp, modulecomposition, moduleloader"),
        };
    }

    private static void ExecuteModule(Module module, string jsonContent, IReadOnlyList<string> invocationArgs)
    {
        Log($"[ObjectIR-CSharp] Reading module: {module.Name}");
        Log($"[ObjectIR-CSharp] Initializing C++ runtime...");
        using var runtime = new RuntimeWrapper();
        Log($"[ObjectIR-CSharp] Loading module into C++ runtime");
        
        try
        {
            runtime.LoadModuleFromString(jsonContent);
        }
        catch (Exception ex)
        {
            Console.WriteLine($"[ERROR] Failed to load module: {ex.Message}");
            PrintRuntimeInfo(module.Name, jsonContent.Length, "Failed");
            throw;
        }
        
        Log($"[ObjectIR-CSharp] Module loaded successfully");

        bool entryFound = TryFindEntryPoint(module, out var entryPoint);
        string status = entryFound ? "Failed" : "Loaded";

        if (!entryFound)
        {
            Console.WriteLine("[ObjectIR] No entry point found. Module loaded but not executed.");
            PrintRuntimeInfo(module.Name, jsonContent.Length, status);
            return;
        }

        object?[] invocationValues;
        try
        {
            invocationValues = BuildInvocationArguments(entryPoint, invocationArgs);
        }
        catch
        {
            PrintRuntimeInfo(module.Name, jsonContent.Length, status);
            throw;
        }

        RuntimeObject? instance = null;
        try
        {
            if (!entryPoint.IsStatic)
            {
                instance = runtime.CreateInstance(entryPoint.ClassName);
            }

            Log($"[ObjectIR-CSharp] Executing entry point: {entryPoint.DisplayName}");

            using RuntimeValue result = runtime.InvokeMethod(
                entryPoint.ClassName,
                entryPoint.Method.Name,
                instance);
    
            if (!entryPoint.ReturnsVoid)
            {
                Log($"[ObjectIR-CSharp] Entry point returned: {result}");
            }

            status = "Completed";
            Console.WriteLine("[ObjectIR] Execution completed successfully");
        }
        finally
        {
            instance?.Dispose();
            PrintRuntimeInfo(module.Name, jsonContent.Length, status);
        }
    }

    private static bool TryFindEntryPoint(Module module, out EntryPointInfo entryPoint)
    {
        if (module.Metadata.TryGetValue("EntryPoint", out var entryMetadata) &&
            entryMetadata is string entryString &&
            TryParseEntryPointMetadata(module, entryString, out entryPoint))
        {
            return true;
        }

        foreach (var type in module.Types)
        {
            if (type is not ClassDefinition classDef)
                continue;

            foreach (var method in classDef.Methods)
            {
                if (method.IsStatic && string.Equals(method.Name, "Main", StringComparison.OrdinalIgnoreCase))
                {
                    entryPoint = new EntryPointInfo(classDef.GetQualifiedName(), method);
                    return true;
                }
            }
        }

        entryPoint = default!;
        return false;
    }

    private static bool TryParseEntryPointMetadata(Module module, string metadata, out EntryPointInfo entryPoint)
    {
        if (string.IsNullOrWhiteSpace(metadata))
        {
            entryPoint = default!;
            return false;
        }

        string trimmed = metadata.Trim();
        string className;
        string methodName;

        int separatorIndex = trimmed.IndexOf("::", StringComparison.Ordinal);
        if (separatorIndex >= 0)
        {
            className = trimmed[..separatorIndex].Trim();
            methodName = trimmed[(separatorIndex + 2)..].Trim();
        }
        else
        {
            int lastDot = trimmed.LastIndexOf('.');
            if (lastDot < 0)
            {
                entryPoint = default!;
                return false;
            }

            className = trimmed[..lastDot].Trim();
            methodName = trimmed[(lastDot + 1)..].Trim();
        }

        if (string.IsNullOrEmpty(className) || string.IsNullOrEmpty(methodName))
        {
            entryPoint = default!;
            return false;
        }

        ClassDefinition? classDef = null;
        foreach (var type in module.Types)
        {
            if (type is ClassDefinition candidate &&
                string.Equals(candidate.GetQualifiedName(), className, StringComparison.OrdinalIgnoreCase))
            {
                classDef = candidate;
                break;
            }
        }

        if (classDef is null)
        {
            entryPoint = default!;
            return false;
        }

        foreach (var method in classDef.Methods)
        {
            if (string.Equals(method.Name, methodName, StringComparison.OrdinalIgnoreCase))
            {
                entryPoint = new EntryPointInfo(classDef.GetQualifiedName(), method);
                return true;
            }
        }

        entryPoint = default!;
        return false;
    }

    private static object?[] BuildInvocationArguments(EntryPointInfo entryPoint, IReadOnlyList<string> invocationArgs)
    {
        int expected = entryPoint.Parameters.Count;
        if (expected == 0)
        {
            if (invocationArgs.Count > 0)
            {
                throw new ArgumentException($"Entry point does not accept parameters, but {invocationArgs.Count} argument(s) were provided.");
            }

            return Array.Empty<object?>();
        }

        if (invocationArgs.Count != expected)
        {
            throw new ArgumentException($"Entry point expects {expected} argument(s), but {invocationArgs.Count} were provided.");
        }

        var values = new object?[expected];
        for (int i = 0; i < expected; ++i)
        {
            var parameter = entryPoint.Parameters[i];
            string rawValue = invocationArgs[i];

            if (!TryConvertArgument(rawValue, parameter.Type, out var converted, out var error))
            {
                string typeName = parameter.Type.GetQualifiedName();
                string message = error is null
                    ? $"Unsupported parameter type '{typeName}' for argument {i}."
                    : $"Failed to convert argument {i} ('{rawValue}') to '{typeName}': {error}";
                throw new ArgumentException(message);
            }

            values[i] = converted;
        }

        return values;
    }

    private static bool TryConvertArgument(string rawValue, TypeReference type, out object? converted, out string? error)
    {
        string normalized = type.GetQualifiedName().ToLowerInvariant();

        switch (normalized)
        {
            case "string":
            case "system.string":
                converted = rawValue;
                error = null;
                return true;

            case "bool":
            case "system.bool":
            case "boolean":
                if (bool.TryParse(rawValue, out bool boolValue))
                {
                    converted = boolValue;
                    error = null;
                    return true;
                }
                if (rawValue == "1" || rawValue == "0")
                {
                    converted = rawValue == "1";
                    error = null;
                    return true;
                }
                converted = null;
                error = "expected a boolean value (true/false/1/0)";
                return false;

            case "int":
            case "int32":
            case "system.int32":
                if (int.TryParse(rawValue, NumberStyles.Integer, CultureInfo.InvariantCulture, out int intValue))
                {
                    converted = intValue;
                    error = null;
                    return true;
                }
                converted = null;
                error = "expected a 32-bit integer";
                return false;

            case "int64":
            case "system.int64":
            case "long":
                if (long.TryParse(rawValue, NumberStyles.Integer, CultureInfo.InvariantCulture, out long longValue))
                {
                    converted = longValue;
                    error = null;
                    return true;
                }
                converted = null;
                error = "expected a 64-bit integer";
                return false;

            case "float":
            case "single":
            case "float32":
            case "system.float32":
                if (float.TryParse(rawValue, NumberStyles.Float | NumberStyles.AllowThousands, CultureInfo.InvariantCulture, out float floatValue))
                {
                    converted = floatValue;
                    error = null;
                    return true;
                }
                converted = null;
                error = "expected a 32-bit floating point value";
                return false;

            case "double":
            case "float64":
            case "system.float64":
                if (double.TryParse(rawValue, NumberStyles.Float | NumberStyles.AllowThousands, CultureInfo.InvariantCulture, out double doubleValue))
                {
                    converted = doubleValue;
                    error = null;
                    return true;
                }
                converted = null;
                error = "expected a 64-bit floating point value";
                return false;

            case "decimal":
            case "system.decimal":
                if (decimal.TryParse(rawValue, NumberStyles.Float | NumberStyles.AllowThousands, CultureInfo.InvariantCulture, out decimal decimalValue))
                {
                    converted = decimalValue;
                    error = null;
                    return true;
                }
                converted = null;
                error = "expected a decimal value";
                return false;
        }

        converted = null;
        error = null;
        return false;
    }

    private static bool IsVoidReturn(TypeReference type)
    {
        string qualifiedName = type.GetQualifiedName();
        return string.Equals(qualifiedName, "void", StringComparison.OrdinalIgnoreCase) ||
               string.Equals(qualifiedName, "system.void", StringComparison.OrdinalIgnoreCase);
    }

    private static void PrintRuntimeInfo(string moduleName, int jsonSize, string status)
    {
        Console.WriteLine("\n[Runtime Info]");
        Console.WriteLine($"  Module: {moduleName}");
        Console.WriteLine($"  JSON Size: {jsonSize} bytes");
        Console.WriteLine($"  Status: {status}");
    }

    private sealed class EntryPointInfo
    {
        public EntryPointInfo(string className, MethodDefinition method)
        {
            ClassName = className;
            Method = method;
        }

        public string ClassName { get; }
        public MethodDefinition Method { get; }
        public bool IsStatic => Method.IsStatic;
        public IReadOnlyList<Parameter> Parameters => Method.Parameters;
        public bool ReturnsVoid => IsVoidReturn(Method.ReturnType);
        public string DisplayName => $"{ClassName}.{Method.Name}";
    }

    static void ShowUsage()
    {
                    Console.WriteLine($@"
    Usage:
        {System.Reflection.Assembly.GetExecutingAssembly().GetName().Name} <command> [options]

    Commands:
        run <module> [args...]          Execute module entry point in C++ runtime (.json or .fob)
        compile <module.json>           Compile module to C# code
        build <example>                 Build example to JSON file
        build-and-run <example>         Build example and execute in C++ runtime
        pipeline <example>              Run full pipeline: build → serialize → codegen → execute
        help                            Show this help message
        --version                       Show version information
    ");
    }

    static void ShowExamples()
    {
        Console.WriteLine(@"
Available Examples:
  calculator                     Simple calculator with arithmetic
  todoapp                        Todo application with list management
  modulecomposition              Demonstrates module composition patterns
  moduleloader                   Shows module loading capabilities
");
    }
}

/// <summary>
/// Example builders for demonstration
/// </summary>
abstract class ExampleBuilder
{
    public abstract Module Build();
}

class CalculatorExampleBuilder : ExampleBuilder
{
    public override Module Build()
    {
        // return ConstructLanguageExample.BuildCalculator();
        return TodoAppExample.BuildTodoApp();
    }
}

class TodoAppExampleBuilder : ExampleBuilder
{
    public override Module Build()
    {
        return TodoAppExample.BuildTodoApp();
    }
}

class ModuleCompositionExampleBuilder : ExampleBuilder
{
    public override Module Build()
    {
        return TodoAppExample.BuildTodoApp();
    }
}

class ModuleLoaderExampleBuilder : ExampleBuilder
{
    public override Module Build()
    {
        return TodoAppExample.BuildTodoApp();
    }
}
