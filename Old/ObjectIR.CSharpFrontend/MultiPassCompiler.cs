using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using ObjectIR.Core.IR;

namespace ObjectIR.CSharpFrontend;

/// <summary>
/// Multi-pass compiler that handles circular dependencies and self-hosting scenarios.
/// 
/// Strategy:
/// 1. First pass: Compile with available references, collect unresolved types
/// 2. Second pass: Load additional assemblies to resolve collected types
/// 3. Third pass: Re-compile with all dependencies available
/// 
/// This enables self-hosting where the compiler can compile itself.
/// </summary>
public class MultiPassCompiler
{
    private readonly string _sourceCode;
    private readonly string _filePath;
    private List<CompilationError> _errors = new();
    private SyntaxTree? _syntaxTree;

    public MultiPassCompiler(string sourceCode, string filePath = "<input>")
    {
        _sourceCode = sourceCode ?? throw new ArgumentNullException(nameof(sourceCode));
        _filePath = filePath;
    }

    /// <summary>
    /// Performs multi-pass compilation to resolve circular dependencies.
    /// Returns compilation info if successful, null if errors.
    /// </summary>
    public RoslynCompilationInfo? CompileWithDependencyResolution(IEnumerable<string>? additionalAssemblyNames = null)
    {
        // Pass 1: Initial compilation with basic references
        var pass1Info = CompilePass1();
        if (pass1Info == null)
            return null;

        // Collect unresolved type references from diagnostics
        var unresolvedTypes = CollectUnresolvedTypes(pass1Info.Compilation);
        
        if (unresolvedTypes.Count == 0)
        {
            // No unresolved types, we're done
            return pass1Info;
        }

        // Pass 2: Load assemblies that might contain the unresolved types
        var additionalReferences = LoadAdditionalAssemblies(unresolvedTypes, additionalAssemblyNames);
        
        if (additionalReferences.Count == 0)
        {
            // No additional assemblies found, return pass 1 result
            return pass1Info;
        }

        // Pass 3: Re-compile with additional references
        var pass3Info = CompilePass3(additionalReferences);
        return pass3Info;
    }

    /// <summary>
    /// Pass 1: Initial compilation with standard references
    /// </summary>
    private RoslynCompilationInfo? CompilePass1()
    {
        try
        {
            _syntaxTree = CSharpSyntaxTree.ParseText(_sourceCode, path: _filePath);
            var diagnostics = _syntaxTree.GetDiagnostics();

            // Collect parse errors
            foreach (var diagnostic in diagnostics)
            {
                if (diagnostic.IsWarningAsError || diagnostic.Severity == DiagnosticSeverity.Error)
                {
                    var location = diagnostic.Location;
                    _errors.Add(new CompilationError(
                        diagnostic.GetMessage(),
                        location.GetLineSpan().StartLinePosition.Line + 1,
                        location.GetLineSpan().StartLinePosition.Character + 1
                    ));
                }
            }

            if (_errors.Count > 0)
                return null;

            var unit = (CompilationUnitSyntax)_syntaxTree.GetRoot();
            var references = BuildStandardReferences();

            var compilation = CSharpCompilation.Create("ObjectIRCompilation")
                .WithOptions(new CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary))
                .AddReferences(references)
                .AddSyntaxTrees(_syntaxTree);

            var semanticModel = compilation.GetSemanticModel(_syntaxTree);

            return new RoslynCompilationInfo
            {
                CompilationUnit = unit,
                Compilation = compilation,
                SemanticModel = semanticModel,
                Tree = _syntaxTree
            };
        }
        catch (Exception ex)
        {
            _errors.Add(new CompilationError($"Pass 1 compilation error: {ex.Message}", 1, 1));
            return null;
        }
    }

    /// <summary>
    /// Collects types that are unresolved in the compilation diagnostics
    /// </summary>
    private HashSet<string> CollectUnresolvedTypes(CSharpCompilation compilation)
    {
        var unresolvedTypes = new HashSet<string>();
        var diagnostics = compilation.GetDiagnostics();

        foreach (var diagnostic in diagnostics)
        {
            // Look for namespace/type resolution errors
            if (diagnostic.Id == "CS0234" || diagnostic.Id == "CS0246" || diagnostic.Id == "CS0246")
            {
                var msg = diagnostic.GetMessage();
                
                // Extract type name from error message
                // "The type or namespace name 'X' could not be found"
                if (msg.Contains("'") && msg.Contains("'"))
                {
                    var start = msg.IndexOf("'") + 1;
                    var end = msg.LastIndexOf("'");
                    if (start < end)
                    {
                        var typeName = msg.Substring(start, end - start);
                        unresolvedTypes.Add(typeName);
                    }
                }
            }
        }

        return unresolvedTypes;
    }

    /// <summary>
    /// Attempts to load assemblies that contain the unresolved types
    /// </summary>
    private List<MetadataReference> LoadAdditionalAssemblies(HashSet<string> unresolvedTypes, IEnumerable<string>? additionalNames = null)
    {
        var references = new List<MetadataReference>();
        var candidateAssemblies = new HashSet<string>();

        // Add explicit assembly names
        if (additionalNames != null)
        {
            foreach (var name in additionalNames)
                candidateAssemblies.Add(name);
        }

        // Common assemblies that might contain unresolved types
        var commonAssemblies = new[]
        {
            "ObjectIR.Core",
            "ObjectIR.CSharpFrontend",
            "System.Collections",
            "System.Collections.Immutable",
            "System.Runtime",
        };

        foreach (var asm in commonAssemblies)
            candidateAssemblies.Add(asm);

        // Try to load each candidate assembly
        foreach (var assemblyName in candidateAssemblies)
        {
            try
            {
                var assembly = Assembly.Load(assemblyName);
                references.Add(MetadataReference.CreateFromFile(assembly.Location));
            }
            catch
            {
                // Assembly not available, skip
            }
        }

        return references;
    }

    /// <summary>
    /// Pass 3: Re-compilation with additional references
    /// </summary>
    private RoslynCompilationInfo? CompilePass3(List<MetadataReference> additionalReferences)
    {
        try
        {
            if (_syntaxTree == null)
                return null;

            var baseReferences = BuildStandardReferences();
            var allReferences = baseReferences.Concat(additionalReferences).Distinct().ToList();

            var compilation = CSharpCompilation.Create("ObjectIRCompilation")
                .WithOptions(new CSharpCompilationOptions(OutputKind.DynamicallyLinkedLibrary))
                .AddReferences(allReferences)
                .AddSyntaxTrees(_syntaxTree);

            var semanticModel = compilation.GetSemanticModel(_syntaxTree);

            // Collect errors, but allow for some unresolved types in self-hosting scenarios
            var diagnostics = compilation.GetDiagnostics();
            foreach (var diagnostic in diagnostics)
            {
                if (diagnostic.Severity == DiagnosticSeverity.Error)
                {
                    // Skip entry point errors
                    if (diagnostic.Id == "CS5001")
                        continue;

                    var location = diagnostic.Location;
                    _errors.Add(new CompilationError(
                        diagnostic.GetMessage(),
                        location.GetLineSpan().StartLinePosition.Line + 1,
                        location.GetLineSpan().StartLinePosition.Character + 1
                    ));
                }
            }

            // For self-hosting, we might accept the result even with some unresolved types
            // if they're namespace-related (the namespace exists, just not all types)
            var criticalErrors = _errors.Where(e => !e.Message.Contains("namespace")).Count();
            if (criticalErrors > 0)
                return null;

            var unit = (CompilationUnitSyntax)_syntaxTree.GetRoot();

            return new RoslynCompilationInfo
            {
                CompilationUnit = unit,
                Compilation = compilation,
                SemanticModel = semanticModel,
                Tree = _syntaxTree
            };
        }
        catch (Exception ex)
        {
            _errors.Add(new CompilationError($"Pass 3 compilation error: {ex.Message}", 1, 1));
            return null;
        }
    }

    /// <summary>
    /// Builds the standard set of references available in all passes
    /// </summary>
    private List<MetadataReference> BuildStandardReferences()
    {
        var references = new List<MetadataReference>();
        
        var assembliesToAdd = new[]
        {
            typeof(object),
            typeof(Enumerable),
            typeof(Console),
            typeof(System.IO.File),
            typeof(System.Collections.Generic.List<>),
            typeof(System.Text.StringBuilder),
            typeof(System.Collections.Stack),
            typeof(System.Collections.Hashtable),
            typeof(Microsoft.CodeAnalysis.CSharp.CSharpCompilation),
            typeof(Microsoft.CodeAnalysis.Compilation),
            typeof(System.Text.Json.JsonSerializer),
            typeof(System.Collections.Immutable.ImmutableArray),
        };

        foreach (var type in assembliesToAdd)
        {
            try
            {
                var assembly = type.Assembly;
                references.Add(MetadataReference.CreateFromFile(assembly.Location));
            }
            catch { }
        }

        // Try to add System.Runtime
        try
        {
            var runtimeAssembly = Assembly.Load("System.Runtime");
            references.Add(MetadataReference.CreateFromFile(runtimeAssembly.Location));
        }
        catch { }

        return references;
    }

    /// <summary>
    /// Gets all compilation errors encountered across all passes
    /// </summary>
    public IReadOnlyList<CompilationError> GetErrors() => _errors.AsReadOnly();
}
