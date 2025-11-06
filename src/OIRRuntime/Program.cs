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
                ShowUsage();
                return;
            }

            string command = args[0].ToLower();

            switch (command)
            {
                case "debug":
                    Debug = true;
                    break;

                case "run":
                    // run <module.json> - Execute a module in C++ runtime
                    if (args.Length < 2)
                    {
                        Console.Error.WriteLine("Error: 'run' command requires a module JSON file");
                        return;
                    }
                    var invocationArgs = args.Length > 2 ? args[2..] : Array.Empty<string>();
                    RunModuleInCppRuntime(args[1], invocationArgs);
                    break;

                case "compile":
                    // compile <module.json> - Compile module to C# code
                    if (args.Length < 2)
                    {
                        Console.Error.WriteLine("Error: 'compile' command requires a module JSON file");
                        return;
                    }
                    CompileModuleToCSharp(args[1]);
                    break;

                case "build-and-run":
                    // build-and-run <example-name> - Build example and run in C++ runtime
                    if (args.Length < 2)
                    {
                        Console.Error.WriteLine("Error: 'build-and-run' command requires an example name");
                        ShowExamples();
                        return;
                    }
                    BuildExampleAndRun(args[1]);
                    break;

                case "build":
                    // build <example-name> - Build example and save to JSON
                    if (args.Length < 2)
                    {
                        Console.Error.WriteLine("Error: 'build' command requires an example name");
                        ShowExamples();
                        return;
                    }
                    BuildExampleToJson(args[1]);
                    break;

                case "pipeline":
                    // pipeline <example-name> - Run full pipeline: build -> serialize -> codegen -> execute
                    if (args.Length < 2)
                    {
                        Console.Error.WriteLine("Error: 'pipeline' command requires an example name");
                        ShowExamples();
                        return;
                    }
                    RunFullPipeline(args[1]);
                    break;

                case "help":
                case "-h":
                case "--help":
                    ShowUsage();
                    break;

                default:
                    Console.Error.WriteLine($"Unknown command: {command}");
                    ShowUsage();
                    break;
            }
        }
        catch (Exception ex)
        {
            Console.Error.WriteLine($"Error: {ex.Message}");
            if (args.Contains("--verbose"))
            {
                Console.Error.WriteLine(ex.StackTrace);
            }
            Environment.Exit(1);
        }
    }

    /// <summary>
    /// Run a compiled module directly in the C++ runtime
    /// </summary>
    static void RunModuleInCppRuntime(string moduleJsonPath, IReadOnlyList<string>? invocationArgs = null)
    {
        Log($"[ObjectIR] Loading module from: {moduleJsonPath}");

        if (!File.Exists(moduleJsonPath))
            throw new FileNotFoundException($"Module file not found: {moduleJsonPath}");

        string jsonContent = File.ReadAllText(moduleJsonPath);
        Module module = ModuleSerializer.LoadFromJson(jsonContent);

        ExecuteModule(module, jsonContent, invocationArgs ?? Array.Empty<string>());
    }

    /// <summary>
    /// Compile a module to C# code
    /// </summary>
    static void CompileModuleToCSharp(string moduleJsonPath)
    {
        Log($"[ObjectIR] Compiling module to C# from: {moduleJsonPath}");

        if (!File.Exists(moduleJsonPath))
            throw new FileNotFoundException($"Module file not found: {moduleJsonPath}");

        string json = File.ReadAllText(moduleJsonPath);
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

        // Serialize to JSON
        Log($"[ObjectIR-CSharp] Serializing module to JSON");
        string json = ModuleSerializer.ToJson(module);
        string jsonFile = $"{moduleName}.json";
        File.WriteAllText(jsonFile, json);

        Log($"[ObjectIR-CSharp] Module serialized: {jsonFile} ({new FileInfo(jsonFile).Length} bytes)");

        // Run in C++ runtime
        Log($"[ObjectIR-CSharp] Executing in C++ runtime...\n");
    RunModuleInCppRuntime(jsonFile, Array.Empty<string>());
    }

    /// <summary>
    /// Build an example to JSON (without execution)
    /// </summary>
    static void BuildExampleToJson(string exampleName)
    {
        Log($"[ObjectIR-CSharp] Building example: {exampleName}");

        Module module = BuildExample(exampleName);
        string moduleName = module.Name;

        Log($"[ObjectIR-CSharp] Serializing module to JSON");
        string json = ModuleSerializer.ToJson(module);
        string jsonFile = $"{moduleName}.json";
        File.WriteAllText(jsonFile, json);

        Log($"[ObjectIR-CSharp] Module saved: {jsonFile} ({new FileInfo(jsonFile).Length} bytes)");
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
        Console.WriteLine($"\n[Step 2/4] Serializing to JSON");
        string json = ModuleSerializer.ToJson(module);
        string jsonFile = $"{moduleName}.json";
        File.WriteAllText(jsonFile, json);
        Console.WriteLine($"  ✓ Serialized: {jsonFile} ({new FileInfo(jsonFile).Length} bytes)");

        // Step 3: Code Generation
        Console.WriteLine($"\n[Step 3/4] Generating C# code");
        CSharpCodeGenerator generator = new CSharpCodeGenerator();
        string code = generator.Generate(module);
        string csFile = $"{moduleName}.generated.cs";
        File.WriteAllText(csFile, code);
        Console.WriteLine($"  ✓ Generated: {csFile} ({new FileInfo(csFile).Length} bytes)");

        // Step 4: Execute in C++ Runtime
        Console.WriteLine($"\n[Step 4/4] Executing in C++ runtime");
        ExecuteModule(module, json, Array.Empty<string>());

        Console.WriteLine($"\n{'='} Pipeline Complete {'='}\n");
        Console.WriteLine($"Output files:");
        Console.WriteLine($"  - {jsonFile} (ObjectIR JSON)");
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
        Console.WriteLine(@"
ObjectIR Unified Runtime - Compiler + C++ Runtime Integration

This tool combines the ObjectIR Construct compiler with the high-performance
C++ runtime, enabling end-to-end compilation and execution in a single tool.

Usage:
  objectir-unified <command> [options]

Commands:
    run <module.json> [args...]     Execute module entry point in C++ runtime
  compile <module.json>            Compile module to C# code
  build <example>                  Build example to JSON file
  build-and-run <example>          Build example and execute in C++ runtime
  pipeline <example>               Run full pipeline: build → serialize → codegen → execute
  help                             Show this help message

Examples:
    objectir-unified run TodoApp.json
    objectir-unified run main.oir
  objectir-unified build calculator
  objectir-unified build-and-run todoapp
  objectir-unified pipeline todoapp

Available Examples:
  - calculator                     Simple calculator with arithmetic
  - todoapp                        Todo application with list management
  - modulecomposition              Demonstrates module composition patterns
  - moduleloader                   Shows module loading capabilities
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
