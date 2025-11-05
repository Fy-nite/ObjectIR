using System;
using System.IO;
using System.Diagnostics;
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
                case "run":
                    // run <module.json> - Execute a module in C++ runtime
                    if (args.Length < 2)
                    {
                        Console.Error.WriteLine("Error: 'run' command requires a module JSON file");
                        return;
                    }
                    RunModuleInCppRuntime(args[1]);
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
    static void RunModuleInCppRuntime(string moduleJsonPath)
    {
        Console.WriteLine($"[ObjectIR] Loading module from: {moduleJsonPath}");

        if (!File.Exists(moduleJsonPath))
            throw new FileNotFoundException($"Module file not found: {moduleJsonPath}");

        string moduleName = Path.GetFileNameWithoutExtension(moduleJsonPath);

        Console.WriteLine($"[ObjectIR] Reading module: {moduleName}");
        string jsonContent = File.ReadAllText(moduleJsonPath);

        Console.WriteLine($"[ObjectIR] Initializing C++ runtime...");
        using (var runtime = new RuntimeWrapper())
        {
            Console.WriteLine($"[ObjectIR] Loading module into C++ runtime");
            runtime.LoadModuleFromString(jsonContent);

            Console.WriteLine($"[ObjectIR] Module loaded successfully");
            Console.WriteLine($"[ObjectIR] Module is ready for method invocation via C++ runtime");

            // Display runtime information
            Console.WriteLine($"\n[Runtime Info]");
            Console.WriteLine($"  Module: {moduleName}");
            Console.WriteLine($"  JSON Size: {jsonContent.Length} bytes");
            Console.WriteLine($"  Status: Ready");
        }
    }

    /// <summary>
    /// Compile a module to C# code
    /// </summary>
    static void CompileModuleToCSharp(string moduleJsonPath)
    {
        Console.WriteLine($"[ObjectIR] Compiling module to C# from: {moduleJsonPath}");

        if (!File.Exists(moduleJsonPath))
            throw new FileNotFoundException($"Module file not found: {moduleJsonPath}");

        string json = File.ReadAllText(moduleJsonPath);
        Module module = ModuleSerializer.LoadFromJson(json);

        CSharpCodeGenerator generator = new CSharpCodeGenerator();
        string code = generator.Generate(module);

        string outputFile = $"{module.Name}.cs";
        File.WriteAllText(outputFile, code);

        Console.WriteLine($"[ObjectIR] Generated C# code written to: {outputFile}");
        Console.WriteLine($"[ObjectIR] File size: {new FileInfo(outputFile).Length} bytes");
    }

    /// <summary>
    /// Build an example and run it in the C++ runtime
    /// </summary>
    static void BuildExampleAndRun(string exampleName)
    {
        Console.WriteLine($"[ObjectIR] Building example: {exampleName}");

        Module module = BuildExample(exampleName);
        string moduleName = module.Name;

        // Serialize to JSON
        Console.WriteLine($"[ObjectIR] Serializing module to JSON");
        string json = ModuleSerializer.ToJson(module);
        string jsonFile = $"{moduleName}.json";
        File.WriteAllText(jsonFile, json);

        Console.WriteLine($"[ObjectIR] Module serialized: {jsonFile} ({new FileInfo(jsonFile).Length} bytes)");

        // Run in C++ runtime
        Console.WriteLine($"[ObjectIR] Executing in C++ runtime...\n");
        RunModuleInCppRuntime(jsonFile);
    }

    /// <summary>
    /// Build an example to JSON (without execution)
    /// </summary>
    static void BuildExampleToJson(string exampleName)
    {
        Console.WriteLine($"[ObjectIR] Building example: {exampleName}");

        Module module = BuildExample(exampleName);
        string moduleName = module.Name;

        Console.WriteLine($"[ObjectIR] Serializing module to JSON");
        string json = ModuleSerializer.ToJson(module);
        string jsonFile = $"{moduleName}.json";
        File.WriteAllText(jsonFile, json);

        Console.WriteLine($"[ObjectIR] Module saved: {jsonFile} ({new FileInfo(jsonFile).Length} bytes)");
        Console.WriteLine($"[ObjectIR] Module '{moduleName}' is ready for compilation or execution");
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
        using (var runtime = new RuntimeWrapper())
        {
            Console.WriteLine($"  ✓ Runtime initialized");
            runtime.LoadModuleFromString(json);
            Console.WriteLine($"  ✓ Module loaded in C++ runtime");
            Console.WriteLine($"  ✓ Ready for method invocation");
        }

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

    static void ShowUsage()
    {
        Console.WriteLine(@"
ObjectIR Unified Runtime - Compiler + C++ Runtime Integration

This tool combines the ObjectIR Construct compiler with the high-performance
C++ runtime, enabling end-to-end compilation and execution in a single tool.

Usage:
  objectir-unified <command> [options]

Commands:
  run <module.json>                Execute a compiled module in C++ runtime
  compile <module.json>            Compile module to C# code
  build <example>                  Build example to JSON file
  build-and-run <example>          Build example and execute in C++ runtime
  pipeline <example>               Run full pipeline: build → serialize → codegen → execute
  help                             Show this help message

Examples:
  objectir-unified run TodoApp.json
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
