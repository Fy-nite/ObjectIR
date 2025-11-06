using System;
using System.IO;
using ObjectIR.Core.Serialization;
using ObjectIR.Examples;

namespace ObjectIR.Tools
{
    /// <summary>
    /// Command-line tool for working with ObjectIR files
    /// Usage:
    ///   objectir-tool build              - Build TodoApp example and save to TodoApp.json
    ///   objectir-tool load &lt;file&gt;        - Load and dump an ObjectIR JSON file
    ///   objectir-tool dump &lt;file&gt;        - Dump module in all formats
    ///   objectir-tool codegen &lt;file&gt;     - Generate C# code from module
    /// </summary>
    class ObjectIRTool
    {
        static void Main(string[] args)
    {
        if (args.Length == 0)
        {
            ShowUsage();
            return;
        }

        try
        {
            string command = args[0].ToLower();

            switch (command)
            {
                case "build":
                    BuildExample();
                    break;

                case "load":
                    if (args.Length < 2)
                    {
                        Console.Error.WriteLine("Error: 'load' command requires a file path");
                        ShowUsage();
                        return;
                    }
                    LoadAndDump(args[1]);
                    break;

                case "dump":
                    if (args.Length < 2)
                    {
                        Console.Error.WriteLine("Error: 'dump' command requires a file path");
                        ShowUsage();
                        return;
                    }
                    DumpModule(args[1]);
                    break;

                case "codegen":
                    if (args.Length < 2)
                    {
                        Console.Error.WriteLine("Error: 'codegen' command requires a file path");
                        ShowUsage();
                        return;
                    }
                    GenerateCode(args[1]);
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
            Console.Error.WriteLine(ex.StackTrace);
            Environment.Exit(1);
        }
    }

    static void ShowUsage()
    {
        Console.WriteLine(@"
ObjectIR Tool - Manage and compile ObjectIR modules

Usage:
  objectir-tool <command> [options]

Commands:
  build              Build the TodoApp example and save to TodoApp.json
  load <file>        Load an ObjectIR JSON file and display its contents
  dump <file>        Dump a module in all available formats
  codegen <file>     Generate C# code from an ObjectIR JSON file
  help               Show this help message

Examples:
  objectir-tool build
  objectir-tool load TodoApp.json
  objectir-tool codegen TodoApp.json
  objectir-tool dump TodoApp.json
");
    }

    static void BuildExample()
    {
        Console.WriteLine("Building TodoApp example...");
        var module = TodoAppExample.BuildTodoApp();
        
        string jsonPath = "TodoApp.json";
        TodoAppExample.SaveToJsonFile(module, jsonPath);
        
        Console.WriteLine($"\n✓ Successfully built and saved to '{jsonPath}'");
        Console.WriteLine($"\nModule details:");
        Console.WriteLine($"  Name: {module.Name}");
        Console.WriteLine($"  Version: {module.Version}");
        Console.WriteLine($"  Types: {module.Types.Count}");
        foreach (var type in module.Types)
        {
            Console.WriteLine($"    - {type.Name}");
        }
    }

    static void LoadAndDump(string filePath)
    {
        if (!File.Exists(filePath))
        {
            Console.Error.WriteLine($"Error: File not found: {filePath}");
            return;
        }

        Console.WriteLine($"Loading module from '{filePath}'...\n");
        
        var module = TodoAppExample.LoadFromJsonFile(filePath);
        
        Console.WriteLine($"\nModule Summary:");
        Console.WriteLine($"  Name: {module.Name}");
        Console.WriteLine($"  Version: {module.Version}");
        Console.WriteLine($"  Types: {module.Types.Count}");
        Console.WriteLine($"  Functions: {module.Functions.Count}");
        
        Console.WriteLine($"\nTypes:");
        foreach (var type in module.Types)
        {
            Console.WriteLine($"  - {type.GetQualifiedName()}");
        }
    }

    static void DumpModule(string filePath)
    {
        if (!File.Exists(filePath))
        {
            Console.Error.WriteLine($"Error: File not found: {filePath}");
            return;
        }

        Console.WriteLine($"Dumping module from '{filePath}'...\n");
        
        var module = TodoAppExample.LoadFromJsonFile(filePath);
        TodoAppExample.DumpModule(module);
    }

    static void GenerateCode(string filePath)
    {
        if (!File.Exists(filePath))
        {
            Console.Error.WriteLine($"Error: File not found: {filePath}");
            return;
        }

        Console.WriteLine($"Generating C# code from '{filePath}'...\n");
        
        var module = TodoAppExample.LoadFromJsonFile(filePath);
        
        var generator = new CSharpBackend.CSharpCodeGenerator();
        string code = generator.Generate(module);
        
        string outputPath = Path.ChangeExtension(filePath, ".cs");
        File.WriteAllText(outputPath, code);
        
        Console.WriteLine($"✓ C# code generated: {outputPath}");
        Console.WriteLine($"\nGenerated file size: {new FileInfo(outputPath).Length} bytes");
    }
}
}
