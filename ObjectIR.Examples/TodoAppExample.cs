using ObjectIR.Core.Builder;
using ObjectIR.Core.IR;
using ObjectIR.Core.Serialization;
using System;
using System.Collections.Generic;
using System.IO;

namespace ObjectIR.Examples;

/// <summary>
/// Example: Complete Todo List Application
/// Demonstrates building and serializing a complete ObjectIR module
/// </summary>
public class TodoAppExample
{
    /// <summary>
    /// Builds the complete TodoApp module using the IR Builder API
    /// </summary>
    public static Module BuildTodoApp()
    {
        var builder = new IRBuilder("TodoApp");

        // Define IItem interface
        builder.Interface("IItem")
            .Namespace("TodoApp")
            .Method("GetId", TypeReference.Int32)
            .Method("GetDescription", TypeReference.String)
            .Method("IsComplete", TypeReference.Bool)
            .EndInterface();

        // Define TodoItem class
        var todoItemRef = TypeReference.FromName("TodoApp.TodoItem");
        var iitemRef = TypeReference.FromName("TodoApp.IItem");

        builder.Class("TodoItem")
            .Namespace("TodoApp")
            .Implements(iitemRef)
            
            // Fields
            .Field("id", TypeReference.Int32)
                .Access(AccessModifier.Private)
                .EndField()
            .Field("description", TypeReference.String)
                .Access(AccessModifier.Private)
                .EndField()
            .Field("isComplete", TypeReference.Bool)
                .Access(AccessModifier.Private)
                .EndField()
            
            // Constructor
            .Constructor()
                .Parameter("id", TypeReference.Int32)
                .Parameter("description", TypeReference.String)
                .Body()
                    .Ldarg("this")
                    .Ldarg("id")
                    .Stloc("_temp")
                    .Ret()
                .EndBody()
                .EndMethod()
            
            // GetId method
            .Method("GetId", TypeReference.Int32)
                .Body()
                    .Ldarg("this")
                    .Ret()
                .EndBody()
                .EndMethod()
            
            // GetDescription method
            .Method("GetDescription", TypeReference.String)
                .Body()
                    .Ldarg("this")
                    .Ret()
                .EndBody()
                .EndMethod()
            
            // IsComplete method
            .Method("IsComplete", TypeReference.Bool)
                .Body()
                    .Ldarg("this")
                    .Ret()
                .EndBody()
                .EndMethod()
            
            // MarkComplete method
            .Method("MarkComplete", TypeReference.Void)
                .Body()
                    .Ret()
                .EndBody()
                .EndMethod()
            
            .EndClass();

        // Define TodoList class
        var todoListRef = TypeReference.FromName("TodoApp.TodoList");
        var listOfTodoItemRef = TypeReference.List(todoItemRef);

        builder.Class("TodoList")
            .Namespace("TodoApp")
            
            // Fields
            .Field("items", listOfTodoItemRef)
                .Access(AccessModifier.Private)
                .EndField()
            .Field("nextId", TypeReference.Int32)
                .Access(AccessModifier.Private)
                .EndField()
            
            // Constructor
            .Constructor()
                .Body()
                    .Ret()
                .EndBody()
                .EndMethod()
            
            // Add method
            .Method("Add", todoItemRef)
                .Parameter("description", TypeReference.String)
                .Local("item", todoItemRef)
                .Local("id", TypeReference.Int32)
                .Body()
                    .Ret()
                .EndBody()
                .EndMethod()
            
            .EndClass();

        // Define Program class
        builder.Class("Program")
            .Namespace("TodoApp")
            
            // Main method
            .Method("Main", TypeReference.Void)
                .Static()
                .Local("todos", todoListRef)
                .Local("item", todoItemRef)
                .Body()
                    .Ret()
                .EndBody()
                .EndMethod()
            
            .EndClass();

        return builder.Build();
    }

    /// <summary>
    /// Saves the TodoApp module to a JSON file
    /// </summary>
    public static void SaveToJsonFile(Module module, string filePath)
    {
        string json = module.DumpJson(indented: true);
        File.WriteAllText(filePath, json);
        Console.WriteLine($"Module saved to: {filePath}");
    }

    /// <summary>
    /// Loads a module from a JSON file
    /// </summary>
    public static Module LoadFromJsonFile(string filePath)
    {
        string json = File.ReadAllText(filePath);
        var module = ModuleSerializer.LoadFromJson(json);
        Console.WriteLine($"Module loaded from: {filePath}");
        return module;
    }

    /// <summary>
    /// Dumps the module in various formats
    /// </summary>
    public static void DumpModule(Module module)
    {
        Console.WriteLine("\n=== Module Summary ===");
        Console.WriteLine(module.GenerateSummaryReport());
        
        Console.WriteLine("\n=== Text Format ===");
        Console.WriteLine(module.DumpText());
        
        Console.WriteLine("\n=== JSON Format ===");
        Console.WriteLine(module.DumpJson(indented: true));
        
        Console.WriteLine("\n=== Markdown Format ===");
        Console.WriteLine(module.DumpMarkdown());
    }

    /// <summary>
    /// Main demonstration
    /// </summary>
    public static void Main()
    {
        try
        {
            Console.WriteLine("=== TodoApp ObjectIR Example ===\n");

            // Build the module
            Console.WriteLine("1. Building TodoApp module...");
            var module = BuildTodoApp();
            Console.WriteLine($"   ✓ Module '{module.Name}' built successfully");
            Console.WriteLine($"   - Types: {module.Types.Count}");
            Console.WriteLine($"   - Functions: {module.Functions.Count}");

            // Save to JSON
            Console.WriteLine("\n2. Saving module to JSON...");
            string jsonPath = "TodoApp.json";
            SaveToJsonFile(module, jsonPath);

            // Load from JSON
            Console.WriteLine("\n3. Loading module from JSON...");
            var loadedModule = LoadFromJsonFile(jsonPath);
            Console.WriteLine($"   ✓ Module '{loadedModule.Name}' loaded successfully");
            Console.WriteLine($"   - Types: {loadedModule.Types.Count}");
            Console.WriteLine($"   - Functions: {loadedModule.Functions.Count}");

            // Dump in all formats
            Console.WriteLine("\n4. Dumping module in various formats...");
            DumpModule(module);

            // Generate C# code
            Console.WriteLine("\n5. Generating C# code from module...");
            var generator = new CSharpBackend.CSharpCodeGenerator();
            string csharpCode = generator.Generate(module);
            string csPath = "TodoApp_Generated.cs";
            File.WriteAllText(csPath, csharpCode);
            Console.WriteLine($"   ✓ C# code generated: {csPath}");

            Console.WriteLine("\n=== Example Complete ===");
            Console.WriteLine($"Generated files:");
            Console.WriteLine($"  - {jsonPath} (serialized IR)");
            Console.WriteLine($"  - {csPath} (generated C# code)");
        }
        catch (Exception ex)
        {
            Console.Error.WriteLine($"Error: {ex.Message}");
            Console.Error.WriteLine(ex.StackTrace);
        }
    }
}
