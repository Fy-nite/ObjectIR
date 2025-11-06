using ObjectIR.Core.Serialization;
using ObjectIR.Core.IR;
using System;
using System.IO;
using System.Collections.Generic;

namespace ObjectIR.Examples;

/// <summary>
/// Example showing how to use ModuleLoader to load, store, and compile IR modules
/// </summary>
public static class ModuleLoaderExample
{
    public static void Main()
    {
        Console.WriteLine("=== ObjectIR Module Loader Example ===\n");

        // Example 1: Load from text format
        LoadFromTextFormat();

        // Example 2: Load/Save from files
        LoadSaveFromFiles();

        // Example 3: Module caching and retrieval
        ModuleCaching();

        // Example 4: Batch loading from directory
        BatchLoadFromDirectory();

        // Example 5: Compile and use loaded modules
        CompileLoadedModules();
    }

    /// <summary>
    /// Example 1: Load a module from text format
    /// </summary>
    static void LoadFromTextFormat()
    {
        Console.WriteLine("--- Example 1: Load from Text Format ---");

        var textFormat = @"
module CalculatorApp

class Calculator {
    field history: List<int32>
    field lastResult: int32
    
    method Add(a: int32, b: int32) -> int32 {
        local result: int32
        
        ldarg a
        ldarg b
        add
        stloc result
        ldloc result
        ret
    }
}
";

        var loader = new ModuleLoader();
        var module = loader.LoadFromText(textFormat);

        Console.WriteLine($"✓ Loaded module: {module.Name}");
        Console.WriteLine($"✓ Types: {module.Types.Count}");
        Console.WriteLine($"✓ Module cached: {loader.GetCachedModule(module.Name) != null}\n");
    }

    /// <summary>
    /// Example 2: Save to and load from files
    /// </summary>
    static void LoadSaveFromFiles()
    {
        Console.WriteLine("--- Example 2: Load/Save from Files ---");

        // Create a temporary directory for this example
        var tempDir = Path.Combine(Path.GetTempPath(), "ObjectIR_Example");
        Directory.CreateDirectory(tempDir);

        // First, create a module using the builder API
        var builder = new ObjectIR.Core.Builder.IRBuilder("TodoApp");
        var classBuilder = builder.Class("TodoItem")
            .Field("id", TypeReference.Int32).EndField()
            .Field("title", TypeReference.String).EndField()
            .Field("completed", TypeReference.Bool).EndField();

        var module = builder.Build();

        // Now use ModuleLoader to save it
        var loader = new ModuleLoader();
        var jsonPath = Path.Combine(tempDir, "TodoApp.json");
        var textPath = Path.Combine(tempDir, "TodoApp.ir.txt");

        loader.SaveToJsonFile(module, jsonPath);
        loader.SaveToTextFile(module, textPath);

        Console.WriteLine($"✓ Saved module to JSON: {jsonPath}");
        Console.WriteLine($"✓ Saved module to text: {textPath}");

        // Now load them back
        var loadedFromJson = loader.LoadFromJsonFile(jsonPath);
        // Note: The text dump is for display only, not for loading back
        // var loadedFromText = loader.LoadFromTextFile(textPath);

        Console.WriteLine($"✓ Loaded from JSON: {loadedFromJson.Name}\n");

        // Cleanup
        Directory.Delete(tempDir, true);
    }

    /// <summary>
    /// Example 3: Module caching and retrieval
    /// </summary>
    static void ModuleCaching()
    {
        Console.WriteLine("--- Example 3: Module Caching ---");

        var loader = new ModuleLoader();

        // Load the same module multiple times
        var textFormat = @"
module MathLib

class MathOps {
    field PI: float64
}
";

        var module1 = loader.LoadFromText(textFormat);
        var module2 = loader.LoadFromText(textFormat);

        // Both should be in cache
        var cached = loader.GetAllCachedModules();
        Console.WriteLine($"✓ Cached modules: {cached.Count}");

        // Retrieve a specific module
        var retrieved = loader.GetCachedModule("MathLib");
        Console.WriteLine($"✓ Retrieved from cache: {retrieved?.Name ?? "Not found"}");

        // Clear cache
        loader.ClearCache();
        Console.WriteLine($"✓ Cache cleared. Modules now: {loader.GetAllCachedModules().Count}\n");
    }

    /// <summary>
    /// Example 4: Batch loading from directory
    /// </summary>
    static void BatchLoadFromDirectory()
    {
        Console.WriteLine("--- Example 4: Batch Load from Directory ---");

        // Create a temporary directory with multiple module files
        var tempDir = Path.Combine(Path.GetTempPath(), "ObjectIR_Modules");
        Directory.CreateDirectory(tempDir);

        // Create sample module files
        var moduleTexts = new Dictionary<string, string>
        {
            { "module1.ir.txt", @"
module Module1
class Class1 {
    field value: int32
}
" },
            { "module2.ir.txt", @"
module Module2
class Class2 {
    field name: string
}
" },
            { "module3.ir.txt", @"
module Module3
class Class3 {
    field count: int32
}
" }
        };

        foreach (var (filename, content) in moduleTexts)
        {
            File.WriteAllText(Path.Combine(tempDir, filename), content);
        }

        var loader = new ModuleLoader();
        var modules = loader.LoadModulesFromDirectory(tempDir, "*.ir.txt");

        Console.WriteLine($"✓ Loaded {modules.Count} modules from directory");
        foreach (var (name, module) in modules)
        {
            Console.WriteLine($"  - {module.Name}: {module.Types.Count} types");
        }

        // Save multiple modules back to a different directory
        var outputDir = Path.Combine(Path.GetTempPath(), "ObjectIR_Backup");
        loader.SaveModulesToDirectory(modules.Values, outputDir);
        Console.WriteLine($"✓ Saved modules to {outputDir}\n");

        // Cleanup
        Directory.Delete(tempDir, true);
        Directory.Delete(outputDir, true);
    }

    /// <summary>
    /// Example 5: Compile and use loaded modules
    /// </summary>
    static void CompileLoadedModules()
    {
        Console.WriteLine("--- Example 5: Compile Loaded Modules ---");

        var textFormat = @"
module DataStructures

class Stack {
    field items: List<int32>
    field count: int32
    
    method Push(value: int32) -> void {
        local temp: int32
        ldarg value
        stloc temp
    }
}
";

        var loader = new ModuleLoader();
        var module = loader.LoadFromText(textFormat);

        // Now you can work with the loaded module programmatically
        Console.WriteLine($"✓ Module: {module.Name}");
        Console.WriteLine($"✓ Types: {module.Types.Count}");

        if (module.Types.Count > 0)
        {
            var firstType = module.Types[0];
            Console.WriteLine($"✓ First type: {firstType.Name}");
            
            if (firstType is ClassDefinition classDef)
            {
                Console.WriteLine($"  - Fields: {classDef.Fields.Count}");
                Console.WriteLine($"  - Methods: {classDef.Methods.Count}");

                foreach (var field in classDef.Fields)
                {
                    Console.WriteLine($"    - Field: {field.Name}: {field.Type.GetQualifiedName()}");
                }

                foreach (var method in classDef.Methods)
                {
                    Console.WriteLine($"    - Method: {method.Name}() -> {method.ReturnType.GetQualifiedName()}");
                }
            }
        }

        // You can now compile this module using any backend
        // Example: var csharpBackend = new CSharpBackend();
        //          csharpBackend.Compile(module, outputStream);

        Console.WriteLine($"✓ Module is ready for compilation\n");
    }
}

/// <summary>
/// Advanced example: Module store for managing multiple modules
/// </summary>
public class ModuleStore
{
    private readonly ModuleLoader _loader = new();
    private readonly Dictionary<string, Module> _modules = new();
    private readonly string _storeDirectory;

    public ModuleStore(string storeDirectory)
    {
        _storeDirectory = storeDirectory;
        Directory.CreateDirectory(_storeDirectory);
    }

    /// <summary>
    /// Load all modules from the store
    /// </summary>
    public void LoadAll()
    {
        var modules = _loader.LoadModulesFromDirectory(_storeDirectory, "*.ir.txt");
        foreach (var (name, module) in modules)
        {
            _modules[module.Name] = module;
        }
        Console.WriteLine($"Loaded {_modules.Count} modules from store");
    }

    /// <summary>
    /// Save all modules in memory to the store
    /// </summary>
    public void SaveAll()
    {
        _loader.SaveModulesToDirectory(_modules.Values, _storeDirectory);
        Console.WriteLine($"Saved {_modules.Count} modules to store");
    }

    /// <summary>
    /// Add a module from text format
    /// </summary>
    public void AddFromText(string moduleText)
    {
        var module = _loader.LoadFromText(moduleText);
        _modules[module.Name] = module;
    }

    /// <summary>
    /// Get a module by name
    /// </summary>
    public Module? GetModule(string name) => _modules.TryGetValue(name, out var m) ? m : null;

    /// <summary>
    /// List all loaded modules
    /// </summary>
    public IEnumerable<Module> GetAllModules() => _modules.Values;

    /// <summary>
    /// Remove a module
    /// </summary>
    public bool RemoveModule(string name) => _modules.Remove(name);

    /// <summary>
    /// Get module metadata
    /// </summary>
    public void PrintModuleInfo(string moduleName)
    {
        var module = GetModule(moduleName);
        if (module == null)
        {
            Console.WriteLine($"Module '{moduleName}' not found");
            return;
        }

        Console.WriteLine($"Module: {module.Name}");
        Console.WriteLine($"  Types: {module.Types.Count}");
        Console.WriteLine($"  Functions: {module.Functions.Count}");
        Console.WriteLine($"  Version: {module.Version}");
    }
}
