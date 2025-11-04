# Module Loader - Load, Store, and Compile IR Modules

## Overview

The `ModuleLoader` is a new feature that allows you to load ObjectIR modules from text format, store them persistently (as JSON or text), and compile them on demand. This enables a complete workflow for managing IR modules as serialized artifacts.

## Features

✅ **Load from Text Format** - Parse IR modules written in the ObjectIR text syntax  
✅ **Load/Save JSON** - Serialize/deserialize modules using JSON  
✅ **Load/Save Text** - Store and reload from human-readable text format  
✅ **File I/O** - Load/save individual files or batch operations  
✅ **Module Caching** - Keep loaded modules in memory for quick access  
✅ **Batch Operations** - Load/save multiple modules from/to directories  
✅ **Compilation Ready** - Loaded modules can immediately be compiled using any backend  

### Builder API Note

When using the fluent builder API, remember to call `.EndField()`, `.EndMethod()`, etc. to return to the parent builder for chaining:

```csharp
builder.Class("MyClass")
    .Field("field1", TypeReference.Int32).EndField()  // Required for chaining
    .Field("field2", TypeReference.String).EndField()
    .Method("MyMethod", TypeReference.Void).EndMethod()
    .EndClass();
```

```csharp
using ObjectIR.Core.Serialization;

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

Console.WriteLine($"Loaded: {module.Name}");
Console.WriteLine($"Types: {module.Types.Count}");
```

### Load/Save from Files

```csharp
var loader = new ModuleLoader();

// Load from text file
var module = loader.LoadFromTextFile("calculator.ir.txt");

// Save to JSON file
loader.SaveToJsonFile(module, "calculator.json");

// Save to text file
loader.SaveToTextFile(module, "calculator_backup.ir.txt");

// Load back from files
var fromJson = loader.LoadFromJsonFile("calculator.json");
var fromText = loader.LoadFromTextFile("calculator_backup.ir.txt");
```

### Module Caching

```csharp
var loader = new ModuleLoader();

// Load modules - they're automatically cached
var mod1 = loader.LoadFromText(textFormat);

// Retrieve from cache by name
var cached = loader.GetCachedModule("CalculatorApp");

// Get all cached modules
var all = loader.GetAllCachedModules();

// Clear cache when done
loader.ClearCache();
```

### Batch Operations

```csharp
var loader = new ModuleLoader();

// Load all modules from a directory
var modules = loader.LoadModulesFromDirectory("./modules", "*.ir.txt");

// Save multiple modules to a directory
loader.SaveModulesToDirectory(modules.Values, "./backup");

foreach (var (name, module) in modules)
{
    Console.WriteLine($"{module.Name}: {module.Types.Count} types");
}
```

### Compile Loaded Modules

```csharp
var loader = new ModuleLoader();
var module = loader.LoadFromTextFile("mymodule.ir.txt");

// Now compile using any backend
using var output = new FileStream("mymodule.dll", FileMode.Create);
var backend = new CSharpBackend();  // or any other backend
backend.Compile(module, output);
```

## Text Format Syntax

The `ModuleLoader` supports the ObjectIR text format as shown in the documentation:

```
module ModuleName

class ClassName {
    field fieldName: Type
    field fieldName: Type
    
    method MethodName(param: Type, param: Type) -> ReturnType {
        local localName: Type
        
        instruction
        instruction
        ret
    }
}

interface InterfaceName {
    method MethodName() -> Type
}

struct StructName {
    field fieldName: Type
}

enum EnumName {
    Value1
    Value2
}
```

### Supported Types

**Primitives:**
- `void`, `bool`
- `int8`, `int16`, `int32`, `int64`
- `uint8`, `uint16`, `uint32`, `uint64`
- `float32`, `float64`
- `char`, `string`

**Generics:**
- `List<T>` - Generic list
- `Dict<K, V>` - Generic dictionary
- `Set<T>` - Generic set
- `Optional<T>` - Optional value

**Custom Types:**
- Any user-defined class, struct, or interface

## Advanced: Module Store

The example includes a `ModuleStore` class for managing multiple modules:

```csharp
var store = new ModuleStore("./my_modules");

// Load all modules from the store
store.LoadAll();

// Add a new module
store.AddFromText(@"
module MyModule
class MyClass { }
");

// Get a specific module
var module = store.GetModule("MyModule");

// Get all modules
var all = store.GetAllModules();

// Remove a module
store.RemoveModule("MyModule");

// Print module information
store.PrintModuleInfo("MyModule");

// Save all modules back to store
store.SaveAll();
```

## API Reference

### ModuleLoader

#### Loading

| Method | Description |
|--------|-------------|
| `LoadFromText(string)` | Parse module from text format |
| `LoadFromJson(string)` | Parse module from JSON string |
| `LoadFromJsonFile(string)` | Load module from JSON file |
| `LoadFromTextFile(string)` | Load module from text file |
| `LoadModulesFromDirectory(string, string)` | Load all modules from directory |

#### Saving

| Method | Description |
|--------|-------------|
| `SaveToJsonFile(Module, string, bool)` | Save module to JSON file |
| `SaveToTextFile(Module, string)` | Save module to text file |
| `SaveModulesToDirectory(IEnumerable<Module>, string)` | Save multiple modules to directory |

#### Caching

| Method | Description |
|--------|-------------|
| `GetCachedModule(string)` | Retrieve a module from cache by name |
| `GetAllCachedModules()` | Get all cached modules |
| `ClearCache()` | Clear the entire cache |

## Use Cases

### 1. **Module Library Development**
Store commonly used modules as text files, load them on demand:

```csharp
var loader = new ModuleLoader();
var mathLib = loader.LoadFromTextFile("stdlib/math.ir.txt");
var ioLib = loader.LoadFromTextFile("stdlib/io.ir.txt");
```

### 2. **Configuration-Driven Compilation**
Load module definitions from configuration, compile to different targets:

```csharp
var modules = loader.LoadModulesFromDirectory("config/modules");

var csharpBackend = new CSharpBackend();
var jsBackend = new JavaScriptBackend();

foreach (var module in modules.Values)
{
    csharpBackend.Compile(module, ...);
    jsBackend.Compile(module, ...);
}
```

### 3. **Cross-Platform Development**
Define modules once in text format, compile for multiple platforms:

```
module MyApp

class DataProcessor {
    ...
}
```

Then compile to C#, C++, JavaScript using different backends.

### 4. **Module Caching for Performance**
Load modules once, reuse in memory for multiple compilations:

```csharp
var loader = new ModuleLoader();
var modules = loader.LoadModulesFromDirectory("./modules");

// Compile multiple times without reloading
for (int i = 0; i < 100; i++)
{
    foreach (var module in loader.GetAllCachedModules().Values)
    {
        backend.Compile(module, ...);
    }
}
```

### 5. **Serialization and Sharing**
Export modules to JSON for sharing or version control:

```csharp
var loader = new ModuleLoader();
var module = loader.LoadFromTextFile("calculator.ir.txt");

// Save to JSON for sharing/version control
loader.SaveToJsonFile(module, "calculator.json");

// Share calculator.json with team
// They can load it back
var loaded = loader.LoadFromJsonFile("calculator.json");
```

## Integration with Existing Code

The `ModuleLoader` works seamlessly with existing ObjectIR tools:

```csharp
// Load with ModuleLoader
var loader = new ModuleLoader();
var module = loader.LoadFromTextFile("mymodule.ir.txt");

// Use with existing serialization
var json = module.DumpJson();
var text = module.DumpText();

// Compile with backends
var backend = new CSharpBackend();
backend.Compile(module, output);
```

## Examples

See `ModuleLoaderExample.cs` for complete working examples including:

1. **LoadFromTextFormat()** - Parse text into modules
2. **LoadSaveFromFiles()** - Save and load from files
3. **ModuleCaching()** - Cache modules in memory
4. **BatchLoadFromDirectory()** - Load multiple modules at once
5. **CompileLoadedModules()** - Compile loaded modules using backends
6. **ModuleStore** - Full module management system

## Performance Considerations

- **Caching**: Loaded modules are cached in memory by default
- **Batch Loading**: Loading multiple modules is optimized for directory operations
- **Lazy Loading**: Load modules only when needed to reduce memory usage
- **File Format**: Text format is more readable; JSON is more compact

## Error Handling

The loader provides clear error messages for common issues:

```csharp
try
{
    var module = loader.LoadFromText(malformedText);
}
catch (ArgumentException ex)
{
    Console.WriteLine($"Parsing error: {ex.Message}");
}
catch (IOException ex)
{
    Console.WriteLine($"File error: {ex.Message}");
}
catch (FormatException ex)
{
    Console.WriteLine($"Format error: {ex.Message}");
}
```

## Future Enhancements

- Binary format for faster loading
- Compression support for smaller file sizes
- Module dependency resolution
- Incremental compilation support
- IDE integration for module management
- Module validation and linting

---

The `ModuleLoader` makes it easy to treat ObjectIR modules as first-class artifacts that can be created, stored, loaded, and compiled on demand.
