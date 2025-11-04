# ModuleLoader - Quick Reference

## Installation

The `ModuleLoader` class is part of `ObjectIR.Core.Serialization` namespace.

```csharp
using ObjectIR.Core.Serialization;

var loader = new ModuleLoader();
```

## Basic Operations

### Parse Text Format
```csharp
var module = loader.LoadFromText(@"
module MyApp
class MyClass {
    field value: int32
}
");
```

### Load from File
```csharp
var module = loader.LoadFromTextFile("module.ir.txt");
var module = loader.LoadFromJsonFile("module.json");
```

### Save to File
```csharp
loader.SaveToTextFile(module, "output.ir.txt");
loader.SaveToJsonFile(module, "output.json");
```

### Cache Management
```csharp
// Access cache
var cached = loader.GetCachedModule("ModuleName");
var all = loader.GetAllCachedModules();
loader.ClearCache();
```

### Batch Operations
```csharp
// Load all from directory
var modules = loader.LoadModulesFromDirectory("./modules");

// Save all to directory
loader.SaveModulesToDirectory(modules.Values, "./backup");
```

## Syntax Reference

### Module Declaration
```
module ModuleName
```

### Type Definitions
```
class ClassName { }
interface InterfaceName { }
struct StructName { }
enum EnumName { Value1, Value2 }
```

### Fields
```
field fieldName: int32
field items: List<string>
```

### Methods
```
method MethodName(a: int32, b: string) -> bool {
    local result: bool
    
    ldarg a
    ldarg b
    ...
    ret
}
```

### Supported Types
- **Primitives**: `int8`, `int16`, `int32`, `int64`, `uint8`, `uint16`, `uint32`, `uint64`, `float32`, `float64`, `bool`, `char`, `string`, `void`
- **Generics**: `List<T>`, `Dict<K,V>`, `Set<T>`, `Optional<T>`
- **Custom**: Any user-defined class/struct/interface

## Common Patterns

### Load and Compile
```csharp
var loader = new ModuleLoader();
var module = loader.LoadFromTextFile("app.ir.txt");
var backend = new CSharpBackend();
backend.Compile(module, output);
```

### Module Store
```csharp
var store = new ModuleStore("./modules");
store.LoadAll();
store.AddFromText(textFormat);
store.SaveAll();
```

### Serialize Module
```csharp
var json = module.DumpJson();
var text = module.DumpText();
```

### Builder API Chaining
```csharp
builder.Class("MyClass")
    .Field("field1", TypeReference.Int32).EndField()  // Required for chaining
    .Field("field2", TypeReference.String).EndField()
    .Method("MyMethod", TypeReference.Void).EndMethod()
    .EndClass();
```

## Error Handling

```csharp
try
{
    var module = loader.LoadFromText(text);
}
catch (FormatException ex)
{
    Console.WriteLine($"Parse error: {ex.Message}");
}
catch (IOException ex)
{
    Console.WriteLine($"File error: {ex.Message}");
}
```

## API Summary

| Method | Returns | Purpose |
|--------|---------|---------|
| `LoadFromText(string)` | `Module` | Parse text format |
| `LoadFromJson(string)` | `Module` | Parse JSON string |
| `LoadFromTextFile(string)` | `Module` | Load text file |
| `LoadFromJsonFile(string)` | `Module` | Load JSON file |
| `LoadModulesFromDirectory(string, string?)` | `Dictionary<string, Module>` | Batch load |
| `SaveToTextFile(Module, string)` | `void` | Save text file |
| `SaveToJsonFile(Module, string, bool)` | `void` | Save JSON file |
| `SaveModulesToDirectory(IEnumerable<Module>, string)` | `void` | Batch save |
| `GetCachedModule(string)` | `Module?` | Retrieve from cache |
| `GetAllCachedModules()` | `IReadOnlyDictionary<string, Module>` | Get all cached |
| `ClearCache()` | `void` | Clear cache |

---

See `MODULE_LOADER.md` for complete documentation and examples.
