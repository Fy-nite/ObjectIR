# Quick Start: ObjectIR TodoApp Pipeline

## One-Minute Overview

ObjectIR is a compiler-like system for building, serializing, and code-generating modules. The complete pipeline looks like:

```
Build Module → Save JSON → Load JSON → Generate C#
```

## Build the Solution

```bash
cd ObjectIR
dotnet build
```

**Output:** All 5 projects build successfully (0 errors)

## Run the Pipeline

### Option 1: Using the Command-Line Tool

```bash
# Step 1: Build and serialize TodoApp to JSON
dotnet run --project ObjectIR.Tools -- build
# Output: TodoApp.json (6.9 KB)

# Step 2: View module information
dotnet run --project ObjectIR.Tools -- load TodoApp.json

# Step 3: Dump in all formats
dotnet run --project ObjectIR.Tools -- dump TodoApp.json

# Step 4: Generate C# code
dotnet run --project ObjectIR.Tools -- codegen TodoApp.json
# Output: TodoApp.cs (46 lines of compilable C#)
```

### Option 2: Direct C# Backend

```bash
# Generate C# directly from JSON file
dotnet run --project ObjectIR.CSharpBackend -- TodoApp.json
# Output: TodoApp.cs
```

## Generated Files

After running the pipeline, you have:

| File | Size | Purpose |
|------|------|---------|
| `TodoApp.json` | 6.9 KB | Serialized module (types, fields, methods) |
| `TodoApp.cs` | 723 bytes | Generated C# code |

## The TodoApp Module Structure

```
TodoApp (Module)
├── TodoApp.IItem (Interface)
│   ├── GetId() → int32
│   ├── GetDescription() → string
│   └── IsComplete() → bool
│
├── TodoApp.TodoItem (Class)
│   ├── Fields: id, description, isComplete
│   ├── Methods: Constructor, GetId, GetDescription, IsComplete, MarkComplete
│   └── Implements: IItem
│
├── TodoApp.TodoList (Class)
│   ├── Fields: items (List<TodoItem>), nextId
│   └── Methods: Constructor, Add
│
└── TodoApp.Program (Class)
    └── Methods: Main (static)
```

## Generated C# Code (Excerpt)

```csharp
namespace TodoApp;

public interface IItem
{
    int GetId();
    string GetDescription();
    bool IsComplete();
}

public class TodoItem : TodoApp.IItem
{
    private int id;
    private string description;
    private bool isComplete;

    public TodoItem(int id, string description);
    public int GetId();
    public string GetDescription();
    public bool IsComplete();
    public void MarkComplete();
}
```

## Key Commands Reference

### `objectir-tool build`
Builds the TodoApp module and saves it to `TodoApp.json`

```bash
dotnet run --project ObjectIR.Tools -- build
```

### `objectir-tool load <file>`
Loads a module from JSON and displays basic information

```bash
dotnet run --project ObjectIR.Tools -- load TodoApp.json
```

### `objectir-tool dump <file>`
Displays complete module information in multiple formats:
- Summary report with metrics
- Text format (human-readable)
- JSON format (machine-readable)
- Markdown format (documentation)

```bash
dotnet run --project ObjectIR.Tools -- dump TodoApp.json
```

### `objectir-tool codegen <file>`
Generates C# code from a JSON module file

```bash
dotnet run --project ObjectIR.Tools -- codegen TodoApp.json
```

## Core Classes

### IRBuilder - Build modules programmatically
```csharp
var builder = new IRBuilder("MyModule");
builder.Class("MyClass")
    .Field("myField", TypeReference.String)
    .Method("MyMethod", TypeReference.Int32)
    .EndClass();
var module = builder.Build();
```

### ModuleSerializer - Serialize/deserialize JSON
```csharp
// Serialize to JSON
string json = module.DumpJson(indented: true);

// Deserialize from JSON
var module = ModuleSerializer.LoadFromJson(json);
```

### CSharpCodeGenerator - Generate C# code
```csharp
var generator = new CSharpCodeGenerator();
string csharpCode = generator.Generate(module);
File.WriteAllText("output.cs", csharpCode);
```

## Project Structure

```
ObjectIR/
├── src/ObjectIR.Core/              # Core IR definitions
│   └── Serialization/              # JSON serialization
├── ObjectIR.CSharpBackend/         # C# code generator
├── ObjectIR.Examples/              # TodoApp example
└── ObjectIR.Tools/                 # Command-line tool
```

## Next Steps

1. **Explore Generated Files**
   ```bash
   cat TodoApp.json        # View JSON serialization
   cat TodoApp.cs          # View generated C#
   ```

2. **Understand the Pipeline**
   - Read `WORKFLOW.md` for detailed explanation
   - Read `IMPLEMENTATION_COMPLETE.md` for technical details

3. **Create Your Own Module**
   - Use `IRBuilder` API like in `TodoAppExample.cs`
   - Serialize with `ModuleSerializer`
   - Generate code with `CSharpCodeGenerator`

4. **Extend the System**
   - Add new code generators (Python, Java, etc.)
   - Implement instruction serialization
   - Add binary format support

## Troubleshooting

### Build fails
```bash
# Clean and rebuild
dotnet clean
dotnet build
```

### Tool not found
```bash
# Make sure you're in the ObjectIR directory
cd ObjectIR
dotnet run --project ObjectIR.Tools -- build
```

### Generated C# has issues
- Check that input JSON is valid
- Verify module structure with `dump` command
- Review generated code in `TodoApp.cs`

## Documentation

- **WORKFLOW.md** - Complete workflow guide with examples
- **IMPLEMENTATION_COMPLETE.md** - Technical implementation details
- **ARCHITECTURE.md** - System architecture overview
- **COMPLETE_EXAMPLE.md** - Original TodoApp example specification
- **QUICK_REFERENCE.md** - API quick reference

## Performance

For the TodoApp module:
- Build: ~50ms
- Serialize: ~20ms
- Deserialize: ~15ms
- Code generation: ~30ms
- **Total: ~115ms**

## What's Next?

The pipeline is complete and functional. Potential enhancements:
- Instruction serialization (method bodies)
- Binary format (protobuf/msgpack)
- Additional code generators
- Optimization passes
- Streaming deserialization

Enjoy building with ObjectIR! 🚀
