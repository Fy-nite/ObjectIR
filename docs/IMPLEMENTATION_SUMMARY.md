# ObjectIR Module Serialization - Feature Summary

## What Was Added

I've implemented a comprehensive module serialization system that solves your original problem: **"after making a module, it doesn't seem to be possible to get the contents of that module."**

Now you can easily dump and inspect module contents in multiple formats!

## New Files Created

### Core Serialization
1. **`src/ObjectIR.Core/Serialization/ModuleSerializer.cs`** 
   - Main serializer class with 4 dump methods
   - Supports JSON, text, structured objects, and arrays
   - ~400 lines of implementation

2. **`src/ObjectIR.Core/Serialization/ModuleSerializationExtensions.cs`**
   - Extension methods on `Module` for easy access
   - Makes usage clean and fluent

3. **`src/ObjectIR.Core/Serialization/AdvancedModuleFormats.cs`**
   - Advanced export formats: CSV, Markdown, YAML
   - Pretty-printed summary reports
   - Useful for documentation and analysis

### Data Transfer Objects (in ModuleSerializer.cs)
- `ModuleData` - Complete module representation
- `TypeData` - Type information  
- `MethodData` - Method information
- `FieldData` - Field information
- `PropertyData` - Property information
- `FunctionData` - Function information
- `ParameterData` - Parameter information
- `LocalVariableData` - Local variable information

### Documentation
1. **`docs/MODULE_DUMPING.md`** - Complete usage guide with examples
2. **`docs/SERIALIZATION.md`** - Detailed reference documentation

### Updated Files
- **`ObjectIR.CSharpTests/Program.cs`** - Updated with serialization examples

## Usage

### Simple Usage
```csharp
var module = builder.Build();

// Get a summary
Console.WriteLine(module.GenerateSummaryReport());

// Get as text
Console.WriteLine(module.DumpText());

// Get as JSON
var json = module.DumpJson();

// Get typed object
var data = module.Dump();
```

### Available Methods

```csharp
// Basic Dumps
module.Dump()              // -> ModuleData (strongly-typed)
module.DumpJson(indented: true)  // -> string (JSON)
module.DumpText()          // -> string (human-readable)

// Arrays for easy iteration
module.DumpTypes()               // -> TypeData[]
module.DumpFunctions() // -> FunctionData[]

// Advanced Formats
module.DumpCsv()   // -> string (for spreadsheets)
module.DumpMarkdown()            // -> string (for documentation)
module.DumpYaml()    // -> string (YAML format)
module.GenerateSummaryReport() // -> string (pretty-printed stats)
```

## Key Features

? **Multiple Export Formats**
- JSON (human-readable or compact)
- Plain text
- CSV (for Excel/Sheets)
- Markdown (for documentation)
- YAML
- Structured .NET objects

? **Easy Access Patterns**
```csharp
// Extension methods on Module
var data = module.Dump();
var types = module.DumpTypes();

// Or create serializer explicitly
var serializer = new ModuleSerializer(module);
var json = serializer.DumpToJson();
```

? **Complete Information Captured**
- Module metadata
- Type definitions (classes, interfaces, structs)
- Fields, methods, properties
- Access modifiers
- Static/virtual/abstract flags
- Generic parameters
- Instructions count

? **Practical Use Cases**
- **Inspection**: Quickly see what's in your module
- **Validation**: Check module structure programmatically
- **Documentation**: Auto-generate docs in Markdown
- **Export**: Save module structure to JSON/CSV/YAML
- **Analysis**: Calculate metrics and statistics
- **Logging**: Human-readable text for debugging

## Examples

### Export to File
```csharp
System.IO.File.WriteAllText("module.json", module.DumpJson(indented: true));
System.IO.File.WriteAllText("module.md", module.DumpMarkdown());
System.IO.File.WriteAllText("module.csv", module.DumpCsv());
```

### Programmatic Analysis
```csharp
var data = module.Dump();
var totalInstructions = data.Types.Sum(t => 
    t.Methods.Sum(m => m.InstructionCount));
Console.WriteLine($"Total Instructions: {totalInstructions}");
```

### Find Large Methods
```csharp
var data = module.Dump();
var large = data.Types
    .SelectMany(t => t.Methods)
    .Where(m => m.InstructionCount > 50)
    .OrderByDescending(m => m.InstructionCount)
    .ToList();
```

### Generate Summary Report
```csharp
Console.WriteLine(module.GenerateSummaryReport());
```

Output:
```
??????????????????????????????????????????????????????????????????
?  MODULE SUMMARY REPORT             ?
??????????????????????????????????????????????????????????????????

Module Name:      MyModule
Version:      1.0.0

? Statistics ?????????????????????????????????????????????????????
Total Types:   3
Total Functions:  2

Type Breakdown:
  • Classes:  2
  • Interfaces:   1
  • Structs:      0

Members:
  • Total Fields:   5
  • Total Methods:  8
  • Total Properties: 2

Code Metrics:
  • Total Instructions: 127
  • Avg Instructions/Method: 15.88

? Top Methods by Instruction Count ???????????????????????????????
  Person.GetFullInfo: 34 instructions
  Person.SetData: 28 instructions
  Database.Connect: 22 instructions
```

## No Breaking Changes

All existing code continues to work exactly as before. This is purely additive functionality!

```csharp
// Your existing code - still works unchanged
var module = builder.Build();

// New: Now you can also do this!
Console.WriteLine(module.DumpJson());
```

## Next Steps

1. Read `docs/MODULE_DUMPING.md` for comprehensive usage guide
2. Check `docs/SERIALIZATION.md` for detailed API reference
3. Look at `ObjectIR.CSharpTests/Program.cs` for working examples
4. Try it out with your modules!

## Benefits

- ? Solves the original problem: now you CAN access module contents
- ? Multiple output formats for different use cases  
- ? Strongly-typed API for safe programmatic access
- ? Great for debugging, documentation, and analysis
- ? Clean, fluent extension method API
- ? Comprehensive serialization of all module information
- ? No breaking changes to existing code
