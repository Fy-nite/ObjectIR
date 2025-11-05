# Module Introspection: What's New

## The Problem (Solved! ?)

Previously, after building an ObjectIR module with the fluent builder API, there was no straightforward way to inspect or access the module's contents programmatically.

## The Solution

added comprehensive **module serialization and dumping capabilities** that make it trivial to inspect, analyze, export, and validate your modules.

## Quick Start

After building your module, you now have instant access to its contents:

```csharp
var module = builder.Build();

// See what's in your module
Console.WriteLine(module.DumpText());

// Or get a detailed report
Console.WriteLine(module.GenerateSummaryReport());

// Or export as JSON
File.WriteAllText("module.json", module.DumpJson());
```

## 8 Ways to Inspect Your Module

```csharp
var module = builder.Build();

// 1. Human-readable text
Console.WriteLine(module.DumpText());

// 2. Structured object (strongly-typed)
var data = module.Dump();

// 3. JSON (for persistence)
var json = module.DumpJson();

// 4. Arrays (for easy iteration)
var types = module.DumpTypes();
var functions = module.DumpFunctions();

// 5. CSV (for spreadsheet analysis)
File.WriteAllText("types.csv", module.DumpCsv());

// 6. Markdown (for documentation)
File.WriteAllText("API.md", module.DumpMarkdown());

// 7. YAML (for configuration)
File.WriteAllText("module.yaml", module.DumpYaml());

// 8. Summary report (for quick metrics)
Console.WriteLine(module.GenerateSummaryReport());
```

## Example: Analyze Your Module

```csharp
var data = module.Dump();

// Calculate statistics
Console.WriteLine($"Types: {data.Types.Length}");
Console.WriteLine($"Total Methods: {data.Types.Sum(t => t.Methods.Length)}");
Console.WriteLine($"Total Instructions: {data.Types.Sum(t => t.Methods.Sum(m => m.InstructionCount))}");

// Find large methods
var large = data.Types
    .SelectMany(t => t.Methods)
    .Where(m => m.InstructionCount > 50)
    .OrderByDescending(m => m.InstructionCount);

foreach (var method in large)
{
    Console.WriteLine($"Large method: {method.Name} ({method.InstructionCount} instructions)");
}
```

## What's Exported

- **Module name & version**
- **All types** (classes, interfaces, structs)
- **Type details**: access, abstract, sealed, base types, interfaces, generics
- **Fields**: name, type, access, static, readonly, initial value
- **Methods**: name, return type, access, static, virtual, override, abstract, constructor flag
- **Method details**: parameters, local variables, instruction count
- **Properties**: name, type, access, getter/setter info
- **Metadata**: custom key-value pairs

## Documentation

- ?? **[MODULE_DUMPING.md](../MODULE_DUMPING.md)** - Complete usage guide with examples
- ?? **[QUICK_REFERENCE.md](../QUICK_REFERENCE.md)** - Quick reference for all methods
- ?? **[SERIALIZATION.md](../SERIALIZATION.md)** - Detailed API documentation

## No Breaking Changes

This is completely additive - all your existing code continues to work exactly as before!

```csharp
// This still works
var module = builder.Build();

// This is new!
Console.WriteLine(module.DumpJson());
```

## Features

? Multiple export formats (JSON, Text, CSV, Markdown, YAML)  
? Strongly-typed object model  
? Array-based access for LINQ queries  
? Pretty-printed reports and statistics  
? Works with all type definitions (classes, interfaces, structs)  
? Captures complete method and field information  
? Includes generic parameters and type constraints  
? No performance overhead  

## Use Cases

- **Debugging** - Quickly see what's in your module
- **Validation** - Verify module structure programmatically  
- **Documentation** - Auto-generate API documentation
- **Export** - Save module definition to JSON/YAML/CSV
- **Analysis** - Calculate metrics and statistics
- **Testing** - Verify module contents in unit tests

## Example: Auto-Generate Documentation

```csharp
var markdown = module.DumpMarkdown();
File.WriteAllText("API.md", markdown);
```

Generated Markdown includes:
- Type hierarchies
- Method signatures
- Field definitions
- Access modifiers
- Property getters/setters
- Formatted as professional documentation

## Example: Batch Export

```csharp
var module = builder.Build();

// Save in all formats at once
File.WriteAllText("module.json", module.DumpJson(indented: true));
File.WriteAllText("module.md", module.DumpMarkdown());
File.WriteAllText("module.csv", module.DumpCsv());
File.WriteAllText("module.yaml", module.DumpYaml());
File.WriteAllText("module.txt", module.DumpText());
File.WriteAllText("report.txt", module.GenerateSummaryReport());

Console.WriteLine("? Module exported in 6 formats!");
```

## Example: Validate Module

```csharp
var data = module.Dump();

// Check for empty types
if (data.Types.Any(t => t.Methods.Length == 0 && t.Fields.Length == 0))
    Console.WriteLine("? Warning: Empty types detected");

// Check for public fields (anti-pattern)
if (data.Types.SelectMany(t => t.Fields).Any(f => f.Access == "Public"))
    Console.WriteLine("? Warning: Public fields detected (use properties instead)");

// Check interface naming convention
if (data.Types.Any(t => t.Kind == "Interface" && !t.Name.StartsWith("I")))
    Console.WriteLine("? Warning: Interfaces should start with 'I'");

Console.WriteLine("? Validation complete!");
```

## Files Added

```
src/ObjectIR.Core/Serialization/
??? ModuleSerializer.cs   # Core serialization engine
??? ModuleSerializationExtensions.cs       # Extension methods on Module
??? AdvancedModuleFormats.cs         # CSV, Markdown, YAML, Reports

docs/
??? MODULE_DUMPING.md   # Complete usage guide
??? QUICK_REFERENCE.md   # Quick reference
??? SERIALIZATION.md            # Detailed API docs
??? IMPLEMENTATION_SUMMARY.md       # Feature summary
```

## Next Steps

1. Read the docs in `docs/` directory
2. Try with your own modules
3. Use for debugging, documentation, or export
4. Let us know what you think!

---

**Summary**: You now have complete, easy access to your module's contents in multiple formats! ??
