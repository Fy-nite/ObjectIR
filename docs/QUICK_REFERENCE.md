# Quick Reference: Module Serialization Methods

## Extension Methods on Module

All of these are available as extension methods directly on your `Module` object:

```csharp
using ObjectIR.Core.Serialization;

var module = builder.Build();
```

### Core Dump Methods

| Method | Returns | Example |
|--------|---------|---------|
| `Dump()` | `ModuleData` | `var data = module.Dump();` |
| `DumpJson(bool indented = true)` | `string` | `var json = module.DumpJson();` |
| `DumpText()` | `string` | `var text = module.DumpText();` |
| `DumpTypes()` | `TypeData[]` | `var types = module.DumpTypes();` |
| `DumpFunctions()` | `FunctionData[]` | `var funcs = module.DumpFunctions();` |

### Advanced Format Methods

| Method | Returns | Use Case |
|--------|---------|----------|
| `DumpCsv()` | `string` | Spreadsheet analysis |
| `DumpMarkdown()` | `string` | Auto-generate documentation |
| `DumpYaml()` | `string` | Configuration/export |
| `GenerateSummaryReport()` | `string` | Quick statistics & analysis |

## Common Patterns

### 1. Quick Inspection
```csharp
Console.WriteLine(module.DumpText());
```

### 2. Save to JSON
```csharp
File.WriteAllText("module.json", module.DumpJson(indented: true));
```

### 3. Analyze Types
```csharp
foreach (var type in module.DumpTypes())
{
    Console.WriteLine($"{type.Kind}: {type.Name}");
}
```

### 4. Get Statistics
```csharp
var data = module.Dump();
Console.WriteLine($"Types: {data.Types.Length}");
Console.WriteLine($"Functions: {data.Functions.Length}");
```

### 5. Validate Structure
```csharp
var data = module.Dump();
var emptyTypes = data.Types.Where(t => t.Methods.Length == 0).ToList();
if (emptyTypes.Any())
    Console.WriteLine($"Warning: {emptyTypes.Count} empty types");
```

### 6. Generate Documentation
```csharp
File.WriteAllText("API.md", module.DumpMarkdown());
```

### 7. Export for Analysis
```csharp
File.WriteAllText("analysis.csv", module.DumpCsv());
// Import into Excel/Sheets for analysis
```

### 8. Pretty Print Summary
```csharp
Console.WriteLine(module.GenerateSummaryReport());
```

## Data Structure Overview

### ModuleData
```
ModuleData
?? Name: string
?? Version: string
?? Metadata: Dictionary<string, string>
?? Types: TypeData[]
?? Functions: FunctionData[]
```

### TypeData
```
TypeData
?? Kind: string ("Class", "Interface", "Struct")
?? Name: string
?? Namespace: string
?? Access: string ("Public", "Internal", "Private")
?? IsAbstract: bool
?? IsSealed: bool
?? BaseType: string
?? Interfaces: string[]
?? GenericParameters: string[]
?? Fields: FieldData[]
?? Methods: MethodData[]
?? Properties: PropertyData[]
```

### MethodData
```
MethodData
?? Name: string
?? ReturnType: string
?? Access: string
?? IsStatic: bool
?? IsVirtual: bool
?? IsOverride: bool
?? IsAbstract: bool
?? IsConstructor: bool
?? Parameters: ParameterData[]
?? LocalVariables: LocalVariableData[]
?? InstructionCount: int
```

### FieldData
```
FieldData
?? Name: string
?? Type: string
?? Access: string
?? IsStatic: bool
?? IsReadOnly: bool
?? InitialValue: string
```

### PropertyData
```
PropertyData
?? Name: string
?? Type: string
?? Access: string
?? HasGetter: bool
?? HasSetter: bool
?? GetterAccess: string
?? SetterAccess: string
```

### FunctionData
```
FunctionData
?? Name: string
?? ReturnType: string
?? Parameters: ParameterData[]
?? LocalVariables: LocalVariableData[]
?? InstructionCount: int
```

## Real-World Examples

### Example 1: Find Largest Methods
```csharp
var data = module.Dump();
var largest = data.Types
  .SelectMany(t => t.Methods, (t, m) => (Type: t.Name, Method: m))
    .OrderByDescending(x => x.Method.InstructionCount)
  .Take(10);

foreach (var (type, method) in largest)
{
    Console.WriteLine($"{type}.{method.Name}: {method.InstructionCount}");
}
```

### Example 2: Count Public APIs
```csharp
var data = module.Dump();
var publicMethods = data.Types
    .SelectMany(t => t.Methods)
    .Where(m => m.Access == "Public" && !m.IsStatic)
    .Count();

Console.WriteLine($"Public instance methods: {publicMethods}");
```

### Example 3: Verify Conventions
```csharp
var data = module.Dump();

// Check that all interfaces start with 'I'
var badInterfaces = data.Types
    .Where(t => t.Kind == "Interface" && !t.Name.StartsWith("I"));

if (badInterfaces.Any())
    Console.WriteLine("Warning: Some interfaces don't start with 'I'");

// Check for public fields (anti-pattern)
var publicFields = data.Types
    .SelectMany(t => t.Fields)
    .Where(f => f.Access == "Public");

if (publicFields.Any())
    Console.WriteLine($"Warning: {publicFields.Count()} public fields found");
```

### Example 4: Generate Type Index
```csharp
var markdown = new StringBuilder();
markdown.AppendLine("# Type Index");

foreach (var type in module.DumpTypes())
{
    markdown.AppendLine($"## {type.Name}");
    markdown.AppendLine($"- **Kind**: {type.Kind}");
    markdown.AppendLine($"- **Namespace**: {type.Namespace}");
    markdown.AppendLine($"- **Access**: {type.Access}");
    markdown.AppendLine($"- **Methods**: {type.Methods.Length}");
    markdown.AppendLine($"- **Fields**: {type.Fields.Length}");
}

File.WriteAllText("INDEX.md", markdown.ToString());
```

## Tips

- ?? **Use `Dump()` for LINQ queries** - strongly-typed and safe
- ?? **Use `DumpJson()` for persistence** - easy to store and load
- ?? **Use `DumpText()` for quick debugging** - human-readable
- ?? **Use `GenerateSummaryReport()` for metrics** - instant statistics
- ?? **Use `DumpMarkdown()` for documentation** - auto-generate API docs
- ?? **Use `DumpCsv()` for analysis** - import into spreadsheets

## Troubleshooting

**Q: My module has no types but I built classes?**
A: Make sure you're calling `.EndClass()` on the `ClassBuilder`. Types aren't added to the module until the builder completes.

**Q: The JSON output is too large?**
A: Use `DumpJson(indented: false)` for compact output, or use `Dump()` and filter before serializing.

**Q: How do I count total instructions?**
A: 
```csharp
var data = module.Dump();
var total = data.Types.Sum(t => t.Methods.Sum(m => m.InstructionCount)) +
            data.Functions.Sum(f => f.InstructionCount);
```

**Q: Can I get just one type?**
A: Yes, use LINQ:
```csharp
var personType = module.DumpTypes().FirstOrDefault(t => t.Name == "Person");
```
