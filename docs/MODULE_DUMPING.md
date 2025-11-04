# Module Dumping & Serialization Guide

Now you can easily dump and inspect the contents of your ObjectIR modules in multiple formats!

## Quick Start

After building your module, you have several ways to inspect its contents:

```csharp
using ObjectIR.Core.Serialization;

var module = builder.Build();

// Get a human-readable summary
Console.WriteLine(module.GenerateSummaryReport());

// Get text representation
Console.WriteLine(module.DumpText());

// Get JSON
var json = module.DumpJson();

// Get strongly-typed object
var data = module.Dump();

// Get as arrays
var types = module.DumpTypes();
var functions = module.DumpFunctions();
```

## All Available Methods

### On `Module` Object

| Method | Returns | Description |
|--------|---------|-------------|
| `Dump()` | `ModuleData` | Strongly-typed structured object with all module info |
| `DumpJson(indented?)` | `string` | JSON representation (human-readable if indented=true) |
| `DumpText()` | `string` | Human-readable text format |
| `DumpTypes()` | `TypeData[]` | Array of all types in module |
| `DumpFunctions()` | `FunctionData[]` | Array of all functions in module |
| `DumpCsv()` | `string` | CSV format for spreadsheet analysis |
| `DumpMarkdown()` | `string` | Markdown format for documentation |
| `DumpYaml()` | `string` | YAML format |
| `GenerateSummaryReport()` | `string` | Pretty-printed summary report |

## Usage Examples

### 1. Get Module Summary

```csharp
var module = builder.Build();
Console.WriteLine(module.GenerateSummaryReport());
```

Output:
```
??????????????????????????????????????????????????????????????????
?  MODULE SUMMARY REPORT        ?
??????????????????????????????????????????????????????????????????

Module Name:      HelloWorld
Version:            1.0.0

? Statistics ?????????????????????????????????????????????????????
Total Types:        1
Total Functions:    0

Type Breakdown:
  • Classes:        1
  • Interfaces:     0
  • Structs:        0

Members:
  • Total Fields:   0
  • Total Methods:  1
  • Total Properties: 0

Code Metrics:
  • Total Instructions: 3

? Top Methods by Instruction Count ???????????????????????????????
  Program.Main: 3 instructions
```

### 2. Get Text Representation

```csharp
Console.WriteLine(module.DumpText());
```

Output:
```
Module: HelloWorld
Version: 1.0.0.0

Types (1):
  class HelloWorld.Program
    Methods:
      Public static System.Void Main() [3 instructions]
```

### 3. Get as JSON

```csharp
// Pretty-printed (indented)
var json = module.DumpJson(indented: true);
System.IO.File.WriteAllText("module.json", json);

// Compact
var compact = module.DumpJson(indented: false);
```

JSON output:
```json
{
  "name": "HelloWorld",
  "version": "1.0.0.0",
  "metadata": {},
  "types": [
    {
      "kind": "Class",
    "name": "Program",
      "namespace": "HelloWorld",
      "access": "Public",
      "isAbstract": false,
      "isSealed": false,
      "baseType": null,
      "interfaces": [],
   "genericParameters": [],
      "fields": [],
      "methods": [
      {
          "name": "Main",
          "returnType": "System.Void",
    "access": "Public",
          "isStatic": true,
          "isVirtual": false,
          "isOverride": false,
    "isAbstract": false,
          "isConstructor": false,
          "parameters": [],
     "localVariables": [],
       "instructionCount": 3
}
      ],
      "properties": []
    }
  ],
  "functions": []
}
```

### 4. Array-Based Access

```csharp
// Get all types and iterate
var types = module.DumpTypes();
foreach (var type in types)
{
    Console.WriteLine($"{type.Kind}: {type.Name}");
    foreach (var method in type.Methods)
    {
        Console.WriteLine($"  - {method.Name}");
    }
}

// Get all standalone functions
var functions = module.DumpFunctions();
foreach (var func in functions)
{
    Console.WriteLine($"Function: {func.Name}");
}
```

### 5. Structured Object Access

```csharp
var data = module.Dump();

// Access strongly-typed properties
Console.WriteLine($"Module: {data.Name} v{data.Version}");

// Calculate statistics
int totalInstructions = 0;
foreach (var type in data.Types)
{
    foreach (var method in type.Methods)
    {
        totalInstructions += method.InstructionCount;
    }
}
Console.WriteLine($"Total Instructions: {totalInstructions}");
```

### 6. Export to CSV

```csharp
var csv = module.DumpCsv();
System.IO.File.WriteAllText("module.csv", csv);
```

Output:
```
Type,Kind,Namespace,Access,IsAbstract,IsSealed,FieldCount,MethodCount,PropertyCount
"Program","Class","HelloWorld","Public",False,False,0,1,0
```

### 7. Export to Markdown

```csharp
var markdown = module.DumpMarkdown();
System.IO.File.WriteAllText("module.md", markdown);
```

Output:
```markdown
# HelloWorld
Version: 1.0.0.0

## Types

### class: Program
**Namespace**: HelloWorld

**Access**: Public

#### Methods
| Name | Return Type | Access | Static | Virtual | Abstract |
|------|-------------|--------|--------|---------|----------|
| Main | System.Void | Public | True | False | False |
```

### 8. Export to YAML

```csharp
var yaml = module.DumpYaml();
System.IO.File.WriteAllText("module.yaml", yaml);
```

Output:
```yaml
module: HelloWorld
version: 1.0.0.0
types:
  - name: Program
 kind: Class
    namespace: HelloWorld
    access: Public
    abstract: false
    sealed: false
    methods:
    - name: Main
        returnType: System.Void
 parameters: 0
```

## Real-World Scenarios

### Scenario 1: Save Module to Multiple Formats

```csharp
var module = builder.Build();

// Save in multiple formats
System.IO.File.WriteAllText("module.json", module.DumpJson(indented: true));
System.IO.File.WriteAllText("module.md", module.DumpMarkdown());
System.IO.File.WriteAllText("module.csv", module.DumpCsv());
System.IO.File.WriteAllText("module.txt", module.DumpText());

Console.WriteLine("Module exported to JSON, Markdown, CSV, and Text formats");
```

### Scenario 2: Analyze Method Complexity

```csharp
var data = module.Dump();
var largeeMethods = new List<(string Type, string Method, int Instructions)>();

foreach (var type in data.Types)
{
    foreach (var method in type.Methods)
    {
        if (method.InstructionCount > 50)
        {
    largeeMethods.Add((type.Name, method.Name, method.InstructionCount));
        }
    }
}

Console.WriteLine("Methods with more than 50 instructions:");
foreach (var (type, method, count) in largeeMethods)
{
    Console.WriteLine($"  {type}.{method}: {count} instructions");
}
```

### Scenario 3: Validate Module Structure

```csharp
var data = module.Dump();

// Check for empty types
var emptyTypes = data.Types.Where(t => t.Methods.Length == 0 && t.Fields.Length == 0).ToList();
if (emptyTypes.Count > 0)
{
  Console.WriteLine($"Warning: {emptyTypes.Count} empty types found");
}

// Check for public fields (might want to use properties instead)
var publicFields = data.Types
    .SelectMany(t => t.Fields.Where(f => f.Access == "Public"))
    .ToList();
if (publicFields.Count > 0)
{
    Console.WriteLine($"Warning: {publicFields.Count} public fields found");
}
```

### Scenario 4: Generate Documentation Report

```csharp
var data = module.Dump();
var report = new StringBuilder();

report.AppendLine("# Module Documentation");
report.AppendLine();

foreach (var type in data.Types)
{
    report.AppendLine($"## {type.Kind}: {type.Name}");
    if (!string.IsNullOrEmpty(type.Namespace))
        report.AppendLine($"**Namespace:** {type.Namespace}");
    
    if (!string.IsNullOrEmpty(type.BaseType))
      report.AppendLine($"**Extends:** {type.BaseType}");
    
    if (type.Methods.Length > 0)
    {
        report.AppendLine("### Methods");
        foreach (var method in type.Methods)
     {
   var name = method.IsConstructor ? ".ctor" : method.Name;
        var paramStr = string.Join(", ", 
         method.Parameters.Select(p => $"{p.Type} {p.Name}"));
     report.AppendLine($"- `{method.ReturnType} {name}({paramStr})`");
        }
    }
}

System.IO.File.WriteAllText("DOCUMENTATION.md", report.ToString());
```

## Data Model Reference

### ModuleData
Complete serialized module information.

**Properties:**
- `Name` (string) - Module name
- `Version` (string) - Module version
- `Metadata` (Dictionary<string, string>) - Custom metadata
- `Types` (TypeData[]) - All type definitions
- `Functions` (FunctionData[]) - All functions

### TypeData
Represents a type (class, interface, struct).

**Properties:**
- `Kind` (string) - "Class", "Interface", or "Struct"
- `Name` (string) - Type name
- `Namespace` (string) - Namespace
- `Access` (string) - "Public", "Internal", "Private", "Protected"
- `IsAbstract` (bool) - Abstract flag
- `IsSealed` (bool) - Sealed flag
- `BaseType` (string) - Base type name
- `Interfaces` (string[]) - Implemented interfaces
- `GenericParameters` (string[]) - Generic parameters
- `Fields` (FieldData[]) - Field definitions
- `Methods` (MethodData[]) - Method definitions
- `Properties` (PropertyData[]) - Property definitions

### MethodData
Represents a method definition.

**Properties:**
- `Name` (string) - Method name
- `ReturnType` (string) - Return type
- `Access` (string) - Access modifier
- `IsStatic` (bool) - Static flag
- `IsVirtual` (bool) - Virtual flag
- `IsOverride` (bool) - Override flag
- `IsAbstract` (bool) - Abstract flag
- `IsConstructor` (bool) - Constructor flag
- `Parameters` (ParameterData[]) - Parameters
- `LocalVariables` (LocalVariableData[]) - Local variables
- `InstructionCount` (int) - Number of instructions

## Tips & Best Practices

1. **Use `DumpJson()` for persistence** - JSON is portable and easy to parse
2. **Use `GenerateSummaryReport()` for quick overview** - Great for debugging
3. **Use `DumpText()` for logging** - Clean, readable format
4. **Use `Dump()` for programmatic analysis** - Strongly-typed and safe
5. **Use `DumpMarkdown()` for documentation** - Automatically generates docs
6. **Use `DumpCsv()` for data analysis** - Import into Excel/Sheets

## No Breaking Changes

The serialization features are completely additive. Your existing code continues to work without any modifications!

```csharp
// Your existing code
var module = builder.Build();

// Now with bonus: easy inspection and export!
Console.WriteLine(module.DumpText());
```
