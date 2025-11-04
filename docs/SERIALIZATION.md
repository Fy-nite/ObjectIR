# Module Serialization Guide

The `ObjectIR.Core` library now provides comprehensive serialization and dumping capabilities for IR modules. This allows you to inspect, export, and analyze the contents of your modules in multiple formats.

## Overview

The `ModuleSerializer` class provides four main ways to dump module contents:

1. **JSON Format** - Machine-readable JSON representation
2. **Text Format** - Human-readable text representation
3. **Structured Objects** - Strongly-typed data transfer objects
4. **Arrays** - Typed arrays of types and functions

## Quick Start

After building your module, you can dump its contents:

```csharp
using ObjectIR.Core.Builder;
using ObjectIR.Core.Serialization;

var builder = new IRBuilder("MyModule");
// ... build your module ...
var module = builder.Build();

// Dump as JSON
string json = module.DumpJson();
Console.WriteLine(json);

// Dump as human-readable text
string text = module.DumpText();
Console.WriteLine(text);

// Dump as strongly-typed object
ModuleData data = module.Dump();

// Dump types as array
TypeData[] types = module.DumpTypes();

// Dump functions as array
FunctionData[] functions = module.DumpFunctions();
```

## Detailed Usage

### 1. JSON Dump

Exports the entire module structure as JSON:

```csharp
var json = module.DumpJson(indented: true);
// or without indentation:
var compactJson = module.DumpJson(indented: false);
```

**Output Format:**
```json
{
  "name": "MyModule",
  "version": "1.0.0.0",
  "metadata": { },
  "types": [
    {
      "kind": "Class",
      "name": "Program",
 "namespace": "MyNamespace",
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

### 2. Text Dump

Exports the module as human-readable text:

```csharp
var text = module.DumpText();
Console.WriteLine(text);
```

**Output Format:**
```
Module: MyModule
Version: 1.0.0.0

Types (1):
  sealed class MyNamespace.Program
    Methods:
      Public static System.Void Main() [3 instructions]

Functions (0):
```

### 3. Structured Object Dump

Returns a `ModuleData` object with all module information:

```csharp
var data = module.Dump();

Console.WriteLine($"Module: {data.Name}");
Console.WriteLine($"Version: {data.Version}");

foreach (var type in data.Types)
{
    Console.WriteLine($"Type: {type.Kind} - {type.Name}");
    foreach (var method in type.Methods)
    {
        Console.WriteLine($"  Method: {method.Name}");
        Console.WriteLine($"    Return Type: {method.ReturnType}");
        Console.WriteLine($"    Parameters: {method.Parameters.Length}");
        Console.WriteLine($"    Instructions: {method.InstructionCount}");
    }
}
```

### 4. Array Dumps

Dump types or functions as arrays for easier iteration:

```csharp
// Get all types in the module
var types = module.DumpTypes();
foreach (var type in types)
{
    Console.WriteLine($"Type: {type.Name} ({type.Kind})");
}

// Get all standalone functions
var functions = module.DumpFunctions();
foreach (var func in functions)
{
    Console.WriteLine($"Function: {func.Name}");
}
```

## Data Structures

### ModuleData
Represents a complete serialized module:
- `Name` - Module name
- `Version` - Module version
- `Metadata` - Custom metadata dictionary
- `Types[]` - Array of type definitions
- `Functions[]` - Array of function definitions

### TypeData
Represents a serialized type (class, interface, or struct):
- `Kind` - Type kind: "Class", "Interface", or "Struct"
- `Name` - Type name
- `Namespace` - Namespace (if any)
- `Access` - Access modifier: "Public", "Internal", "Private", "Protected"
- `IsAbstract` - Whether type is abstract
- `IsSealed` - Whether type is sealed
- `BaseType` - Qualified name of base type
- `Interfaces[]` - Qualified names of implemented interfaces
- `GenericParameters[]` - Generic parameter names
- `Fields[]` - Field definitions
- `Methods[]` - Method definitions
- `Properties[]` - Property definitions

### MethodData
Represents a serialized method:
- `Name` - Method name (or ".ctor" for constructors)
- `ReturnType` - Qualified return type name
- `Access` - Access modifier
- `IsStatic` - Whether method is static
- `IsVirtual` - Whether method is virtual
- `IsOverride` - Whether method overrides base method
- `IsAbstract` - Whether method is abstract
- `IsConstructor` - Whether this is a constructor
- `Parameters[]` - Method parameters
- `LocalVariables[]` - Local variable definitions
- `InstructionCount` - Number of instructions in method body

### FunctionData
Represents a serialized standalone function:
- `Name` - Function name
- `ReturnType` - Qualified return type name
- `Parameters[]` - Function parameters
- `LocalVariables[]` - Local variable definitions
- `InstructionCount` - Number of instructions in function body

## Usage Examples

### Example 1: Export Module to JSON File

```csharp
var module = builder.Build();
var json = module.DumpJson(indented: true);
System.IO.File.WriteAllText("module.json", json);
```

### Example 2: Validate Module Structure

```csharp
var data = module.Dump();
if (data.Types.Length == 0)
    Console.WriteLine("Warning: Module has no types!");

foreach (var type in data.Types)
{
    if (type.Methods.Length == 0)
        Console.WriteLine($"Warning: Type {type.Name} has no methods!");
}
```

### Example 3: Generate Documentation

```csharp
var data = module.Dump();
var sb = new StringBuilder();

sb.AppendLine("# Module Documentation");
sb.AppendLine($"## {data.Name} v{data.Version}");

foreach (var type in data.Types)
{
    sb.AppendLine($"### {type.Kind}: {type.Name}");
    
    if (type.BaseType != null)
  sb.AppendLine($"Base Type: `{type.BaseType}`");
    
    if (type.Methods.Length > 0)
    {
        sb.AppendLine("#### Methods:");
        foreach (var method in type.Methods)
 {
            var paramStr = string.Join(", ", method.Parameters.Select(p => $"`{p.Type} {p.Name}`"));
    sb.AppendLine($"- `{method.ReturnType} {method.Name}({paramStr})`");
        }
    }
}

Console.WriteLine(sb.ToString());
```

### Example 4: Analyze Instructions

```csharp
var data = module.Dump();
int totalInstructions = 0;

foreach (var type in data.Types)
{
    foreach (var method in type.Methods)
    {
    totalInstructions += method.InstructionCount;
        if (method.InstructionCount > 100)
          Console.WriteLine($"Large method: {type.Name}.{method.Name} has {method.InstructionCount} instructions");
    }
}

Console.WriteLine($"Total instructions: {totalInstructions}");
```

## Extension Methods

The library provides convenient extension methods on `Module`:

```csharp
module.Serialize()          // Create a ModuleSerializer instance
module.Dump()           // Get ModuleData object
module.DumpJson()   // Get JSON string
module.DumpText() // Get text representation
module.DumpTypes()    // Get TypeData array
module.DumpFunctions()      // Get FunctionData array
```

## Best Practices

1. **Use `DumpJson()` for persistence** - JSON is easy to store, parse, and debug
2. **Use `DumpText()` for logging** - Human-readable format is great for console output
3. **Use `Dump()` for programmatic access** - Strongly-typed objects are safer to work with
4. **Use array dumps for filtering** - `DumpTypes()` and `DumpFunctions()` give you easy LINQ access

## Integration with Existing Code

No changes to your module building code are required. The serialization feature is completely additive:

```csharp
// Existing code remains unchanged
var module = builder.Build();

// New: Add serialization
var json = module.DumpJson();
```
