# ObjectIR C# Backend

This is a C# backend for the ObjectIR framework that converts ObjectIR intermediate representation into compilable C# code.

## Overview

The C# backend takes ObjectIR modules and generates well-formatted, idiomatic C# code. It handles:

- **Type Definitions**: Classes, interfaces, structs, and enums
- **Members**: Fields, properties, and methods
- **Instructions**: All IR instructions are converted to C# statements and expressions
- **Control Flow**: If-else, while loops, try-catch-finally, and break/continue statements
- **Access Modifiers**: Public, private, protected, and internal
- **Generics**: Generic parameters and constraints

## Architecture

### Main Components

#### CSharpCodeGenerator
The main generator class that orchestrates the code generation process:

- Manages indentation and formatting
- Generates type definitions (classes, interfaces, structs, enums)
- Handles namespace and using statements
- Formats type references to C# syntax
- Manages parameter and field generation

#### CSharpInstructionVisitor
Implements the visitor pattern to convert ObjectIR instructions to C# code:

- Converts stack-based IR instructions to C# variables and expressions
- Handles arithmetic, comparison, and logical operations
- Manages method calls (both static and virtual)
- Implements control flow visitors (if, while, try-catch)
- Maintains a stack for expression evaluation

## Usage

### Basic Example

```csharp
using ObjectIR.Core.IR;
using ObjectIR.CSharpBackend;

// Create a module
var module = new Module("MyNamespace");

// Define a class
var myClass = module.DefineClass("MyClass");
myClass.Namespace = "MyNamespace";

// Add a method
var method = myClass.DefineMethod("MyMethod", TypeReference.String);
method.DefineParameter("input", TypeReference.String);

// Add instructions
method.Instructions.EmitLoadArg("input");
method.Instructions.EmitReturn();

// Generate C# code
var generator = new CSharpCodeGenerator();
string csharpCode = generator.Generate(module);

// Save or use the code
File.WriteAllText("output.cs", csharpCode);
```

### Advanced Example

```csharp
// Define a struct with interfaces
var myStruct = module.DefineStruct("Point");
myStruct.Interfaces.Add(new TypeReference("IComparable"));

// Add fields
var xField = myStruct.DefineField("X", TypeReference.Float32);
xField.Access = AccessModifier.Public;

var yField = myStruct.DefineField("Y", TypeReference.Float32);
yField.Access = AccessModifier.Public;

// Generate code
var generator = new CSharpCodeGenerator();
string output = generator.Generate(module);
```

## Instruction Support

The backend supports the following IR instructions:

### Load Instructions
- `LoadArgInstruction` - Load parameter value
- `LoadLocalInstruction` - Load local variable
- `LoadFieldInstruction` - Load instance field
- `LoadStaticFieldInstruction` - Load static field
- `LoadConstantInstruction` - Load constant value
- `LoadNullInstruction` - Load null value

### Store Instructions
- `StoreArgInstruction` - Store to parameter
- `StoreLocalInstruction` - Store to local variable
- `StoreFieldInstruction` - Store to instance field
- `StoreStaticFieldInstruction` - Store to static field

### Arithmetic & Logic
- `ArithmeticInstruction` - Operations: +, -, *, /, %, &, |, ^, <<, >>
- `ComparisonInstruction` - Operations: ==, !=, >, >=, <, <=

### Method Calls
- `CallInstruction` - Static/direct method call
- `CallVirtualInstruction` - Virtual method call

### Object Creation
- `NewObjectInstruction` - Create object instance
- `NewArrayInstruction` - Create array

### Type Operations
- `CastInstruction` - Type cast
- `IsInstanceInstruction` - Type check (is operator)
- `ConversionInstruction` - Type conversion

### Control Flow
- `IfInstruction` - If-else statements
- `WhileInstruction` - While loops
- `BreakInstruction` - Break statement
- `ContinueInstruction` - Continue statement
- `TryInstruction` - Try-catch-finally blocks
- `ThrowInstruction` - Throw exception

### Stack Operations
- `DupInstruction` - Duplicate stack value
- `PopInstruction` - Pop stack value
- `ReturnInstruction` - Return from method

## Type Mapping

ObjectIR primitive types are mapped to C# types:

| ObjectIR | C# |
|----------|-----|
| void | void |
| bool | bool |
| int8 | sbyte |
| uint8 | byte |
| int16 | short |
| uint16 | ushort |
| int32 | int |
| uint32 | uint |
| int64 | long |
| uint64 | ulong |
| float32 | float |
| float64 | double |
| char | char |
| string | string |

## Features

### Namespace Support
File-scoped namespaces (C# 10+):
```csharp
namespace MyNamespace;
```

### Generic Support
Classes, methods, and types can be generic:
```csharp
public class MyClass<T, U> where T : class, IComparable where U : struct
{
    // ...
}
```

### Access Modifiers
All four C# access levels are supported:
- `public`
- `private`
- `protected`
- `internal`

### Auto-Properties
Properties are generated as auto-properties:
```csharp
public int Value { get; set; }
```

### Method Modifiers
Supported method modifiers:
- `static`
- `virtual`
- `override`
- `abstract`

## Output Formatting

The generated code uses:
- **Indentation**: 4 spaces per level
- **Namespaces**: File-scoped (C# 10+)
- **Usings**: Standard .NET usings (System, System.Collections.Generic, System.Linq)
- **Blank Lines**: Strategic placement between members for readability

## Example Output

Input:
```csharp
var module = new Module("Demo");
var cls = module.DefineClass("Calculator");
var method = cls.DefineMethod("Add", TypeReference.Int32);
method.DefineParameter("a", TypeReference.Int32);
method.DefineParameter("b", TypeReference.Int32);
```

Output:
```csharp
using System;
using System.Collections.Generic;
using System.Linq;

namespace Demo;
public class Calculator
{
    public int Add(int a, int b)
    {
    }
}
```

## Building and Running

```bash
# Build the project
dotnet build ObjectIR.CSharpBackend

# Run the example
dotnet run --project ObjectIR.CSharpBackend

# The generated code is saved to generated_example.cs
```

## Limitations

- String concatenation and complex expressions require manual instruction composition
- Custom property getters/setters use auto-properties (enhancement opportunity)
- Delegate types are not yet supported
- Attribute generation is not implemented

## Future Enhancements

- Support for attributes on types, methods, and properties
- Custom getter/setter generation for properties
- Delegate type support
- Async/await support
- Lambda expressions
- Expression trees
- Nullable reference type annotations
