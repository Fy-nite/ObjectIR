# ObjectIR JSON Specification

This document specifies the JSON format used for ObjectIR module serialization, which is consumed by the ObjectIR runtime (OJRuntime) and produced by various ObjectIR compilers including the Fortran compiler.

## Overview

ObjectIR modules are serialized as JSON objects containing metadata, type definitions, and executable instructions. This format enables:

- **Execution**: Runtime loading and execution of compiled modules
- **Inspection**: Analysis and debugging of module contents
- **Interoperability**: Exchange of modules between different ObjectIR implementations
- **Persistence**: Storage and retrieval of compiled code

## JSON Schema

### Root Module Object

```json
{
  "Name": "string",
  "Version": "string",
  "Metadata": {
    "additionalProperties": "string"
  },
  "Types": [
    // Array of type definitions
  ],
  "Functions": [
    // Array of global function definitions (optional)
  ]
}
```

#### Fields

- **Name** (string): Module name identifier
- **Version** (string): Module version in semantic versioning format (e.g., "1.0.0")
- **Metadata** (object): Optional key-value metadata pairs
- **Types** (array): Array of type definitions (classes, interfaces, structs, enums)
- **Functions** (array): Array of global function definitions (may be empty)

### Type Definition

```json
{
  "Kind": "Class" | "Interface" | "Struct" | "Enum",
  "Name": "string",
  "Namespace": "string",
  "Access": "Public" | "Private" | "Protected" | "Internal",
  "IsAbstract": boolean,
  "IsSealed": boolean,
  "BaseType": "string" | null,
  "Interfaces": ["string"],
  "BaseInterfaces": ["string"],
  "GenericParameters": ["string"],
  "Fields": [
    // Field definitions
  ],
  "Methods": [
    // Method definitions
  ],
  "Properties": [
    // Property definitions
  ]
}
```

#### Type Kinds

- **Class**: Reference type with fields and methods
- **Interface**: Contract defining method signatures
- **Struct**: Value type with fields
- **Enum**: Enumeration of named constants

#### Notes

- `BaseType` specifies the base class (null for interfaces and object)
- `Interfaces` lists implemented interfaces
- `BaseInterfaces` lists base interfaces (for interfaces only)

### Field Definition

```json
{
  "Name": "string",
  "Type": "string",
  "Access": "Public" | "Private" | "Protected" | "Internal",
  "IsStatic": boolean,
  "IsReadOnly": boolean
}
```

### Function Definition

```json
{
  "Name": "string",
  "ReturnType": "string",
  "Parameters": [
    {
      "Name": "string",
      "Type": "string"
    }
  ],
  "LocalVariables": [
    {
      "Name": "string",
      "Type": "string"
    }
  ],
  "InstructionCount": number,
  "Instructions": [
    // Instruction objects
  ]
}
```

Global functions are similar to static methods but defined at the module level rather than within a type.

### Property Definition

```json
{
  "Name": "string",
  "Type": "string",
  "Access": "Public" | "Private" | "Protected" | "Internal",
  "IsStatic": boolean,
  "HasGetter": boolean,
  "HasSetter": boolean
}
```

### Method Definition

```json
{
  "Name": "string",
  "ReturnType": "string",
  "Access": "Public" | "Private" | "Protected" | "Internal",
  "IsStatic": boolean,
  "IsVirtual": boolean,
  "IsOverride": boolean,
  "IsAbstract": boolean,
  "IsConstructor": boolean,
  "Parameters": [
    {
      "Name": "string",
      "Type": "string"
    }
  ],
  "LocalVariables": [
    {
      "Name": "string",
      "Type": "string"
    }
  ],
  "InstructionCount": number,
  "Instructions": [
    // Instruction objects
  ]
}
```

## Instruction Format

Instructions are represented as objects with an `opCode` and optional `operand`:

```json
{
  "opCode": "string",
  "operand": object | null
}
```

### Load Instructions

#### ldarg - Load Argument
```json
{
  "opCode": "ldarg",
  "operand": {
    "argumentName": "string"
  }
}
```

#### ldloc - Load Local Variable
```json
{
  "opCode": "ldloc",
  "operand": {
    "localName": "string"
  }
}
```

#### ldc - Load Constant
```json
{
  "opCode": "ldc",
  "operand": {
    "value": "string",
    "type": "string"
  }
}
```

#### ldfld - Load Instance Field
```json
{
  "opCode": "ldfld",
  "operand": {
    "field": {
      "declaringType": "string",
      "name": "string",
      "type": "string"
    }
  }
}
```

#### ldsfld - Load Static Field
```json
{
  "opCode": "ldsfld",
  "operand": {
    "field": {
      "declaringType": "string",
      "name": "string",
      "type": "string"
    }
  }
}
```

#### ldnull - Load Null Reference
```json
{
  "opCode": "ldnull"
}
```

### Store Instructions

#### starg - Store to Argument
```json
{
  "opCode": "starg",
  "operand": {
    "argumentName": "string"
  }
}
```

#### stloc - Store to Local Variable
```json
{
  "opCode": "stloc",
  "operand": {
    "localName": "string"
  }
}
```

#### stfld - Store to Instance Field
```json
{
  "opCode": "stfld",
  "operand": {
    "field": {
      "declaringType": "string",
      "name": "string",
      "type": "string"
    }
  }
}
```

#### stsfld - Store to Static Field
```json
{
  "opCode": "stsfld",
  "operand": {
    "field": {
      "declaringType": "string",
      "name": "string",
      "type": "string"
    }
  }
}
```

### Arithmetic Instructions

#### add - Addition
```json
{
  "opCode": "add"
}
```

#### sub - Subtraction
```json
{
  "opCode": "sub"
}
```

#### mul - Multiplication
```json
{
  "opCode": "mul"
}
```

#### div - Division
```json
{
  "opCode": "div"
}
```

#### rem - Remainder
```json
{
  "opCode": "rem"
}
```

### Comparison Instructions

#### ceq - Equal
```json
{
  "opCode": "ceq"
}
```

#### cne - Not Equal
```json
{
  "opCode": "cne"
}
```

#### cgt - Greater Than
```json
{
  "opCode": "cgt"
}
```

#### cge - Greater Than or Equal
```json
{
  "opCode": "cge"
}
```

#### clt - Less Than
```json
{
  "opCode": "clt"
}
```

#### cle - Less Than or Equal
```json
{
  "opCode": "cle"
}
```

### Call Instructions

#### call - Static/Instance Method Call
```json
{
  "opCode": "call",
  "operand": {
    "method": {
      "declaringType": "string",
      "name": "string",
      "returnType": "string",
      "parameterTypes": ["string"]
    }
  }
}
```

#### callvirt - Virtual Method Call
```json
{
  "opCode": "callvirt",
  "operand": {
    "method": {
      "declaringType": "string",
      "name": "string",
      "returnType": "string",
      "parameterTypes": ["string"]
    }
  }
}
```

### Object Instructions

#### newobj - Create New Object
```json
{
  "opCode": "newobj",
  "operand": {
    "type": "string"
  }
}
```

#### newarr - Create New Array
```json
{
  "opCode": "newarr",
  "operand": {
    "elementType": "string"
  }
}
```

#### cast - Type Cast
```json
{
  "opCode": "cast",
  "operand": {
    "targetType": "string"
  }
}
```

### Stack Manipulation

#### dup - Duplicate Top of Stack
```json
{
  "opCode": "dup"
}
```

#### pop - Remove Top of Stack
```json
{
  "opCode": "pop"
}
```

### Unary Operations

#### neg - Negate
```json
{
  "opCode": "neg"
}
```

#### not - Logical Not
```json
{
  "opCode": "not"
}
```

### Array Instructions

#### ldelem - Load Array Element
```json
{
  "opCode": "ldelem"
}
```

#### stelem - Store Array Element
```json
{
  "opCode": "stelem"
}
```

### Control Flow Instructions

#### ret - Return from Method
```json
{
  "opCode": "ret"
}
```

#### if - Conditional Branch
```json
{
  "opCode": "if",
  "operand": {
    "thenBlock": [
      // Instruction array
    ],
    "elseBlock": [
      // Instruction array (optional)
    ]
  }
}
```

#### while - While Loop
```json
{
  "opCode": "while",
  "operand": {
    "condition": "string", // Simplified for spec
    "body": [
      // Instruction array
    ]
  }
}
```

#### try - Try-Catch-Finally
```json
{
  "opCode": "try",
  "operand": {
    "tryBlock": [
      // Instruction array
    ],
    "catchClauses": [
      {
        "exceptionType": "string" | null,
        "body": [
          // Instruction array
        ]
      }
    ],
    "finallyBlock": [
      // Instruction array (optional)
    ]
  }
}
```

#### throw - Throw Exception
```json
{
  "opCode": "throw"
}
```

## Type System

### Primitive Types

- `void` - No value
- `bool` - Boolean value
- `char` - Unicode character
- `int8` - 8-bit signed integer
- `int16` - 16-bit signed integer
- `int32` - 32-bit signed integer
- `int64` - 64-bit signed integer
- `uint8` - 8-bit unsigned integer
- `uint16` - 16-bit unsigned integer
- `uint32` - 32-bit unsigned integer
- `uint64` - 64-bit unsigned integer
- `float32` - 32-bit floating point
- `float64` - 64-bit floating point

### Reference Types

- `System.string` - String type
- `System.object` - Base object type
- Arrays: `TypeName[]` (e.g., `System.string[]`)
- User-defined types: `Namespace.TypeName`

## Examples

### Simple Module

```json
{
  "Name": "HelloWorld",
  "Version": "1.0.0",
  "Metadata": {},
  "Types": [
    {
      "Kind": "Class",
      "Name": "Program",
      "Namespace": "HelloWorld",
      "Access": "Public",
      "IsAbstract": false,
      "IsSealed": false,
      "Interfaces": [],
      "GenericParameters": [],
      "Fields": [],
      "Methods": [
        {
          "Name": "Main",
          "ReturnType": "void",
          "Access": "Public",
          "IsStatic": true,
          "IsVirtual": false,
          "IsOverride": false,
          "IsAbstract": false,
          "IsConstructor": false,
          "Parameters": [],
          "LocalVariables": [],
          "InstructionCount": 1,
          "Instructions": [
            {
              "opCode": "ret"
            }
          ]
        }
      ],
      "Properties": []
    }
  ]
}
```

### Arithmetic Example

```json
{
  "Name": "Calculator",
  "Version": "1.0.0",
  "Metadata": {},
  "Types": [
    {
      "Kind": "Class",
      "Name": "Calculator",
      "Namespace": "Calculator",
      "Access": "Public",
      "IsAbstract": false,
      "IsSealed": false,
      "Interfaces": [],
      "GenericParameters": [],
      "Fields": [],
      "Methods": [
        {
          "Name": "Add",
          "ReturnType": "int32",
          "Access": "Public",
          "IsStatic": true,
          "IsVirtual": false,
          "IsOverride": false,
          "IsAbstract": false,
          "IsConstructor": false,
          "Parameters": [
            {
              "Name": "a",
              "Type": "int32"
            },
            {
              "Name": "b",
              "Type": "int32"
            }
          ],
          "LocalVariables": [
            {
              "Name": "result",
              "Type": "int32"
            }
          ],
          "InstructionCount": 4,
          "Instructions": [
            {
              "opCode": "ldarg",
              "operand": {
                "argumentName": "a"
              }
            },
            {
              "opCode": "ldarg",
              "operand": {
                "argumentName": "b"
              }
            },
            {
              "opCode": "add"
            },
            {
              "opCode": "ret"
            }
          ]
        }
      ],
      "Properties": []
    }
  ]
}
```

## Runtime Consumption

The ObjectIR runtime (OJRuntime) loads and executes modules in this JSON format. The runtime:

1. Parses the JSON structure
2. Validates the schema compliance
3. Loads types and methods into memory
4. Executes instructions in method bodies
5. Manages the evaluation stack and local variables

Invalid JSON or unsupported constructs will cause runtime errors. Ensure all referenced types and methods are properly defined or available in the runtime environment.

- All string values are case-sensitive
- Type names should be fully qualified (e.g., `System.string` not `string`)
- Array types are denoted with `[]` suffix
- Generic types use angle brackets: `List<int32>`
- Null values are represented as JSON `null`
- Boolean values are lowercase `true`/`false`
- Numeric values in constants are stored as strings to preserve precision

## Validation

JSON modules should be validated against this schema before execution. The runtime will reject malformed modules with appropriate error messages.

## Version History

- **v1.0.0**: Initial specification covering basic types, methods, and instructions
- Future versions may add support for additional instruction types, type constructs, and metadata fields</content>
<parameter name="filePath">d:\ObjectIR\docs\OBJECTIR_JSON_SPEC.md