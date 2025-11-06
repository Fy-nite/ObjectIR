# ObjectIR

**Object Intermediate Representation** - A high-level IR for object-oriented languages, inspired by LLVM IR but designed specifically for OOP semantics.

## Overview

ObjectIR is a typed intermediate representation that sits between high-level OOP languages (C#, Java, C++) and their target runtimes. It combines the precision of stack-based IRs like CIL with structured control flow for better readability and tooling support.

**Primary Design:** ObjectIR is primarily designed for execution on its own high-performance runtime, which provides a unified object model and advanced features for analysis, transformation, and cross-language interoperability.

**Multi-Target Capability:** In addition to its own runtime, ObjectIR can also be compiled to each language's native runtime (such as .NET CIL, JVM bytecode, C++ code, JavaScript, etc.), making it a powerful bridge for multi-platform and multi-language development.

### Key Features

- **Unified ObjectIR Runtime**: Execute modules directly on the ObjectIR runtime for maximum compatibility and advanced features
- **Hybrid approach**: CIL-style stack instructions + structured control flow (if/while/for)
- **Type-safe**: Strongly typed with full generic support
- **Multi-target**: Compile to .NET CIL, JVM bytecode, JavaScript, C++, Lua, etc.
- **Standard library**: Built-in generic types (List<T>, Dict<K,V>, Set<T>) that map to native implementations
- **Builder API**: Fluent C# API for constructing IR programmatically
- **Extensible**: Easy to add custom backends and optimization passes

## Quick Start

### Installation

```bash
dotnet add package ObjectIR.Core
```

### Example: Building a Calculator Class

```csharp
using ObjectIR.Core.Builder;
using ObjectIR.Core.IR;

var builder = new IRBuilder("CalculatorApp");

builder.Class("Calculator")
    .Field("history", TypeReference.List(TypeReference.Int32))
    .Field("lastResult", TypeReference.Int32)
    
    .Method("Add", TypeReference.Int32)
        .Parameter("a", TypeReference.Int32)
        .Parameter("b", TypeReference.Int32)
        .Local("result", TypeReference.Int32)
        .Body()
            .Ldarg("a")
            .Ldarg("b")
            .Add()
            .Stloc("result")
            .Ldloc("result")
            .Ret()
        .EndBody()
        .EndMethod()
    .EndClass();

var module = builder.Build();
```

### Generated IR (Text Format)

```
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
```

## Architecture

### Type System

ObjectIR provides a rich type system:

- **Primitives**: `void`, `bool`, `int8/16/32/64`, `uint8/16/32/64`, `float32/64`, `char`, `string`
- **Reference Types**: Classes, interfaces, delegates
- **Value Types**: Structs, enums
- **Generics**: Full generic support with constraints
- **Arrays**: Single and multi-dimensional

### Standard Library Types

Built-in generic types that backends must implement:

```csharp
System.List<T>
System.Dict<K, V>
System.Set<T>
System.Optional<T>
System.String
```

### Instruction Set

ObjectIR uses a hybrid instruction set:

**Stack-based operations** (CIL-style):
- Load/Store: `ldarg`, `ldloc`, `ldfld`, `stloc`, `stfld`
- Constants: `ldc.i4`, `ldc.r4`, `ldstr`, `ldnull`
- Arithmetic: `add`, `sub`, `mul`, `div`, `rem`, `neg`
- Comparison: `ceq`, `cgt`, `clt`
- Calls: `call`, `callvirt`, `newobj`
- Stack manipulation: `dup`, `pop`

**Structured control flow** (high-level):
- `if (condition) { ... } else { ... }`
- `while (condition) { ... }`
- `for (init; condition; increment) { ... }`
- `try { ... } catch (Type ex) { ... } finally { ... }`

## Backend Architecture

Backends implement the `IBackend` interface:

```csharp
public interface IBackend
{
    void Compile(Module module, Stream output);
    string[] GetSupportedTypes();
}
```

Example backends:

- **CSharpBackend**: Generates .NET CIL via System.Reflection.Emit
- **CppBackend**: Generates C++ with standard library mappings
- **JavaScriptBackend**: Generates JavaScript with runtime support
- **LuaBackend**: Generates Lua with metatables for OOP

### Type Mapping Example

| ObjectIR | C# | Java | C++ | JavaScript |
|----------|-----|------|-----|------------|
| `List<T>` | `List<T>` | `ArrayList<T>` | `std::vector<T>` | `Array` |
| `Dict<K,V>` | `Dictionary<K,V>` | `HashMap<K,V>` | `std::unordered_map<K,V>` | `Map` |
| `string` | `string` | `String` | `std::string` | `string` |

## Use Cases

### Compiler Development
Build compilers for new languages that target multiple platforms:
```
YourLanguage → ObjectIR → C# / C++ / JavaScript / etc.
```

### Code Analysis & Transformation
- Static analysis tools
- Optimization passes
- Code transformations
- Security analysis

### Cross-Platform Development
Write once in ObjectIR, compile to any supported platform.

### Language Interop
Bridge between different OOP languages with a common IR.

## Project Structure

```
ObjectIR/
├── src/
│   └── ObjectIR.Core/
│       ├── IR/                  # Core IR types
│       │   ├── Module.cs
│       │   ├── TypeDefinition.cs
│       │   ├── Instruction.cs
│       │   └── ...
│       └── Builder/             # Fluent builder API
│           └── IRBuilder.cs
├── examples/                    # Example programs
├── docs/                        # Documentation
└── tests/                       # Unit tests
```

## Formal Grammar

See [GRAMMAR.md](docs/GRAMMAR.md) for the complete formal specification.

## Roadmap

- [ ] Core IR implementation ✓ (Done)
- [ ] Builder API ✓ (Done)
- [ ] Text format parser
- [ ] Binary format (for performance) ✓ (BSON format implemented - [see docs](docs/BSON_SERIALIZATION.md))
- [ ] C# backend (CIL emission)
- [ ] JavaScript backend
- [ ] C++ backend
- [ ] Standard library definitions
- [ ] Optimization passes
- [ ] Debugger support (DWARF, PDB)
- [ ] LLVM backend (for native compilation)

## Contributing

Contributions welcome! Areas of interest:
- Backend implementations
- Optimization passes
- Standard library expansions
- Language frontends

## License

AGPL-V3 License - See LICENSE file for details

## Inspiration

ObjectIR draws inspiration from:
- **LLVM IR**: Modular architecture, SSA form concepts
- **.NET CIL**: Stack-based operations, rich type system
- **JVM Bytecode**: Cross-platform vision
- **WebAssembly**: Structured control flow
- **MLIR**: Extensible dialect system

## Getting Help

- Documentation: [docs/](docs/)
- Examples: [examples/](examples/)
- Issues: GitHub Issues
- Discussions: GitHub Discussions

---

**Status**: Early Development - API subject to change
