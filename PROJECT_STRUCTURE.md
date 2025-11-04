# ObjectIR Project Structure

This document provides an overview of the entire ObjectIR project structure.

## Directory Layout

```
ObjectIR/
├── README.md                          # Project overview and quick start
├── LICENSE                            # MIT License
├── ObjectIR.sln                       # Visual Studio solution
│
├── src/
│   └── ObjectIR.Core/                 # Core library
│       ├── ObjectIR.Core.csproj       # Project file
│       ├── IR/                        # IR representation
│       │   ├── Module.cs              # Top-level module container
│       │   ├── TypeDefinition.cs      # Class, interface, struct, enum definitions
│       │   ├── TypeReference.cs       # Type references and primitive types
│       │   ├── Members.cs             # Fields, properties, generic parameters
│       │   ├── MethodDefinition.cs    # Methods, functions, parameters
│       │   ├── Instruction.cs         # Base instruction classes
│       │   ├── Instructions.cs        # Concrete instruction implementations
│       │   ├── ControlFlow.cs         # Structured control flow (if, while, try)
│       │   └── OpCodes.cs             # Enumeration of all opcodes
│       └── Builder/                   # Fluent builder API
│           └── IRBuilder.cs           # Builder classes for constructing IR
│
├── examples/                          # Example programs
│   └── CalculatorExample.cs           # Complete calculator example
│
├── docs/                              # Documentation
│   ├── GRAMMAR.md                     # Formal EBNF grammar specification
│   ├── GETTING_STARTED.md             # Beginner's guide
│   ├── ARCHITECTURE.md                # Architecture and design decisions
│   └── COMPLETE_EXAMPLE.md            # Todo list application example
│
└── tests/                             # Unit tests (to be added)
    └── ObjectIR.Core.Tests/
```

## Key Files

### Core Library (`src/ObjectIR.Core/`)

#### IR Representation (`IR/`)

**Module.cs**
- `Module` - Top-level container for types and functions
- Entry point for defining your IR program

**TypeDefinition.cs**
- `TypeDefinition` - Base class for all types
- `ClassDefinition` - Reference types with fields, methods, inheritance
- `InterfaceDefinition` - Abstract contracts
- `StructDefinition` - Value types
- `EnumDefinition` - Named integer constants
- `TypeKind` enum - Discriminates type kinds
- `AccessModifier` enum - Public, private, protected, internal

**TypeReference.cs**
- `TypeReference` - References to types in the IR
- Static properties for primitive types (Int32, String, etc.)
- Helper methods for generic types (List<T>, Dict<K,V>)
- Type construction methods (MakeArrayType, MakeGenericType)

**Members.cs**
- `FieldDefinition` - Instance and static fields
- `PropertyDefinition` - Properties with getters/setters
- `EnumMember` - Enum value definitions
- `GenericParameter` - Generic type parameters
- `TypeConstraint` - Constraints on generic parameters

**MethodDefinition.cs**
- `MethodDefinition` - Method definitions with body
- `FunctionDefinition` - Standalone functions
- `Parameter` - Method parameters
- `LocalVariable` - Local variables in methods
- `MethodReference` - References to methods for calls
- `FieldReference` - References to fields for access

**Instruction.cs**
- `Instruction` - Base class for all instructions
- `IInstructionVisitor` - Visitor pattern for instruction traversal
- `InstructionList` - Collection with helper emit methods

**Instructions.cs**
- All concrete instruction implementations:
  - Load instructions (LoadArgInstruction, LoadLocalInstruction, etc.)
  - Store instructions (StoreLocalInstruction, StoreFieldInstruction, etc.)
  - Arithmetic (ArithmeticInstruction)
  - Comparison (ComparisonInstruction)
  - Calls (CallInstruction, CallVirtualInstruction)
  - Object operations (NewObjectInstruction, CastInstruction, etc.)
  - Stack manipulation (DupInstruction, PopInstruction)
  - Conversions (ConversionInstruction)

**ControlFlow.cs**
- `IfInstruction` - Structured if-else statements
- `WhileInstruction` - While loops
- `TryInstruction` - Try-catch-finally blocks
- `CatchClause` - Exception catch clauses
- `ThrowInstruction` - Throw exceptions
- `Condition` - Base class for conditions
  - `StackCondition` - Use stack top as condition
  - `BinaryCondition` - Compare two values
  - `ExpressionCondition` - Evaluate expression

**OpCodes.cs**
- `OpCode` enum - All instruction opcodes
- `ArithmeticOp` enum - Arithmetic operations
- `ComparisonOp` enum - Comparison operations

#### Builder API (`Builder/`)

**IRBuilder.cs**
- `IRBuilder` - Main fluent builder class
- `ClassBuilder` - Build class definitions
- `InterfaceBuilder` - Build interface definitions
- `StructBuilder` - Build struct definitions
- `FieldBuilder` - Build field definitions
- `MethodBuilder` - Build method definitions
- `InstructionBuilder` - Build instruction sequences

### Examples (`examples/`)

**CalculatorExample.cs**
- Complete example showing how to build a Calculator class
- Demonstrates:
  - Class definition with fields
  - Constructor
  - Methods with parameters and locals
  - Stack-based operations
  - Generic collections (List<T>)
  - Control flow (if, while)
  - Method references and field references
- Includes pretty-printer for generated IR

### Documentation (`docs/`)

**GRAMMAR.md**
- Complete formal specification in EBNF
- Covers all language constructs
- Examples of each grammar rule
- Type safety rules
- Literal syntax

**GETTING_STARTED.md**
- Beginner-friendly introduction
- Core concepts explained
- Step-by-step examples
- Hello World
- Counter class
- Generic Stack<T>
- Control flow examples
- Common patterns
- Troubleshooting

**ARCHITECTURE.md**
- High-level architecture overview
- Design goals and decisions
- Core component descriptions
- Backend architecture
- Optimization pipeline
- Memory model
- Generics implementation
- Exception handling
- Debugging support
- Performance considerations
- Comparison with LLVM/CIL/JVM
- Future roadmap

**COMPLETE_EXAMPLE.md**
- Full todo list application
- Demonstrates:
  - Multiple classes
  - Interface implementation
  - Generic collections
  - Complex control flow
  - String operations
  - Complete program structure
- Shows both IR text format and C# builder API
- Includes backend considerations
- Testing strategies

## Getting Started with the Codebase

### 1. Explore the IR Classes

Start with `Module.cs` and work your way through:
```
Module → TypeDefinition → MethodDefinition → Instruction
```

### 2. Try the Builder API

Look at `CalculatorExample.cs` to see how to construct IR:
```csharp
var builder = new IRBuilder("MyApp");
builder.Class("MyClass")
    .Field("value", TypeReference.Int32)
    .Method("GetValue", TypeReference.Int32)
        .Body()
            // Instructions here
        .EndBody()
    .EndClass();
```

### 3. Read the Documentation

1. Start with README.md for overview
2. Read GETTING_STARTED.md for basic usage
3. Study GRAMMAR.md for formal specification
4. Dive into ARCHITECTURE.md for design details
5. Explore COMPLETE_EXAMPLE.md for real-world usage

### 4. Build Your First Program

Try building a simple class:
```csharp
var builder = new IRBuilder("HelloWorld");

builder.Class("Greeter")
    .Method("SayHello", TypeReference.Void)
        .Body()
            .Ldstr("Hello, ObjectIR!")
            .Call(new MethodReference(
                TypeReference.FromName("System.Console"),
                "WriteLine",
                TypeReference.Void,
                new List<TypeReference> { TypeReference.String }))
            .Ret()
        .EndBody()
        .EndMethod()
    .EndClass();

var module = builder.Build();
```

## Next Steps

### For Users
1. Install the package (when published): `dotnet add package ObjectIR.Core`
2. Read GETTING_STARTED.md
3. Try the examples
4. Build your own IR

### For Contributors
1. Read ARCHITECTURE.md
2. Look at the source code
3. Pick an area to work on:
   - Implement a backend (C#, JavaScript, C++, etc.)
   - Add optimization passes
   - Extend standard library
   - Write more examples
   - Improve documentation

### For Backend Developers
1. Study ARCHITECTURE.md backend section
2. Implement `IBackend` interface (to be added)
3. Map standard library types to your target
4. Handle instruction translation
5. Test with provided examples

## Building the Project

```bash
# Clone the repository
git clone https://github.com/yourusername/ObjectIR.git
cd ObjectIR

# Build the solution
dotnet build

# Run tests (when added)
dotnet test

# Create NuGet package
dotnet pack
```

## IDE Support

### Visual Studio
- Open `ObjectIR.sln`
- All projects and references configured

### Visual Studio Code
- Open the root folder
- Use C# extension
- Tasks configured for build/test

### JetBrains Rider
- Open `ObjectIR.sln`
- Full support for C# features

## Design Principles

1. **Simplicity** - Easy to understand and use
2. **Composability** - Small, focused classes that work together
3. **Type Safety** - Strong typing prevents errors
4. **Extensibility** - Easy to add new features
5. **Performance** - Efficient representation and operations
6. **Testability** - Easy to test each component

## Code Organization Principles

### Separation of Concerns
- **IR** - Pure data structures
- **Builder** - Construction API
- **Backends** - Target-specific compilation
- **Optimizers** - Transformation passes

### Immutability vs Mutability
- TypeReference: Immutable (safe to share)
- Instructions: Immutable once created
- Definitions: Mutable during construction, immutable after

### Naming Conventions
- Classes: PascalCase
- Methods: PascalCase
- Parameters: camelCase
- Fields: camelCase with _ prefix for private
- Constants: PascalCase

## Version History

- **v0.1.0** (Current)
  - Initial core IR implementation
  - Builder API
  - Type system
  - Instruction set
  - Documentation

## Roadmap

See ARCHITECTURE.md for detailed roadmap. Key milestones:

- **v0.2.0** - Text format parser and printer
- **v0.3.0** - Basic optimizer and validator
- **v0.4.0** - C# backend (CIL emission)
- **v0.5.0** - JavaScript backend
- **v1.0.0** - Production-ready release

## Contributing

We welcome contributions! Areas of interest:

1. **Backend implementations** - C#, C++, JavaScript, etc.
2. **Optimization passes** - Constant folding, dead code elimination, etc.
3. **Standard library** - More collection types, utilities
4. **Tools** - Visualizers, analyzers, formatters
5. **Documentation** - Tutorials, guides, examples
6. **Tests** - Unit tests, integration tests, benchmarks

## Community

- GitHub Issues - Bug reports and feature requests
- GitHub Discussions - Questions and ideas
- Pull Requests - Code contributions

## License

MIT License - See LICENSE file for details

## Credits

Inspired by:
- LLVM IR - Modular architecture
- .NET CIL - Type system and instruction set
- JVM Bytecode - Cross-platform vision
- WebAssembly - Structured control flow

---

**Project Status**: 🚧 Early Development

The core IR is functional but the project is in active development. APIs may change. Contributions and feedback welcome!
