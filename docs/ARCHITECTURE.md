# ObjectIR Architecture

This document describes the architecture and design decisions of ObjectIR.

## Design Goals

1. **Multi-target compilation** - Single IR → Multiple platforms
2. **OOP semantics preservation** - Maintain high-level concepts
3. **Developer friendly** - Easy to read, write, and understand
4. **Extensible** - Simple to add backends and optimizations
5. **Type safe** - Catch errors at IR level, not runtime
6. **Performant** - Efficient representation and compilation

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│                    Frontend Languages                        │
│     (Your Language, C#, Java, C++, TypeScript, etc.)        │
└────────────────────┬────────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────────┐
│                      ObjectIR Core                           │
│  ┌──────────────────────────────────────────────────────┐   │
│  │  IR Representation (Module, Types, Instructions)     │   │
│  └──────────────────────────────────────────────────────┘   │
│  ┌──────────────────────────────────────────────────────┐   │
│  │  Builder API (Fluent interface)                      │   │
│  └──────────────────────────────────────────────────────┘   │
│  ┌──────────────────────────────────────────────────────┐   │
│  │  Validator (Type checking, stack validation)        │   │
│  └──────────────────────────────────────────────────────┘   │
│  ┌──────────────────────────────────────────────────────┐   │
│  │  Optimizer (Optional transformation passes)         │   │
│  └──────────────────────────────────────────────────────┘   │
└────────────────────┬────────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────────┐
│                       Backends                               │
│   ┌──────────┬──────────┬───────────┬──────────┬─────────┐ │
│   │   CIL    │   JVM    │    C++    │   JS/TS  │   Lua   │ │
│   └──────────┴──────────┴───────────┴──────────┴─────────┘ │
└────────────────────┬────────────────────────────────────────┘
                     │
                     ▼
┌─────────────────────────────────────────────────────────────┐
│                    Target Runtimes                           │
│         (.NET, JVM, Native, Browser, Lua VM, ObjectIR runtime)                │
└─────────────────────────────────────────────────────────────┘
```

## Core Components

### 1. IR Representation

The IR consists of several key types:

#### Module
- Top-level container
- Contains types and metadata
- Version information

#### Types
- **ClassDefinition**: Reference types with inheritance
- **InterfaceDefinition**: Contracts
- **StructDefinition**: Value types
- **EnumDefinition**: Named constants

#### Members
- **FieldDefinition**: Data storage
- **MethodDefinition**: Executable code
- **PropertyDefinition**: Accessors
- **GenericParameter**: Type parameters

#### Instructions
- **Load/Store**: Stack manipulation
- **Arithmetic/Logic**: Operations
- **Calls**: Method invocation
- **Control Flow**: Branching and loops
- **Object Operations**: Creation and casting

### 2. Type System

ObjectIR uses a nominal type system with:

```
TypeReference
├── Primitive (int32, float32, bool, etc.)
├── Reference (classes, interfaces)
├── Generic (List<T>, Dict<K,V>)
└── Constructed (arrays, pointers)
```

**Type Safety Rules:**
1. All operations must have compatible types
2. Stack must be balanced at control flow joins
3. Generic type arguments must satisfy constraints
4. No implicit conversions (except safe widening)

### 3. Instruction Model

ObjectIR uses a **hybrid model**:

**Stack-based (like CIL/JVM):**
```
ldarg x
ldarg y
add
stloc result
```

**Structured control flow (like high-level languages):**
```
if (condition) {
    ...
} else {
    ...
}
```

This combines the benefits of both:
- Stack operations are compact and efficient
- Structured flow is readable and easy to optimize

#### Stack Semantics

Every instruction has a stack effect:
```
Instruction         | Stack Before | Stack After
--------------------|--------------|-------------
ldarg x             | ...          | ..., value
add                 | ..., a, b    | ..., result
stloc x             | ..., value   | ...
call F(T1,T2)->R    | ..., arg1, arg2 | ..., result
```

The validator ensures stack consistency at all merge points (branches, loops).

### 4. Standard Library

ObjectIR defines a minimal standard library that all backends must support:

```csharp
namespace System {
    class List<T> {
        method Add(item: T) -> void
        method Remove(item: T) -> bool
        method get_Item(index: int32) -> T
        method get_Count() -> int32
        // ... more methods
    }
    
    class Dict<K, V> {
        method Add(key: K, value: V) -> void
        method ContainsKey(key: K) -> bool
        method TryGetValue(key: K, out value: V) -> bool
        // ... more methods
    }
    
    class Set<T> { ... }
    class String { ... }
    class Optional<T> { ... }
}
```

Backends map these to native implementations:
- **C#**: `System.Collections.Generic.*`
- **Java**: `java.util.*`
- **C++**: `std::vector`, `std::unordered_map`
- **JavaScript**: `Array`, `Map`, `Set`

## Backend Architecture

### Backend Interface

```csharp
public interface IBackend
{
    // Compile a module to the target format
    void Compile(Module module, Stream output);
    
    // Get supported standard library types
    string[] GetSupportedTypes();
    
    // Optional: Optimize for target
    Module Optimize(Module module);
}
```

### Backend Responsibilities

1. **Type mapping** - Map ObjectIR types to target types
2. **Instruction translation** - Convert IR instructions to target code
3. **Control flow** - Handle if/while/try structures
4. **Calling conventions** - Match target ABI
5. **Standard library** - Provide runtime support for List<T>, etc.
6. **Memory management** - Handle GC, reference counting, or manual

### Example: C# Backend Flow

```
ObjectIR Module
    ↓
Type Mapping
    ClassDefinition → TypeBuilder
    MethodDefinition → MethodBuilder
    ↓
Instruction Translation
    ldarg → IL.Emit(OpCodes.Ldarg)
    if → IL.Emit(OpCodes.Br), labels
    ↓
Assembly Generation
    Save to .dll or .exe
```

### Example: C++ Backend Flow

```
ObjectIR Module
    ↓
Type Mapping
    ClassDefinition → class declaration
    List<T> → std::vector<T>
    ↓
Instruction Translation
    ldarg → variable access
    add → + operator
    if → if statement (preserve structure)
    ↓
Code Generation
    Generate .h and .cpp files
    Add standard library wrappers
```

## Optimization Pipeline

```
IR Input
    ↓
┌────────────────────┐
│ Validation Pass    │ ← Check type safety, stack balance
└────────────────────┘
    ↓
┌────────────────────┐
│ Constant Folding   │ ← Evaluate constants at compile time
└────────────────────┘
    ↓
┌────────────────────┐
│ Dead Code Elim     │ ← Remove unreachable code
└────────────────────┘
    ↓
┌────────────────────┐
│ Inlining           │ ← Inline small methods
└────────────────────┘
    ↓
┌────────────────────┐
│ Backend Specific   │ ← Target-specific optimizations
└────────────────────┘
    ↓
Optimized IR
```

## Memory Model

ObjectIR is **abstract** about memory management:

- **Reference types** (classes) are assumed to be heap-allocated
- **Value types** (structs) can be stack or heap allocated
- **Garbage collection** is assumed for references (backends without GC must provide alternative)

Backends handle memory differently:
- **C#/Java**: Use native GC
- **C++**: Smart pointers (`shared_ptr`, `unique_ptr`)
- **JavaScript**: Use native GC
- **Lua**: Use native GC

## Generics

ObjectIR supports **reified generics** (like C#, not Java):

```
class Container<T> {
    field items: List<T>
    
    method Add(item: T) -> void {
        ldarg this
        ldfld Container<T>.items
        ldarg item
        callvirt List<T>.Add(T) -> void
        ret
    }
}
```

At runtime, `Container<int32>` and `Container<string>` are distinct types.

**Backend strategies:**
- **C#/Java**: Native generic support
- **C++**: Template instantiation (monomorphization)
- **JavaScript**: Type erasure with runtime checks

## Exception Handling

```
try {
    // Code that might throw
} catch (ExceptionType ex) {
    // Handle exception
} finally {
    // Always execute
}
```

Maps to:
- **C#**: Native try-catch-finally
- **C++**: try-catch (finally via RAII)
- **JavaScript**: try-catch-finally
- **Java**: try-catch-finally

## Metadata and Attributes

ObjectIR supports custom metadata on types and members:

```csharp
class MyClass {
    [Serializable]
    [Description("User data")]
    field userData: string
}
```

Metadata is preserved through compilation and available via reflection.

## Text Format

ObjectIR has both binary and text formats:

**Text format** (human-readable):
```
module MyApp

class Calculator {
    field value: int32
    
    method Add(x: int32) -> void {
        ldarg this
        ldarg x
        add
        stfld value
        ret
    }
}
```

**Binary format** (compact, fast to parse):
- Magic number: `0x4F424952` ("OBIR")
- Version header
- Type table
- Method bodies (compressed instructions)

## Debugging Support

ObjectIR supports debugging through:

1. **Line numbers** - Instructions track source location
2. **Local variable names** - Preserved for debugging
3. **Debug symbols** - Generate PDB (Windows) or DWARF (Unix)
4. **Source maps** - For JavaScript backend

## Interoperability

### Calling External Code

```
[External("System.Math")]
class Math {
    static method Sqrt(x: float64) -> float64
}
```

### P/Invoke (Native code)

```
[DllImport("user32.dll")]
static method MessageBox(hwnd: int32, text: string, caption: string, type: int32) -> int32
```

## Performance Considerations

### IR Size
- Compact instruction encoding
- Shared string table for identifiers
- Type reference deduplication

### Compilation Speed
- Parallel method compilation
- Incremental compilation support
- Cached type resolution

### Runtime Performance
- Inline small methods
- Optimize hot paths
- Backend-specific optimizations

## Extensibility Points

1. **Custom instructions** - Add backend-specific hints
2. **Optimization passes** - Plug in custom optimizers
3. **Backends** - Implement `IBackend` interface
4. **Standard library extensions** - Add new generic types
5. **Metadata attributes** - Custom attributes for tools

## Security

### Type Safety
- All type operations validated
- No uninitialized memory access
- Bounds checking on arrays

### Sandboxing
- Backends can restrict:
  - File system access
  - Network access
  - System calls

### Code Verification
- Stack depth limits
- Loop bound analysis (optional)
- Resource usage tracking

## Comparison with Other IRs

| Feature | ObjectIR | LLVM IR | CIL | JVM Bytecode |
|---------|----------|---------|-----|--------------|
| Level | High (OOP) | Low | Medium | Medium |
| Control Flow | Structured | CFG | Stack+Labels | Stack+Labels |
| Generics | Reified | No | Reified | Erased |
| OOP | Native | Manual | Native | Native |
| Multi-target | Yes | Via backends | .NET only | JVM only |
| Readability | High | Medium | Low | Low |

## Future Directions

### Short Term
- [ ] Text format parser
- [ ] Basic optimizer
- [ ] C# backend (CIL emission)
- [ ] JavaScript backend

### Medium Term
- [ ] LLVM backend for native compilation
- [ ] JIT compilation support
- [ ] Advanced optimizations (loop unrolling, etc.)
- [ ] WebAssembly backend

### Long Term
- [ ] Async/await support
- [ ] SIMD instructions
- [ ] GPU compute kernels
- [ ] Formal verification tools
- [ ] Interactive debugger

## Contributing

See [CONTRIBUTING.md](../CONTRIBUTING.md) for guidelines on:
- Adding backends
- Writing optimization passes
- Extending the standard library
- Documentation improvements

## References

- [LLVM IR Language Reference](https://llvm.org/docs/LangRef.html)
- [ECMA-335 CLI Specification](https://www.ecma-international.org/publications/standards/Ecma-335.htm)
- [Java Virtual Machine Specification](https://docs.oracle.com/javase/specs/jvms/se17/html/)
- [WebAssembly Specification](https://webassembly.github.io/spec/)
