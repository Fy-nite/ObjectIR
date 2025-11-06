# ObjectIR C++ Runtime - Architecture & Design

## Overview

The ObjectIR C++ Runtime is a high-performance, object-oriented execution engine designed to interpret and execute ObjectIR intermediate representation with full support for OOP semantics.

## Design Philosophy

1. **Fidelity to ObjectIR Semantics**: Precise implementation of ObjectIR's type system and execution model
2. **Modern C++**: Leverage C++17 features (variant, optional, shared_ptr) for safe memory management
3. **Extensibility**: Clean interfaces for adding new types, methods, and backends
4. **Performance**: Minimize overhead in hot paths (method dispatch, stack operations)
5. **Developer Experience**: Fluent APIs and clear error messages

## System Architecture

```
┌────────────────────────────────────────────────────────┐
│                  Application Layer                     │
│          (Your C++ program using the runtime)          │
└────────────────────┬─────────────────────────────────┘
                     │
┌────────────────────┴─────────────────────────────────┐
│                 RuntimeBuilder API                    │
│         (Fluent interface for building classes)       │
└────────────────────┬─────────────────────────────────┘
                     │
┌────────────────────┴─────────────────────────────────┐
│                 VirtualMachine                        │
│   - Class registry                                    │
│   - Object factory                                    │
│   - Method dispatcher                                │
│   - Execution context management                     │
└────────────────────┬─────────────────────────────────┘
                     │
       ┌─────────────┼─────────────┐
       │             │             │
       ▼             ▼             ▼
   ┌────────┐   ┌────────┐   ┌────────┐
   │ Type   │   │ Object │   │ Method │
   │System  │   │ Model  │   │Dispatch│
   └────────┘   └────────┘   └────────┘
```

## Core Components

### 1. Type System (TypeReference)

**Purpose**: Represent types at runtime

**Design**:
- **Primitive Types**: Built-in types (int32, float64, string, bool, void)
- **Reference Types**: User-defined classes and interfaces
- **Nominal Typing**: Types identified by name, not structure

**Key Methods**:
```cpp
TypeReference::Int32()      // Create int32 type
TypeReference::Object(cls)  // Create class reference type
bool IsPrimitive() const    // Check if primitive
```

**Memory Model**: Stack-allocated, immutable after creation

### 2. Value Type (Value)

**Purpose**: Represent runtime values with variant storage

**Design**:
- **Tagged Union**: Uses `std::variant` for type-safe storage
- **8 Value Variants**: null, int32, int64, float32, float64, bool, string, object
- **Type Checking**: Compile-time and runtime validation

**Key Methods**:
```cpp
Value v(42);                    // Constructor
v.IsInt32()                     // Type check
int32_t n = v.AsInt32()        // Type-safe extraction
```

**Type Safety**: Exception thrown on invalid type conversion

**Variant Index Layout**:
```
Index 0: nullptr_t
Index 1: int32_t
Index 2: int64_t
Index 3: float
Index 4: double
Index 5: bool
Index 6: std::string
Index 7: ObjectRef (shared_ptr<Object>)
```

### 3. Object Model (Object, Class)

**Purpose**: Implement OOP object model with inheritance and composition

**Design**:

**Object Class**:
- Represents runtime instances
- Stores field values in `unordered_map<string, Value>`
- Tracks class type and base instance for inheritance

**Class Class**:
- Represents class metadata
- Contains field and method definitions
- Supports single inheritance and interface implementation

**Inheritance Chain**:
```cpp
class Base { ... };
class Derived : Base { ... };

auto derived = std::make_shared<Class>("Derived");
derived->SetBaseClass(baseClass);

// Method lookup traverses inheritance chain
auto method = derived->LookupMethod("name");
```

**Field Resolution**:
```cpp
// GetField searches in order:
// 1. Object's own fields
// 2. Base instance fields (if inherited)
// 3. Throw if not found
```

### 4. Method System (Method, NativeMethodImpl)

**Purpose**: Implement method dispatch and invocation

**Design**:

**Method Definition**:
- Stores name, return type, parameter list, static flag
- Holds native implementation function pointer

**Native Method Signature**:
```cpp
using NativeMethodImpl = std::function<Value(ObjectRef, const std::vector<Value>&, VirtualMachine*)>;
                                     │        │        │
                                     │        │        └─ Runtime reference
                                     │        └──────────── Arguments
                                     └─────────────────────── 'this' pointer
```

**Method Dispatch** (Virtual Dispatch):
```cpp
// Instance method call
auto method = object->GetClass()->LookupMethod(name);
method->GetNativeImpl()(object, args, vm);

// Static method call
auto method = classType->LookupMethod(name);
method->GetNativeImpl()(nullptr, args, vm);
```

### 5. Execution Context (ExecutionContext)

**Purpose**: Maintain execution state during method invocation

**Design**:
- **Stack**: LIFO for expression evaluation
- **Locals**: Fixed-size array for local variables
- **This Pointer**: Current object context
- **Method Reference**: Currently executing method

**Stack Operations**:
```cpp
ctx->PushStack(Value(42));      // Push value
Value v = ctx->PopStack();      // Pop and get
Value peek = ctx->PeekStack();  // Peek without pop
```

**Local Variables**:
```cpp
ctx->SetLocal(0, Value(10));    // Set local
Value v = ctx->GetLocal(0);     // Get local
```

### 6. Virtual Machine (VirtualMachine)

**Purpose**: Runtime engine managing execution and state

**Design**:

**Responsibilities**:
1. **Class Registry**: Map from class name to ClassRef
2. **Object Factory**: Create instances
3. **Method Dispatcher**: Route method calls to implementations
4. **Context Stack**: Manage nested method calls

**Class Registry**:
```cpp
vm->RegisterClass(myClass);          // Register
auto cls = vm->GetClass("MyClass");   // Lookup
bool has = vm->HasClass("MyClass");   // Check
```

**Method Invocation**:
```cpp
// Instance method
Value result = vm->InvokeMethod(obj, "methodName", {arg1, arg2});

// Static method
Value result = vm->InvokeStaticMethod(classType, "methodName", {arg1});
```

**Execution Stack**:
```cpp
// When calling nested methods
vm->PushContext(newContext);  // Save current, switch to new
vm->PopContext();             // Restore previous
```

### 7. Generic Collections

**Purpose**: Provide type-safe collection support

**Design**:

**Base Class Hierarchy**:
```cpp
class ListBase : public Object {  // Polymorphic interface
    virtual size_t GetSize() = 0;
    virtual Value GetAt(size_t) = 0;
    virtual void Add(const Value&) = 0;
};

template<typename T>
class List : public ListBase {    // Template implementation
    std::vector<T> _items;
    // Type-specific storage
};
```

**Specializations**:
- `List<int32_t>`: Integer lists
- `List<std::string>`: String lists
- `List<ObjectRef>`: Object lists

**Type Mapping**:
```cpp
TypeReference::Int32()    →  List<int32_t>
TypeReference::String()   →  List<std::string>
TypeReference::Object()   →  List<ObjectRef>
```

### 8. Builder API (RuntimeBuilder)

**Purpose**: Fluent interface for constructing runtime metadata

**Design**: Fluent method chaining pattern

```cpp
builder
    .Class("MyClass")
        .Field("x", TypeReference::Int32())
        .Method("getX", TypeReference::Int32(), false)
            .NativeImpl([...] { return x; })
            .EndMethod()
    .EndClass()
    .Class("OtherClass")
        ...
    .EndClass();

auto vm = builder.Release();
```

**State Machine**:
```
Initial
  → Class() → InClass
  → Field() → InClass (can repeat)
  → Method() → InMethod
  → Parameter() → InMethod (can repeat)
  → NativeImpl() → InMethod
  → EndMethod() → InClass
  → EndClass() → Initial
```

## Memory Management Model

### Ownership

**Reference Counting** via `std::shared_ptr`:

```cpp
using ObjectRef = std::shared_ptr<Object>;
using ClassRef = std::shared_ptr<Class>;

// Automatic cleanup when last ref is dropped
ObjectRef obj = vm->CreateObject(cls);  // Ref count = 1
{
    ObjectRef copy = obj;                // Ref count = 2
}  // Ref count = 1
// Ref count = 0 when obj goes out of scope → destroyed
```

### Lifetime

**ClassRef Lifetime**:
- Registered with VirtualMachine
- Kept alive by VM's class registry
- Destroyed when VM is destroyed

**ObjectRef Lifetime**:
- Created by VM or explicitly
- Referenced by fields and local variables
- Automatically cleaned up by shared_ptr

**MethodRef Lifetime**:
- Owned by Class
- Kept alive as long as Class exists

## Execution Flow Example

**Code**:
```cpp
auto calc = vm->CreateObject("Calculator");
calc->SetField("lastResult", Value(0));
auto result = vm->InvokeMethod(calc, "Add", {Value(10), Value(20)});
```

**Execution Steps**:

1. **Object Creation**:
   - `CreateObject("Calculator")` finds class in registry
   - `Class::CreateInstance()` creates new Object
   - Sets class reference on object
   - Returns ObjectRef with shared_ptr count = 1

2. **Field Setting**:
   - `SetField` stores Value in object's `_fieldValues` map
   - String key: "lastResult"
   - Value variant: int32_t with value 0

3. **Method Invocation**:
   - `InvokeMethod` looks up method in object's class
   - Class::LookupMethod traverses inheritance chain
   - Finds MethodRef with native implementation
   - Calls `impl(calc, {10, 20}, vm)`
   - Implementation receives:
     - `thisPtr`: calc object reference
     - `args`: {Value(10), Value(20)}
     - `vm`: reference to virtual machine
   - Returns Value result

4. **Return Value**:
   - Result Value extracted and returned
   - Automatically cleaned up if not stored

## Error Handling

**Strategy**: Exception-based

```cpp
// Type mismatch
Value v(42);
int64_t x = v.AsInt64();  // Throws: "Value is not int64"

// Method not found
vm->InvokeMethod(obj, "NoSuchMethod", {});  // Throws: "Method not found"

// Field not found
obj->GetField("NoField");  // Throws: "Field not found"

// Stack underflow
ctx->PopStack();  // Throws: "Stack underflow"
```

## Performance Characteristics

### Method Dispatch
- **Direct Methods**: O(1) - single lookup in class
- **Inherited Methods**: O(h) - h = inheritance depth
- **Overall**: Typical O(1) for most cases

### Field Access
- **Own Fields**: O(log n) - unordered_map lookup
- **Inherited Fields**: O(h * log n) - recursive walk

### Stack Operations
- **Push/Pop**: O(1) amortized
- **Reallocation**: When capacity exceeded

### Object Creation
- **Allocation**: `std::make_shared` ≈ 1 allocation
- **Setup**: Field map initialization (empty initially)

## Extensibility Points

### Adding New Types
1. Create new class with Class("TypeName")
2. Implement NativeMethodImpl for each method
3. Register with VM

### Adding New Collections
1. Create template specialization of ListBase
2. Implement required virtual methods
3. Register type specialization

### Adding New Primitives
1. Add variant index to Value
2. Add IsXXX() and AsXXX() methods
3. Add static factory method to TypeReference
4. Update ListBase specialization

## Design Decisions & Trade-offs

### Decision 1: Value as Tagged Union
**Alternative**: Heap-allocated polymorphic base class
**Chosen**: Stack-allocated variant
**Rationale**: Better cache locality, no pointer indirection, simpler ownership

### Decision 2: Reference Counting
**Alternative**: Tracing GC, manual memory, arena allocation
**Chosen**: Reference counting via shared_ptr
**Rationale**: Deterministic cleanup, no GC pauses, simpler C++ integration

### Decision 3: Nominal Type System
**Alternative**: Structural subtyping
**Chosen**: Nominal types (named classes)
**Rationale**: Matches ObjectIR semantics, easier to implement inheritance

### Decision 4: Native Method Functions
**Alternative**: Interpret ObjectIR bytecode
**Chosen**: Native C++ lambda/functions
**Rationale**: Higher performance, easier for examples, can add bytecode later

## Future Enhancements

### Phase 1: Bytecode Interpreter
- Deserialize ObjectIR JSON/binary format
- Implement instruction interpreter
- Execute compiled ObjectIR directly

### Phase 2: Advanced GC
- Cycle detection for reference counting
- Generational GC for better performance
- Memory profiling and optimization

### Phase 3: JIT Compilation
- Hot method detection
- Native code generation
- Adaptive optimization

### Phase 4: Reflection & Introspection
- Runtime type information (RTTI++)
- Method/field enumeration
- Attribute/annotation support

## Testing Strategy

### Unit Tests
- Value type conversions
- Object field operations
- Method dispatch resolution
- Collection operations

### Integration Tests
- Complex inheritance chains
- Multi-level method calls
- Cross-class interactions

### Example Programs
- Calculator (arithmetic operations)
- TodoApp (object composition)
- Game engine pattern (OOP + state)
