# ObjectIR C++ Runtime

A high-performance, object-oriented runtime for executing ObjectIR intermediate representation in C++. This runtime provides full support for OOP features including inheritance, virtual methods, interfaces, and generic collections.

## Overview

The ObjectIR C++ Runtime is designed to:

1. **Execute ObjectIR instructions** with high fidelity to the IR semantics
2. **Support OOP features** including classes, inheritance, interfaces, and virtual methods
3. **Provide type safety** through C++17 strong typing
4. **Enable efficient method dispatch** with virtual method tables
5. **Manage memory** with automatic reference counting (via `std::shared_ptr`)
6. **Support generic collections** like `List<T>`, `Dict<K,V>`, and `Set<T>`

## Architecture

### Core Components

```
┌─────────────────────────────────────┐
│  TypeReference & Value              │ - Type system & stack values
├─────────────────────────────────────┤
│  Object & Class                     │ - OOP object model & metadata
├─────────────────────────────────────┤
│  Method & Field                     │ - Class members & method dispatch
├─────────────────────────────────────┤
│  ExecutionContext                   │ - Stack & local variable state
├─────────────────────────────────────┤
│  VirtualMachine                     │ - Runtime engine & method invocation
├─────────────────────────────────────┤
│  RuntimeBuilder                     │ - Fluent API for building classes
└─────────────────────────────────────┘
```

## Quick Start

### Building the Runtime

```bash
cd ObjectIR/ObjectIR.CppRuntime
mkdir build && cd build
cmake ..
cmake --build .
```

### Building Examples

```bash
cmake --build . --target calculator_example
cmake --build . --target todoapp_example
```

### Running Examples

```bash
./calculator_example
./todoapp_example
```

## Core Features

### 1. Type System

ObjectIR supports primitive and reference types:

```cpp
using namespace ObjectIR;

// Primitive types
auto intType = TypeReference::Int32();
auto stringType = TypeReference::String();
auto boolType = TypeReference::Bool();

// Reference types
auto classRef = std::make_shared<Class>("MyClass");
auto objectType = TypeReference::Object(classRef);
```

### 2. Value Stack

All runtime values are stored in the `Value` class:

```cpp
Value v1(42);           // int32
Value v2(3.14);         // double
Value v3("Hello");      // string
Value v4(true);         // bool
Value v5(objectRef);    // reference to object

// Type checking
if (v1.IsInt32()) {
    int32_t n = v1.AsInt32();
}
```

### 3. Object Model

Define and instantiate classes with the runtime builder:

```cpp
RuntimeBuilder builder;

builder.Class("MyClass")
    .Field("count", TypeReference::Int32())
    .Field("name", TypeReference::String())
    .EndClass();

auto vm = builder.Release();
auto myClass = vm->GetClass("MyClass");
auto instance = vm->CreateObject(myClass);
```

### 4. Method Implementation

Implement methods using native C++ functions:

```cpp
builder.Class("Calculator")
    .Method("Add", TypeReference::Int32(), false)
        .Parameter("a", TypeReference::Int32())
        .Parameter("b", TypeReference::Int32())
        .NativeImpl([](ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine*) {
            int32_t result = args[0].AsInt32() + args[1].AsInt32();
            return Value(result);
        })
        .EndMethod()
    .EndClass();

// Invoke method
auto calc = vm->CreateObject(vm->GetClass("Calculator"));
auto result = vm->InvokeMethod(calc, "Add", {Value(10), Value(20)});
std::cout << result.AsInt32() << std::endl;  // Output: 30
```

### 5. Object State

Access and modify object fields:

```cpp
auto obj = vm->CreateObject(myClass);

// Set field
obj->SetField("count", Value(42));
obj->SetField("name", Value(std::string("Test")));

// Get field
Value count = obj->GetField("count");
Value name = obj->GetField("name");
```

### 6. Inheritance Support

Classes can inherit from base classes:

```cpp
auto baseClass = std::make_shared<Class>("Base");
auto derivedClass = std::make_shared<Class>("Derived");
derivedClass->SetBaseClass(baseClass);

// Method resolution uses virtual method tables
auto method = derivedClass->LookupMethod("SomeMethod");
```

### 7. Interface Contracts

Classes can implement interfaces:

```cpp
auto interface = std::make_shared<Class>("IComparable");
auto myClass = std::make_shared<Class>("MyClass");
myClass->AddInterface(interface);

// Check implementation
bool implements = myClass->ImplementsInterface(interface);
```

### 8. Generic Collections

Built-in support for generic collections:

```cpp
// List<int32>
auto intList = std::make_shared<List<int32_t>>();
intList->Add(Value(10));
intList->Add(Value(20));
int32_t first = intList->GetAt(0).AsInt32();

// List<string>
auto stringList = std::make_shared<List<std::string>>();
stringList->Add(Value(std::string("Hello")));
```

## API Reference

### VirtualMachine

Main runtime engine:

```cpp
class VirtualMachine {
public:
    // Class registry
    void RegisterClass(ClassRef classType);
    ClassRef GetClass(const std::string& name) const;
    bool HasClass(const std::string& name) const;
    
    // Object creation
    ObjectRef CreateObject(ClassRef classType);
    ObjectRef CreateObject(const std::string& className);
    
    // Method invocation
    Value InvokeMethod(ObjectRef object, const std::string& methodName, 
                      const std::vector<Value>& args);
    Value InvokeStaticMethod(ClassRef classType, const std::string& methodName, 
                            const std::vector<Value>& args);
};
```

### RuntimeBuilder

Fluent API for building runtime classes:

```cpp
class RuntimeBuilder {
public:
    RuntimeBuilder& Class(const std::string& name);
    RuntimeBuilder& Field(const std::string& name, const TypeReference& type);
    RuntimeBuilder& Method(const std::string& name, const TypeReference& returnType, 
                          bool isStatic = false);
    RuntimeBuilder& Parameter(const std::string& name, const TypeReference& type);
    RuntimeBuilder& NativeImpl(NativeMethodImpl impl);
    RuntimeBuilder& EndMethod();
    RuntimeBuilder& EndClass();
    
    VirtualMachine* Build();
    std::unique_ptr<VirtualMachine> Release();
};
```

### Class

Type definition at runtime:

```cpp
class Class {
public:
    explicit Class(std::string name);
    
    // Field management
    void AddField(FieldRef field);
    FieldRef GetField(const std::string& name) const;
    const std::vector<FieldRef>& GetAllFields() const;
    
    // Method management
    void AddMethod(MethodRef method);
    MethodRef GetMethod(const std::string& name) const;
    MethodRef LookupMethod(const std::string& name) const;
    const std::vector<MethodRef>& GetAllMethods() const;
    
    // Inheritance
    ClassRef GetBaseClass() const;
    void SetBaseClass(ClassRef base);
    
    // Interfaces
    void AddInterface(ClassRef interface);
    bool ImplementsInterface(ClassRef interface) const;
    
    // Object creation
    ObjectRef CreateInstance() const;
};
```

### Object

Runtime instance:

```cpp
class Object {
public:
    void SetField(const std::string& fieldName, const Value& value);
    Value GetField(const std::string& fieldName) const;
    
    ClassRef GetClass() const;
    void SetClass(ClassRef classType);
    
    bool IsInstanceOf(ClassRef classType) const;
    ObjectRef GetBaseInstance() const;
    void SetBaseInstance(ObjectRef base);
};
```

## Examples

### Calculator Example

See `examples/calculator_example.cpp` for a complete implementation of:

- Multiple methods (Add, Subtract, Multiply, Divide)
- Instance state (lastResult, history)
- Method invocation and return values

### TodoApp Example

See `examples/todoapp_example.cpp` for a complete implementation of:

- Multiple classes (TodoItem, TodoList)
- Object creation and initialization
- Method chains and state management

## Performance Considerations

1. **Method Dispatch**: Virtual method lookup is O(1) for direct methods, O(n) for inherited methods
2. **Memory**: Objects use `std::shared_ptr` for automatic memory management
3. **Value Copying**: `Value` objects are stack-allocated and use variant for type erasure
4. **Stack Operations**: ExecutionContext uses `std::vector` for O(1) push/pop

## Future Extensions

- [ ] Bytecode interpreter for ObjectIR serialized format
- [ ] Garbage collection optimization beyond reference counting
- [ ] JIT compilation for hot methods
- [ ] Module loader from JSON/binary ObjectIR files
- [ ] Exception handling and try-catch support
- [ ] Reflection and introspection API

## Building with CMake

```bash
mkdir build
cd build
cmake -DCMAKE_BUILD_TYPE=Release ..
cmake --build . -j 4
ctest --output-on-failure
```

## Dependencies

- C++17 or later
- CMake 3.16+
- No external dependencies (uses only standard library)

## License

See the main ObjectIR project LICENSE file.
