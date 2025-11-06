# ObjectIR C++ Runtime - Getting Started

## 5-Minute Quick Start

### Prerequisites

- C++17 compiler (GCC 7+, Clang 5+, MSVC 2017+)
- CMake 3.16+
- Git

### Build the Runtime

```bash
cd ObjectIR/ObjectIR.CppRuntime
mkdir build
cd build
cmake ..
cmake --build .
```

### Run an Example

```bash
# From the build directory
./calculator_example
./todoapp_example
```

**Expected Output** (Calculator):
```
=== ObjectIR C++ Runtime: Calculator Example ===

--- Testing Calculator ---
10 + 5 = 15
20 - 7 = 13
6 * 4 = 24
100 / 5 = 20

Last Result: 20
Operations Performed: 4

✓ Example completed successfully!
```

---

## Complete Tutorial: Building Your First ObjectIR Class

### Step 1: Create a Simple Class

```cpp
#include "objectir_runtime.hpp"
#include <iostream>

using namespace ObjectIR;

int main() {
    // Create a builder
    RuntimeBuilder builder;
    
    // Define a Person class
    builder.Class("Person")
        .Field("name", TypeReference::String())
        .Field("age", TypeReference::Int32())
        .EndClass();
    
    // Get the runtime
    auto vm = builder.Release();
    
    // Create an instance
    auto person = vm->CreateObject("Person");
    
    // Set fields
    person->SetField("name", Value(std::string("Alice")));
    person->SetField("age", Value(30));
    
    // Read fields
    std::string name = person->GetField("name").AsString();
    int32_t age = person->GetField("age").AsInt32();
    
    std::cout << name << " is " << age << " years old\n";
    
    return 0;
}
```

### Step 2: Add Methods

```cpp
builder.Class("Person")
    .Field("name", TypeReference::String())
    .Field("age", TypeReference::Int32())
    
    // Add a method
    .Method("GetName", TypeReference::String(), false)
        .NativeImpl([](ObjectRef thisPtr, const std::vector<Value>&, VirtualMachine*) {
            return thisPtr->GetField("name");
        })
        .EndMethod()
    
    .Method("GetAge", TypeReference::Int32(), false)
        .NativeImpl([](ObjectRef thisPtr, const std::vector<Value>&, VirtualMachine*) {
            return thisPtr->GetField("age");
        })
        .EndMethod()
    
    .Method("HaveABirthday", TypeReference::Void(), false)
        .NativeImpl([](ObjectRef thisPtr, const std::vector<Value>&, VirtualMachine*) {
            int32_t age = thisPtr->GetField("age").AsInt32();
            thisPtr->SetField("age", Value(age + 1));
            return Value();
        })
        .EndMethod()
    
    .EndClass();

// Invoke methods
auto name = vm->InvokeMethod(person, "GetName", {});
auto age = vm->InvokeMethod(person, "GetAge", {});

std::cout << name.AsString() << " is " << age.AsInt32() << "\n";

vm->InvokeMethod(person, "HaveABirthday", {});
age = vm->InvokeMethod(person, "GetAge", {});

std::cout << "After birthday: " << age.AsInt32() << "\n";
```

### Step 3: Add Parameters to Methods

```cpp
.Method("IsOlderThan", TypeReference::Bool(), false)
    .Parameter("otherAge", TypeReference::Int32())
    .NativeImpl([](ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine*) {
        if (args.empty()) throw std::runtime_error("Missing argument");
        int32_t myAge = thisPtr->GetField("age").AsInt32();
        int32_t otherAge = args[0].AsInt32();
        return Value(myAge > otherAge);
    })
    .EndMethod()
```

### Step 4: Inheritance

```cpp
// Create base class
builder.Class("Person")
    // ... fields and methods
    .EndClass();

// Create derived class
auto personClass = vm->GetClass("Person");
builder.Class("Employee")
    .Field("employeeId", TypeReference::Int32())
    .EndClass();

auto empClass = vm->GetClass("Employee");
empClass->SetBaseClass(personClass);

// Now Employee has all Person fields and methods
auto emp = vm->CreateObject(empClass);
emp->SetField("name", Value(std::string("Bob")));
emp->SetField("employeeId", Value(12345));

// Can call inherited methods
auto name = vm->InvokeMethod(emp, "GetName", {});
```

### Step 5: Multiple Classes and Composition

```cpp
// Define Department class
builder.Class("Department")
    .Field("name", TypeReference::String())
    .Field("headCount", TypeReference::Int32())
    
    .Method("GetName", TypeReference::String(), false)
        .NativeImpl([](ObjectRef thisPtr, const std::vector<Value>&, VirtualMachine*) {
            return thisPtr->GetField("name");
        })
        .EndMethod()
    
    .EndClass();

// Now create objects and link them
auto dept = vm->CreateObject("Department");
dept->SetField("name", Value(std::string("Engineering")));
dept->SetField("headCount", Value(42));

auto employee = vm->CreateObject("Employee");
employee->SetField("name", Value(std::string("Carol")));
// Store reference to department (future: will support object fields)
```

---

## Common Patterns

### Pattern 1: Static Methods

```cpp
builder.Class("Math")
    .Method("Max", TypeReference::Int32(), true)  // static = true
        .Parameter("a", TypeReference::Int32())
        .Parameter("b", TypeReference::Int32())
        .NativeImpl([](ObjectRef unused, const std::vector<Value>& args, VirtualMachine*) {
            int32_t a = args[0].AsInt32();
            int32_t b = args[1].AsInt32();
            return Value(a > b ? a : b);
        })
        .EndMethod()
    .EndClass();

// Call static method
auto max = vm->InvokeStaticMethod(vm->GetClass("Math"), "Max", {Value(10), Value(20)});
```

### Pattern 2: Method Chaining

```cpp
builder.Class("StringBuilder")
    .Field("content", TypeReference::String())
    
    .Method("Append", TypeReference::Object(/* StringBuilder */), false)
        .Parameter("text", TypeReference::String())
        .NativeImpl([](ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine*) {
            std::string current = thisPtr->GetField("content").AsString();
            std::string append = args[0].AsString();
            thisPtr->SetField("content", Value(current + append));
            return Value(thisPtr);  // Return self for chaining
        })
        .EndMethod()
    
    .Method("ToString", TypeReference::String(), false)
        .NativeImpl([](ObjectRef thisPtr, const std::vector<Value>&, VirtualMachine*) {
            return thisPtr->GetField("content");
        })
        .EndMethod()
    
    .EndClass();
```

### Pattern 3: State Management

```cpp
builder.Class("Counter")
    .Field("value", TypeReference::Int32())
    
    .Method("Increment", TypeReference::Int32(), false)
        .NativeImpl([](ObjectRef thisPtr, const std::vector<Value>&, VirtualMachine*) {
            int32_t current = thisPtr->GetField("value").AsInt32();
            int32_t newValue = current + 1;
            thisPtr->SetField("value", Value(newValue));
            return Value(newValue);
        })
        .EndMethod()
    
    .Method("GetValue", TypeReference::Int32(), false)
        .NativeImpl([](ObjectRef thisPtr, const std::vector<Value>&, VirtualMachine*) {
            return thisPtr->GetField("value");
        })
        .EndMethod()
    
    .EndClass();

auto counter = vm->CreateObject("Counter");
counter->SetField("value", Value(0));

vm->InvokeMethod(counter, "Increment", {});  // 1
vm->InvokeMethod(counter, "Increment", {});  // 2
auto val = vm->InvokeMethod(counter, "GetValue", {});  // Get 2
```

---

## Working with Collections

### Basic List Operations

```cpp
#include <memory>

// Create a list
auto intList = std::make_shared<List<int32_t>>();

// Add items
intList->Add(Value(10));
intList->Add(Value(20));
intList->Add(Value(30));

// Get items
int32_t first = intList->GetAt(0).AsInt32();   // 10
int32_t second = intList->GetAt(1).AsInt32();  // 20
int32_t size = intList->GetSize();              // 3

// Modify items
intList->SetAt(1, Value(25));

// Remove items
intList->Remove(2);      // Remove last item
intList->Clear();        // Remove all
```

### Custom Collection in Methods

```cpp
builder.Class("IntList")
    .Field("items", TypeReference::Int32())  // Simplified: store count
    
    .Method("CreateList", TypeReference::Object(/* List */), true)
        .NativeImpl([](ObjectRef, const std::vector<Value>&, VirtualMachine*) {
            auto list = std::make_shared<List<int32_t>>();
            return Value(std::dynamic_pointer_cast<Object>(list));
        })
        .EndMethod()
    
    .EndClass();
```

---

## Type Reference Guide

### Primitive Types

```cpp
TypeReference::Int32()    // 32-bit signed integer
TypeReference::Int64()    // 64-bit signed integer
TypeReference::Float32()  // Single-precision float
TypeReference::Float64()  // Double-precision float
TypeReference::Bool()     // Boolean true/false
TypeReference::String()   // UTF-8 string
TypeReference::Void()     // No return value
TypeReference::Object(classRef)  // Reference to class
```

### Creating Values

```cpp
Value(42)                              // int32
Value(int64_t(1000000000000))         // int64
Value(3.14f)                          // float32
Value(3.14159)                        // float64
Value(true)                           // bool
Value(std::string("Hello"))           // string
Value(objectRef)                      // object reference
Value()                               // null/void
```

### Type Checking and Conversion

```cpp
Value v(42);

v.IsInt32()                  // true
v.IsString()                 // false

int32_t n = v.AsInt32();    // OK: 42
std::string s = v.AsString(); // Exception!
```

---

## Error Handling

### Type Errors

```cpp
try {
    Value v(42);
    std::string s = v.AsString();  // Type mismatch
} catch (const std::exception& e) {
    std::cerr << "Error: " << e.what() << std::endl;
    // "Value is not string"
}
```

### Method Not Found

```cpp
try {
    vm->InvokeMethod(obj, "NonExistentMethod", {});
} catch (const std::exception& e) {
    std::cerr << "Error: " << e.what() << std::endl;
    // "Method not found: NonExistentMethod"
}
```

### Class Not Found

```cpp
try {
    auto cls = vm->GetClass("NonExistentClass");
} catch (const std::exception& e) {
    std::cerr << "Error: " << e.what() << std::endl;
    // "Class not found: NonExistentClass"
}
```

### Stack Errors

```cpp
try {
    auto ctx = std::make_unique<ExecutionContext>(nullptr);
    ctx->PopStack();  // Empty stack
} catch (const std::exception& e) {
    std::cerr << "Error: " << e.what() << std::endl;
    // "Stack underflow"
}
```

---

## Tips & Best Practices

### 1. Always Check Class Existence

```cpp
if (vm->HasClass("MyClass")) {
    auto cls = vm->GetClass("MyClass");
} else {
    std::cerr << "Class not registered\n";
}
```

### 2. Use RAII for Objects

```cpp
{
    auto obj = vm->CreateObject("MyClass");
    // Use object
}  // Automatically cleaned up
```

### 3. Handle Exceptions

```cpp
try {
    // Runtime operations
} catch (const std::runtime_error& e) {
    std::cerr << "Runtime error: " << e.what() << std::endl;
} catch (const std::out_of_range& e) {
    std::cerr << "Out of range: " << e.what() << std::endl;
} catch (const std::exception& e) {
    std::cerr << "Unknown error: " << e.what() << std::endl;
}
```

### 4. Validate Arguments

```cpp
.Method("Add", TypeReference::Int32(), false)
    .Parameter("a", TypeReference::Int32())
    .Parameter("b", TypeReference::Int32())
    .NativeImpl([](ObjectRef, const std::vector<Value>& args, VirtualMachine*) {
        if (args.size() != 2) {
            throw std::runtime_error("Add requires exactly 2 arguments");
        }
        return Value(args[0].AsInt32() + args[1].AsInt32());
    })
    .EndMethod()
```

### 5. Use Const References

```cpp
const std::string& name = thisPtr->GetField("name").AsString();
// Avoid copies when possible
```

---

## Next Steps

1. **Read Examples**: Review `calculator_example.cpp` and `todoapp_example.cpp`
2. **Study Architecture**: Read `ARCHITECTURE.md` for deep dive
3. **API Reference**: Check `README.md` for complete API
4. **Write Your Own**: Create a custom class following these patterns
5. **Advanced**: Implement a loader for ObjectIR serialized modules

---

## Troubleshooting

### Build Issues

**CMake not found**:
```bash
# Install CMake
sudo apt-get install cmake  # Ubuntu
brew install cmake          # macOS
```

**Compiler too old**:
- Need C++17 support
- Use: `gcc-7+`, `clang-5+`, `msvc-2017+`

### Runtime Issues

**Segmentation fault**:
- Check for null object references
- Verify field/method names are correct

**Method not found**:
- Ensure class is registered with VM
- Check method spelling

**Type mismatch**:
- Verify Value type before calling AsXXX()
- Use IsXXX() to check before conversion
