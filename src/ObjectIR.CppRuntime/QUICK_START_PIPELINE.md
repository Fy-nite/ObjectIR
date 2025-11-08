# Builder → File → Runtime: Quick Start Guide

Get up and running with the ObjectIR runtime pipeline in 5 minutes!

## What is the Pipeline?

The ObjectIR pipeline allows you to:

1. **Build** an IR module using the C# API
2. **Serialize** it to FOB (Finite Open Bytecode) or JSON format  
3. **Load** the file in C++ at runtime (auto-detects format)
4. **Execute** methods with native implementations

```
C# Builder API → FOB/JSON File → C++ Loader → Execute
```

## Quick Example

### In C# (ObjectIR.CSharpBackend)

```csharp
// Build a calculator module
var builder = new IRBuilder("Calculator");
var module = builder
    .Class("MyCalc")
        .Method("Add", TypeReference.Int32())
            .Parameter("a", TypeReference.Int32())
            .Parameter("b", TypeReference.Int32())
        .EndMethod()
    .EndClass()
    .Build();

// Save to FOB (recommended) or JSON
System.IO.File.WriteAllBytes("calc.fob", module.DumpFOB());
System.IO.File.WriteAllText("calc.json", module.DumpJson());
```

### In C++ (ObjectIR.CppRuntime)

```cpp
#include "ir_loader.hpp"

// Load the FOB or JSON module (auto-detected)
auto vm = ObjectIR::IRLoader::LoadFromFile("calc.fob");

// Get the class and create an instance
auto calcClass = vm->GetClass("MyCalc");
auto instance = vm->CreateObject(calcClass);

// Bind a native implementation
auto addMethod = calcClass->GetMethod("Add");
addMethod->SetNativeImpl([](ObjectRef self, const std::vector<Value>& args, VirtualMachine*) {
    return ObjectIR::Value(args[0].AsInt32() + args[1].AsInt32());
});

// Call the method
std::vector<ObjectIR::Value> args = { ObjectIR::Value(10), ObjectIR::Value(20) };
ObjectIR::Value result = vm->InvokeMethod(instance, "Add", args);
std::cout << result.AsInt32() << std::endl;  // Output: 30
```

## Step-by-Step Setup

### 1. Generate JSON from C#

Build your module and export:

```csharp
var builder = new IRBuilder("MyModule");
// ... define types, methods, fields ...
var module = builder.Build();

string json = module.DumpJson();
File.WriteAllText("mymodule.json", json);
```

### 2. Build C++ Runtime

```bash
cd ObjectIR/ObjectIR.CppRuntime
mkdir build && cd build
cmake ..
cmake --build .
```

### 3. Load in Your Code

```cpp
#include "ir_loader.hpp"
using namespace ObjectIR;

// Load the module
auto vm = IRLoader::LoadFromFile("mymodule.json");

// Access classes
auto myClass = vm->GetClass("MyNamespace.MyClass");

// Create instances
auto instance = vm->CreateObject(myClass);

// Bind implementations and call
auto method = myClass->GetMethod("MyMethod");
method->SetNativeImpl([](ObjectRef self, const std::vector<Value>& args, VirtualMachine*) {
    // Your C++ implementation
    return Value();
});

vm->InvokeMethod(instance, "MyMethod", {});
```

## Common Tasks

### Load JSON from String

```cpp
std::string jsonStr = R"({
  "Name": "MyApp",
  "Types": [...]
})";

auto vm = IRLoader::LoadFromString(jsonStr);
```

### Check if Class Exists

```cpp
if (vm->HasClass("MyNamespace.MyClass")) {
    auto cls = vm->GetClass("MyNamespace.MyClass");
    // ...
}
```

### Inspect Class Metadata

```cpp
auto cls = vm->GetClass("MyClass");

// Get fields
for (const auto& field : cls->GetAllFields()) {
    std::cout << "Field: " << field->GetName() << std::endl;
}

// Get methods
for (const auto& method : cls->GetAllMethods()) {
    std::cout << "Method: " << method->GetName() << std::endl;
    for (const auto& [paramName, paramType] : method->GetParameters()) {
        std::cout << "  Param: " << paramName << std::endl;
    }
}
```

### Pass Arguments to Methods

```cpp
std::vector<Value> args;
args.push_back(Value(42));              // int32
args.push_back(Value(3.14));            // double
args.push_back(Value("hello"));         // string
args.push_back(Value(true));            // bool

Value result = vm->InvokeMethod(obj, "MyMethod", args);
```

### Work with Fields

```cpp
// Get field value
Value fieldValue = instance->GetField("myField");

// Set field value
instance->SetField("myField", Value(123));
```

### Check Inheritance

```cpp
auto baseClass = vm->GetClass("Base");
auto derivedClass = vm->GetClass("Derived");

// Set base class (if not already serialized)
derivedClass->SetBaseClass(baseClass);

// Check instance
if (instance->IsInstanceOf(baseClass)) {
    std::cout << "Instance is derived from Base\n";
}
```

## Supported Types

### Primitives
- `int32` / `int` - 32-bit signed integer
- `int64` / `long` - 64-bit signed integer
- `float32` / `float` - 32-bit floating point
- `float64` / `double` - 64-bit floating point
- `bool` / `boolean` - Boolean value
- `string` - Text string
- `void` - No value (for method return types)

### References
- `ClassName` - Reference to a class
- `Namespace.ClassName` - Fully qualified class name

## Example: Pipeline Example

Run the complete example:

```bash
cd build
cmake --build . --target pipeline_example
./pipeline_example
```

This demonstrates:
- Loading JSON modules
- Inspecting metadata
- Creating instances
- Binding native methods
- Calling methods with arguments
- Working with fields

## Troubleshooting

### "Class not found" Error

```cpp
// Wrong - namespace is separate
auto cls = vm->GetClass("MyClass");  // ✗ Won't find MyNamespace.MyClass

// Right - use fully qualified name
auto cls = vm->GetClass("MyNamespace.MyClass");  // ✓ Correct
```

### "Method not found" Error

```cpp
// Verify method exists
auto method = cls->GetMethod("MyMethod");
if (!method) {
    std::cout << "Method not found. Available methods:\n";
    for (const auto& m : cls->GetAllMethods()) {
        std::cout << "  - " << m->GetName() << "\n";
    }
}
```

### JSON Parse Errors

```bash
# Validate JSON format
python -m json.tool mymodule.json
```

Ensure JSON structure matches the format shown in RUNTIME_PIPELINE.md.

## Next Steps

1. **Create Your Module** in C# using ObjectIR builder
2. **Export to JSON** with `module.DumpJson()`
3. **Load in C++** with `IRLoader::LoadFromFile()`
4. **Bind Native Methods** using lambda functions
5. **Scale Up** to more complex modules

## See Also

- **RUNTIME_PIPELINE.md** - Complete pipeline documentation
- **examples/pipeline_example.cpp** - Full working example
- **include/ir_loader.hpp** - IRLoader API reference
- **include/instruction_executor.hpp** - Instruction execution
- **ARCHITECTURE.md** - Runtime architecture details

## API Reference

### IRLoader

```cpp
// Load from file
static std::shared_ptr<VirtualMachine> LoadFromFile(const std::string& filePath);

// Load from JSON string
static std::shared_ptr<VirtualMachine> LoadFromString(const std::string& jsonStr);
```

### VirtualMachine

```cpp
// Class registry
void RegisterClass(ClassRef classType);
ClassRef GetClass(const std::string& name) const;
bool HasClass(const std::string& name) const;

// Object creation
ObjectRef CreateObject(ClassRef classType);
ObjectRef CreateObject(const std::string& className);

// Method invocation
Value InvokeMethod(ObjectRef object, const std::string& methodName, const std::vector<Value>& args);
Value InvokeStaticMethod(ClassRef classType, const std::string& methodName, const std::vector<Value>& args);
```

### Class

```cpp
// Metadata
const std::string& GetName() const;
const std::string& GetNamespace() const;
bool IsAbstract() const;
bool IsSealed() const;

// Members
void AddField(FieldRef field);
FieldRef GetField(const std::string& name) const;
const std::vector<FieldRef>& GetAllFields() const;

void AddMethod(MethodRef method);
MethodRef GetMethod(const std::string& name) const;
const std::vector<MethodRef>& GetAllMethods() const;

// Object creation
ObjectRef CreateInstance() const;

// Interfaces
void AddInterface(ClassRef interface);
bool ImplementsInterface(ClassRef interface) const;
```

### Method

```cpp
const std::string& GetName() const;
const TypeReference& GetReturnType() const;
bool IsStatic() const;
bool IsVirtual() const;
const std::vector<std::pair<std::string, TypeReference>>& GetParameters() const;

void SetNativeImpl(NativeMethodImpl impl);
NativeMethodImpl GetNativeImpl() const;
```

### Object

```cpp
// Fields
void SetField(const std::string& fieldName, const Value& value);
Value GetField(const std::string& fieldName) const;

// Type checking
ClassRef GetClass() const;
bool IsInstanceOf(ClassRef classType) const;
```

### Value

```cpp
// Constructors
Value();
explicit Value(int32_t);
explicit Value(int64_t);
explicit Value(float);
explicit Value(double);
explicit Value(bool);
explicit Value(const std::string&);
explicit Value(ObjectRef);

// Type checking
bool IsInt32() const;
bool IsInt64() const;
bool IsFloat32() const;
bool IsFloat64() const;
bool IsBool() const;
bool IsString() const;
bool IsObject() const;
bool IsNull() const;

// Type conversion
int32_t AsInt32() const;
int64_t AsInt64() const;
float AsFloat32() const;
double AsFloat64() const;
bool AsBool() const;
std::string AsString() const;
ObjectRef AsObject() const;
```

## Support

For issues or questions:
1. Check RUNTIME_PIPELINE.md for detailed documentation
2. Review examples/pipeline_example.cpp for working code
3. Verify JSON format matches the schema
4. Enable debug output and check logs
