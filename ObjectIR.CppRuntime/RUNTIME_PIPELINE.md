# ObjectIR C++ Runtime: Builder → File → Runtime Pipeline

## Overview

The ObjectIR C++ Runtime now supports a complete pipeline for building, serializing, and executing ObjectIR modules:

```
┌──────────────────────┐
│  C# Builder API      │  (ObjectIR.CSharpBackend)
└──────────┬───────────┘
           │ generates JSON
┌──────────▼───────────┐
│  JSON IR Format      │  (module.json)
└──────────┬───────────┘
           │ IRLoader::LoadFromFile()
┌──────────▼───────────┐
│  C++ Runtime (VM)    │  (VirtualMachine)
└──────────┬───────────┘
           │ Invoke methods
┌──────────▼───────────┐
│  Execution Results   │  (Values on stack)
└──────────────────────┘
```

## Key Components

### 1. IR Loader (`ir_loader.hpp/cpp`)

Deserializes ObjectIR modules from JSON format into a C++ runtime.

**Features:**
- Load modules from file path or JSON string
- Parse type definitions (classes, interfaces, structs)
- Extract field and method metadata
- Fully reconstruct class hierarchies
- Support for namespaces and type references

**Usage:**

```cpp
#include "ir_loader.hpp"

// Load from file
auto vm = ObjectIR::IRLoader::LoadFromFile("calculator.json");

// Or from string
std::string jsonStr = R"({...})";
auto vm = ObjectIR::IRLoader::LoadFromString(jsonStr);

// Access loaded classes
auto calcClass = vm->GetClass("Examples.Calculator");
auto instance = vm->CreateObject(calcClass);
```

### 2. Instruction Executor (`instruction_executor.hpp/cpp`)

Interprets and executes ObjectIR instructions at runtime.

**Supported Instructions:**

#### Stack Operations
- `nop` - No operation
- `dup` - Duplicate top value
- `pop` - Remove top value

#### Load Operations
- `ldi4` - Load 32-bit integer
- `ldi8` - Load 64-bit integer
- `ldr4` - Load 32-bit float
- `ldr8` - Load 64-bit double
- `ldstr` - Load string
- `ldtrue`/`ldfalse` - Load boolean
- `ldnull` - Load null

#### Arithmetic
- `add` - Addition (supports string concatenation)
- `sub` - Subtraction
- `mul` - Multiplication
- `div` - Division
- `rem` - Remainder/modulo
- `neg` - Negation

#### Comparisons
- `ceq` - Compare equal
- `cne` - Compare not equal
- `clt` - Compare less than
- `cle` - Compare less than or equal
- `cgt` - Compare greater than
- `cge` - Compare greater than or equal

#### Control Flow
- `ret` - Return from method
- `br` - Unconditional branch
- `brtrue`/`brfalse` - Conditional branch

#### Object Operations
- `newobj` - Create new object
- `call` - Call method
- `callvirt` - Call virtual method
- `castclass` - Cast to type
- `isinst` - Instance type check

**Usage:**

```cpp
#include "instruction_executor.hpp"

// Parse instruction from JSON
json instrJson = R"({"opCode": "ldi4", "operand": {"value": 42}})"_json;
auto instr = ObjectIR::InstructionExecutor::ParseJsonInstruction(instrJson);

// Execute in context
ObjectIR::InstructionExecutor::Execute(instr, context, vm);
```

### 3. Enhanced VirtualMachine

The VirtualMachine now includes:

- **RegisterClass()** - Register loaded classes
- **GetClass()** - Retrieve class by name
- **CreateObject()** - Instantiate from class
- **InvokeMethod()** - Call methods on objects
- **InvokeStaticMethod()** - Call static methods

### 4. Extended Class Definition

Classes now support:

```cpp
class->GetNamespace()           // Get namespace
class->SetNamespace(ns)         // Set namespace
class->IsAbstract()             // Check if abstract
class->SetAbstract(bool)        // Mark as abstract
class->IsSealed()               // Check if sealed
class->SetSealed(bool)          // Mark as sealed
```

## Complete Workflow Example

### Step 1: Build a Module (in C#)

```csharp
// Using ObjectIR.CSharpBackend
var builder = new IRBuilder("Calculator");
var calc = builder.Class("Calculator")
    .Field("result", TypeReference.Int32())
    .Method("Add", TypeReference.Int32())
        .Parameter("a", TypeReference.Int32())
        .Parameter("b", TypeReference.Int32())
    .EndMethod()
    .EndClass()
    .Build();

// Serialize to JSON
string json = calc.DumpJson();
System.IO.File.WriteAllText("calculator.json", json);
```

### Step 2: Load in C++ Runtime

```cpp
#include "ir_loader.hpp"

auto vm = ObjectIR::IRLoader::LoadFromFile("calculator.json");
auto calcClass = vm->GetClass("Calculator");
auto instance = vm->CreateObject(calcClass);
```

### Step 3: Bind Native Implementations

```cpp
auto addMethod = calcClass->GetMethod("Add");
if (addMethod) {
    NativeMethodImpl addImpl = [](ObjectRef self, const std::vector<Value>& args, VirtualMachine*) {
        return Value(args[0].AsInt32() + args[1].AsInt32());
    };
    addMethod->SetNativeImpl(addImpl);
}
```

### Step 4: Execute

```cpp
std::vector<Value> args = { Value(10), Value(20) };
Value result = vm->InvokeMethod(instance, "Add", args);
std::cout << "Result: " << result.AsInt32() << std::endl;  // Output: 30
```

## JSON Module Format

### Example Structure

```json
{
  "Name": "CalculatorModule",
  "Version": "1.0.0",
  "Metadata": {},
  "Types": [
    {
      "Kind": "Class",
      "Name": "Calculator",
      "Namespace": "Examples",
      "Access": "Public",
      "IsAbstract": false,
      "IsSealed": false,
      "BaseType": null,
      "Interfaces": [],
      "GenericParameters": [],
      "Fields": [
        {
          "Name": "lastResult",
          "Type": "int32",
          "Access": "Private",
          "IsReadOnly": false
        }
      ],
      "Methods": [
        {
          "Name": "Add",
          "ReturnType": "int32",
          "Access": "Public",
          "IsStatic": false,
          "IsVirtual": false,
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
          ]
        }
      ],
      "Properties": []
    }
  ],
  "Functions": []
}
```

### Supported Type Names

- **Primitives**: `int32`, `int64`, `float32`, `float64`, `bool`, `string`, `void`
- **Qualified Names**: `Namespace.ClassName`

## Advanced Features

### Namespaces

Classes preserve namespaces from JSON:

```cpp
auto myClass = vm->GetClass("MyNamespace.MyClass");
std::cout << myClass->GetNamespace();  // "MyNamespace"
```

### Inheritance

Base class references are preserved (use `SetBaseClass()` during loading):

```cpp
auto baseClass = vm->GetClass("Base");
auto derivedClass = vm->GetClass("Derived");
derivedClass->SetBaseClass(baseClass);
```

### Interfaces

Interfaces are represented as abstract classes:

```cpp
auto iface = vm->GetClass("IMyInterface");
if (iface->IsAbstract()) {
    std::cout << "This is an interface\n";
}
```

### Generic Collections

The Value type supports:
- `int32`, `int64`, `float`, `double` - Arithmetic types
- `bool` - Boolean values
- `string` - Text
- `ObjectRef` - Object references

## Building and Running

### Build the Runtime

```bash
cd ObjectIR.CppRuntime
mkdir build && cd build
cmake ..
cmake --build .
```

### Run the Pipeline Example

```bash
cmake --build . --target pipeline_example
./pipeline_example
```

### Output

```
=== ObjectIR Runtime Pipeline Example ===

STEP 1: Loading ObjectIR module from JSON...
✓ Module loaded successfully!

STEP 2: Inspecting loaded classes...
✓ Found class: Examples.Calculator
  Fields: 1
    - lastResult
  Methods: 2
    - Add
    - Multiply

STEP 3: Creating calculator instance...
✓ Calculator instance created!

STEP 4: Binding native methods...
✓ Native 'Add' method bound
✓ Native 'Multiply' method bound

STEP 5: Executing methods...
  15 + 27 = 42
  6 × 7 = 42

STEP 6: Working with instance fields...
  Set lastResult = 42
  Retrieved lastResult = 42

=== Pipeline Complete ===
```

## Integration Guide

### For ObjectIR Users

1. **Build your module** using C# ObjectIR Builder
2. **Export to JSON** using `module.DumpJson()`
3. **Copy JSON to C++ project** directory
4. **Load in runtime**:
   ```cpp
   auto vm = ObjectIR::IRLoader::LoadFromFile("mymodule.json");
   ```
5. **Bind native methods** as needed
6. **Execute** via `vm->InvokeMethod()`

### For C++ Extension Developers

1. **Design your class hierarchy** in C#
2. **Generate JSON** from the builder
3. **Load in C++** with IRLoader
4. **Implement methods** as native C++ functions
5. **Expose to higher-level APIs** (Python, Lua, etc.)

## Limitations & Future Work

### Current Limitations

- Instructions are not serialized in JSON (structure only)
- Abstract methods cannot auto-stub (requires manual binding)
- No reflection on private members
- Generic types not yet supported in loader

### Roadmap

- [ ] Support for instruction serialization
- [ ] Automatic method binding for simple operations
- [ ] Generic type parameters
- [ ] Property getter/setter serialization
- [ ] Attribute/metadata support
- [ ] Event handling
- [ ] Async method execution

## Troubleshooting

### "Class not found" Error

Ensure the fully qualified name matches:
```cpp
// JSON has Namespace: "MyApp", Name: "Calculator"
auto cls = vm->GetClass("MyApp.Calculator");  // ✓ Correct
auto cls = vm->GetClass("Calculator");        // ✗ Wrong
```

### JSON Parse Error

Validate JSON format is correct:
```bash
# Check JSON syntax
python -m json.tool module.json
```

### Link Errors with nlohmann_json

Ensure CMakeLists.txt has:
```cmake
target_link_libraries(objectir_runtime PUBLIC nlohmann_json::nlohmann_json)
```

## See Also

- `examples/pipeline_example.cpp` - Complete working example
- `ObjectIR.CSharpBackend` - JSON generation from C#
- ARCHITECTURE.md - Runtime design details
- GETTING_STARTED.md - Basic runtime usage
