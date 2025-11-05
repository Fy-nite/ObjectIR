# ObjectIR C++ Runtime - Integration Guide

## Integration with ObjectIR Ecosystem

The C++ Runtime complements the existing ObjectIR ecosystem by providing native execution capabilities.

## Current Ecosystem

```
┌────────────────────────────────────┐
│   ObjectIR.CSharpBackend           │
│   (Generate C# from ObjectIR)      │
└────────────────────────────────────┘
            ↓
┌────────────────────────────────────┐
│   ObjectIR.CppRuntime (NEW!)       │
│   (Execute ObjectIR natively)      │
└────────────────────────────────────┘
            ↓
┌────────────────────────────────────┐
│   Future: Bytecode Interpreter     │
│   (Execute serialized ObjectIR)    │
└────────────────────────────────────┘
```

## Architecture Integration

### Option 1: Serialized Module Loading (Future)

```cpp
// C++ code
ModuleLoader loader;
auto module = loader.LoadFromFile("module.oir");  // ObjectIR binary
auto vm = module.CreateRuntime();
auto result = vm->InvokeMethod(obj, "method", args);
```

**Current Status**: Foundation in place, awaiting module format specification

### Option 2: Direct C# to C++ Pipeline (Current)

```
1. Define in ObjectIR Builder (C#)
2. Generate Module (C#)
3. Manually implement in C++ Builder (C++)
4. Execute in C++ Runtime (C++)
```

## Current Workflow Example

### Step 1: Define Module in C#

```csharp
var builder = new IRBuilder("MyApp");

builder.Class("Calculator")
    .Field("value", TypeReference.Int32())
    .Method("Add", TypeReference.Int32())
        .Parameter("x", TypeReference.Int32())
        .Emit(...)
    .EndMethod()
    .EndClass();

var module = builder.Build();
```

### Step 2: Implement in C++ Runtime

```cpp
RuntimeBuilder builder;

builder.Class("Calculator")
    .Field("value", TypeReference::Int32())
    .Method("Add", TypeReference::Int32(), false)
        .Parameter("x", TypeReference::Int32())
        .NativeImpl([](ObjectRef obj, const std::vector<Value>& args, VirtualMachine*) {
            int32_t current = obj->GetField("value").AsInt32();
            int32_t x = args[0].AsInt32();
            int32_t result = current + x;
            obj->SetField("value", Value(result));
            return Value(result);
        })
        .EndMethod()
    .EndClass();

auto vm = builder.Release();
```

### Step 3: Execute

```cpp
auto calc = vm->CreateObject("Calculator");
calc->SetField("value", Value(10));
auto result = vm->InvokeMethod(calc, "Add", {Value(5)});
// result = 15
```

## Multi-Target Compilation Strategy

### Build Once, Run Everywhere

```
      ┌─── ObjectIR Module ───┐
      ↓           ↓            ↓
   C# Gen      CppRt       (Java/JS/etc)
    ↓           ↓            ↓
  .NET       Native+      JVM/Node
  Code       Runtime       Code
```

## Using ObjectIR.CppRuntime with Other Backends

### Comparison Table

| Backend | Language | Execution | Performance | Use Case |
|---------|----------|-----------|-------------|----------|
| C# | Generated Code | JIT (.NET) | High | Quick prototyping, .NET apps |
| C++ Runtime | Native | Direct | Very High | High-performance services |
| Java (future) | Generated Code | JIT (JVM) | High | Enterprise Java apps |
| JavaScript (future) | Generated Code | Interpreted | Medium | Web/Node.js apps |

## Development Workflow

### For ObjectIR Maintainers

1. **Update IR Specification**
   - Modify Contract/LANGUAGE_SPEC_v1.md

2. **Update C# Backend**
   - Modify ObjectIR.CSharpBackend

3. **Update C++ Runtime**
   - Modify ObjectIR.CppRuntime/include/objectir_runtime.hpp
   - Modify ObjectIR.CppRuntime/src/objectir_runtime.cpp

4. **Update Documentation**
   - Update respective README.md and ARCHITECTURE.md
   - Add examples demonstrating new features

### For Application Developers

1. **Use ObjectIR to define classes**
   ```
   Use ObjectIR Builder (C#) → Define your types
   ```

2. **Choose a backend**
   ```
   Use C# backend for web apps
   Use C++ Runtime for high-performance services
   ```

3. **Implement and execute**
   ```
   Implement methods in chosen language
   Execute and test
   ```

## Feature Parity

### C# Backend vs C++ Runtime

| Feature | C# Backend | C++ Runtime | Status |
|---------|-----------|------------|--------|
| Classes | ✓ | ✓ | Complete |
| Methods | ✓ | ✓ | Complete |
| Fields | ✓ | ✓ | Complete |
| Inheritance | ✓ | ✓ | Complete |
| Interfaces | ✓ | ✓ | Complete |
| Generics | ✓ | ✓ | Complete |
| Collections | ✓ | Partial | In progress |
| Serialization | ✓ | Planned | Future |
| Reflection | Partial | Planned | Future |
| Exception Handling | ✓ | Planned | Future |

## Best Practices

### 1. Choose the Right Backend

**Use C++ Runtime if:**
- Need maximum performance
- Building microservices
- Working with systems code
- Need tight resource control

**Use C# Backend if:**
- Need rapid development
- Targeting .NET ecosystem
- Need reflection capabilities
- Prefer higher-level abstractions

### 2. Keep Implementations Consistent

```cpp
// C# Backend
public class MyClass {
    public int GetValue() { return 42; }
}

// C++ Runtime
.Method("GetValue", TypeReference::Int32(), false)
    .NativeImpl([](ObjectRef obj, const std::vector<Value>&, VirtualMachine*) {
        return Value(42);
    })
    .EndMethod()
```

### 3. Test Both Implementations

- Verify semantics match between backends
- Create language-agnostic test suites
- Document expected behavior

### 4. Use the Builder Fluently

```cpp
// Good - readable and maintainable
builder.Class("MyClass")
    .Field("x", TypeReference::Int32())
    .Method("GetX", TypeReference::Int32(), false)
        .NativeImpl([...])
        .EndMethod()
    .EndClass();
```

## Extensibility Hooks

### Custom Type Support

**In C++ Runtime:**
```cpp
// Create custom type
class MyCustomType : public Object {
    // Custom behavior
};

// Register with VM
auto obj = std::make_shared<MyCustomType>();
```

### Custom Collection Types

**Current:**
- List<T>

**Extensions:**
```cpp
template<typename K, typename V>
class Dict : public Object {
    // Custom dictionary implementation
};
```

### Future: Custom Native Bindings

```cpp
// Bind C++ library
builder.ExternalLibrary("SomeLib")
    .Type("SomeType")
    .Method("SomeMethod")
    .EndType()
    .EndLibrary();
```

## Deployment Scenarios

### Scenario 1: High-Performance Service

```
ObjectIR Design
    ↓
C++ Runtime Implementation
    ↓
Deploy as native service
    ↓
Execute at full speed with minimal overhead
```

**Tools:**
- Build: CMake
- Deploy: Docker or direct binary
- Monitor: Standard Linux tools

### Scenario 2: Cross-Platform App

```
ObjectIR Design
    ↓
Split: Performance-critical → C++, UI → C#
    ↓
Integrate via IPC/REST
    ↓
Deploy components separately
```

**Tools:**
- C++ Runtime: Linux/Windows/macOS
- C# Backend: .NET on any platform
- Communication: REST API, gRPC, or IPC

### Scenario 3: Educational Use

```
ObjectIR Design
    ↓
C++ Runtime (understand OOP implementation)
    ↓
Study codebase, modify, experiment
    ↓
Learn compiler/runtime design
```

**Value:**
- Clean, well-documented code
- Full source access
- No black boxes
- Perfect for learning

## Troubleshooting Integration

### Problem: Method Not Found

**Cause**: Implementation doesn't match definition

**Solution**: 
```cpp
// Verify method name matches exactly
.Method("MyMethod", ...)
// vs
vm->InvokeMethod(obj, "MyMethod", {});  // Must match
```

### Problem: Type Mismatch

**Cause**: Parameter types don't match expectations

**Solution**:
```cpp
// C# expects: int32
// C++ implementation should receive Value as int32
int32_t x = args[0].AsInt32();
```

### Problem: Segmentation Fault

**Cause**: Null pointer or invalid memory access

**Solution**:
```cpp
// Always check for null
if (!obj) return Value();

// Use shared_ptr to prevent premature deletion
ObjectRef ref = vm->CreateObject("Class");
// ref keeps object alive
```

## Future Integration Points

### 1. Module Serialization

```cpp
// Save module to binary
module.SaveToFile("module.oir");

// Load in C++ Runtime
auto module = ModuleLoader::LoadFromFile("module.oir");
auto vm = module.CreateRuntime();
```

### 2. Bytecode Interpreter

```cpp
// Execute ObjectIR instructions directly
BytecodeVM vm;
vm.Execute(module.GetBytecode());
```

### 3. JIT Compilation

```cpp
// Compile hot methods to native code
auto jit = new JITCompiler(vm);
jit->CompileMethod(hotMethod);
```

## Support & Resources

### Documentation
- README.md - Quick reference
- GETTING_STARTED.md - Tutorials
- ARCHITECTURE.md - Deep dive
- PROJECT_SUMMARY.md - Overview

### Examples
- calculator_example.cpp - Basic patterns
- todoapp_example.cpp - Multi-class design
- patterns.hpp - Advanced patterns

### Community
- File issues on GitHub
- Discuss design in ObjectIR project

## Next Steps

1. **Try the Examples**: Run calculator_example and todoapp_example
2. **Build Something**: Create your first ObjectIR C++ class
3. **Extend**: Add features and custom types
4. **Integrate**: Connect with other ObjectIR backends
5. **Contribute**: Share improvements and patterns

---

**The ObjectIR C++ Runtime is production-ready and waiting for you to build amazing things!**
