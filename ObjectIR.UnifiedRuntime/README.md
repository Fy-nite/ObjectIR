# ObjectIR Unified Runtime

A comprehensive end-to-end solution that combines the **ObjectIR Construct compiler** with the **high-performance C++ runtime**, enabling single-executable compilation and execution of ObjectIR modules.

## Overview

The Unified Runtime integrates:

1. **Construct Language Compiler** - C# backend code generation from ObjectIR IR
2. **IR Builder & Serialization** - Module construction and JSON serialization
3. **C++ Runtime Integration** - P/Invoke bindings to the native ObjectIR runtime
4. **CLI Tool** - Unified command-line interface for the complete pipeline

```
┌─────────────────────────────────────────────────────────────────┐
│                  ObjectIR Unified Runtime                       │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  ┌─────────────────┐    ┌──────────────┐   ┌────────────────┐ │
│  │  IR Builder     │───▶│  Serializer  │──▶│  C++ Runtime   │ │
│  │  (C#)           │    │  (JSON)      │   │  (Native)      │ │
│  └─────────────────┘    └──────────────┘   └────────────────┘ │
│          │                                           │          │
│          └───────────────────────────────────────────┘          │
│                          │                                      │
│                          ▼                                      │
│                  ┌──────────────────┐                          │
│                  │  Code Generator  │                          │
│                  │  (C# Backend)    │                          │
│                  └──────────────────┘                          │
│                          │                                      │
│                          ▼                                      │
│         ┌─────────────────────────────────┐                    │
│         │  Generated C# + Execution       │                    │
│         └─────────────────────────────────┘                    │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

## Architecture

### Components

- **ObjectIR.Runtime.Wrapper** - P/Invoke bindings for C++ runtime access
- **ObjectIR.UnifiedRuntime** - Main CLI tool and orchestration logic
- **ObjectIR.Core** - IR definitions, builder API, serialization
- **ObjectIR.CSharpBackend** - Code generation to C#
- **ObjectIR.Examples** - Example modules for testing

### Pipeline

```
1. Build Module (IR Builder API)
   ↓
2. Serialize to JSON
   ↓
3. Generate C# Code (Optional)
   ↓
4. Load into C++ Runtime (Native execution)
   ↓
5. Invoke Methods and Execute
```

## Usage

### Build and Run Example in C++ Runtime

```bash
dotnet run --project ObjectIR.UnifiedRuntime -- build-and-run todoapp
```

### Compile Module to C#

```bash
dotnet run --project ObjectIR.UnifiedRuntime -- compile TodoApp.json
```

### Execute Compiled Module

```bash
dotnet run --project ObjectIR.UnifiedRuntime -- run TodoApp.json
```

### Full Pipeline (Build → Serialize → Codegen → Execute)

```bash
dotnet run --project ObjectIR.UnifiedRuntime -- pipeline todoapp
```

## Command Reference

### Commands

- `run <module.json>` - Execute a compiled module in C++ runtime
- `compile <module.json>` - Compile module to C# code
- `build <example>` - Build example to JSON file
- `build-and-run <example>` - Build example and execute in C++ runtime
- `pipeline <example>` - Run complete pipeline
- `help` - Show help

### Available Examples

- `calculator` - Simple calculator with arithmetic operations
- `todoapp` - Todo application with list management
- `modulecomposition` - Demonstrates module composition patterns
- `moduleloader` - Shows module loading capabilities

## Quick Start

### 1. Build the Solution

```bash
cd ObjectIR
dotnet build
```

### 2. Build and Run an Example

```bash
dotnet run --project ObjectIR.UnifiedRuntime -- build-and-run calculator
```

### 3. Run the Full Pipeline

```bash
dotnet run --project ObjectIR.UnifiedRuntime -- pipeline todoapp
```

This will:
1. Build the TodoApp module using the IR Builder
2. Serialize it to `TodoApp.json`
3. Generate C# code to `TodoApp.generated.cs`
4. Load and execute it in the C++ runtime

## Output Files

After running the pipeline, you'll have:

- `<Module>.json` - Serialized ObjectIR module (machine-readable format)
- `<Module>.generated.cs` - Generated C# implementation
- Console output showing execution status

## Runtime Wrapper (P/Invoke)

The `ObjectIR.Runtime.Wrapper` project provides C# bindings to the C++ runtime:

### Key Classes

```csharp
// Initialize runtime
using (var runtime = new RuntimeWrapper())
{
    // Load module from JSON
    runtime.LoadModuleFromFile("module.json");
    
    // Create instance
    var instance = runtime.CreateInstance("ClassName");
    
    // Invoke method
    var result = runtime.InvokeMethod("ClassName", "MethodName", instance);
    
    // Get result
    string value = result.ToString();
}
```

### Exception Handling

```csharp
try
{
    runtime.LoadModuleFromString(jsonContent);
}
catch (RuntimeException ex)
{
    Console.WriteLine($"Runtime error: {ex.Message}");
}
```

## Building the C++ Runtime

The wrapper requires the compiled C++ runtime library. To build it:

```bash
cd ObjectIR.CppRuntime
mkdir build && cd build
cmake ..
cmake --build .
```

This generates:
- Windows: `objectir_runtime.dll`
- Linux: `libobjctir_runtime.so`
- macOS: `libobjctir_runtime.dylib`

Place the compiled library in a location where the runtime can find it (e.g., system library path, or alongside the .NET executable).

## Features

### Unified Compilation

- Single tool for building, compiling, and executing ObjectIR modules
- No external dependencies beyond .NET runtime and C++

### Type Safety

- Full type checking through IR definitions
- Generic type support (List<T>, Dict<K,V>, Set<T>)

### OOP Support

- Classes, interfaces, inheritance
- Virtual methods and method dispatch
- Property support

### Memory Management

- Automatic memory management in C++
- Reference counting via smart pointers
- C# IDisposable pattern for resource cleanup

### Performance

- Native C++ execution for high performance
- Minimal P/Invoke overhead
- Efficient method dispatch

## Examples

### Building a Calculator

```csharp
var builder = new IRBuilder("CalculatorApp");

builder.Class("Calculator")
    .Method("Add", TypeReference.Int32)
        .Parameter("a", TypeReference.Int32)
        .Parameter("b", TypeReference.Int32)
        .Body()
            .Ldarg("a")
            .Ldarg("b")
            .Add()
            .Ret()
        .EndBody()
        .EndMethod()
    .EndClass();

var module = builder.Build();
var json = ModuleSerializer.ToJson(module);
```

### Running in C++ Runtime

```csharp
using (var runtime = new RuntimeWrapper())
{
    runtime.LoadModuleFromString(json);
    
    var calculator = runtime.CreateInstance("Calculator");
    var result = runtime.InvokeMethod("Calculator", "Add", calculator);
    
    Console.WriteLine($"Result: {result}");
}
```

## Architecture Decisions

### P/Invoke vs C++/CLI

Used P/Invoke instead of C++/CLI for:
- Platform independence (Windows, Linux, macOS support)
- Simpler deployment (no additional runtime requirements)
- Better interop with open-source C++ libraries

### Native Runtime Integration

Chose native C++ runtime for:
- Performance-critical execution
- OOP fidelity to C++ semantics
- Leverage existing C++ runtime investment
- Type safety with C++ templates

## Troubleshooting

### "DLL not found" or "Cannot load shared library"

Ensure the C++ runtime library is built and accessible:
```bash
# Build C++ runtime
cd ObjectIR.CppRuntime/build
cmake --build .

# Copy library to known location (or add to PATH/LD_LIBRARY_PATH)
```

### P/Invoke marshaling errors

Verify:
- C++ function signatures match P/Invoke declarations
- String marshaling (CharSet.Ansi) is correct
- Calling conventions match (CallingConvention.Cdecl)

## Future Enhancements

- [ ] AOT (Ahead-of-Time) compilation support
- [ ] JIT optimization within C++ runtime
- [ ] Additional target backends (JavaScript, WebAssembly)
- [ ] Interactive REPL mode
- [ ] Debugger integration
- [ ] Performance profiling tools

## License

Same as parent ObjectIR project
