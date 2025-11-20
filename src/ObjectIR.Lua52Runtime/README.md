# ObjectIR Lua 5.2 Runtime

A pure Lua 5.2 implementation of the ObjectIR runtime, enabling execution of ObjectIR intermediate representation (IR) bytecode in pure Lua environments.

## Overview

This runtime provides full support for ObjectIR semantics including:

- **OOP Features**: Classes, inheritance, interfaces, virtual methods
- **Type System**: Primitives (int32, int64, float32, float64, bool, string) and objects
- **Memory Management**: Automatic garbage collection via Lua's GC
- **Method Dispatch**: Virtual method tables for inheritance
- **Collections**: Support for arrays and associative tables

## Architecture

```
┌──────────────────────────────┐
│   Application Layer          │
└──────────┬───────────────────┘
           │
┌──────────▼───────────────────┐
│   lua_runtime.lua            │ - VirtualMachine, ExecutionContext
├──────────────────────────────┤
│   lua_ir_instruction.lua     │ - Instruction executor
├──────────────────────────────┤
│   lua_types.lua              │ - Type system, Class model
├──────────────────────────────┤
│   lua_value.lua              │ - Value representation
├──────────────────────────────┤
│   lua_fob_loader.lua         │ - FOB binary parser
└──────────────────────────────┘
```

## Quick Start

### Loading and Executing a Module

```lua
local fobLoader = require("lua_fob_loader")
local runtime = require("lua_runtime")

-- Load a FOB module
local vm, entryTypeIdx, entryMethodIdx = fobLoader.LoadFromFile("my_module.fob")

-- Execute the entry point
vm:ExecuteMethod(entryTypeIdx, entryMethodIdx, {})
```

### Building Classes Programmatically

```lua
local runtime = require("lua_runtime")

local vm = runtime.VirtualMachine.new()
local builder = runtime.ClassBuilder.new(vm)

-- Create a simple class
builder:Class("Calculator")
    :Method("Add", "Int32")
        :Parameter("a", "Int32")
        :Parameter("b", "Int32")
    :EndMethod()
:EndClass()

-- Execute a method
local result = vm:InvokeMethod("Calculator", "Add", {10, 20})
```

## Supported Opcodes

### Stack Operations
- `Nop`, `Dup`, `Pop`

### Load Operations
- `LdArg`, `LdLoc`, `LdFld`, `LdCon`, `LdStr`
- `LdI4`, `LdI8`, `LdR4`, `LdR8`
- `LdTrue`, `LdFalse`, `LdNull`

### Store Operations
- `StLoc`, `StFld`, `StArg`

### Arithmetic
- `Add`, `Sub`, `Mul`, `Div`, `Rem`, `Neg`

### Comparison
- `Ceq`, `Cne`, `Clt`, `Cle`, `Cgt`, `Cge`

### Control Flow
- `Ret`, `Br`, `BrTrue`, `BrFalse`

### Object Operations
- `NewObj`, `Call`, `CallVirt`, `CastClass`, `IsInst`

### Array Operations
- `NewArr`, `LdElem`, `StElem`, `LdLen`

## Module Structure

- `src/lua_fob_loader.lua` - FOB binary format parser
- `src/lua_value.lua` - Runtime value type representation
- `src/lua_types.lua` - Type system and class definitions
- `src/lua_ir_instruction.lua` - Instruction executor
- `src/lua_runtime.lua` - Core virtual machine
- `examples/` - Example programs and tutorials
- `tests/` - Unit tests

## Requirements

- Lua 5.2 or compatible (5.2, 5.3 with compatibility mode, LuaJIT 2.0+)
- No external dependencies (pure Lua)

## Performance Considerations

Since this is a pure Lua implementation, performance is primarily determined by:
- Lua VM execution speed
- Number of bytecode instructions
- Depth of method call stacks
- Object creation frequency

For production use with performance-critical code, consider using the C++ runtime instead.

## Status

Currently under development. Core features implemented:
- ✓ FOB binary loader
- ✓ Type system and classes
- ✓ Value type representation
- ✓ Stack-based instruction execution
- ✓ Method dispatch (including virtual methods)
- ✓ Object creation and field access
- ⚠️ Standard library integration (partial)
- ⚠️ Debugging support

## License

Same as ObjectIR project.
