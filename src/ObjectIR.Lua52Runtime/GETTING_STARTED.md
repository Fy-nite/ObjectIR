# ObjectIR Lua 5.2 Runtime - Getting Started Guide

## What is This?

The ObjectIR Lua 5.2 Runtime is a **pure Lua implementation** of the ObjectIR runtime, allowing you to execute ObjectIR bytecode on any Lua 5.2+ environment without needing C++ compilation or external dependencies.

## Prerequisites

- Lua 5.2, 5.3, 5.4, or LuaJIT 2.0+
- No external dependencies
- ~500 lines of Lua code total

## Installation

1. Copy the `src/` directory contents to your project
2. Ensure your Lua path includes the directory containing the Lua modules

```bash
cd ObjectIR/src/ObjectIR.Lua52Runtime
```

## Quick Start

### 1. Running the Tests

```bash
cd tests
lua test_value.lua
lua test_types.lua
lua test_runtime.lua
```

### 2. Running Examples

```bash
cd examples
lua example_simple_calculator.lua
lua example_oop_demo.lua
lua example_fob_loader.lua /path/to/fob/file
```

## Module Structure

Each module is self-contained and can be required independently:

```lua
local Value = require("lua_value")              -- Value type system
local types = require("lua_types")              -- Type system & classes
local FOBLoader = require("lua_fob_loader")    -- FOB binary parser
local runtime = require("lua_runtime")         -- Virtual machine
local executor = require("lua_ir_instruction") -- Instruction executor
```

## Common Usage Patterns

### Creating Classes Programmatically

```lua
local runtime = require("lua_runtime")
local Value = require("lua_value")

local vm = runtime.VirtualMachine.new()
local builder = runtime.ClassBuilder.new(vm)

builder:Class("MyClass")
    :Field("name", "String")
    :Method("Greet", "String")
        :Parameter("greeting", "String")
    :EndMethod()
:EndClass()
```

### Creating Object Instances

```lua
-- Create an instance
local obj = vm:NewObject("MyClass")

-- Set fields
obj:SetField("name", Value.string("John"))

-- Get fields
local name = obj:GetField("name")
print(name:ToNative())  -- "John"
```

### Invoking Methods

```lua
-- Register a native method
local greetClass = vm:GetClassByName("MyClass")
local greetMethod = greetClass:GetMethod("Greet")
greetMethod:AddParameter("greeting", "String")

greetMethod.body = function(instance, args)
    local greeting = args[1]:ToNative()
    local name = instance:GetField("name"):ToNative()
    return Value.string(greeting .. ", " .. name .. "!")
end

-- Invoke
local result = vm:InvokeMethod("MyClass", "Greet", {Value.string("Hello")})
```

### Loading FOB Modules

```lua
local FOBLoader = require("lua_fob_loader")
local runtime = require("lua_runtime")

-- Load from file
local fobData = FOBLoader.LoadFromFile("my_module.fob")

-- Build runtime
local vm = runtime.VirtualMachine.LoadFromData(
    io.open("my_module.fob", "rb"):read("*a")
)

-- Execute entry point
local entryPoint = fobData.header.entryPoint
-- ... execute methods
```

## Value Types

All runtime values are represented using the `Value` type:

```lua
local Value = require("lua_value")

-- Creating values
local i32 = Value.int32(42)
local i64 = Value.int64(999999999999)
local f32 = Value.float32(3.14)
local f64 = Value.float64(2.71828)
local b = Value.bool(true)
local s = Value.string("hello")
local obj = Value.object(myObject)
local null = Value.null()

-- Checking types
i32:IsType(Value.TYPE_INT32)  -- true
s:IsType(Value.TYPE_STRING)   -- true

-- Converting to native Lua
local native = s:ToNative()   -- "hello"

-- Arithmetic
local sum = i32:Arithmetic("+", Value.int32(8))  -- 50

-- Comparison
local isEqual = i32:Compare("==", Value.int32(42))  -- true
```

## Type System

The runtime implements an object-oriented type system:

```lua
local types = require("lua_types")

-- Type references
local intType = types.TypeReference.Int32()
local strType = types.TypeReference.String()
local voidType = types.TypeReference.Void()

-- Fields
local field = types.Field.new("age", types.TypeReference.Int32())

-- Methods
local method = types.Method.new("GetName", types.TypeReference.String())
method:AddParameter("id", types.TypeReference.Int32())

-- Classes with inheritance
local baseClass = types.Class.new("Animal")
local dogClass = types.Class.new("Dog", baseClass)

-- Objects
local dog = types.Object.new(dogClass)
dog:SetField("name", Value.string("Buddy"))
```

## Instruction Execution

The runtime can execute ObjectIR bytecode instructions:

```lua
local executor = require("lua_ir_instruction")

-- Instructions are executed in the context of a method
local result = executor.Execute(vm, context, instruction)

-- Available opcodes:
-- Stack: Nop, Dup, Pop
-- Load: LdArg, LdLoc, LdFld, LdCon, LdStr, LdI4, LdI8, LdR4, LdR8, LdTrue, LdFalse, LdNull
-- Store: StLoc, StFld, StArg
-- Arithmetic: Add, Sub, Mul, Div, Rem, Neg
-- Comparison: Ceq, Cne, Clt, Cle, Cgt, Cge
-- Control Flow: Ret, Br, BrTrue, BrFalse
-- Object: NewObj, Call, CallVirt, CastClass, IsInst
-- Array: NewArr, LdElem, StElem, LdLen
```

## FOB Format

The FOB (Finite Open Bytecode) format is a binary serialization format for ObjectIR modules:

**Header:**
- Magic: "FOB" (3 bytes)
- Fork name length (1 byte)
- Fork name: "OBJECTIR,FOB" (variable length)
- File size (4 bytes, little-endian)
- Entry point (4 bytes, little-endian)

**Sections:**
- `.strings` - String table
- `.types` - Type definitions (classes and interfaces)
- `.code` - Bytecode instructions
- `.constants` - Constant values

The FOBLoader automatically parses this format and populates the runtime.

## Performance Notes

Since this is a pure Lua implementation, keep these in mind:

1. **Interpretation overhead**: Every instruction is interpreted by Lua
2. **No JIT compilation**: LuaJIT may provide some speedup
3. **Suitable for**:
   - Teaching/learning ObjectIR
   - Scripts and tools (not performance-critical)
   - Embedded scripting in Lua applications
   - Cross-platform deployment
4. **Not suitable for**:
   - CPU-intensive computations
   - Real-time applications
   - High-frequency trading systems

For production performance needs, use the C++ runtime.

## Debugging

Each module provides good error messages:

```lua
-- Runtime errors are caught and reported
local result = vm:InvokeMethod("NonExistentClass", "Method", {})
-- Error: Class not found: NonExistentClass

-- Stack traces show the call stack
-- Each instruction executor tracks its state
```

## Extending the Runtime

You can extend the runtime with native methods:

```lua
-- Add a custom class
local myClass = types.Class.new("CustomClass")
local myMethod = types.Method.new("CustomMethod", types.TypeReference.String())

-- Set native implementation
myMethod.body = function(instance, args)
    -- Custom Lua code here
    return Value.string("Custom result")
end

myClass:AddMethod(myMethod)
vm:RegisterClass(myClass)
```

## Limitations

Current limitations compared to the C++ runtime:

- No standard library integration (partially implemented)
- Limited debugging support (debugging metadata not parsed)
- No module caching optimization
- No SIMD or vectorization
- Single-threaded only

## API Reference

See the docstrings in each module for complete API documentation:

- `lua_value.lua` - Value creation and operations
- `lua_types.lua` - Type system, classes, methods, fields, objects
- `lua_fob_loader.lua` - FOB binary format parsing
- `lua_ir_instruction.lua` - Instruction execution
- `lua_runtime.lua` - Virtual machine and execution context

## Contributing

To improve the Lua runtime:

1. Add more opcodes in `lua_ir_instruction.lua`
2. Add standard library integration in `lua_runtime.lua`
3. Implement debugging support in `lua_fob_loader.lua`
4. Add more test cases in `tests/`
5. Add example programs in `examples/`

## License

Same as ObjectIR project.

## Support

For issues or questions:
1. Check the example programs
2. Review the test cases
3. Check ObjectIR documentation
4. Review C++ runtime implementation for reference
