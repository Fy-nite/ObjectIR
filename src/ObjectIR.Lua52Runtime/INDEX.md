# ObjectIR Lua 5.2 Runtime - Project Index

## Quick Links

- **[README.md](README.md)** - Overview and architecture
- **[GETTING_STARTED.md](GETTING_STARTED.md)** - Installation and quick start
- **[IMPLEMENTATION.md](IMPLEMENTATION.md)** - Developer guide
- **[SUMMARY.md](SUMMARY.md)** - Complete feature summary

## Core Modules

All modules are located in `src/`:

1. **[src/lua_value.lua](src/lua_value.lua)** - Value type system
   - 8 primitive value types
   - Arithmetic and comparison operations
   - Type conversions

2. **[src/lua_types.lua](src/lua_types.lua)** - Type system and classes
   - TypeReference: Type representation
   - Class: User-defined classes with inheritance
   - Method: Method definitions
   - Field: Class fields
   - Object: Class instances

3. **[src/lua_fob_loader.lua](src/lua_fob_loader.lua)** - Binary format parser
   - FOB (Finite Open Bytecode) format parsing
   - OBJECTIR fork support
   - Section parsing (.strings, .types, .code, .constants)
   - Opcode enumeration

4. **[src/lua_ir_instruction.lua](src/lua_ir_instruction.lua)** - Instruction executor
   - 47 opcode implementations
   - Stack-based execution
   - Operand handling

5. **[src/lua_runtime.lua](src/lua_runtime.lua)** - Virtual machine
   - VirtualMachine: Central execution engine
   - ExecutionContext: Method execution frames
   - ClassBuilder: Fluent API for class definition

## Examples

Located in `examples/`:

1. **[examples/example_simple_calculator.lua](examples/example_simple_calculator.lua)**
   - Simple arithmetic calculator
   - Native method implementation
   - Basic class definition

2. **[examples/example_oop_demo.lua](examples/example_oop_demo.lua)**
   - Shape class hierarchy
   - Inheritance demonstration
   - Field access and initialization

3. **[examples/example_fob_loader.lua](examples/example_fob_loader.lua)**
   - Loading FOB modules from files
   - Module inspection
   - Entry point execution

## Tests

Located in `tests/`:

1. **[tests/test_value.lua](tests/test_value.lua)** - 30+ tests
   - Value creation and operations
   - Arithmetic operations
   - Comparison operations
   - Type conversions

2. **[tests/test_types.lua](tests/test_types.lua)** - 25+ tests
   - Type system
   - Class definitions
   - Inheritance
   - Object creation

3. **[tests/test_runtime.lua](tests/test_runtime.lua)** - 20+ tests
   - VirtualMachine functionality
   - ClassBuilder API
   - Method invocation

## Verification

- **[verify_installation.lua](verify_installation.lua)** - Installation verification script
  - Checks Lua version
  - Loads all modules
  - Verifies basic functionality

## Getting Started

### 1. Quick Verification

```bash
lua verify_installation.lua
```

### 2. Run Examples

```bash
cd examples
lua example_simple_calculator.lua
lua example_oop_demo.lua
lua example_fob_loader.lua /path/to/fob/file
```

### 3. Run Tests

```bash
cd tests
lua test_value.lua
lua test_types.lua
lua test_runtime.lua
```

### 4. Use in Your Code

```lua
local runtime = require("lua_runtime")
local Value = require("lua_value")

local vm = runtime.VirtualMachine.new()
-- ... use the runtime
```

## Architecture

```
┌─────────────────────────────────┐
│  User Application               │
└──────────────┬──────────────────┘
               │
┌──────────────▼──────────────────┐
│  lua_runtime.lua                │
│  (VirtualMachine, ClassBuilder) │
└──────────────┬──────────────────┘
               │
┌──────────────▼──────────────────┐
│  lua_ir_instruction.lua         │
│  (Instruction Executor)         │
└──────────────┬──────────────────┘
               │
┌──────────────▼──────────────────┐
│  lua_types.lua                  │
│  (Type System, Classes)         │
└──────────────┬──────────────────┘
               │
┌──────────────▼──────────────────┐
│  lua_value.lua                  │
│  (Value Representation)         │
└──────────────┬──────────────────┘
               │
┌──────────────▼──────────────────┐
│  lua_fob_loader.lua             │
│  (Binary Format Parser)         │
└─────────────────────────────────┘
```

## Statistics

- **Total Lines of Code**: ~2700+
- **Lua Modules**: 5
- **Documentation Files**: 4
- **Example Programs**: 3
- **Test Files**: 3
- **Verification Script**: 1
- **Test Cases**: 75+
- **Opcodes Implemented**: 47

## Features

✓ Pure Lua 5.2+ implementation
✓ Complete ObjectIR bytecode execution
✓ FOB binary format support
✓ Full OOP with inheritance
✓ 47 instruction opcodes
✓ 8 value types
✓ No external dependencies
✓ Comprehensive documentation
✓ Working examples
✓ Extensive test suite

## Requirements

- Lua 5.2 or later
- No external dependencies
- No C compilation needed

## Performance

Suitable for:
- Educational purposes
- Tool development
- Cross-platform scripts
- Embedded Lua applications

Not suitable for:
- CPU-intensive computation
- Real-time applications
- High-performance scenarios

For production performance, use the C++ runtime.

## File Organization

```
ObjectIR.Lua52Runtime/
├── README.md                    # Overview
├── GETTING_STARTED.md          # Quick start
├── IMPLEMENTATION.md           # Developer guide
├── SUMMARY.md                  # Feature summary
├── INDEX.md                    # This file
├── verify_installation.lua     # Verification script
├── src/
│   ├── lua_value.lua           # Value types (180 lines)
│   ├── lua_types.lua           # Type system (240 lines)
│   ├── lua_fob_loader.lua      # FOB parser (450 lines)
│   ├── lua_ir_instruction.lua  # Instruction executor (380 lines)
│   └── lua_runtime.lua         # Virtual machine (360 lines)
├── examples/
│   ├── example_simple_calculator.lua
│   ├── example_oop_demo.lua
│   └── example_fob_loader.lua
└── tests/
    ├── test_value.lua          # 30+ tests
    ├── test_types.lua          # 25+ tests
    └── test_runtime.lua        # 20+ tests
```

## Next Steps

1. **Read**: Start with [GETTING_STARTED.md](GETTING_STARTED.md)
2. **Verify**: Run `verify_installation.lua`
3. **Explore**: Try the examples in `examples/`
4. **Test**: Run tests in `tests/`
5. **Learn**: Study [IMPLEMENTATION.md](IMPLEMENTATION.md)
6. **Extend**: Add custom functionality

## Support

For questions or issues:
1. Check [GETTING_STARTED.md](GETTING_STARTED.md) for common patterns
2. Review examples in `examples/`
3. Check test cases in `tests/`
4. Read [IMPLEMENTATION.md](IMPLEMENTATION.md) for details

## License

Same as ObjectIR project.

## Related Resources

- **ObjectIR Repository**: Main ObjectIR project
- **C++ Runtime**: High-performance production runtime
- **FOB Format Spec**: Binary format specification
- **ObjectIR Documentation**: Full language documentation
