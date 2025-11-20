# ObjectIR Lua 5.2 Runtime - Implementation Guide

## Architecture Overview

The Lua 5.2 runtime is structured in 5 core modules that work together to execute ObjectIR bytecode:

```
User Application
        ↓
    lua_runtime.lua (VirtualMachine, ExecutionContext, ClassBuilder)
        ↓
    lua_ir_instruction.lua (Instruction Executor)
        ↓
    lua_types.lua (Type System, Classes, Objects)
        ↓
    lua_value.lua (Value Representation)
        ↓
    lua_fob_loader.lua (Binary Format Parser)
```

## Module Responsibilities

### 1. lua_value.lua - Value Representation

**Purpose**: Represent runtime values with type information

**Key Concepts**:
- **Tagged Union Pattern**: Each value has a type tag and associated data
- **8 Value Types**: null, int32, int64, float32, float64, bool, string, object
- **Type-Safe Operations**: Arithmetic and comparison that respects types

**Exported Classes**:
- `Value`: Tagged union value type

**Key Methods**:
```lua
Value.int32(42)          -- Create int32 value
Value.string("hello")    -- Create string value
value:ToNative()         -- Convert to Lua native
value:Arithmetic("+", b) -- Perform arithmetic
value:Compare("==", b)   -- Perform comparison
```

**Design Pattern**: Each value is immutable after creation. Operations return new values.

### 2. lua_types.lua - Type System and Object Model

**Purpose**: Implement OOP type system with classes, inheritance, and virtual methods

**Key Concepts**:
- **Type References**: Represent types (primitive or user-defined)
- **Nominal Typing**: Types identified by name
- **Inheritance**: Classes can inherit from base classes
- **Virtual Methods**: Method lookup follows inheritance hierarchy
- **Field Access**: Through descriptors pattern

**Exported Classes**:
- `TypeReference`: Type representation
- `Field`: Class field definition
- `Method`: Method definition with optional native implementation
- `Class`: User-defined class with fields and methods
- `Object`: Instance of a class

**Key Methods**:
```lua
Class:AddField(field)        -- Add field definition
Class:GetField("name")       -- Get field (including inherited)
Class:GetMethod("name")      -- Get method (including inherited)
Object:GetField("name")      -- Get field value from instance
Object:SetField("name", val) -- Set field value on instance
```

**Inheritance**: 
- Methods and fields are inherited from base classes
- Method lookup is depth-first in the inheritance hierarchy
- Fields are initialized to null when objects are created

### 3. lua_fob_loader.lua - Binary Format Parser

**Purpose**: Parse FOB (Finite Open Bytecode) binary format into structured data

**Key Concepts**:
- **FOB Format**: Base format defined in FOB spec
- **OBJECTIR Fork**: ObjectIR-specific extensions
- **Binary Sections**: .strings, .types, .code, .constants
- **Type Definitions**: Complete class and method definitions

**Exported Functions**:
- `FOBLoader.LoadFromFile(path)`: Load from file
- `FOBLoader.LoadFromData(data)`: Load from binary data

**Opcode Mapping**: Maps opcode names to numeric values

**Binary Reading**:
- Little-endian encoding for multi-byte values
- Null-terminated strings in format
- Fixed section structure with headers

**Return Format**:
```lua
{
    header = { magic, forkName, fileSize, entryPoint },
    strings = { ... },
    types = { ... },      -- Type definitions
    instructions = { ... },
    constants = { ... },
}
```

### 4. lua_ir_instruction.lua - Instruction Executor

**Purpose**: Execute individual ObjectIR bytecode instructions

**Key Concepts**:
- **Stack-Based VM**: All operations use execution stack
- **Instruction Dispatch**: Match opcode to executor function
- **Operand Handling**: Extract operands from instruction
- **Context Management**: Modify execution state

**Main Export**:
- `InstructionExecutor.Execute(vm, context, instruction)`: Execute one instruction

**Opcode Categories**:
1. **Stack Operations**: Nop, Dup, Pop
2. **Load Operations**: LdArg, LdLoc, LdFld, LdCon, LdStr, LdI4, LdI8, LdR4, LdR8, LdTrue, LdFalse, LdNull
3. **Store Operations**: StLoc, StFld, StArg
4. **Arithmetic**: Add, Sub, Mul, Div, Rem, Neg
5. **Comparison**: Ceq, Cne, Clt, Cle, Cgt, Cge
6. **Control Flow**: Ret, Br, BrTrue, BrFalse
7. **Object Operations**: NewObj, Call, CallVirt, CastClass, IsInst
8. **Array Operations**: NewArr, LdElem, StElem, LdLen

**Execution Context**:
```lua
{
    stack = { ... },          -- Evaluation stack
    locals = { ... },         -- Local variables
    arguments = { ... },      -- Method arguments
    pc = 0,                   -- Program counter
    returnValue = nil,        -- Return value
}
```

**Return Value**: 
- `true`: Continue execution
- `false`: Stop execution (after return)

### 5. lua_runtime.lua - Virtual Machine and Execution

**Purpose**: Orchestrate instruction execution and provide runtime services

**Key Concepts**:
- **Virtual Machine**: Central execution engine
- **Class Registry**: Maps class names to class objects
- **String Table**: Interned string pool
- **Constant Table**: Compile-time constants
- **Execution Stack**: Call stack for nested method calls

**Exported Classes**:
- `VirtualMachine`: Runtime execution engine
- `ExecutionContext`: Stack frame for method execution
- `ClassBuilder`: Fluent API for programmatic class definition

**Key Methods**:
```lua
VirtualMachine:RegisterClass(class)              -- Register class
VirtualMachine:GetClass(index)                   -- Get class by index
VirtualMachine:GetClassByName(name)              -- Get class by name
VirtualMachine:InvokeMethod(className, method, args)  -- Call method
VirtualMachine:NewObject(className)              -- Create instance
VirtualMachine.LoadFromFile(path)                -- Load FOB module
VirtualMachine.LoadFromData(data)                -- Load from binary
```

**ClassBuilder Fluent API**:
```lua
builder:Class("Name", "BaseName")    -- Start class definition
    :Field("name", "String")         -- Add field
    :Method("getName", "String")     -- Add method
        :Parameter("id", "Int32")    -- Add parameter
    :EndMethod()                     -- End method
:EndClass()                          -- Register class
```

**Execution Flow**:
1. `InvokeMethod` looks up class and method
2. Creates `ExecutionContext` with initial stack
3. Loops through method instructions
4. For each instruction, calls `InstructionExecutor.Execute`
5. Returns value left on stack after `Ret` instruction

## Data Flow Example

### Creating and Using an Object

```
ClassBuilder:Class("Point")       Create class definition
    ↓
VirtualMachine:RegisterClass()    Register in runtime
    ↓
VirtualMachine:NewObject()        Create instance
    ↓
Object:SetField()                 Initialize fields
    ↓
Object:GetField()                 Access field values
```

### Loading and Executing FOB

```
FOBLoader.LoadFromFile()          Parse binary format
    ↓
BuildVirtualMachineFromFOB()      Create runtime from data
    ↓
VirtualMachine:InvokeMethod()     Execute entry point
    ↓
InstructionExecutor.Execute()     Execute each bytecode
    ↓
Return results                    Program complete
```

## Adding New Opcodes

To add a new opcode:

1. **Add to FOBLoader.OpCode**: Define numeric opcode
2. **Add executor in lua_ir_instruction.lua**:
   ```lua
   elseif opcode == FOBLoader.OpCode.NewOpcode then
       -- Implementation
       return true  -- or false to stop
   end
   ```
3. **Update documentation**: List in README.md
4. **Add tests**: Create test case in tests/

## Memory Management

The Lua 5.2 runtime relies on Lua's garbage collector:

- **Objects**: Referenced via Lua tables
- **Values**: Small objects allocated on stack or in tables
- **Strings**: Interned in string table, managed by Lua GC
- **Classes**: Registered once, not garbage collected
- **Method Execution**: Stack frames freed when method returns

**Key Principle**: No manual memory management. Objects live as long as referenced.

## Debugging and Error Handling

**Error Handling**:
- Errors are thrown as Lua exceptions (pcall friendly)
- Clear error messages with context
- Stack traces preserved by Lua

**Debugging Features**:
- FOB loader validates format
- Type system checks inheritance
- Instruction executor validates stack depth
- Method lookup follows clear rules

**Adding Debugging**:
- Add print statements to execution
- Check stack state after instructions
- Validate operand values
- Trace method calls

## Performance Considerations

**Optimization Areas**:
1. **String Interning**: Already done by lua_fob_loader
2. **Method Caching**: Classes cache method lookups
3. **Instruction Dispatch**: Simple if-elseif chain (could use table dispatch)
4. **Stack Operations**: Lua table operations are fast

**Bottlenecks**:
1. Instruction interpretation (inherent)
2. String lookups (mitigated by table)
3. Type checking on every value operation

## Testing Strategy

**Unit Tests**: Test each module independently
- `test_value.lua`: Value operations
- `test_types.lua`: Type system and classes
- `test_runtime.lua`: Virtual machine

**Integration Tests**: Test interaction between modules
- FOB loading + execution
- Inheritance + method dispatch
- Object creation + field access

**Example Programs**: Demonstrate realistic usage
- `example_simple_calculator.lua`
- `example_oop_demo.lua`
- `example_fob_loader.lua`

## Comparison with C++ Runtime

| Aspect | Lua | C++ |
|--------|-----|-----|
| **Performance** | Interpreted | Compiled |
| **Deployment** | No compilation | Requires build |
| **Portability** | Universal (Lua VMs) | Per-platform binary |
| **Development** | Quick iteration | Slower build cycle |
| **Memory** | GC friendly | Manual (RAII) |
| **Use Case** | Teaching, tools | Production, performance |

## Future Enhancements

Potential improvements:

1. **Standard Library**: Implement System.* classes
2. **Debugging**: Parse .symbols section for debugging info
3. **Optimization**: JIT compile hot paths with LuaJIT
4. **Caching**: Cache compiled methods to .lua files
5. **Documentation**: Generate from source code
6. **Performance**: Profile and optimize hot paths

## References

- FOB Format: `docs/spec.md`
- ObjectIR FOB Spec: `docs/OBJECTIR_FOB_SPEC.md`
- C++ Runtime: `src/ObjectIR.CppRuntime/`
- Tests: `tests/`
- Examples: `examples/`
