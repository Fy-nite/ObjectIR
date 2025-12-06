# ObjectIR Text Format Examples

This directory contains example `.ir` files demonstrating the text-based instruction format used by the ObjectIR C++ Virtual Machine.

## Files

### [simple_addition.ir](simple_addition.ir)
Basic arithmetic operations demonstrating:
- Method parameters (`ldarg`)
- Local variables (`ldloc`, `stloc`)
- Arithmetic instructions (`add`, `sub`, `mul`)
- Return statements (`ret`)

**Key concepts**: Basic stack operations, parameter handling, local variable storage

---

### [control_flow.ir](control_flow.ir)
Control flow and comparison operations demonstrating:
- Conditional statements (`if`/`else`)
- Loop statements (`while`)
- All comparison operations (`ceq`, `cne`, `clt`, `cle`, `cgt`, `cge`)
- Boolean operations

**Key concepts**: Structured control flow, comparison operations, stack-based conditions

---

### [factorial.ir](factorial.ir)
Advanced loop patterns demonstrating:
- While loops with complex conditions
- Break statement (`break`)
- Continue statement (`continue`)
- Multiple local variables
- Nested control flow

**Key concepts**: Loop control, early exit, iteration patterns, accumulator pattern

---

## Running Examples

To use these examples with the ObjectIR C++ runtime:

1. **Build the C++ runtime**:
   ```bash
   cd src/ObjectIR.CppRuntime
   mkdir build && cd build
   cmake ..
   make
   ```

2. **Run an example**:
   ```bash
   ./ObjectIRRuntime ../../docs/examples/simple_addition.ir
   ```

## Learning Path

1. Start with `simple_addition.ir` to understand basic instructions
2. Move to `control_flow.ir` to learn conditionals and comparisons
3. Study `factorial.ir` for advanced loop patterns

## Writing Your Own

When writing text-based IR files:

1. **Start with the module declaration**:
   ```
   module YourModuleName version 1.0.0
   ```

2. **Define classes**:
   ```
   class YourClass {
       // methods here
   }
   ```

3. **Create methods**:
   ```
   method MethodName(param1: type1, param2: type2) -> returnType {
       // instructions here
   }
   ```

4. **Use proper instruction syntax**:
   - Load: `ldarg name`, `ldloc name`, `ldc.i4 42`
   - Store: `stloc name`, `starg name`
   - Arithmetic: `add`, `sub`, `mul`, `div`, `rem`, `neg`
   - Compare: `ceq`, `cgt`, `clt`, `cge`, `cle`, `cne`
   - Control: `if (condition) { }`, `while (condition) { }`, `ret`

## Complete Documentation

For complete instruction reference, see [VM_INSTRUCTIONS_TEXT_FORMAT.md](../VM_INSTRUCTIONS_TEXT_FORMAT.md).

## Related Documentation

- [VM_INSTRUCTIONS_TEXT_FORMAT.md](../VM_INSTRUCTIONS_TEXT_FORMAT.md) - Complete instruction reference
- [GRAMMAR.md](../GRAMMAR.md) - Formal grammar specification
- [INSTRUCTION_SERIALIZATION.md](../INSTRUCTION_SERIALIZATION.md) - JSON format documentation
