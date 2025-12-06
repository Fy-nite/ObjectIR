# ObjectIR C++ VM Instructions - Text Format Reference

## Overview

This document describes the **text-based instruction format** used by the ObjectIR C++ Virtual Machine. These instructions represent the low-level operations that execute on the C++ runtime, providing a human-readable format for defining method bodies and program logic.

The text format is distinct from the JSON serialization format (documented in [INSTRUCTION_SERIALIZATION.md](INSTRUCTION_SERIALIZATION.md)). The text format is parsed by the C++ runtime's text parser (`ir_text_parser.cpp`) and is the format used when writing `.ir` files.

## Instruction Categories

The C++ VM supports several categories of instructions:

1. [Stack Operations](#stack-operations)
2. [Load Instructions](#load-instructions)
3. [Store Instructions](#store-instructions)
4. [Arithmetic Operations](#arithmetic-operations)
5. [Comparison Operations](#comparison-operations)
6. [Control Flow](#control-flow)
7. [Method Calls](#method-calls)

---

## Stack Operations

Stack operations manipulate values on the evaluation stack without modifying memory.

### `nop`

**Description**: No operation. Does nothing.

**Stack Behavior**: `... → ...`

**Syntax**:
```
nop
```

**Example**:
```
nop  // Placeholder or alignment
```

---

### `dup`

**Description**: Duplicates the value on top of the stack.

**Stack Behavior**: `..., value → ..., value, value`

**Syntax**:
```
dup
```

**Example**:
```
ldarg x
dup          // Stack now has x twice
stloc temp   // Store one copy to temp
add          // Use the other copy
```

---

### `pop`

**Description**: Removes the top value from the stack.

**Stack Behavior**: `..., value → ...`

**Syntax**:
```
pop
```

**Example**:
```
call SomeMethod  // Method returns a value
pop              // Discard the return value
```

---

## Load Instructions

Load instructions push values onto the evaluation stack.

### `ldarg` - Load Argument

**Description**: Loads a method parameter value onto the stack.

**Stack Behavior**: `... → ..., value`

**Syntax**:
```
ldarg <parameterName>
```

**Parameters**:
- `parameterName`: Name of the parameter to load

**Example**:
```
method Add(a: int32, b: int32) -> int32 {
    ldarg a      // Load parameter 'a'
    ldarg b      // Load parameter 'b'
    add
    ret
}
```

---

### `ldloc` - Load Local Variable

**Description**: Loads a local variable value onto the stack.

**Stack Behavior**: `... → ..., value`

**Syntax**:
```
ldloc <localName>
```

**Parameters**:
- `localName`: Name of the local variable to load

**Example**:
```
method Calculate() -> int32 {
    local result: int32
    ldc.i4 42
    stloc result
    ldloc result  // Load local variable 'result'
    ret
}
```

---

### `ldc.i4` - Load 32-bit Integer Constant

**Description**: Loads a 32-bit integer constant onto the stack.

**Stack Behavior**: `... → ..., value`

**Syntax**:
```
ldc.i4 <value>
```

**Parameters**:
- `value`: 32-bit integer constant

**Example**:
```
ldc.i4 42       // Load integer 42
ldc.i4 -10      // Load integer -10
ldc.i4 0        // Load zero
```

---

### `ldc.i8` - Load 64-bit Integer Constant

**Description**: Loads a 64-bit integer constant onto the stack.

**Stack Behavior**: `... → ..., value`

**Syntax**:
```
ldc.i8 <value>
```

**Parameters**:
- `value`: 64-bit integer constant

**Example**:
```
ldc.i8 9223372036854775807   // Load max int64
ldc.i8 -1000000000000        // Load large negative number
```

---

### `ldc.r4` - Load 32-bit Float Constant

**Description**: Loads a 32-bit floating-point constant onto the stack.

**Stack Behavior**: `... → ..., value`

**Syntax**:
```
ldc.r4 <value>
```

**Parameters**:
- `value`: 32-bit floating-point constant

**Example**:
```
ldc.r4 3.14159
ldc.r4 -0.5
ldc.r4 0.0
```

---

### `ldc.r8` - Load 64-bit Float Constant

**Description**: Loads a 64-bit floating-point constant onto the stack.

**Stack Behavior**: `... → ..., value`

**Syntax**:
```
ldc.r8 <value>
```

**Parameters**:
- `value`: 64-bit floating-point constant

**Example**:
```
ldc.r8 3.141592653589793
ldc.r8 2.718281828459045
ldc.r8 -1.234567890123456
```

---

### `ldstr` - Load String Constant

**Description**: Loads a string constant onto the stack.

**Stack Behavior**: `... → ..., value`

**Syntax**:
```
ldstr "<string>"
```

**Parameters**:
- `string`: String literal (in quotes)

**Example**:
```
ldstr "Hello, World!"
ldstr "Error: Invalid input"
ldstr ""  // Empty string
```

---

### `ldnull` - Load Null Reference

**Description**: Loads a null reference onto the stack.

**Stack Behavior**: `... → ..., null`

**Syntax**:
```
ldnull
```

**Example**:
```
ldnull
stloc obj  // Initialize obj to null
```

---

### `ldtrue` - Load Boolean True

**Description**: Loads the boolean value `true` onto the stack.

**Stack Behavior**: `... → ..., true`

**Syntax**:
```
ldtrue
```

**Example**:
```
ldtrue
stloc isValid
```

---

### `ldfalse` - Load Boolean False

**Description**: Loads the boolean value `false` onto the stack.

**Stack Behavior**: `... → ..., false`

**Syntax**:
```
ldfalse
```

**Example**:
```
ldfalse
stloc isReady
```

---

### `ldcon` - Load Generic Constant

**Description**: Loads a constant value of any type onto the stack. This is a general-purpose constant loader.

**Stack Behavior**: `... → ..., value`

**Syntax**:
```
ldcon <value>
```

**Parameters**:
- `value`: Constant value to load

**Example**:
```
ldcon 100
ldcon "text"
```

**Note**: Prefer specific `ldc.*` instructions when the type is known.

---

## Store Instructions

Store instructions pop values from the stack and store them in memory locations.

### `starg` - Store to Argument

**Description**: Stores the top stack value to a method parameter.

**Stack Behavior**: `..., value → ...`

**Syntax**:
```
starg <parameterName>
```

**Parameters**:
- `parameterName`: Name of the parameter to store to

**Example**:
```
method Process(x: int32) -> void {
    ldarg x
    ldc.i4 10
    add
    starg x      // Update parameter x
}
```

---

### `stloc` - Store to Local Variable

**Description**: Stores the top stack value to a local variable.

**Stack Behavior**: `..., value → ...`

**Syntax**:
```
stloc <localName>
```

**Parameters**:
- `localName`: Name of the local variable to store to

**Example**:
```
method Calculate() -> int32 {
    local result: int32
    ldc.i4 42
    stloc result  // Store 42 to result
    ldloc result
    ret
}
```

---

## Arithmetic Operations

Arithmetic operations perform mathematical calculations on stack values.

### `add` - Addition

**Description**: Adds the top two stack values.

**Stack Behavior**: `..., value1, value2 → ..., result`

**Syntax**:
```
add
```

**Example**:
```
ldarg a
ldarg b
add          // result = a + b
ret
```

---

### `sub` - Subtraction

**Description**: Subtracts the second stack value from the first.

**Stack Behavior**: `..., value1, value2 → ..., result`

**Syntax**:
```
sub
```

**Example**:
```
ldarg a
ldarg b
sub          // result = a - b
ret
```

---

### `mul` - Multiplication

**Description**: Multiplies the top two stack values.

**Stack Behavior**: `..., value1, value2 → ..., result`

**Syntax**:
```
mul
```

**Example**:
```
ldarg a
ldarg b
mul          // result = a * b
ret
```

---

### `div` - Division

**Description**: Divides the first stack value by the second.

**Stack Behavior**: `..., value1, value2 → ..., result`

**Syntax**:
```
div
```

**Example**:
```
ldarg a
ldarg b
div          // result = a / b
ret
```

**Note**: Division by zero behavior is runtime-dependent.

---

### `rem` - Remainder

**Description**: Computes the remainder of dividing the first value by the second.

**Stack Behavior**: `..., value1, value2 → ..., result`

**Syntax**:
```
rem
```

**Example**:
```
ldarg a
ldarg b
rem          // result = a % b
ret
```

---

### `neg` - Negation

**Description**: Negates the top stack value.

**Stack Behavior**: `..., value → ..., -value`

**Syntax**:
```
neg
```

**Example**:
```
ldarg x
neg          // result = -x
ret
```

---

## Comparison Operations

Comparison operations compare two values and push a boolean result.

### `ceq` - Compare Equal

**Description**: Compares two values for equality.

**Stack Behavior**: `..., value1, value2 → ..., (value1 == value2)`

**Syntax**:
```
ceq
```

**Example**:
```
ldarg a
ldarg b
ceq          // result = (a == b)
```

---

### `cne` - Compare Not Equal

**Description**: Compares two values for inequality.

**Stack Behavior**: `..., value1, value2 → ..., (value1 != value2)`

**Syntax**:
```
cne
```

**Example**:
```
ldarg a
ldarg b
cne          // result = (a != b)
```

---

### `clt` - Compare Less Than

**Description**: Compares if the first value is less than the second.

**Stack Behavior**: `..., value1, value2 → ..., (value1 < value2)`

**Syntax**:
```
clt
```

**Example**:
```
ldarg a
ldarg b
clt          // result = (a < b)
```

---

### `cle` - Compare Less Than or Equal

**Description**: Compares if the first value is less than or equal to the second.

**Stack Behavior**: `..., value1, value2 → ..., (value1 <= value2)`

**Syntax**:
```
cle
```

**Example**:
```
ldarg a
ldarg b
cle          // result = (a <= b)
```

---

### `cgt` - Compare Greater Than

**Description**: Compares if the first value is greater than the second.

**Stack Behavior**: `..., value1, value2 → ..., (value1 > value2)`

**Syntax**:
```
cgt
```

**Example**:
```
ldarg a
ldarg b
cgt          // result = (a > b)
```

---

### `cge` - Compare Greater Than or Equal

**Description**: Compares if the first value is greater than or equal to the second.

**Stack Behavior**: `..., value1, value2 → ..., (value1 >= value2)`

**Syntax**:
```
cge
```

**Example**:
```
ldarg a
ldarg b
cge          // result = (a >= b)
```

---

## Control Flow

Control flow instructions manage program execution flow.

### `ret` - Return

**Description**: Returns from the current method.

**Stack Behavior**: 
- For void methods: `... → (empty)`
- For value-returning methods: `..., value → (empty)`

**Syntax**:
```
ret
```

**Example**:
```
// Void method
method DoSomething() -> void {
    ldstr "Done"
    call System.Console.WriteLine(string) -> void
    ret
}

// Value-returning method
method GetValue() -> int32 {
    ldc.i4 42
    ret          // Returns 42
}
```

---

### `if` - Conditional Statement

**Description**: Executes a block of code conditionally based on a condition.

**Syntax**:
```
if (<condition>) {
    <then-instructions>
}
```

or

```
if (<condition>) {
    <then-instructions>
} else {
    <else-instructions>
}
```

**Condition Types**:
- `stack`: Uses the boolean value on top of the stack
- Comparison expression: e.g., `a < b`

**Example**:
```
method Max(a: int32, b: int32) -> int32 {
    ldarg a
    ldarg b
    cgt
    
    if (stack) {
        ldarg a
        ret
    } else {
        ldarg b
        ret
    }
}
```

---

### `while` - Loop Statement

**Description**: Repeats a block of code while a condition is true.

**Syntax**:
```
while (<condition>) {
    <body-instructions>
}
```

**Example**:
```
method CountDown(n: int32) -> void {
    local i: int32
    ldarg n
    stloc i
    
    while (i > 0) {
        ldloc i
        call System.Console.WriteLine(int32) -> void
        
        ldloc i
        ldc.i4 1
        sub
        stloc i
    }
}
```

---

### `break` - Break from Loop

**Description**: Exits the current loop immediately.

**Syntax**:
```
break
```

**Example**:
```
while (true) {
    ldloc counter
    ldc.i4 10
    cge
    
    if (stack) {
        break    // Exit the loop
    }
    
    ldloc counter
    ldc.i4 1
    add
    stloc counter
}
```

---

### `continue` - Continue Loop

**Description**: Skips the rest of the current loop iteration and continues with the next iteration.

**Syntax**:
```
continue
```

**Example**:
```
while (i < 10) {
    ldloc i
    ldc.i4 2
    rem
    ldc.i4 0
    ceq
    
    if (stack) {
        ldloc i
        ldc.i4 1
        add
        stloc i
        continue    // Skip even numbers
    }
    
    ldloc i
    call Print(int32) -> void
    
    ldloc i
    ldc.i4 1
    add
    stloc i
}
```

---

### `throw` - Throw Exception

**Description**: Throws an exception object.

**Stack Behavior**: `..., exception → (empty)`

**Syntax**:
```
throw
```

**Example**:
```
method ValidatePositive(value: int32) -> void {
    ldarg value
    ldc.i4 0
    clt
    
    if (stack) {
        ldstr "Value must be positive"
        newobj Exception(string)
        throw
    }
}
```

---

## Method Calls

Method call instructions invoke methods and functions.

### `call` - Static/Instance Method Call

**Description**: Calls a static or instance method.

**Stack Behavior**: 
- Static: `..., arg1, arg2, ... → ..., [result]`
- Instance: `..., instance, arg1, arg2, ... → ..., [result]`

**Syntax**:
```
call <DeclaringType>.<MethodName>(<paramType1>, <paramType2>, ...) -> <returnType>
```

**Parameters**:
- `DeclaringType`: The type that declares the method
- `MethodName`: Name of the method
- `paramType1, paramType2, ...`: Parameter types (comma-separated)
- `returnType`: Return type of the method

**Example**:
```
// Static method call
ldstr "Hello"
call System.Console.WriteLine(string) -> void

// Static method with return value
ldarg x
call System.Math.Abs(int32) -> int32
ret

// Instance method call
ldloc myList
ldc.i4 42
call System.List.Add(int32) -> void
```

---

### `callvirt` - Virtual Method Call

**Description**: Calls a virtual or interface method with late binding.

**Stack Behavior**: `..., instance, arg1, arg2, ... → ..., [result]`

**Syntax**:
```
callvirt <DeclaringType>.<MethodName>(<paramType1>, <paramType2>, ...) -> <returnType>
```

**Parameters**: Same as `call`

**Example**:
```
method CallSpeak(animal: Animal) -> void {
    ldarg animal
    callvirt Animal.Speak() -> string
    call System.Console.WriteLine(string) -> void
}
```

**Note**: `callvirt` performs virtual dispatch, calling the most-derived implementation of the method.

---

## Complete Examples

### Example 1: Simple Addition

```
module MathLib version 1.0.0

class Calculator {
    method Add(a: int32, b: int32) -> int32 {
        ldarg a
        ldarg b
        add
        ret
    }
}
```

### Example 2: Factorial Function

```
module Factorial version 1.0.0

class Math {
    method Factorial(n: int32) -> int32 {
        local result: int32
        local i: int32
        
        ldc.i4 1
        stloc result
        
        ldc.i4 2
        stloc i
        
        while (i <= n) {
            ldloc result
            ldloc i
            mul
            stloc result
            
            ldloc i
            ldc.i4 1
            add
            stloc i
        }
        
        ldloc result
        ret
    }
}
```

### Example 3: String Manipulation

```
module StringDemo version 1.0.0

class StringHelper {
    method PrintMessage(name: string) -> void {
        ldstr "Hello, "
        ldarg name
        call System.String.Concat(string, string) -> string
        ldstr "!"
        call System.String.Concat(string, string) -> string
        call System.Console.WriteLine(string) -> void
        ret
    }
}
```

### Example 4: Conditional Logic

```
module Comparison version 1.0.0

class Logic {
    method Max(a: int32, b: int32) -> int32 {
        ldarg a
        ldarg b
        cgt
        
        if (stack) {
            ldarg a
            ret
        } else {
            ldarg b
            ret
        }
    }
    
    method IsEven(n: int32) -> bool {
        ldarg n
        ldc.i4 2
        rem
        ldc.i4 0
        ceq
        ret
    }
}
```

### Example 5: Loop with Break and Continue

```
module LoopDemo version 1.0.0

class Loops {
    method PrintOddNumbers(max: int32) -> void {
        local i: int32
        ldc.i4 0
        stloc i
        
        while (i < max) {
            ldloc i
            ldc.i4 10
            cge
            
            if (stack) {
                break
            }
            
            ldloc i
            ldc.i4 2
            rem
            ldc.i4 0
            ceq
            
            if (stack) {
                ldloc i
                ldc.i4 1
                add
                stloc i
                continue
            }
            
            ldloc i
            call System.Console.WriteLine(int32) -> void
            
            ldloc i
            ldc.i4 1
            add
            stloc i
        }
    }
}
```

---

## Stack Semantics

The ObjectIR C++ VM uses an evaluation stack for intermediate values:

1. **Stack-based execution**: Most operations consume values from the stack and push results back
2. **Strongly typed**: Each stack slot holds a typed value
3. **LIFO (Last In, First Out)**: The most recently pushed value is the first to be consumed
4. **Method boundary**: The stack is cleared when entering and exiting methods

### Stack Example

```
ldc.i4 10      // Stack: [10]
ldc.i4 20      // Stack: [10, 20]
add            // Stack: [30]
ldc.i4 5       // Stack: [30, 5]
mul            // Stack: [150]
ret            // Returns 150, stack is cleared
```

---

## Type System

The C++ VM supports these primitive types in text format:

- **Integers**: `int8`, `int16`, `int32`, `int64`, `uint8`, `uint16`, `uint32`, `uint64`
- **Floating-point**: `float32`, `float64`
- **Boolean**: `bool`
- **Character**: `char`
- **String**: `string`
- **Void**: `void` (for methods with no return value)

Reference types are specified by their fully-qualified names:
- `System.Console`
- `System.String`
- `System.List<int32>`
- Custom types: `MyNamespace.MyClass`

---

## Comparison with JSON Format

The text format is designed for human readability, while the JSON format (documented in [INSTRUCTION_SERIALIZATION.md](INSTRUCTION_SERIALIZATION.md)) is used for serialization and tool interchange.

**Text Format**:
```
ldarg x
ldarg y
add
ret
```

**JSON Format**:
```json
[
  { "opCode": "ldarg", "operand": { "argumentName": "x" } },
  { "opCode": "ldarg", "operand": { "argumentName": "y" } },
  { "opCode": "add" },
  { "opCode": "ret" }
]
```

Both formats represent the same instruction sequence and are interchangeable.

---

## Parser Implementation

The text format is parsed by `ir_text_parser.cpp` in the C++ runtime, which:

1. **Tokenizes** the input text into tokens (keywords, identifiers, operators)
2. **Parses** module structure (classes, methods, fields)
3. **Converts** text instructions to internal `Instruction` structures
4. **Validates** instruction operands and types

For parser details, see the implementation in:
- `/src/ObjectIR.CppRuntime/src/ir_text_parser.cpp`
- `/src/ObjectIR.CppRuntime/include/ir_text_parser.hpp`

---

## Related Documentation

- [INSTRUCTION_SERIALIZATION.md](INSTRUCTION_SERIALIZATION.md) - JSON instruction format
- [GRAMMAR.md](GRAMMAR.md) - Complete ObjectIR grammar
- [ARCHITECTURE.md](ARCHITECTURE.md) - Overall system architecture
- [OBJECTIR_JSON_SPEC.md](OBJECTIR_JSON_SPEC.md) - JSON module specification
- [GETTING_STARTED.md](GETTING_STARTED.md) - Getting started guide

---

## Implementation Notes

### Instruction Execution

Each instruction is executed by the `InstructionExecutor` class in the C++ runtime:

```cpp
void InstructionExecutor::Execute(
    const Instruction& instr,
    ExecutionContext* context,
    VirtualMachine* vm
);
```

The executor maintains:
- An evaluation stack for intermediate values
- Local variable storage
- Argument storage
- Control flow state (for loops and conditionals)

### OpCode Mapping

The text parser converts instruction mnemonics to internal `OpCode` enum values:

| Text Format | OpCode Enum | Description |
|-------------|-------------|-------------|
| `ldarg` | `OpCode::LdArg` | Load argument |
| `stloc` | `OpCode::StLoc` | Store local |
| `add` | `OpCode::Add` | Addition |
| `ceq` | `OpCode::Ceq` | Compare equal |
| `call` | `OpCode::Call` | Method call |
| `ret` | `OpCode::Ret` | Return |

For the complete mapping, see `ir_instruction.hpp`.

---

## Best Practices

### 1. Use Descriptive Names

```
// Good
method CalculateTotal(price: float32, quantity: int32) -> float32

// Less clear
method Calc(p: float32, q: int32) -> float32
```

### 2. Declare Locals Clearly

```
method Process() -> int32 {
    local result: int32
    local temp: int32
    local counter: int32
    // ...
}
```

### 3. Balance Stack Operations

Ensure stack is balanced at control flow merge points:

```
// Balanced
if (condition) {
    ldc.i4 10    // Stack: [10]
} else {
    ldc.i4 20    // Stack: [20]
}
// Both branches have same stack depth
```

### 4. Use Structured Control Flow

Prefer `if`, `while` over low-level branching when possible:

```
// Good - readable
while (i < 10) {
    // body
}

// Less clear - manual branching
// (not available in text format)
```

### 5. Comment Complex Logic

```
method ComplicateCalculation(x: int32) -> int32 {
    // Calculate (x * 2) + (x / 3)
    ldarg x
    ldc.i4 2
    mul          // x * 2
    
    ldarg x
    ldc.i4 3
    div          // x / 3
    
    add          // sum both parts
    ret
}
```

---

## Troubleshooting

### Common Errors

**Stack Underflow**:
```
// Error: trying to add with only one value
ldc.i4 10
add         // Error: needs two values
```

**Type Mismatch**:
```
// Error: cannot add string and int
ldstr "hello"
ldc.i4 42
add         // Error: incompatible types
```

**Undefined Variable**:
```
// Error: 'result' not declared
ldloc result   // Error: must declare with 'local result: int32'
```

**Unbalanced Stack**:
```
if (condition) {
    ldc.i4 10
    ldc.i4 20
} else {
    ldc.i4 30    // Error: stack depths don't match
}
```

---

## Version History

- **Version 1.0** (Current): Initial C++ VM instruction reference
  - All basic instructions documented
  - Examples provided for each category
  - Complete instruction set coverage

---

## Contributing

To add or modify instructions:

1. Update the OpCode enum in `ir_instruction.hpp`
2. Implement the executor in `instruction_executor.cpp`
3. Add parser support in `ir_text_parser.cpp`
4. Update this documentation
5. Add test cases

---

**Last Updated**: December 2025  
**Status**: Current and Complete
