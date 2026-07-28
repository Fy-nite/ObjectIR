// )
#import "@preview/xyznote:0.5.0": *
#import "helpers.typ": *
#set text(font: "Fira Code", size: 15pt)
#show: xyznote.with(
  title: "ObjectIR Version 3",
  author: "charlie santana - Finite",
  abstract: "The official ObjectIR V3 spec.",
  createtime: "2024-11-27",
  lang: "au",
  bibliography-style: "ieee",
  preface: [
    
= What this document is

This document defines the ObjectIR Version 3 specification, including:

- the ObjectIR abstract machine.
- the instruction set.
- the execution semantics.
- the required behaviour of conforming implementations.

= What this document is not

This document does not specify:

- how an implementation is written.
- optimisation strategies.
- a programming language.
- development tools or debugging facilities.
- a binary container or executable file format unless explicitly stated.
  ] //Annotate this line to delete the preface page.
)



#divider()
#linebreak()
#set heading(numbering: "1.")

#linebreak()

= Introduction

ObjectIR is a typed, object-oriented intermediate representation designed
to represent programs independently of their source language and execution
environment. ObjectIR may be executed by virtual machines, compiled to
native code, translated to other intermediate representations, transformed
by tooling, or embedded as an in-memory program representation.

This specification defines the observable behaviour of ObjectIR programs.
It intentionally does not prescribe how an implementation achieves that
behaviour, allowing interpreters, virtual machines, static compilers,
dynamic translators, and other execution engines to conform to the same
specification.


== Goals

ObjectIR's primary goals are:

- Be simple to parse, generate, and implement.
- Provide a stable intermediate representation between frontends, backends and runtimes.
- Preserve high-level program structure where practical.
- Support object-oriented programming semantics.
- Remain language and runtime agnostic.
- Enable program analysis, transformation, and optimisation.
- Be deterministic and portable.
- Define behaviour independently of any implementation.

== Use Cases

ObjectIR can be used for:

- Transpiling between programming languages.
- Building language compilers.
- Implementing interpreters and JIT compilers.
- Static analysis and code inspection.
- Program transformation and optimisation.
- Reverse engineering and decompilation.
- Serialization of executable program logic.
- Visual scripting backends.
- Educational language implementations.

= Formats
ObjectIR can come in many formats, most notably our Fob/IR, Text-IR and Json formats.

= types

ObjectIR defines a fixed set of primitive types. Type names in operands
are #strong[case-insensitive];: conforming tooling must normalise them to
lower-case before lookup, so `System.String`, `system.string`, and
`string` are all equivalent.


#figure(
  align(center)[#table(
    columns: 3,
    align: (left,left,left,),
    table.header([#strong[Primary IR name(s)];], [#strong[Width /
      Semantics];], [],),
    table.hline(),
    [`void`], [No value; return type only.], [],
    [`bool`], [Boolean true/false.], [],
    [`int8`], [8-bit signed integer.], [],
    [`uint8`], [8-bit unsigned integer.], [],
    [`int16`], [16-bit signed integer.], [],
    [`uint16`], [16-bit unsigned integer.], [],
    [`int32`], [32-bit signed integer.], [],
    [`uint32`], [32-bit unsigned integer.], [],
    [`int64`], [64-bit signed integer.], [],
    [`uint64`], [64-bit unsigned integer.], [],
    [`float32`, `single`], [32-bit IEEE 754 float.], [],
    [`float64`, `double`], [64-bit IEEE 754 float.], [],
    [`char`], [Unicode scalar (UTF-16 unit).], [],
    [`string`], [Immutable Unicode character sequence.], [],
    [`object`], [Top type; any value.], [],
    [`decimal`], [High-precision fixed-point decimal.], [],
    [`datetime`], [Date and time instant.], [],
    [`timespan`], [Duration / time interval.], [],
    [`guid`], [128-bit globally unique identifier.], [],
  )]
  , kind: table
  )

= Instruction Set Reference

ObjectIR uses a stack machine execution model. Each method invocation
creates a new call frame with an evaluation stack of untyped values,
a named argument table, a local variable table, a pending exception slot,
and a program counter.

For the binary encoding of each instruction's opcode, see the
separate *FOB/IR v3 Encoding Reference* (`fob-encoding.typ`).

== Stack Manipulation

#instruction(
  "nop",
  "---",
  "None",
  "No-operation; advances the program counter only."
)

#instruction(
  "dup",
  "v -> v, v",
  "Stack underflow",
  "Duplicate the top value on the evaluation stack."
)

#instruction(
  "pop",
  "v ->",
  "Stack underflow",
  "Discard the top value on the evaluation stack."
)

#instruction(
  "ldnull",
  "-> null",
  "None",
  "Push null onto the evaluation stack."
)

== Load Constants

#instruction(
  "ldc.i4",
  "-> i32",
  "None",
  "Push a 32-bit integer constant onto the stack. Operand: bare integer."
)

#instruction(
  "ldc.i8",
  "-> i64",
  "None",
  "Push a 64-bit integer constant onto the stack. Operand: bare integer."
)

#instruction(
  "ldc.r4",
  "-> f32",
  "None",
  "Push a 32-bit float constant onto the stack. Operand: bare number."
)

#instruction(
  "ldc.r8",
  "-> f64",
  "None",
  "Push a 64-bit float constant onto the stack. Operand: bare number."
)

#instruction(
  "ldstr",
  "-> string",
  "None",
  "Push a string constant onto the stack. Operand: \"...\""
)

== Arguments and Local Variables

#instruction(
  "ldarg",
  "-> value",
  "None",
  "Push a method argument onto the stack. Operand: parameter name or 0-based index. Index 0 / name 'this' is the receiver."
)

#instruction(
  "starg",
  "value ->",
  "Stack underflow",
  "Pop a value and store it in the named argument slot."
)

#instruction(
  "ldloc",
  "-> value",
  "None",
  "Push a local variable onto the stack by name."
)

#instruction(
  "stloc",
  "value ->",
  "Stack underflow",
  "Pop a value and store it in the named local variable."
)

== Field Access

#instruction(
  "ldfld",
  "obj -> value",
  "Null reference",
  "Pop an object (or use the implicit receiver) and push the value of the named instance field. If the object has a host backing, the runtime should read the corresponding property on the host object before consulting the field store."
)

#instruction(
  "stfld",
  "obj, value ->",
  "Null reference",
  "Pop a value, then pop an object; store the value in the named instance field (and on the host backing, if present)."
)

#instruction(
  "ldsfld",
  "-> value",
  "None",
  "Push the value of a static field. Operand: field name."
)

#instruction(
  "stsfld",
  "value ->",
  "Stack underflow",
  "Pop a value and store it in the named static field."
)

== Arithmetic

#instruction(
  "add",
  "a b -> result",
  "Stack underflow",
  "Pop b, pop a, push a + b. In float mode, also performs string concatenation when either operand is a string."
)

#instruction(
  "sub",
  "a b -> result",
  "Stack underflow",
  "Pop b, pop a, push a - b."
)

#instruction(
  "mul",
  "a b -> result",
  "Stack underflow",
  "Pop b, pop a, push a * b."
)

#instruction(
  "div",
  "a b -> result",
  "Stack underflow",
  "Pop b, pop a, push a / b."
)

#instruction(
  "rem",
  "a b -> result",
  "Stack underflow",
  "Pop b, pop a, push a mod b."
)

#instruction(
  "neg",
  "v -> result",
  "Stack underflow",
  "Pop v, push -v."
)

Mode selection: if either operand is floating-point (or a string for add),
both are promoted to 64-bit float; otherwise 64-bit signed integer
arithmetic is used.

== Logical

#instruction(
  "not",
  "v -> bool",
  "Stack underflow",
  "Pop v, push the boolean negation: !ToBool(v)."
)

#instruction(
  "and",
  "a b -> result",
  "Stack underflow",
  "Pop b, pop a, push a & b (bitwise AND). Integer operands only."
)

#instruction(
  "xor",
  "a b -> result",
  "Stack underflow",
  "Pop b, pop a, push a ^ b (bitwise XOR). Integer operands only."
)

#instruction(
  "or",
  "a b -> result",
  "Stack underflow",
  "Pop b, pop a, push a | b (bitwise OR). Integer operands only."
)

== Comparison

Each opcode pops b then a and pushes a bool.

#instruction(
  "ceq",
  "a b -> bool",
  "Stack underflow",
  "Push true if a == b, otherwise false."
)

#instruction(
  "cne",
  "a b -> bool",
  "Stack underflow",
  "Push true if a != b, otherwise false."
)

#instruction(
  "cgt",
  "a b -> bool",
  "Stack underflow",
  "Push true if a > b, otherwise false."
)

#instruction(
  "cge",
  "a b -> bool",
  "Stack underflow",
  "Push true if a >= b, otherwise false."
)

#instruction(
  "clt",
  "a b -> bool",
  "Stack underflow",
  "Push true if a < b, otherwise false."
)

#instruction(
  "cle",
  "a b -> bool",
  "Stack underflow",
  "Push true if a <= b, otherwise false."
)

String comparisons use ordinal (byte-by-byte) ordering. Boolean values are
supported for ceq and cne only. All other operand pairs are promoted to
64-bit float before comparison.

== Type Conversion and Testing

#instruction(
  "conv",
  "value -> converted",
  "Conversion failure",
  "Pop a value and push it converted to the named target type according to the coercion rules."
)

#instruction(
  "castclass",
  "obj -> obj",
  "InvalidCastError",
  "Assert that the top-of-stack managed object has the given type name; raise a cast error if it does not."
)

#instruction(
  "isinst",
  "value -> bool",
  "None",
  "Pop a value; push true if it is a managed object with the given type name, otherwise false."
)

== Object and Array Operations

#instruction(
  "newobj",
  "-> obj",
  "TypeNotFoundError",
  "Create a new managed object of the named type and push it. If the type is a registered host type, the runtime should also construct the corresponding host backing object."
)

#instruction(
  "newarr",
  "-> array",
  "None",
  "Push a new, empty array. Optionally with a type argument to create a typed array (e.g. newarr int)."
)

#instruction(
  "ldelem",
  "array index -> value",
  "IndexOutOfRange",
  "Pop an index (0-based integer), pop an array; push array[index]."
)

#instruction(
  "stelem",
  "array index value ->",
  "IndexOutOfRange",
  "Pop a value, pop an index, pop an array; set array[index] = value."
)

== Method Calls

#instruction(
  "call",
  "args... -> [result]",
  "MissingMethodError",
  "Pop arguments left-to-right, then pop the receiver (for instance methods); invoke the target method. Operand: \"Namespace.Type.Method(Args) -> ReturnType\"."
)

#instruction(
  "callvirt",
  "args... obj -> [result]",
  "MissingMethodError, NullReference",
  "Same as call, but dispatch is virtual: the actual type of the receiver determines which override is executed."
)

#instruction(
  "ret",
  "[value] ->",
  "None",
  "Return from the current frame. If a value is present on the evaluation stack it is transferred to the caller's stack."
)

== Control Flow

#instruction(
  "if",
  "(condition-dependent)",
  "None",
  "Evaluate the condition. If truthy, execute the thenBlock; otherwise execute the optional elseBlock."
)

#instruction(
  "while",
  "(condition-dependent)",
  "None",
  "Repeatedly evaluate the condition and execute the body. A break inside the body exits the loop; a continue skips to the next condition evaluation."
)

#instruction(
  "break",
  "---",
  "None",
  "Exit the nearest enclosing while loop."
)

#instruction(
  "continue",
  "---",
  "None",
  "Skip to the next iteration of the nearest enclosing while loop."
)

#instruction(
  "try",
  "---",
  "None",
  "Execute the tryBlock. If an exception is raised, test each catchBlock in order. Execute the optional finallyBlock in all cases."
)

#instruction(
  "throw",
  "value -> (raises exception)",
  "None",
  "Pop a value and raise it as a pending exception on the current frame."
)


= Storage



The abstract machine provides a temporary storage area that is distinct
from the evaluation stack and local variables.

Temporary slots are addressed by index and are intended for transient
intermediate values. They do not form part of the program state and are
not visible outside the currently executing method.