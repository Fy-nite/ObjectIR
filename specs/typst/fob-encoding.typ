// )
#import "@preview/xyznote:0.5.0": *
#show: xyznote.with(
  title: "ObjectIR — FOB/IR v3 Encoding Reference",
  author: "charlie santana - Finite",
  abstract: "Binary opcode map and encoding scheme for the FOB/IR v3 format.",
  createtime: "2026-07-29",
  lang: "au",
)

#set heading(numbering: "1.")

= FOB/IR v3 Binary Encoding

This document defines the binary encoding of instructions in the FOB/IR v3
format — the compact binary representation of ObjectIR modules. It is a
companion to the main ObjectIR V3 specification and describes only the
encoding layer, not instruction semantics.

= File Layout

A FOB/IR v3 file consists of four sequential sections:

#figure(
  align(center)[#table(
    columns: (20%, 80%),
    align: (left, left),
    table.header([#strong[Section]], [#strong[Contents]]),
    table.hline(),
    [Header], [
      4-byte magic `0x464F4249` (`FOBI`), 1-byte format version
      (`0x03`), module name (length-prefixed UTF-8), version triple
      (three `uint16`).
    ],
    [String pool], [
      A length-prefixed table of interned UTF-8 strings referenced by
      index throughout the rest of the file.
    ],
    [Type table], [
      Sequential type records: kind byte, name index, namespace index,
      base type index, interface index list, field count + field records,
      method count + method records.
    ],
    [Method bodies], [
      Per-method: parameter count + parameter records, local count +
      local records, instruction count + instruction records.
    ],
  )],
  kind: table,
  caption: [FOB/IR v3 file sections],
)

= Opcode Encoding Scheme

Opcodes use a variable-length prefix encoding that allows existing opcodes
to remain stable while new opcodes can be added without renumbering. The
scheme is analogous to UTF-8: the leading byte encodes the total length in
its high bits, and continuation bytes carry the remaining payload.

#figure(
  align(center)[#table(
    columns: 4,
    align: (left, left, left, left),
    table.header(
      [#strong[Range]], [#strong[Byte 1]], [#strong[Bytes 2--4]],
      [#strong[Available]],
    ),
    table.hline(),
    [`0x00`--`0x7F`],  [`0xxxxxxx`], [(none)], [128],
    [`0xC0`--`0xDF`],  [`110xxxxx`], [`10xxxxxx`], [1,024],
    [`0xE0`--`0xEF`],  [`1110xxxx`], [`10xxxxxx 10xxxxxx`], [16,384],
    [`0xF0`--`0xF7`],  [`11110xxx`], [`10xxxxxx 10xxxxxx 10xxxxxx`], [131,072],
  )],
  kind: table,
  caption: [Variable-length opcode encoding structure],
)

Bytes in the range `0x80`--`0xBF` are reserved as continuation bytes and
are never valid as the start of an opcode.

All instructions currently use single-byte opcodes in the `0x00`--`0x7F`
range. Future extensions *must* use multi-byte sequences (`0xC0`+ prefix)
so that existing assignments remain unchanged.

= Opcode Map

Each instruction is encoded as a 1-byte opcode followed by a variable-length
operand (the format of which is instruction-specific).

#figure(
  align(center)[#table(
    columns: (10%, 25%, 10%, 25%),
    align: (left, left, left, left),
    table.header(
      [#strong[Byte]], [#strong[Mnemonic]], [#strong[Byte]], [#strong[Mnemonic]],
    ),
    table.hline(),
    [`0x00`], [`nop`],       [`0x10`], [`ldsfld`],
    [`0x01`], [`ldc`],       [`0x11`], [`stsfld`],
    [`0x02`], [`ldstr`],     [`0x12`], [`newobj`],
    [`0x03`], [`ldarg`],     [`0x13`], [`newarr`],
    [`0x04`], [`starg`],     [`0x14`], [`ldelem`],
    [`0x05`], [`ldloc`],     [`0x15`], [`stelem`],
    [`0x06`], [`stloc`],     [`0x16`], [`call`],
    [`0x07`], [`add`],       [`0x17`], [`callvirt`],
    [`0x08`], [`sub`],       [`0x18`], [`ret`],
    [`0x09`], [`mul`],       [`0x19`], [`if`],
    [`0x0A`], [`div`],       [`0x1A`], [`while`],
    [`0x0B`], [`rem`],       [`0x1B`], [`break`],
    [`0x0C`], [`neg`],       [`0x1C`], [`continue`],
    [`0x0D`], [`ceq`],       [`0x1D`], [`try`],
    [`0x0E`], [`cne`],       [`0x1E`], [`throw`],
    [`0x0F`], [`ldfld`],     [`0x1F`], [`conv`],
    [`0x20`], [`castclass`], [`0x21`], [`isinst`],
    [`0x22`], [`dup`],       [`0x23`], [`pop`],
    [`0x24`], [`ldnull`],    [`0x25`], [`not`],
    [`0x26`], [`cgt`],       [`0x27`], [`cge`],
    [`0x28`], [`clt`],       [`0x29`], [`cle`],
    [`0x2A`], [`stfld`],     [`0x2B`], [`ldc.i4`],
    [`0x2C`], [`ldc.i8`],    [`0x2D`], [`ldc.r4`],
    [`0x2E`], [`ldc.r8`],    [`0x2F`], [`and`],
    [`0x30`], [`xor`],       [`0x31`], [`or`],
    [`0x32`--`0x7F`], [*reserved*],  [],
  )],
  kind: table,
  caption: [FOB/IR v3 binary opcode map],
)

= String Pool Format

The string pool is a contiguous block of concatenated, length-prefixed
UTF-8 strings. Each entry:

#figure(
  align(center)[#table(
    columns: (30%, 70%),
    align: (left, left),
    table.header([#strong[Field]], [#strong[Description]]),
    table.hline(),
    [`length (uint16)`], [Byte count of the string data (0--65,535)],
    [`data (bytes)`],    [UTF-8 encoded string content],
  )],
  kind: table,
  caption: [String pool entry format],
)

Strings are referenced by their 0-based index in declaration order.

= Type Record Format

Each type in the type table is encoded as:

#figure(
  align(center)[#table(
    columns: (30%, 70%),
    align: (left, left),
    table.header([#strong[Field]], [#strong[Description]]),
    table.hline(),
    [`kind (byte)`],          [`0x01` = Class, `0x02` = Interface, `0x03` = Struct, `0x04` = Enum],
    [`name_index (uint16)`],  [Index of type name in string pool],
    [`namespace_index (uint16)`], [Index of namespace in string pool],
    [`access (byte)`],        [`0x01` = Public, `0x02` = Private, `0x03` = Protected, `0x04` = Internal],
    [`flags (byte)`],         [Bit flags: `0x01` = Abstract, `0x02` = Sealed],
    [`base_type_index (int32)`], [Index into type table, or `-1` for none],
    [`interface_count (uint16)`], [Number of implemented interfaces],
    [`interface_indices`],    [Array of `uint16` indexes into type table],
    [`field_count (uint16)`], [Number of fields],
    [`field_records`],        [Array of field records (see below)],
    [`method_count (uint16)`], [Number of methods],
    [`method_records`],       [Array of method records (see below)],
  )],
  kind: table,
  caption: [Type record format],
)

= Method Record Format

#figure(
  align(center)[#table(
    columns: (30%, 70%),
    align: (left, left),
    table.header([#strong[Field]], [#strong[Description]]),
    table.hline(),
    [`name_index (uint16)`],     [Index of method name in string pool],
    [`signature_index (uint16)`], [Index of full signature string in string pool],
    [`access (byte)`],           [Access modifier flags],
    [`flags (byte)`],            [`0x01` = Static, `0x02` = Virtual, `0x04` = Override, `0x08` = Abstract],
    [`param_count (uint16)`],    [Number of parameters],
    [`param_records`],           [Array of `(name_index, type_index)` pairs],
    [`local_count (uint16)`],    [Number of local variables],
    [`local_records`],           [Array of `(name_index, type_index)` pairs],
    [`instr_count (uint32)`],    [Number of instructions in the body],
    [`instruction_data`],        [Serialised instruction stream (opcode + operands)],
  )],
  kind: table,
  caption: [Method record format],
)

= Instruction Operand Encoding

Each instruction begins with its opcode byte (or multi-byte sequence for
extended opcodes). The operand encoding that follows depends on the
instruction:

#figure(
  align(center)[#table(
    columns: (20%, 40%, 40%),
    align: (left, left, left),
    table.header(
      [#strong[Instruction group]], [#strong[Operand encoding]], [#strong[Notes]],
    ),
    table.hline(),
    [`ldc.i4`, `ldc.i8`, `ldc.r4`, `ldc.r8`], [Immediate value (fixed-width)], [i4=4 bytes, i8=8 bytes, r4=4 bytes, r8=8 bytes],
    [`ldstr`], [Length-prefixed UTF-8 string], [`uint16` length + data],
    [`ldarg`, `starg`, `ldloc`, `stloc`], [`uint16` index], [0-based index into arg/local table],
    [`ldfld`, `stfld`, `ldsfld`, `stsfld`], [`uint16` field name string index], [Index into string pool],
    [`call`, `callvirt`], [`uint16` signature string index], [Index into string pool],
    [`newobj`], [`uint16` type name string index], [Index into string pool],
    [`newarr`], [`uint16` element type string index], [`0xFFFF` = untyped],
    [`if`, `while`], [Structured operand block], [See condition operand layout below],
    [`try`], [Structured operand block], [See exception handler layout below],
    [`conv`, `castclass`, `isinst`], [`uint16` type name string index], [Index into string pool],
    [All others], [No operand], [],
  )],
  kind: table,
  caption: [Instruction operand encoding by group],
)

== Condition Operand Layout

For `if` and `while`, the operand is a structured block:

```
kind: byte          // 0x00=stack, 0x01=binary, 0x02=expression, 0x03=block
// For binary kind:
comparison: byte    // opcode of comparison instruction
// For expression/block kind:
instr_count: uint32
instruction_data    // embedded instruction stream
```

== Exception Handler Operand Layout

For `try`, the operand is structured as:

```
try_block_len: uint32
try_block:         // embedded instruction stream
catch_count: uint16
catch_records:     // array of { type_index: uint16, body_len: uint32, body }
has_finally: uint8 // 0x00 or 0x01
finally_block_len: uint32  // present only if has_finally == 0x01
finally_block:              // present only if has_finally == 0x01
```
