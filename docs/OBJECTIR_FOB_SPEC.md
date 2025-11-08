# ObjectIR FOB (Finite Open Bytecode) Specification

This document defines the **ObjectIR** fork of the FOB (Finite Open Bytecode) format, specifically designed for serializing ObjectIR intermediate representation modules in a compact binary format.

## Fork Declaration

The ObjectIR format is declared as: `OBJECTIR,FOB`

This means it follows the base FOB specification with ObjectIR-specific extensions and section definitions.

## Overview

ObjectIR FOB files contain complete module definitions including types (classes and interfaces), methods, fields, and instructions. The format is designed for efficient loading and execution by the ObjectIR C++ runtime.

### Debugging Support

ObjectIR provides comprehensive debugging metadata including:

- **Source location mapping**: Line and column information for every instruction
- **Symbol information**: Function, class, method, field, and variable symbols
- **Source file tracking**: File paths, modification times, and content hashes
- **Instruction-level debugging**: Precise mapping from bytecode to source code

This enables full debugging support with features like breakpoints, stepping, variable inspection, and stack traces.

## Sections

ObjectIR defines the following standard sections:

### .types

Contains serialized type definitions (classes and interfaces).

**Format:**

```text
(type_count: number)
[type_definition] * type_count
```

Where each `type_definition` is:

```text
(kind: byte)  // 0x01 = Class, 0x02 = Interface
(name_index: number)  // Index into .strings section
(namespace_index: number)  // Index into .strings section
(access: byte)  // 0x01 = Public, 0x02 = Private, 0x03 = Protected, 0x04 = Internal
(flags: byte)  // Bit flags: 0x01 = Abstract, 0x02 = Sealed, 0x04 = Static
(base_type_index: number)  // Index into .types section, or 0xFFFFFFFF for none
(interface_count: number)
[interface_index: number] * interface_count  // Indexes into .types section
(field_count: number)
[field_definition] * field_count
(method_count: number)
[method_definition] * method_count
```

Field definition:

```text
(name_index: number)
(type_index: number)  // Index into .types section
(access: byte)
(flags: byte)  // 0x01 = Static, 0x02 = ReadOnly
```

Method definition:

```text
(name_index: number)
(return_type_index: number)
(access: byte)
(flags: byte)  // Bit flags: 0x01 = Static, 0x02 = Virtual, 0x04 = Override, 0x08 = Abstract
(parameter_count: number)
[parameter_definition] * parameter_count
(local_count: number)
[local_definition] * local_count
(instruction_count: number)
[instruction_offset: number] * instruction_count  // Offsets into .code section
```

Parameter/Local definition:

```text
(name_index: number)
(type_index: number)
```

### .strings

String table containing all string literals used in the module.

**Format:**

```text
(string_count: number)
[string_entry] * string_count
```

String entry:

```text
(length: number)
[utf8_bytes] * length
```

### .code

Contains the actual instruction bytecode for all methods.

**Format:**

```text
(instruction_count: number)
[instruction] * instruction_count
```

Each instruction:

```text
(opcode: byte)
(operand_count: byte)
[operand] * operand_count
```

Operands can be:

- String index (for ldstr, etc.): `0x01 (string_index: number)`
- Integer: `0x02 (value: number)`
- Double: `0x03 (value: double as bytes)`
- Type index: `0x04 (type_index: number)`
- Method index: `0x05 (method_index: number)`  // Encoded as type_index.method_index
- Field index: `0x06 (field_index: number)`  // Encoded as type_index.field_index

### .constants

Constant pool for literal values.

**Format:**

```text
(constant_count: number)
[constant] * constant_count
```

Constant:

```text
(type: byte)  // 0x01 = Int32, 0x02 = Int64, 0x03 = Float, 0x04 = Double, 0x05 = String, 0x06 = Bool, 0x07 = Null
[value: variable]  // Based on type
```

### .symbols (Optional)

Debug symbol information including source location mapping.

**Format:** Extends the base FOB .symbols section format with ObjectIR-specific debug information:

```text
(symbol_count: number)
[symbol_entry] * symbol_count
```

Each symbol entry:

```text
(type: byte)
(name_index: number)  // Index into .strings section
(address: number)     // Memory address or offset
(size: number)        // Size in bytes (0 for points)
(source_file_index: number)  // Index into .strings section
(source_line: number)        // Line number in source
(source_column: number)      // Column number in source
```

Symbol types:

- 0x00: Function
- 0x01: Class
- 0x02: Interface
- 0x03: Field
- 0x04: Method
- 0x05: Local Variable
- 0x06: Parameter
- 0x07: Instruction (for instruction-level debugging)

### .source_files (Optional)

Source file information for debugging.

**Format:**

```text
(file_count: number)
[source_file_entry] * file_count
```

Source file entry:

```text
(path_index: number)     // Index into .strings section
(content_hash: number)   // Hash of file contents for change detection
(modified_time: number)  // File modification timestamp
```

## Entry Point

The entry point (from FOB header) points to the main method's index in the .types section.

## Example

A simple "Hello World" module would contain:

1. **Header:** `FOB OBJECTIR\0` + file size + entry point
2. **.strings:** "HelloWorld", "Program", "Main", "System.Console", "WriteLine", "Hello, World!"
3. **.types:** Program class with Main method
4. **.code:** Instructions for Main method (ldstr "Hello, World!", call Console.WriteLine)

## Implementation Notes

- All multi-byte values use little-endian byte order
- String indexes are 0-based into the .strings section
- Type indexes are 0-based into the .types section
- Method/field indexes are encoded as `(type_index << 16) | member_index`
- The format is designed to be streamable and memory-efficient for runtime loading
