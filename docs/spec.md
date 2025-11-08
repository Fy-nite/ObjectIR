# FOB (Finite Open Bytecode)

The FOB format is a basic format for programs that want to be able to output bytecode (asm or a custom format) without inventing a entire new format. FOB is intended to be "forked" where every program would use its own format that follows the format defined in this spec.

Because this spec is very generic, FOB supports "forks" which are the exact same as a different format but with a different name (eg, ASM could be the exact same as GENERIC but the name ASM implies that the data is machine code). For this you would do `(fork name),(base spec)` eg. for ASM it would be `ASM,GENERIC`

# This document
In this document every example will use the fork name `GENERIC`. The "fork" is the name of the specific format being used. (fork name is the name of the fork)

# Types
## Number
In this document a "number" is simply just one byte specifiying the size of the int and then any number of bytes for the actual number (eg. 1 is `0x01 0x01` and 256 is `0x02 0x01 0x00`

# Header

## Magic
Every file has two magics, one which is simpley the text `FOB` and the second which is the name of the fork. Then it ends with a null byte. The name of the fork cannot exceed 255 bytes.

Example:
```
00000000  46 4f 42 07 47 45 4e 45  52 49 43 00              |FOB.GENERIC.|
```

## File Size
Next comes the file size (in bytes after header) as a number

Example:
```
00000000  46 4f 42 07 47 45 4e 45  52 49 43 00 00 00 00 ff  |FOB.GENERIC.....|
```

## Entry point
Next is the entry point into the program, this is stored as just a number

# Sections
The FOB file is split into sections. These sections can be called anything (yet forks can have specific names). The format for these in the file is `(section_name)\0(start_addr)(size)`

`start_addr` and `size` are numbers, start_addr is a offset from 0 in the file, size is how many bytes the section contains, start_addr+size cannot exceed the file size.

For example, Lets say I have 5 sections, `.data`(20B) `.code`(25B) `.exports`(5B) `.text`(40B) and `.copyright`(24B).

The data in the file would contain the following:
```
00000010  2e 64 61 74 61 00 01 31  01 14 2e 63 6f 64 65 00  |.data..1...code.|
00000020  01 45 01 19 2e 65 78 70  6f 72 74 73 00 01 5e 01  |.E...exports..^.|
00000030  05 2e 74 65 78 74 00 01  63 01 28 2e 63 6f 70 79  |..text..c.(.copy|
00000040  72 69 67 68 74 00 01 c2  8b 01 18                 |right......|
```

Then every section starts with `SEC` (`0x53 0x45 0x43`) as a verification

# Other
This section is for other standerds in the base / GENERIC. None of these are required in this spec.

## Sections

> These sections **are not required**. If a parser sees a section called `.import`, it will try to parse it as a import table.

### Data
This section is called `.data` and is for storing data a program may use

### Code
This section is called `.code` and is for storing the bytecode of your program

### Imports
This section is called `.import` and is used for specifying imported files / symbols. This section does have a format which is the following:

```
(type)(nameLength)(name)\0
``` 
This repeats for every symbol and then ends with `ff ff`

Where type is one of the following:
| type | desc |
|------|------|
| 0x01 | File name, name of a file that is imported / linked |
| 0x02 | Symbol, name of a symbol that is used. |

Name is the name of the file/symbol

### Exports
This section is called `.export` and is used for specifying exported files. The format for this section is just a list of numbers of indexes in the Symbol table to be exported.

This repeats for every exported symbol and then ends with `ff ff`

Name is the name of the symbol
Addr/size is a number of where the symbol stuff is (or the value of the symbol)

### Symbols
This section is called `.symbols` and is used for debug symbols. Debug symbols follow a simallar format as the previous 2 sections which is:
```
(type)(nameLength)(name)(addr/value)\0
```
and repeats for every symbol in the file.

Type is:
| Byte | Name |
|------|------|
| 0x00 | Function |
| 0x01 | Class |
| 0x02 | Struct |
| 0x03 | Enums |
| 0x04 | Variables |
