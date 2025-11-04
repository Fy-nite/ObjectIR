# ObjectIR Formal Grammar

This document defines the complete formal grammar for ObjectIR in Extended Backus-Naur Form (EBNF).

## Notation

- `::=` means "is defined as"
- `|` means "or"
- `[ ]` means optional (zero or one)
- `{ }` means repetition (zero or more)
- `( )` means grouping
- `" "` means literal text

## Module Structure

```ebnf
Module ::= ModuleDeclaration { TypeDefinition | FunctionDefinition }

ModuleDeclaration ::= "module" Identifier [ "version" Version ]

Version ::= Integer "." Integer "." Integer

Identifier ::= Letter { Letter | Digit | "_" }

QualifiedName ::= Identifier { "." Identifier }
```

## Type Definitions

```ebnf
TypeDefinition ::= ClassDeclaration
                 | InterfaceDeclaration
                 | StructDeclaration
                 | EnumDeclaration

ClassDeclaration ::= 
    [ AccessModifier ] "class" Identifier [ GenericParameters ]
    [ ":" BaseType { "," InterfaceType } ]
    "{" { ClassMember } "}"

InterfaceDeclaration ::=
    [ AccessModifier ] "interface" Identifier [ GenericParameters ]
    [ ":" InterfaceType { "," InterfaceType } ]
    "{" { InterfaceMember } "}"

StructDeclaration ::=
    [ AccessModifier ] "struct" Identifier [ GenericParameters ]
    "{" { StructMember } "}"

EnumDeclaration ::=
    [ AccessModifier ] "enum" Identifier [ ":" IntegerType ]
    "{" EnumMember { "," EnumMember } "}"

AccessModifier ::= "public" | "private" | "protected" | "internal"

GenericParameters ::= "<" GenericParam { "," GenericParam } ">"

GenericParam ::= Identifier [ ":" TypeConstraint ]

TypeConstraint ::= "class" | "struct" | Type
```

## Type System

```ebnf
Type ::= PrimitiveType
       | ReferenceType
       | GenericType
       | ArrayType
       | PointerType

PrimitiveType ::= "void" | "bool"
                | "int8" | "uint8" | "int16" | "uint16"
                | "int32" | "uint32" | "int64" | "uint64"
                | "float32" | "float64"
                | "char" | "string"

ReferenceType ::= QualifiedName

GenericType ::= QualifiedName "<" TypeList ">"

ArrayType ::= Type "[" "]"

PointerType ::= Type "*"

TypeList ::= Type { "," Type }
```

## Members

```ebnf
ClassMember ::= FieldDeclaration
              | MethodDeclaration
              | PropertyDeclaration
              | ConstructorDeclaration

InterfaceMember ::= MethodDeclaration
                  | PropertyDeclaration

StructMember ::= FieldDeclaration
               | MethodDeclaration

FieldDeclaration ::=
    [ AccessModifier ] { "static" | "readonly" }
    "field" Identifier ":" Type

MethodDeclaration ::=
    [ AccessModifier ] { MethodModifier }
    "method" Identifier [ GenericParameters ]
    "(" [ ParameterList ] ")" "->" Type
    [ "implements" QualifiedName ]
    [ MethodBody ]

ConstructorDeclaration ::=
    [ AccessModifier ] "constructor" "(" [ ParameterList ] ")" MethodBody

MethodModifier ::= "static" | "virtual" | "override" | "abstract"

ParameterList ::= Parameter { "," Parameter }

Parameter ::= Identifier ":" Type

PropertyDeclaration ::=
    [ AccessModifier ] "property" Identifier ":" Type
    "{" [ Getter ] [ Setter ] "}"

Getter ::= "get" MethodBody

Setter ::= "set" MethodBody

EnumMember ::= Identifier [ "=" Integer ]
```

## Method Body

```ebnf
MethodBody ::= "{" { LocalDeclaration } { Instruction } "}"

LocalDeclaration ::= "local" Identifier ":" Type

FunctionDefinition ::=
    "function" Identifier [ GenericParameters ]
    "(" [ ParameterList ] ")" "->" Type
    MethodBody
```

## Instructions

### Load Instructions

```ebnf
LoadInstruction ::= "ldarg" Identifier
                  | "ldloc" Identifier
                  | "ldfld" FieldReference
                  | "ldsfld" FieldReference
                  | "ldelem" Type
                  | "ldlen"
                  | "ldnull"
                  | "ldc.i4" Integer
                  | "ldc.i8" Integer
                  | "ldc.r4" Float
                  | "ldc.r8" Float
                  | "ldstr" String
                  | "ldtoken" ( Type | FieldReference | MethodReference )

FieldReference ::= Type "." Identifier

MethodReference ::= Type "." Identifier [ GenericArguments ]
                    "(" [ TypeList ] ")" "->" Type

GenericArguments ::= "<" TypeList ">"
```

### Store Instructions

```ebnf
StoreInstruction ::= "starg" Identifier
                   | "stloc" Identifier
                   | "stfld" FieldReference
                   | "stsfld" FieldReference
                   | "stelem" Type
```

### Arithmetic Instructions

```ebnf
ArithmeticInstruction ::= "add" | "sub" | "mul" | "div" | "rem"
                        | "neg"
                        | "and" | "or" | "xor" | "not"
                        | "shl" | "shr" | "shr.un"
```

### Comparison Instructions

```ebnf
ComparisonInstruction ::= "ceq" | "cgt" | "clt"
                        | "cgt.un" | "clt.un"
```

### Call Instructions

```ebnf
CallInstruction ::= "call" MethodReference
                  | "callvirt" MethodReference
                  | "calli" Signature
                  | "newobj" ConstructorReference

ConstructorReference ::= Type "." "constructor" "(" [ TypeList ] ")"

Signature ::= "(" [ TypeList ] ")" "->" Type
```

### Object Instructions

```ebnf
ObjectInstruction ::= "newobj" Type
                    | "newarr" Type
                    | "castclass" Type
                    | "isinst" Type
                    | "box" Type
                    | "unbox" Type
                    | "sizeof" Type
```

### Stack Manipulation

```ebnf
StackInstruction ::= "dup" | "pop" | "swap"
```

### Conversion Instructions

```ebnf
ConversionInstruction ::= "conv.i4" | "conv.i8" | "conv.r4" | "conv.r8"
                        | "conv.u4" | "conv.u8"
```

### Control Flow Instructions

```ebnf
ControlFlowInstruction ::= IfStatement
                         | WhileStatement
                         | ForStatement
                         | SwitchStatement
                         | "break"
                         | "continue"
                         | "ret" [ Expression ]
                         | TryStatement
                         | "throw"

IfStatement ::= "if" "(" Condition ")" Block
                { "else" "if" "(" Condition ")" Block }
                [ "else" Block ]

WhileStatement ::= "while" "(" Condition ")" Block

ForStatement ::= "for" "(" [ Instruction ] ";" [ Condition ] ";" [ Instruction ] ")" Block

SwitchStatement ::= "switch" "(" Expression ")" "{"
                    { CaseClause }
                    [ DefaultClause ]
                    "}"

CaseClause ::= "case" Constant ":" { Instruction }

DefaultClause ::= "default" ":" { Instruction }

TryStatement ::= "try" Block
                 { CatchClause }
                 [ FinallyClause ]

CatchClause ::= "catch" "(" Type Identifier ")" Block

FinallyClause ::= "finally" Block

Condition ::= Expression | "stack" | ComparisonExpression

ComparisonExpression ::= Expression ComparisonOp Expression

ComparisonOp ::= "==" | "!=" | "<" | "<=" | ">" | ">="

Expression ::= Identifier | "stack"

Block ::= "{" { Instruction } "}"
```

### Label

```ebnf
Label ::= Identifier ":"
```

## Complete Instruction Set

```ebnf
Instruction ::= LoadInstruction
              | StoreInstruction
              | ArithmeticInstruction
              | ComparisonInstruction
              | CallInstruction
              | ObjectInstruction
              | StackInstruction
              | ConversionInstruction
              | ControlFlowInstruction
              | Label
```

## Literals

```ebnf
Integer ::= [ "-" ] Digit { Digit }

Float ::= [ "-" ] Digit { Digit } "." { Digit } [ Exponent ]

Exponent ::= ( "e" | "E" ) [ "+" | "-" ] Digit { Digit }

String ::= "\"" { Character } "\""

Character ::= Any Unicode character except " \ newline
            | "\\" ( "n" | "t" | "r" | "\"" | "\\" )
            | "\\u" HexDigit HexDigit HexDigit HexDigit

Letter ::= "a".."z" | "A".."Z" | "_"

Digit ::= "0".."9"

HexDigit ::= Digit | "a".."f" | "A".."F"

Constant ::= Integer | Float | String | "true" | "false" | "null"
```

## Example

Complete example demonstrating the grammar:

```
module Example version 1.0.0

class Calculator : ICalculator {
    field history: List<int32>
    field count: int32
    
    constructor() {
        newobj List<int32>
        stfld Calculator.history
        ldc.i4 0
        stfld Calculator.count
    }
    
    method Add(a: int32, b: int32) -> int32 {
        local result: int32
        
        ldarg a
        ldarg b
        add
        dup
        stloc result
        
        ldarg this
        ldfld Calculator.history
        ldloc result
        callvirt List<int32>.Add(int32) -> void
        
        ldarg this
        dup
        ldfld Calculator.count
        ldc.i4 1
        add
        stfld Calculator.count
        
        ldloc result
        ret
    }
    
    method GetAverage() -> float32 {
        local sum: int32
        local i: int32
        
        ldarg this
        ldfld Calculator.count
        ldc.i4 0
        ceq
        
        if (stack) {
            ldc.r4 0.0
            ret
        }
        
        ldc.i4 0
        stloc sum
        ldc.i4 0
        stloc i
        
        while (i < count) {
            ldarg this
            ldfld Calculator.history
            ldloc i
            callvirt List<int32>.get_Item(int32) -> int32
            ldloc sum
            add
            stloc sum
            
            ldloc i
            ldc.i4 1
            add
            stloc i
        }
        
        ldloc sum
        conv.r4
        ldarg this
        ldfld Calculator.count
        conv.r4
        div
        ret
    }
}

interface ICalculator {
    method Add(a: int32, b: int32) -> int32
}
```

## Notes

1. **Whitespace**: Whitespace (spaces, tabs, newlines) is generally ignored except as a separator
2. **Comments**: 
   - Single-line: `// comment`
   - Multi-line: `/* comment */`
3. **Case Sensitivity**: Keywords and opcodes are case-insensitive, identifiers are case-sensitive
4. **Semicolons**: Optional after instructions (implementation choice)
5. **Implicit 'this'**: The first parameter of instance methods is implicitly named "this"

## Type Safety Rules

1. Stack must be balanced at control flow merge points
2. Types must match for operations (no implicit conversions except widening)
3. Method calls must match signature exactly
4. Fields and locals must be initialized before use
5. Generic type arguments must satisfy constraints
