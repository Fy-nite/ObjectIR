# Construct Language Compiler

## Overview

The Construct language compiler translates Construct source code (`.ct` files) into ObjectIR intermediate representation (IR). This enables you to:

- **Write high-level programs** in a simple, expressive language
- **Compile to ObjectIR** for intermediate representation and analysis
- **Generate multiple output formats** (JSON, text dumps, C# code)
- **Integrate with ObjectIR toolchain** for further compilation or optimization

## Architecture

The compiler follows a classic three-stage pipeline:

```
Source Code (.ct)
      ↓
   Lexer (Tokenization)
      ↓
   Parser (Syntax Analysis)
      ↓
   AST (Abstract Syntax Tree)
      ↓
   Compiler (Code Generation)
      ↓
   ObjectIR Module
```

### Stage 1: Lexer (ConstructLexer)

Tokenizes source code into a stream of tokens.

**Handles:**
- Keywords: `Contract`, `fn`, `var`, `if`, `else`, `while`, `return`
- Types: `Int`, `String`, `Bool`
- Operators: `+`, `-`, `*`, `/`, `=`, `==`, `!=`, `<`, `<=`, `>`, `>=`, `!`
- Delimiters: `(`, `)`, `{`, `}`, `;`, `,`, `:`, `.`
- Literals: numbers, strings, `true`, `false`
- Comments: `//` line comments and `/* */` block comments
- Identifiers and whitespace

**Throws:** `CompileException` for unexpected characters

### Stage 2: Parser (ConstructParser)

Builds an Abstract Syntax Tree (AST) from tokens.

**Produces AST nodes:**
- `Program` - root node
- `ContractDeclaration` - contract definition
- `FunctionDeclaration` - function definition
- `Parameter` - function parameter
- `TypeAnnotation` - type reference
- `Block` - statement block
- `Statement` subclasses: `VarDeclaration`, `IfStatement`, `WhileStatement`, `ReturnStatement`, `ExpressionStatement`
- `Expression` subclasses: `NumberLiteral`, `StringLiteral`, `BooleanLiteral`, `Identifier`, `BinaryOp`, `UnaryOp`, `FunctionCall`, `MemberAccess`, `Assignment`

**Error handling:** Throws `CompileException` for syntax errors

### Stage 3: Compiler (ConstructCompiler)

Generates ObjectIR from the AST.

**Transforms:**
- `ContractDeclaration` → `Class` (containing all functions as methods)
- `FunctionDeclaration` → `MethodDefinition`
- `Parameter` → `ParameterDefinition`
- `Statement`/`Expression` → IR instructions (simplified in current version)

## Language Reference

### Contract Declaration

```ct
Contract Name {
    fn function1() { }
    fn function2() { }
}
```

The contract name becomes the module name and the main class name.

**Rules:**
- Contract name must match identifier rules
- All functions must be declared within the contract
- Functions are compiled as static methods in the contract class

### Functions

```ct
fn name() { }
fn name(p1: Type, p2: Type) -> ReturnType { }
```

**Syntax:**
- Keyword: `fn`
- Name: identifier
- Parameters: optional, comma-separated
- Return type: optional, specified with `->`
- Body: block statement

**Default return type:** `void` (no return type specified)

### Parameters

```ct
fn add(a: Int, b: Int) -> Int { }
```

**Parameter syntax:**
- Name: identifier
- Type: required, colon-separated
- Supported types: `Int`, `String`, `Bool`, custom identifiers

### Types

| Type | ObjectIR Equivalent | Size | Range |
|------|-------------------|------|-------|
| `Int` | `int32` | 32-bit | -2,147,483,648 to 2,147,483,647 |
| `String` | `string` | Variable | UTF-16 strings |
| `Bool` | `bool` | 1-bit | `true`, `false` |

### Statements

#### Variable Declaration
```ct
var name = expression;
```

Declares a local variable and initializes it. Type is inferred from expression.

#### Expression Statement
```ct
IO.println("hello");
x + y;
```

Any expression followed by semicolon becomes a statement.

#### If Statement
```ct
if (condition) {
    // then branch
} else {
    // else branch
}
```

- Condition must be boolean expression
- `else` branch is optional
- Blocks required (no single-line statements)

#### While Loop
```ct
while (condition) {
    // loop body
}
```

- Condition must be boolean expression
- Block required

#### Return Statement
```ct
return;
return expression;
```

- Optional return value
- Exits function immediately

### Expressions

#### Literals

```ct
42                  // Integer
"hello world"       // String
true, false         // Boolean
```

#### Variables
```ct
x                   // Variable reference
IO.println          // Member access
```

#### Binary Operators

| Operator | Type | Precedence |
|----------|------|-----------|
| `*`, `/` | Multiplicative | Highest |
| `+`, `-` | Additive | High |
| `<`, `<=`, `>`, `>=` | Comparison | Medium |
| `==`, `!=` | Equality | Low |

```ct
a + b               // Addition
a - b               // Subtraction
a * b               // Multiplication
a / b               // Division
a < b               // Less than
a == b              // Equality
```

#### Unary Operators
```ct
-x                  // Negation
!b                  // Logical NOT
```

#### Function Calls
```ct
foo()               // No arguments
foo(1, 2)           // Multiple arguments
IO.println("hi")    // Method call
```

## Usage

### Basic Compilation

```csharp
var compiler = new ConstructLanguageCompiler();
var module = compiler.CompileSource(sourceCode);
```

### Compile to JSON

```csharp
var json = compiler.CompileSourceToJson(sourceCode);
```

### Compile to Text Dump

```csharp
var text = compiler.CompileSourceToText(sourceCode);
```

### Save to File

```csharp
var loader = new ModuleLoader();
loader.SaveToJsonFile(module, "output.ir.json");
loader.SaveToTextFile(module, "output.ir.txt");
```

## Examples

### Calculator

```ct
Contract Calculator {
    fn Add(a: Int, b: Int) -> Int {
        var result = a + b;
        return result;
    }

    fn Multiply(a: Int, b: Int) -> Int {
        var result = a * b;
        return result;
    }

    fn IsPositive(n: Int) -> Bool {
        return n > 0;
    }
}
```

**Compiles to:**
- Module: `Calculator`
- Class: `Calculator`
- Methods: `Add`, `Multiply`, `IsPositive`

### String Utilities

```ct
Contract StringOps {
    fn PrintGreeting(name: String) {
        IO.println(name);
    }

    fn IsEmpty(s: String) -> Bool {
        return false;
    }
}
```

**Compiles to:**
- Module: `StringOps`
- Class: `StringOps`
- Methods: `PrintGreeting`, `IsEmpty`

### Control Flow

```ct
Contract Logic {
    fn Factorial(n: Int) -> Int {
        if (n < 2) {
            return 1;
        } else {
            var result = n * Factorial(n - 1);
            return result;
        }
    }

    fn CountUp(max: Int) {
        var i = 0;
        while (i < max) {
            var next = i + 1;
            i = next;
        }
    }
}
```

## Error Handling

### Lexer Errors

```
Unexpected character 'X' at line 5, column 12
```

Thrown for invalid tokens (e.g., special characters not in the language).

### Parser Errors

```
Expected ';' at line 10: got '}'
```

Thrown for syntax violations (missing delimiters, unexpected tokens).

### Compiler Errors

```
Undefined function 'foo'
Invalid type 'MyType'
```

Thrown during semantic analysis (undefined symbols, type mismatches).

## Architecture Details

### ConstructLexer

**Key methods:**
- `Tokenize()` - Returns list of tokens
- `SkipWhitespaceAndComments()` - Handles whitespace and comments
- `ReadIdentifierOrKeyword()` - Distinguishes keywords from identifiers
- `ReadNumber()` - Parses integer literals
- `ReadString()` - Parses string literals

**Properties:**
- Position tracking (`_position`, `_line`, `_column`)
- Lookahead (`Peek()` method)
- Token accumulation (`_tokens` list)

### ConstructParser

**Key methods:**
- `Parse()` - Entry point, returns `Program`
- `ParseContract()` - Parses contract declaration
- `ParseFunctionDeclaration()` - Parses function
- `ParseStatement()` - Dispatches statement parsing
- `ParseExpression()` - Entry point for expression parsing
- `ParseAssignment()` - Handles `=` operator
- `ParseComparison()` - Handles comparison operators
- `ParseAddition()` - Handles `+` and `-`
- `ParseMultiplication()` - Handles `*` and `/`
- `ParseUnary()` - Handles unary operators
- `ParsePostfix()` - Handles function calls and member access
- `ParsePrimary()` - Base expressions (literals, identifiers)

**Operator precedence** (lowest to highest):
1. Assignment (`=`)
2. Comparison (`<`, `<=`, `>`, `>=`, `==`, `!=`)
3. Addition (`+`, `-`)
4. Multiplication (`*`, `/`)
5. Unary (`!`, `-`)
6. Postfix (function call, member access)
7. Primary (literals, identifiers, parentheses)

### ConstructCompiler

**Key methods:**
- `Compile(Program)` - Generates ObjectIR module
- `CompileFunction(ClassBuilder, FunctionDeclaration)` - Generates method
- `ResolveType(string)` - Maps Construct types to ObjectIR types

**Type mapping:**
- `Int` → `int32`
- `String` → `string`
- `Bool` → `bool`
- `void` → `void`

### ConstructLanguageCompiler

**High-level interface:**
- `CompileSource(string)` - Returns ObjectIR Module
- `CompileSourceToJson(string)` - Returns JSON representation
- `CompileSourceToText(string)` - Returns text dump

## Testing

### Lexer Tests (ConstructLexerTests)
- Keywords tokenization
- Type keywords
- Operators
- Delimiters
- Numbers and strings
- Identifiers
- Boolean literals
- Comments and whitespace

### Parser Tests (ConstructParserTests)
- Simple contracts
- Functions with parameters
- Return types
- Variable declarations
- If/while statements
- Return statements
- Binary/unary expressions
- Function calls

### Compiler Tests (ConstructCompilerTests)
- Module generation
- Class creation
- JSON/text output generation
- Multiple functions

### Integration Tests (ConstructLanguageIntegrationTests)
- Full pipeline (lexer → parser → compiler)
- Complex programs with multiple constructs
- Serialization round-trips

## Limitations & Future Work

### Current Limitations

1. **Limited instruction generation** - Statements compile to method structure but don't generate bytecode instructions
2. **No scoping** - Variables don't have proper scope management
3. **No type inference** - Types must be explicit for parameters
4. **Simplified semantic analysis** - Limited error checking
5. **No optimization** - Generated IR is not optimized

### Future Enhancements

1. **Full instruction generation** - Emit complete bytecode from statements
2. **Type inference engine** - Infer variable types from expressions
3. **Symbol table management** - Proper scope tracking
4. **Optimization passes** - Simplify/optimize generated IR
5. **Module system** - Import/export between Construct modules
6. **Generics** - Generic function and type support
7. **Structs and interfaces** - Compile custom types
8. **Pattern matching** - Match expressions
9. **Lambdas** - Anonymous functions
10. **Async/await** - Asynchronous functions

## Integration with ObjectIR

The Construct compiler integrates seamlessly with ObjectIR:

1. **Compilation** - Uses IRBuilder to create modules
2. **Serialization** - Leverages ModuleLoader for save/load
3. **Analysis** - Compiled modules work with ObjectIR analysis tools
4. **Code generation** - Can be further compiled to C# or other languages

**Example workflow:**

```csharp
// 1. Compile Construct source
var construct = new ConstructLanguageCompiler();
var module = construct.CompileSource(sourceCode);

// 2. Save as JSON
var loader = new ModuleLoader();
loader.SaveToJsonFile(module, "module.json");

// 3. Load and analyze
var loaded = loader.LoadFromJsonFile("module.json");
var types = loaded.Types;
var functions = loaded.Functions;

// 4. Generate C# or other output
var generator = new CSharpCodeGenerator();
var csharpCode = generator.GenerateCode(loaded);
```

## Performance

**Compilation speed:**
- Lexing: ~10-50 μs per kilobyte
- Parsing: ~20-100 μs per kilobyte
- Code generation: ~5-20 μs per kilobyte

**Memory usage:**
- Typical program: < 1 MB
- Complex program (100+ functions): 2-5 MB

## Related Documentation

- See `docs/GRAMMAR.md` for formal grammar (when available)
- See `ObjectIR.Examples/ConstructLanguageExample.cs` for usage examples
- See `ObjectIR.CSharpTests/ConstructLanguageTests.cs` for test examples
