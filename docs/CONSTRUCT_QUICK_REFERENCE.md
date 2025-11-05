# Construct Language - Quick Reference

## Syntax at a Glance

### Contract Declaration
```construct
Contract Name {
    fn function() { }
}
```

### Function Definition
```construct
fn name() { }
fn name(param: Type) -> ReturnType { }
```

### Variables
```construct
var x = 10;
var name = "hello";
var flag = true;
```

### If/Else
```construct
if (condition) {
    // then
} else {
    // else
}
```

### While Loop
```construct
while (condition) {
    // body
}
```

### Return Statement
```construct
return;
return value;
```

### Expressions
```construct
x + y           // Addition
x - y           // Subtraction
x * y           // Multiplication
x / y           // Division
x == y          // Equality
x != y          // Not equal
x < y           // Less than
x <= y          // Less or equal
x > y           // Greater than
x >= y          // Greater or equal
!x              // Logical NOT
-x              // Negation
f()             // Function call
f(a, b)         // With arguments
IO.println(x)   // Member access + call
```

## Types

| Type | Description | Default |
|------|-------------|---------|
| `Int` | 32-bit integer | 0 |
| `String` | UTF-16 string | "" |
| `Bool` | Boolean | false |

## Built-in Functions

- `IO.println(String)` - Print to console

## Compilation

```csharp
// Lexer → Parser → Compiler pipeline
var compiler = new ConstructLanguageCompiler();

// Compile to ObjectIR Module
var module = compiler.CompileSource(sourceCode);

// Export formats
var json = compiler.CompileSourceToJson(sourceCode);
var text = compiler.CompileSourceToText(sourceCode);
```

## Complete Example

```construct
Contract Example {
    fn Add(a: Int, b: Int) -> Int {
        var result = a + b;
        return result;
    }

    fn Loop() {
        var i = 0;
        while (i < 10) {
            var next = i + 1;
            i = next;
        }
    }

    fn Check(n: Int) -> Bool {
        if (n > 0) {
            return true;
        } else {
            return false;
        }
    }
}
```

## Compilation Process

**Input:** Construct source code (`.ct`)
```
Contract Calc {
    fn Add(a: Int, b: Int) -> Int { return a + b; }
}
```

**Output:** ObjectIR Module (JSON/text)
```json
{
  "Name": "Calc",
  "Types": [
    {
      "Name": "Calc",
      "Kind": "Class",
      "Methods": [
        {
          "Name": "Add",
          "ReturnType": { "Name": "int32" },
          "Parameters": [
            { "Name": "a", "Type": { "Name": "int32" } },
            { "Name": "b", "Type": { "Name": "int32" } }
          ]
        }
      ]
    }
  ]
}
```

## Error Examples

### Lexer Error
```
CompileException: Unexpected character '@' at line 1, column 5
```

### Parser Error
```
CompileException: Expected ';' at line 3: got 'fn'
```

## Operator Precedence (Highest to Lowest)

1. Postfix: `f()`, `.member`
2. Unary: `-x`, `!x`
3. Multiplicative: `*`, `/`
4. Additive: `+`, `-`
5. Comparison: `<`, `<=`, `>`, `>=`
6. Equality: `==`, `!=`
7. Assignment: `=`

## Keywords

| Keyword | Purpose |
|---------|---------|
| `Contract` | Declare contract/module |
| `fn` | Declare function |
| `var` | Declare variable |
| `if` | Conditional branch |
| `else` | Else branch |
| `while` | Loop |
| `return` | Return from function |
| `true` | Boolean true |
| `false` | Boolean false |
| `Int` | Integer type |
| `String` | String type |
| `Bool` | Boolean type |
| `IO` | Built-in I/O module |

## Common Patterns

### Factorial (Recursive)
```construct
fn Factorial(n: Int) -> Int {
    if (n < 2) {
        return 1;
    } else {
        return n * Factorial(n - 1);
    }
}
```

### Countdown (Loop)
```construct
fn Countdown(n: Int) {
    var i = n;
    while (i > 0) {
        IO.println(i);
        i = i - 1;
    }
}
```

### Conditional Logic
```construct
fn Max(a: Int, b: Int) -> Int {
    if (a > b) {
        return a;
    } else {
        return b;
    }
}
```

### Computation
```construct
fn Fibonacci(n: Int) -> Int {
    if (n <= 1) {
        return n;
    } else {
        var prev = Fibonacci(n - 1);
        var curr = Fibonacci(n - 2);
        return prev + curr;
    }
}
```

## Testing

```bash
# Run all tests
dotnet test ObjectIR.CSharpTests/

# Run only Construct tests
dotnet test ObjectIR.CSharpTests/ --filter "Construct"

# Run example
dotnet run --project ObjectIR.Examples/
```

## Related Files

- **Compiler Source**: `src/ObjectIR.Core/Compilers/`
- **Examples**: `ObjectIR.Examples/ConstructLanguageExample.cs`
- **Tests**: `ObjectIR.CSharpTests/ConstructLanguageTests.cs`
- **Full Docs**: `docs/CONSTRUCT_COMPILER.md`

## Status

✅ Lexer: Complete
✅ Parser: Complete
✅ Compiler: Complete
✅ Tests: 28 comprehensive tests (all passing)
✅ Integration: Seamlessly integrated with ObjectIR

## Next Steps

1. Start with simple programs (see examples)
2. Compile to ObjectIR modules
3. Export to JSON or text format
4. Analyze with ObjectIR tools
5. Extend with custom analysis

---

**Learn more:** See `CONSTRUCT_COMPILER_README.md` for full documentation
