# OIC Compiler - Type System and I/O Support

## Overview

The OIC (ObjectIR C Compiler) now supports additional C features including `char*` string pointers and built-in I/O function bindings to .NET console APIs.

## Supported Types

### Primitive Types
- `int` - 32-bit signed integer
- `char` - 8-bit signed integer (int8)
- `short` - 16-bit signed integer
- `long` - 64-bit signed integer
- `float` - 32-bit floating point
- `double` - 64-bit floating point
- `void` - No return value
- Modifiers: `unsigned`, `signed`, `const`, `volatile`

### Pointer Types
- `char*` - Pointer to character string
- `int*` - Pointer to integer
- `void*` - Generic pointer

String literals and character literals are fully supported.

## Built-in I/O Functions

The compiler provides automatic bindings for standard C I/O functions to .NET console operations:

### printf - Console Output
Maps C's `printf()` to `Console.WriteLine()` for basic output.

**Supported formats:**
- Simple format strings: `printf("Hello, World!\n")`
- String substitution: `printf("%s\n", stringVar)`
- Multiple arguments: `printf("%s: %d\n", str, num)`

**Example:**
```c
char* name = "Alice";
int age = 30;
printf("%s is %d years old\n", name, age);
// Compiles to: Console.WriteLine(name); Console.WriteLine(age);
```

**Note:** Format specifiers are partially supported. Complex format strings with multiple types are simplified.

### scanf - Console Input
Maps C's `scanf()` to `Console.ReadLine()` for reading input.

**Example:**
```c
char* input = scanf("%s", buffer);
// Compiles to: string input = Console.ReadLine();
```

### puts - Simple String Output
Maps C's `puts()` to `Console.WriteLine()` for string output.

**Example:**
```c
puts("Hello");
// Compiles to: Console.WriteLine("Hello");
```

### strlen - String Length
Maps C's `strlen()` to string length calculation.

**Example:**
```c
int len = strlen("Hello");
// Returns the length of the string
```

## String Literal Support

String literals are fully supported and compile to .NET string objects:

```c
char* greeting = "Hello, World!";
char* format = "%s: %d\n";
```

Character literals are also supported:
```c
char c = 'A';
char newline = '\n';
```

## Type System Mapping

The OIC compiler maps C types to ObjectIR types as follows:

| C Type | ObjectIR Type | Size |
|--------|---------------|------|
| `void` | Void | - |
| `char` | int8 | 1 byte |
| `short` | int16 | 2 bytes |
| `int` | int32 | 4 bytes |
| `long` | int64 | 8 bytes |
| `float` | float32 | 4 bytes |
| `double` | float64 | 8 bytes |
| `char*` | String | variable |
| `int*` | int32* | - |

## Example Programs

### Example 1: String Function
```c
char* greet(const char* name) {
    return "Hello, ";
}

int main() {
    char* message = greet("Alice");
    return 0;
}
```

### Example 2: I/O Operations
```c
int main() {
    char* prompt = "Enter text: ";
    char* input = scanf("%s");
    puts(input);
    return 0;
}
```

### Example 3: Type Mixing
```c
int add(int a, int b) {
    int result = 0;
    result = a + b;
    return result;
}

int multiply(int x, int y) {
    return x * y;
}

int main() {
    int x = 5;
    int y = 3;
    int sum = add(x, y);
    int product = multiply(sum, y);
    return product;
}
```

## Compilation and Output

### Compiling with Type Information

The compiler maintains full type information during compilation:

```bash
# Compile and see type details
dotnet run -- program.c --format json

# Output shows types for all declarations
{
  "Types": [
    {
      "Methods": [
        {
          "Name": "getMessage",
          "ReturnType": "string",  // char* -> string
          "Parameters": []
        }
      ]
    }
  ]
}
```

### Console I/O Binding

When you use I/O functions, they compile to the following .NET calls:

```c
printf("text");
// ↓
MethodReference(
  DeclaringType: System.Console,
  Name: WriteLine,
  ReturnType: Void,
  Parameters: [String]
)
```

## Limitations and Future Work

### Current Limitations
1. Format specifiers in `printf()` are simplified (no full printf formatting)
2. `scanf()` returns strings (simplified version)
3. `strlen()` returns placeholder value (proper implementation requires property access)
4. No support for variable arguments (varargs) syntax
5. Pointers are treated as type qualifiers, not true pointer types

### Future Enhancements
1. Full printf/scanf format specifier support
2. True pointer type system with dereference operations
3. Struct member access through pointers (->)
4. Memory allocation functions (malloc, free)
5. File I/O functions (fopen, fread, fwrite, fclose)
6. String manipulation functions (strcpy, strcmp, strcat)
7. Math functions (sin, cos, sqrt, etc.)

## Integration with ObjectIR

The compiled modules can be:
- Serialized to multiple formats (JSON, BSON, IR Code, etc.)
- Used as input to ObjectIR runtime systems
- Integrated with other ObjectIR compiled languages
- Executed in ObjectIR-compatible environments

## Runtime Considerations

When using the compiled code in a .NET runtime:
1. String types map to `System.String`
2. I/O functions use `System.Console` APIs
3. All arithmetic follows .NET semantics
4. Type conversions are implicit where compatible

## Building and Testing

```bash
cd src/OIC
dotnet build
dotnet run -- example.c --format text
dotnet run -- stdio_example.c --format json
```

## Performance Notes

- String operations compile to .NET string operations
- I/O functions use buffered console I/O
- Method calls are direct (no virtual dispatch for C functions)
- Type information is preserved for optimization opportunities

## See Also

- [CLI_GUIDE.md](CLI_GUIDE.md) - Command-line interface documentation
- [SERIALIZATION_FORMATS.md](../../../docs/SERIALIZATION_FORMATS.md) - Output format details
- ObjectIR Core documentation for IR details
