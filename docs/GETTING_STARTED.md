# Getting Started with ObjectIR

This guide will walk you through creating your first ObjectIR module and understanding the core concepts.

## Installation

### Using NuGet (when published)
```bash
dotnet add package ObjectIR.Core
```

### Building from Source
```bash
git clone https://github.com/yourusername/ObjectIR.git
cd ObjectIR
dotnet build
```

## Core Concepts

### 1. Modules

A **Module** is the top-level container for your IR. It contains type definitions and can have metadata like version information.

```csharp
var module = new Module("MyApp");
module.Version = new Version(1, 0, 0);
```

### 2. Types

ObjectIR supports several kinds of types:

- **Classes**: Reference types with fields, methods, inheritance
- **Interfaces**: Contract definitions
- **Structs**: Value types
- **Enums**: Named integer constants

### 3. Type References

`TypeReference` represents a reference to a type:

```csharp
// Primitive types
TypeReference.Int32
TypeReference.String
TypeReference.Bool

// Generic types
TypeReference.List(TypeReference.Int32)
TypeReference.Dict(TypeReference.String, TypeReference.Int32)

// Custom types
TypeReference.FromName("MyNamespace.MyClass")
```

### 4. Instructions

Instructions are the building blocks of methods. ObjectIR uses a hybrid approach:

**Stack-based instructions** for operations:
```
ldarg a      // Load argument 'a' onto stack
ldarg b      // Load argument 'b' onto stack
add          // Pop two values, add them, push result
stloc result // Store top of stack to local 'result'
```

**Structured control flow** for branching:
```csharp
if (condition) {
    // then block
} else {
    // else block
}

while (condition) {
    // loop body
}
```

## Your First Module: Hello World

Let's create a simple program that prints "Hello, World!":

```csharp
using ObjectIR.Core.Builder;
using ObjectIR.Core.IR;

// Create a module
var builder = new IRBuilder("HelloWorld");

// Define a class with a Main method
builder.Class("Program")
    .Namespace("HelloWorld")
    .Method("Main", TypeReference.Void)
        .Static()
        .Body()
            // Load the string constant
            .Ldstr("Hello, World!")
            
            // Call Console.WriteLine
            .Call(new MethodReference(
                TypeReference.FromName("System.Console"),
                "WriteLine",
                TypeReference.Void,
                new List<TypeReference> { TypeReference.String }))
            
            .Ret()
        .EndBody()
        .EndMethod()
    .EndClass();

var module = builder.Build();
```

This generates IR equivalent to:

```
module HelloWorld

class HelloWorld.Program {
    static method Main() -> void {
        ldstr "Hello, World!"
        call System.Console.WriteLine(string) -> void
        ret
    }
}
```

## Example: A Simple Counter Class

Let's build something more complex - a counter with increment and decrement:

```csharp
using ObjectIR.Core.Builder;
using ObjectIR.Core.IR;

var builder = new IRBuilder("CounterApp");

var counterType = TypeReference.FromName("CounterApp.Counter");

builder.Class("Counter")
    .Namespace("CounterApp")
    
    // Private field to store the value
    .Field("value", TypeReference.Int32)
        .Access(AccessModifier.Private)
        .EndField()
    
    // Constructor initializes value to 0
    .Constructor()
        .Body()
            .LdcI4(0)
            .Stfld(new FieldReference(counterType, "value", TypeReference.Int32))
            .Ret()
        .EndBody()
        .EndMethod()
    
    // Increment method
    .Method("Increment", TypeReference.Void)
        .Body()
            // this.value = this.value + 1
            .Ldarg("this")
            .Dup()  // Duplicate 'this' for both load and store
            .Ldfld(new FieldReference(counterType, "value", TypeReference.Int32))
            .LdcI4(1)
            .Add()
            .Stfld(new FieldReference(counterType, "value", TypeReference.Int32))
            .Ret()
        .EndBody()
        .EndMethod()
    
    // Get current value
    .Method("GetValue", TypeReference.Int32)
        .Body()
            .Ldarg("this")
            .Ldfld(new FieldReference(counterType, "value", TypeReference.Int32))
            .Ret()
        .EndBody()
        .EndMethod()
    
    .EndClass();

var module = builder.Build();
```

Generated IR:

```
module CounterApp

class CounterApp.Counter {
    private field value: int32
    
    constructor() {
        ldc.i4 0
        stfld CounterApp.Counter.value
        ret
    }
    
    method Increment() -> void {
        ldarg this
        dup
        ldfld CounterApp.Counter.value
        ldc.i4 1
        add
        stfld CounterApp.Counter.value
        ret
    }
    
    method GetValue() -> int32 {
        ldarg this
        ldfld CounterApp.Counter.value
        ret
    }
}
```

## Working with Generics

ObjectIR fully supports generics. Here's a generic Stack<T>:

```csharp
var builder = new IRBuilder("Collections");

builder.Class("Stack")
    .Namespace("Collections")
    .Generic("T")  // Define generic parameter
    
    .Field("items", TypeReference.List(TypeReference.FromName("T")))
        .Access(AccessModifier.Private)
        .EndField()
    
    .Constructor()
        .Body()
            // Initialize items with new List<T>
            .Newobj(TypeReference.List(TypeReference.FromName("T")))
            .Stfld(new FieldReference(
                TypeReference.FromName("Collections.Stack").MakeGenericType(TypeReference.FromName("T")),
                "items",
                TypeReference.List(TypeReference.FromName("T"))))
            .Ret()
        .EndBody()
        .EndMethod()
    
    .Method("Push", TypeReference.Void)
        .Parameter("item", TypeReference.FromName("T"))
        .Body()
            // items.Add(item)
            .Ldarg("this")
            .Ldfld(new FieldReference(
                TypeReference.FromName("Collections.Stack<T>"),
                "items",
                TypeReference.List(TypeReference.FromName("T"))))
            .Ldarg("item")
            .Callvirt(new MethodReference(
                TypeReference.List(TypeReference.FromName("T")),
                "Add",
                TypeReference.Void,
                new List<TypeReference> { TypeReference.FromName("T") }))
            .Ret()
        .EndBody()
        .EndMethod()
    
    .EndClass();
```

## Control Flow

### If Statements

```csharp
.Method("IsPositive", TypeReference.Bool)
    .Parameter("x", TypeReference.Int32)
    .Body()
        // Load x and 0
        .Ldarg("x")
        .LdcI4(0)
        .Cgt()  // Compare: x > 0
        
        // If true, return true, else return false
        .If(Condition.Stack(),
            then => then.LdcI4(1).Ret(),
            elze => elze.LdcI4(0).Ret())
    .EndBody()
```

### While Loops

```csharp
.Method("CountToN", TypeReference.Void)
    .Parameter("n", TypeReference.Int32)
    .Local("i", TypeReference.Int32)
    .Body()
        // i = 0
        .LdcI4(0)
        .Stloc("i")
        
        // while (i < n)
        .While(Condition.Binary(ComparisonOp.Less),
            body => body
                // Print i
                .Ldloc("i")
                .Call(new MethodReference(
                    TypeReference.FromName("System.Console"),
                    "WriteLine",
                    TypeReference.Void,
                    new List<TypeReference> { TypeReference.Int32 }))
                
                // i++
                .Ldloc("i")
                .LdcI4(1)
                .Add()
                .Stloc("i"))
    .EndBody()
```

## Standard Library Types

ObjectIR provides standard generic types that backends must implement:

### List<T>
```csharp
var listType = TypeReference.List(TypeReference.Int32);
var addMethod = new MethodReference(
    listType,
    "Add",
    TypeReference.Void,
    new List<TypeReference> { TypeReference.Int32 });
```

### Dict<K, V>
```csharp
var dictType = TypeReference.Dict(TypeReference.String, TypeReference.Int32);
var containsKeyMethod = new MethodReference(
    dictType,
    "ContainsKey",
    TypeReference.Bool,
    new List<TypeReference> { TypeReference.String });
```

## Next Steps

- Read the [Architecture Guide](ARCHITECTURE.md) to understand the design
- Check out [Backend Development](BACKEND_DEVELOPMENT.md) to create your own backend
- Explore [Examples](../examples/) for more complex scenarios
- See the [API Reference](API_REFERENCE.md) for detailed documentation

## Common Patterns

### Factory Pattern

```csharp
.Method("Create", counterType)
    .Static()
    .Body()
        .Newobj(counterType)
        .Ret()
    .EndBody()
```

### Property Getters/Setters

```csharp
.Method("get_Value", TypeReference.Int32)
    .Body()
        .Ldarg("this")
        .Ldfld(valueField)
        .Ret()
    .EndBody()
    .EndMethod()

.Method("set_Value", TypeReference.Void)
    .Parameter("value", TypeReference.Int32)
    .Body()
        .Ldarg("this")
        .Ldarg("value")
        .Stfld(valueField)
        .Ret()
    .EndBody()
```

### Exception Handling

```csharp
var tryInst = new TryInstruction();

// Try block
tryInst.TryBlock.EmitCall(riskyMethod);

// Catch block
tryInst.CatchClauses.Add(new CatchClause(
    TypeReference.FromName("System.Exception"),
    "ex")
{
    Body = new InstructionList()
});

tryInst.CatchClauses[0].Body.EmitLoadLocal("ex");
// Handle exception...

// Finally block
tryInst.FinallyBlock = new InstructionList();
tryInst.FinallyBlock.EmitCall(cleanupMethod);
```

## Tips and Best Practices

1. **Use the Builder API** - It's easier than constructing IR manually
2. **Type safety** - Always ensure stack types match operation requirements
3. **Field references** - Create them once and reuse
4. **Keep methods small** - Break complex logic into smaller methods
5. **Document your IR** - Use metadata to attach comments and documentation
6. **Test with multiple backends** - Ensure your IR works across targets

## Troubleshooting

### Stack Imbalance
```
Error: Stack depth mismatch at merge point
```
**Solution**: Ensure all code paths leave the same number of items on the stack

### Type Mismatch
```
Error: Cannot add float32 and int32
```
**Solution**: Use conversion instructions (`conv.r4`, `conv.i4`) to convert types

### Missing Field/Method
```
Error: Field 'X' not found on type 'Y'
```
**Solution**: Ensure the field is defined and the reference matches exactly

## Resources

- [GitHub Repository](https://github.com/yourusername/ObjectIR)
- [API Documentation](API_REFERENCE.md)
- [Grammar Specification](GRAMMAR.md)
- [Example Programs](../examples/)
