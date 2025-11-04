# Instruction Serialization Guide

## Overview

**Instruction Serialization** enables converting ObjectIR instructions (the low-level operations in method bodies) to and from JSON format. This allows:

- **Persistence**: Save method bodies to files
- **Transport**: Move code between systems
- **Inspection**: View and analyze instruction sequences
- **Reconstruction**: Recreate instruction lists from JSON
- **Debugging**: Examine exact instruction sequences

## Quick Start

### Serialize Instructions to JSON

```csharp
using ObjectIR.Core.Serialization;

var instructions = new InstructionList();
instructions.Emit(new LoadArgInstruction("x"));
instructions.Emit(new LoadArgInstruction("y"));
instructions.Emit(new ArithmeticInstruction(ArithmeticOp.Add));
instructions.Emit(new ReturnInstruction(null));

var json = InstructionSerializer.SerializeInstructions(instructions);
var jsonString = json.ToString(); // Pretty JSON
```

### Deserialize Instructions from JSON

```csharp
var jsonString = @"[
    { ""opCode"": ""ldarg"", ""operand"": { ""argumentName"": ""x"" } },
    { ""opCode"": ""ldarg"", ""operand"": { ""argumentName"": ""y"" } },
    { ""opCode"": ""add"" },
    { ""opCode"": ""ret"" }
]";

var json = JsonDocument.Parse(jsonString).RootElement;
var instructions = InstructionSerializer.DeserializeInstructions(json);
```

## Supported Instructions

### Load Instructions

**LoadArgInstruction** - Load argument from method parameter
```json
{ "opCode": "ldarg", "operand": { "argumentName": "x" } }
```

**LoadLocalInstruction** - Load local variable
```json
{ "opCode": "ldloc", "operand": { "localName": "result" } }
```

**LoadConstantInstruction** - Load constant value
```json
{ "opCode": "ldc", "operand": { "value": "42", "type": "System.Int32" } }
```

**LoadFieldInstruction** - Load instance field
```json
{ "opCode": "ldfld", "operand": { "field": { "declaringType": "MyClass", "name": "id" } } }
```

**LoadStaticFieldInstruction** - Load static field
```json
{ "opCode": "ldsfld", "operand": { "field": { "declaringType": "MyClass", "name": "counter" } } }
```

**LoadNullInstruction** - Load null reference
```json
{ "opCode": "ldnull" }
```

### Store Instructions

**StoreArgInstruction** - Store to argument
```json
{ "opCode": "starg", "operand": { "argumentName": "x" } }
```

**StoreLocalInstruction** - Store to local variable
```json
{ "opCode": "stloc", "operand": { "localName": "result" } }
```

**StoreFieldInstruction** - Store to instance field
```json
{ "opCode": "stfld", "operand": { "field": { "declaringType": "MyClass", "name": "id" } } }
```

**StoreStaticFieldInstruction** - Store to static field
```json
{ "opCode": "stsfld", "operand": { "field": { "declaringType": "MyClass", "name": "counter" } } }
```

### Arithmetic Instructions

```json
{ "opCode": "add" }   // Addition
{ "opCode": "sub" }   // Subtraction
{ "opCode": "mul" }   // Multiplication
{ "opCode": "div" }   // Division
{ "opCode": "rem" }   // Remainder
```

### Comparison Instructions

```json
{ "opCode": "ceq" }   // Equal
{ "opCode": "cgt" }   // Greater than
{ "opCode": "clt" }   // Less than
```

### Call Instructions

**CallInstruction** - Static/instance method call
```json
{
  "opCode": "call",
  "operand": {
    "method": {
      "declaringType": "Math",
      "name": "Abs",
      "returnType": "System.Int32",
      "parameterTypes": ["System.Int32"]
    }
  }
}
```

**CallVirtualInstruction** - Virtual method call
```json
{
  "opCode": "callvirt",
  "operand": {
    "method": {
      "declaringType": "Animal",
      "name": "Speak",
      "returnType": "System.String",
      "parameterTypes": []
    }
  }
}
```

### Object Instructions

**NewObjectInstruction** - Create new object
```json
{ "opCode": "newobj", "operand": { "type": "MyClass" } }
```

**NewArrayInstruction** - Create array
```json
{ "opCode": "newarr", "operand": { "elementType": "System.Int32" } }
```

**CastInstruction** - Type cast
```json
{ "opCode": "castclass", "operand": { "targetType": "Animal" } }
```

**IsInstanceInstruction** - Type check
```json
{ "opCode": "isinst", "operand": { "targetType": "Dog" } }
```

### Stack Manipulation

```json
{ "opCode": "dup" }     // Duplicate top of stack
{ "opCode": "pop" }     // Remove top of stack
```

### Control Flow

**ReturnInstruction** - Return from method
```json
{ "opCode": "ret" }
```

**BreakInstruction** - Break from loop
```json
{ "opCode": "break" }
```

**ContinueInstruction** - Continue loop
```json
{ "opCode": "continue" }
```

**ConversionInstruction** - Type conversion
```json
{ "opCode": "conv", "operand": { "targetType": "System.Int64" } }
```

## Complete Examples

### Simple Arithmetic Method

```csharp
// Original C# behavior:
// int Add(int a, int b) {
//     int result = a + b;
//     return result;
// }

var instructions = new InstructionList();
instructions.Emit(new LoadArgInstruction("a"));
instructions.Emit(new LoadArgInstruction("b"));
instructions.Emit(new ArithmeticInstruction(ArithmeticOp.Add));
instructions.Emit(new StoreLocalInstruction("result"));
instructions.Emit(new LoadLocalInstruction("result"));
instructions.Emit(new ReturnInstruction(null));

var json = InstructionSerializer.SerializeInstructions(instructions);
```

JSON output:
```json
[
  { "opCode": "ldarg", "operand": { "argumentName": "a" } },
  { "opCode": "ldarg", "operand": { "argumentName": "b" } },
  { "opCode": "add" },
  { "opCode": "stloc", "operand": { "localName": "result" } },
  { "opCode": "ldloc", "operand": { "localName": "result" } },
  { "opCode": "ret" }
]
```

### Polymorphic Call

```csharp
// Original C# behavior:
// string CallSpeak(Animal animal) {
//     return animal.Speak();
// }

var instructions = new InstructionList();
var speakMethod = new MethodReference(
    new TypeReference("Animal"),
    "Speak",
    TypeReference.String,
    new List<TypeReference>()
);

instructions.Emit(new LoadArgInstruction("animal"));
instructions.Emit(new CallVirtualInstruction(speakMethod));
instructions.Emit(new ReturnInstruction(null));
```

## Working with Methods

### Serialize Method Body

```csharp
var method = myClass.DefineMethod("Calculate", TypeReference.Int32);

// Add instructions
method.DefineParameter("x", TypeReference.Int32);
method.DefineLocal("result", TypeReference.Int32);
method.Instructions.Emit(new LoadArgInstruction("x"));
method.Instructions.Emit(new LoadConstantInstruction(10, TypeReference.Int32));
method.Instructions.Emit(new ArithmeticInstruction(ArithmeticOp.Mul));
method.Instructions.Emit(new StoreLocalInstruction("result"));
method.Instructions.Emit(new LoadLocalInstruction("result"));
method.Instructions.Emit(new ReturnInstruction(null));

// Serialize the full module
var serializer = new ModuleSerializer(module);
var json = serializer.DumpToJson();
```

### Round-Trip Preservation

```csharp
// Create instructions
var original = new InstructionList();
original.Emit(new LoadArgInstruction("input"));
original.Emit(new LoadConstantInstruction(5, TypeReference.Int32));
original.Emit(new ArithmeticInstruction(ArithmeticOp.Mul));
original.Emit(new ReturnInstruction(null));

// Serialize to JSON
var json = InstructionSerializer.SerializeInstructions(original);

// Deserialize back
var restored = InstructionSerializer.DeserializeInstructions(json);

// Verify they match
Assert.Equal(original.Count, restored.Count);
for (int i = 0; i < original.Count; i++)
{
    Assert.Equal(original[i].GetType(), restored[i].GetType());
}
```

## Advanced Usage

### Instruction Analysis

```csharp
var json = JsonDocument.Parse(jsonString).RootElement;
var instructions = InstructionSerializer.DeserializeInstructions(json);

// Count instruction types
var counts = instructions
    .GroupBy(i => i.GetType().Name)
    .ToDictionary(g => g.Key, g => g.Count());

foreach (var (type, count) in counts)
{
    Console.WriteLine($"{type}: {count}");
}
```

### Instruction Validation

```csharp
public static void ValidateInstructionSequence(InstructionList instructions)
{
    int stackDepth = 0;
    
    foreach (var instr in instructions)
    {
        switch (instr)
        {
            case LoadArgInstruction or LoadLocalInstruction or 
                 LoadConstantInstruction or LoadNullInstruction:
                stackDepth++;
                break;
            case StoreArgInstruction or StoreLocalInstruction or
                 StoreFieldInstruction or StoreStaticFieldInstruction:
                stackDepth--;
                break;
            case PopInstruction:
                stackDepth--;
                break;
            case DupInstruction:
                stackDepth++;
                break;
            case ReturnInstruction:
                if (stackDepth != 0 && stackDepth != 1)
                    throw new InvalidOperationException("Invalid stack depth at return");
                break;
        }
        
        if (stackDepth < 0)
            throw new InvalidOperationException("Stack underflow");
    }
}
```

## Type System Integration

### Type References in Instructions

Type references use qualified names:

```json
{
  "opCode": "newobj",
  "operand": { "type": "System.Collections.Generic.List<System.Int32>" }
}
```

Built-in types:
- `System.Void`
- `System.Boolean`
- `System.Int32`, `System.Int64`
- `System.Single`, `System.Double`
- `System.String`

Custom types include their namespace:
- `MyNamespace.MyClass`
- `MyNamespace.MyGeneric<T>`

## Performance Notes

- **Serialization**: O(n) where n is number of instructions
- **Deserialization**: O(n) where n is number of instructions
- **JSON size**: Typically 2-3x the size of binary instruction encoding
- **Parsing**: Uses System.Text.Json for efficiency

## Testing

See `ObjectIR.CSharpTests/InstructionSerializationTests.cs` for comprehensive test cases:

```csharp
// Test round-trip preservation
[Fact]
public void RoundTrip_PreservesInstructions()
{
    // Create, serialize, deserialize, verify
}
```

## Error Handling

Common errors when deserializing:

```csharp
try
{
    var instructions = InstructionSerializer.DeserializeInstructions(json);
}
catch (InvalidOperationException ex)
{
    // Missing required property: "opCode"
    // Unsupported opCode value
}
catch (JsonException ex)
{
    // Invalid JSON format
}
```

## Related Topics

- [Module Composition](MODULE_COMPOSITION.md)
- [Module Serialization](../docs/MODULE_DUMPING.md)
- [Builder API](../docs/GETTING_STARTED.md)
- [IR Architecture](../docs/ARCHITECTURE.md)
