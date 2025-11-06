# BSON Serialization Guide

## Overview

ObjectIR now supports BSON (Binary JSON) serialization for module files, providing **30-40% file size reduction** compared to standard JSON format while maintaining full round-trip fidelity.

## What is BSON?

**BSON** (Binary JSON) is a binary-encoded serialization of JSON-like documents. It offers:

- **Smaller file sizes**: Binary encoding is more compact than text JSON
- **Faster parsing**: Binary format is faster to deserialize
- **Full compatibility**: Can be converted back to JSON without loss of information
- **Type safety**: Preserves type information (integers, strings, arrays, etc.)

## Features

✅ Seamless serialization to BSON format
✅ Lossless round-trip conversion (JSON → BSON → JSON)
✅ Integration with existing ModuleLoader API
✅ Extension methods on Module class
✅ File I/O support (.bson file extension)

## Usage

### Basic Serialization

```csharp
using ObjectIR.Core.Builder;
using ObjectIR.Core.Serialization;

// Create a module
var builder = new IRBuilder("MyModule");
var module = builder.Build();

// Get BSON as byte array
byte[] bsonData = module.DumpBson();

// Load BSON back to module
var loader = new ModuleLoader();
var loadedModule = loader.LoadFromBson(bsonData);
```

### File Operations

```csharp
var loader = new ModuleLoader();

// Save module as BSON file
loader.SaveToBsonFile(module, "mymodule.bson");

// Load module from BSON file
var loaded = loader.LoadFromBsonFile("mymodule.bson");
```

### Extension Methods

```csharp
// Dump to BSON
byte[] bsonData = module.DumpBson();

// Load from BSON (create dummy module first)
var dummy = new IRBuilder("dummy").Build();
var loaded = dummy.LoadFromBson(bsonData);
```

## File Size Comparison

### Example Results

| Format | Size | Reduction |
|--------|------|-----------|
| JSON | 787 bytes | - |
| BSON | 546 bytes | 241 bytes (30.62%) |

Typical reduction for modules with multiple classes and methods: **30-40%**

## API Reference

### ModuleLoader

```csharp
// Load from BSON data
public Module LoadFromBson(byte[] bsonData)

// Load from BSON file
public Module LoadFromBsonFile(string filePath)

// Save to BSON file
public void SaveToBsonFile(Module module, string filePath)
```

### ModuleSerializer

```csharp
// Dump module to BSON
public byte[] DumpToBson()

// Load module from BSON
public static Module LoadFromBson(byte[] bsonData)

// Convert module to BSON
public static byte[] ToBson(Module module)
```

### Extension Methods (ModuleSerializationExtensions)

```csharp
// Dump module to BSON
public static byte[] DumpBson(this Module module)

// Load module from BSON
public static Module LoadFromBson(this Module module, byte[] bsonData)
```

## Round-Trip Fidelity

The BSON serialization ensures perfect round-trip conversion:

```csharp
// Original module
var original = new IRBuilder("Test").Build();
var json1 = original.DumpJson(indented: false);

// Serialize to BSON and back
byte[] bson = original.DumpBson();
var restored = new ModuleLoader().LoadFromBson(bson);
var json2 = restored.DumpJson(indented: false);

// json1 == json2 ✓
Assert.Equal(json1, json2);
```

## Implementation Details

### Binary Format

- Uses MongoDB.Bson library (NuGet: `MongoDB.Bson`)
- Serializes ModuleData and all nested types to BSON documents
- Preserves all type information and metadata
- Handles null values correctly

### Type Mapping

| ObjectIR Type | BSON Type |
|---------------|-----------|
| ModuleData | BsonDocument |
| TypeData | BsonDocument |
| MethodData | BsonDocument |
| FieldData | BsonDocument |
| Arrays | BsonArray |
| Strings | BsonString |
| Numbers | BsonInt32/Int64/Double |
| Objects | BsonDocument |

### Special Handling

- **Null values**: Converted to BsonNull to preserve round-trip fidelity
- **Empty strings**: Distinguished from null (empty string remains empty string)
- **Nested objects**: Recursively serialized to nested BSON documents
- **JsonNode** (instructions): Converted to BSON and back without loss

## Performance Characteristics

### File Size Benefits
- Text JSON: Full Unicode representation of all data
- BSON: Binary encoding, significantly more compact
- Typical reduction: 30-40% depending on module complexity

### Serialization Speed
- Serialization: Similar to JSON (structure is converted once)
- Deserialization: Slightly faster than JSON parsing
- Conversion to/from JSON: Fast (direct element access)

## Examples

### Complete Example

```csharp
using ObjectIR.Core.Builder;
using ObjectIR.Core.Serialization;
using System;
using System.IO;

// Create a module
var builder = new IRBuilder("DataStructures");
builder.Class("Stack")
    .Field("items", TypeReference.List(TypeReference.Int32))
    .Method("Push", TypeReference.Void)
        .Parameter("value", TypeReference.Int32)
        .EndMethod()
    .EndClass();

var module = builder.Build();

// Save as BSON
var loader = new ModuleLoader();
loader.SaveToBsonFile(module, "stack.bson");

// Check file size
var fileInfo = new FileInfo("stack.bson");
Console.WriteLine($"BSON file size: {fileInfo.Length} bytes");

// Load back
var loaded = loader.LoadFromBsonFile("stack.bson");
Console.WriteLine($"Loaded module: {loaded.Name} with {loaded.Types.Count} types");
```

## Migration from JSON

If you have existing JSON files, you can easily convert them:

```csharp
var loader = new ModuleLoader();

// Load from JSON
var module = loader.LoadFromJsonFile("mymodule.json");

// Save as BSON
loader.SaveToBsonFile(module, "mymodule.bson");

// Verify round-trip
var restored = loader.LoadFromBsonFile("mymodule.bson");
var jsonOriginal = module.DumpJson(indented: false);
var jsonRestored = restored.DumpJson(indented: false);
assert(jsonOriginal == jsonRestored);
```

## Advantages and Trade-offs

### Advantages
✅ Significant file size reduction (30-40%)
✅ Binary format is more efficient for storage and transmission
✅ Perfect round-trip compatibility
✅ Type information preserved
✅ Faster deserialization for binary format

### Trade-offs
⚠️ Binary format not human-readable
⚠️ Requires MongoDB.Bson library dependency
⚠️ Text-based tools cannot inspect BSON directly (but can convert to JSON)

## Best Practices

1. **Use BSON for storage**: Store modules as `.bson` files to save disk space
2. **Use JSON for debugging**: Keep JSON format for human inspection and debugging
3. **Convert on demand**: Load BSON, export to JSON if needed for inspection
4. **File extensions**: Use `.bson` extension to clearly indicate binary format
5. **Backup strategy**: BSON files are more compact, making backups smaller

## Troubleshooting

### Round-trip mismatch
If round-trip JSON conversion doesn't match:
- Check for null values vs. empty strings
- Verify no custom serialization logic in types
- Use DebugBsonRoundTrip example to identify differences

### File size larger than JSON
In rare cases with very small modules, BSON might be slightly larger:
- This is normal for small data sets (BSON has overhead)
- Compression algorithms (gzip) work well with both formats
- Savings increase with module size

## Dependencies

- **MongoDB.Bson** (v2.25.0+): Provides BSON serialization support

## See Also

- [SERIALIZATION.md](./SERIALIZATION.md) - General serialization guide
- [MODULE_DUMPING.md](./MODULE_DUMPING.md) - Module format reference
- BsonSerializationExample.cs - Complete working example
