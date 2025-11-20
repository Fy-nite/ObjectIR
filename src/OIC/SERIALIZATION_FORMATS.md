# ObjectIR Module Serialization Formats

The C compiler outputs modules in multiple formats for different use cases. Here's a complete guide:

## Available Formats

### 1. **JSON** (`.json`)
**Use Case**: Data interchange, web APIs, scripting  
**Advantages**: Human-readable, widely supported, easy to parse  
**File Size**: ~4KB (for sample module)

```csharp
// Generate JSON
string json = module.DumpJson();
string jsonCompact = module.DumpJson(indented: false);

// Save to file
File.WriteAllText("module.json", module.DumpJson());
```

**Sample Output**:
```json
{
  "Name": "CProgram",
  "Version": "1.0.0",
  "Types": [
    {
      "Kind": "Class",
      "Name": "Program",
      "Methods": [...]
    }
  ]
}
```

---

### 2. **Text Format** (`.txt`)
**Use Case**: Human-readable display, debugging, documentation  
**Advantages**: Simple hierarchy display, easy to read  
**File Size**: ~0.25KB (for sample module)

```csharp
// Generate text
string text = module.DumpText();

// Save to file
File.WriteAllText("module.txt", module.DumpText());
```

**Sample Output**:
```
Module: CProgram
Version: 1.0.0

Types (1):
  class Program
    Methods:
      Public static int32 add(int32 a, int32 b) [5 instructions]
      Public static int32 multiply(int32 x, int32 y) [5 instructions]
```

---

### 3. **IR Code** (`.ir`)
**Use Case**: Assembly-like representation, low-level analysis  
**Advantages**: Shows instruction sequences, similar to assembly language  
**File Size**: ~0.6KB (for sample module)

```csharp
// Generate IR code
string irCode = module.Serialize().DumpToIRCode();

// Save to file
File.WriteAllText("module.ir", module.Serialize().DumpToIRCode());
```

**Sample Output**:
```
module CProgram version 1.0.0
// Program class
class Program {
    method add(a: int32, b: int32) -> int32 {
        local result: int32
        ldc.i4 0
        pop
        ldarg result
        ret
    }
}
```

---

### 4. **BSON** (`.bson`)
**Use Case**: Binary storage, database serialization, MongoDB compatibility  
**Advantages**: Smaller file size than JSON, binary efficiency  
**File Size**: ~1.9KB (for sample module)

```csharp
// Generate BSON (binary)
byte[] bsonData = module.DumpBson();

// Save to file
File.WriteAllBytes("module.bson", module.DumpBson());

// Load from BSON
Module loaded = ModuleSerializer.LoadFromBson(bsonData);
```

---

### 5. **CSV** (`.csv`)
**Use Case**: Spreadsheet analysis, data extraction, statistics  
**Advantages**: Compatible with Excel and data analysis tools  
**File Size**: ~0.13KB (for sample module)

```csharp
// Generate CSV
string csv = module.DumpCsv();

// Save to file
File.WriteAllText("module.csv", module.DumpCsv());
```

**Use Cases**:
- Import into spreadsheet applications
- Extract method/type statistics
- Create reports and analyses

---

### 6. **Markdown** (`.md`)
**Use Case**: Documentation generation, GitHub wikis, documentation sites  
**Advantages**: Professional documentation format, version control friendly  
**File Size**: ~0.4KB (for sample module)

```csharp
// Generate Markdown
string markdown = module.DumpMarkdown();

// Save to file
File.WriteAllText("module.md", module.DumpMarkdown());
```

**Sample Output**:
```markdown
# CProgram
Version: 1.0.0

## Types
### Class: Program
**Access**: Public

#### Methods
| Name | Return Type | Access | Static |
|------|-------------|--------|--------|
| add | int32 | Public | True |
| multiply | int32 | Public | True |
```

---

### 7. **YAML** (`.yaml`)
**Use Case**: Configuration files, DevOps tools, human-friendly data exchange  
**Advantages**: Very human-readable, good for configs  
**File Size**: ~0.4KB (for sample module)

```csharp
// Generate YAML
string yaml = module.DumpYaml();

// Save to file
File.WriteAllText("module.yaml", module.DumpYaml());
```

**Sample Output**:
```yaml
module: CProgram
version: 1.0.0
types:
  - name: Program
    kind: Class
    access: Public
    methods:
      - name: add
        returnType: int32
        parameters: 2
```

---

### 8. **Summary Report** (`.txt`)
**Use Case**: Code metrics, module overview, analysis  
**Advantages**: High-level statistics and insights  
**File Size**: ~0.8KB (for sample module)

```csharp
// Generate summary report
string report = module.GenerateSummaryReport();

// Save to file
File.WriteAllText("module_report.txt", module.GenerateSummaryReport());
```

**Sample Output**:
```
MODULE SUMMARY REPORT

Module Name:        CProgram
Version:            1.0.0

Statistics:
Total Types:        1
Total Functions:    0
Total Fields:       0
Total Methods:      3
Total Instructions: 15
Avg Instructions/Method: 5.00

Top Methods by Instruction Count:
  Program.add: 5 instructions
  Program.multiply: 5 instructions
  Program.main: 5 instructions
```

---

### 9. **FOB** (`.fob`) - *Binary*
**Use Case**: Runtime execution, optimized storage (requires entry point)  
**Note**: Currently requires a valid entry point to be defined  

```csharp
// Generate FOB (Finite Open Bytecode)
try
{
    byte[] fobData = module.DumpFob();
    File.WriteAllBytes("module.fob", fobData);
}
catch (Exception ex)
{
    // FOB requires entry point - may fail for some modules
}
```

---

## Easy Methods in CLanguageCompiler

The `CLanguageCompiler` class provides convenience methods:

```csharp
var compiler = new CLanguageCompiler();
string sourceCode = "int main() { return 0; }";

// Compile to specific format
string json = compiler.CompileSourceToJson(sourceCode);
string text = compiler.CompileSourceToText(sourceCode);
string irCode = compiler.CompileSourceToIRCode(sourceCode);
byte[] bson = compiler.CompileSourceToBson(sourceCode);
string csv = compiler.CompileSourceToCsv(sourceCode);
string markdown = compiler.CompileSourceToMarkdown(sourceCode);
string yaml = compiler.CompileSourceToYaml(sourceCode);

// Save all formats at once
compiler.SaveToAllFormats(sourceCode, "output_directory");
```

---

## Format Comparison

| Format | Type | Size | Human-Readable | Efficient | Use Case |
|--------|------|------|-----------------|-----------|----------|
| JSON | Text | Large | ✓ | ✗ | Data interchange |
| Text | Text | Tiny | ✓ | ✗ | Quick display |
| IR Code | Text | Small | ✓ | ✗ | Assembly analysis |
| BSON | Binary | Small | ✗ | ✓ | Database storage |
| FOB | Binary | ? | ✗ | ✓ | Runtime execution |
| CSV | Text | Tiny | ✓ | ✗ | Spreadsheets |
| Markdown | Text | Small | ✓ | ✗ | Documentation |
| YAML | Text | Small | ✓ | ✗ | Config/DevOps |
| Report | Text | Small | ✓ | ✗ | Metrics/Analysis |

---

## Usage Examples

### Save to Single Format
```csharp
var compiler = new CLanguageCompiler();
string json = compiler.CompileSourceToJson(sourceCode);
File.WriteAllText("output.json", json);
```

### Save to Multiple Formats
```csharp
var compiler = new CLanguageCompiler();
compiler.SaveToAllFormats(sourceCode, "output");
// Creates: module.json, module.txt, module.ir, module.bson, 
//          module.csv, module.md, module.yaml, module_report.txt
```

### Load from Serialized Format
```csharp
// From JSON
string json = File.ReadAllText("module.json");
Module loaded = ModuleSerializer.LoadFromJson(json);

// From BSON
byte[] bsonData = File.ReadAllBytes("module.bson");
Module loaded = ModuleSerializer.LoadFromBson(bsonData);
```

---

## File Size Reference

For the sample module with 3 methods (add, multiply, main):

- **JSON**: 4,017 bytes
- **BSON**: 1,894 bytes (~47% of JSON)
- **Text**: 266 bytes (~6.6% of JSON)
- **IR Code**: 590 bytes (~14.7% of JSON)
- **CSV**: 134 bytes (~3.3% of JSON)
- **Markdown**: 377 bytes (~9.4% of JSON)
- **YAML**: 382 bytes (~9.5% of JSON)
- **Report**: 852 bytes (~21.2% of JSON)

**Recommendation**: Use JSON for portability, BSON for efficiency, CSV for analysis, and Markdown for documentation.
