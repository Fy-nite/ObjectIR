# OIC - C to ObjectIR Compiler

A command-line compiler that translates C source code into ObjectIR modules with support for multiple output formats.

## Installation

```bash
cd src/OIC
dotnet build
```

## Usage

### Basic Usage

```bash
dotnet run -- <input.c> [options]
```

### Arguments

- `<input.c>` - Path to the C source file to compile

### Options

| Option | Short | Description | Default |
|--------|-------|-------------|---------|
| `--format <format>` | `-f` | Output format (see Format Options) | fob |
| `--output <file>` | `-o` | Custom output file path | Auto-generated |
| `--output-dir <dir>` | `-d` | Output directory | `./output` |
| `--quiet` | `-q` | Suppress verbose output | false |
| `--help` | `-h` | Show help message | - |

## Format Options

The compiler supports 10 different output formats:

### Text-Based Formats
- **json** - Structured data format (JSON) - Good for tooling and analysis
- **text** - Human-readable summary - Quick overview of compiled module
- **ir** - Assembly-like IR code - Low-level instruction view
- **csv** - Spreadsheet compatible format - For data analysis
- **md** - Markdown documentation - For creating documentation
- **yaml** - YAML configuration format - For configuration files
- **report** - Module statistics and metrics - Summary report

### Binary Formats
- **bson** - Binary JSON format - Compact binary representation (~50% of JSON size)
- **fob** - Finite Open Bytecode (DEFAULT) - ObjectIR native bytecode format

### Batch
- **all** - Save to all formats at once (except FOB if no entry point)

## Examples

### Compile with default FOB format
```bash
dotnet run -- program.c
```

### Compile to JSON format
```bash
dotnet run -- program.c --format json
```

### Compile with custom output file
```bash
dotnet run -- program.c --format text --output result.txt
```

### Generate all formats in custom directory
```bash
dotnet run -- program.c --format all --output-dir dist/
```

### Compile quietly without verbose output
```bash
dotnet run -- program.c --format json --quiet
```

### Generate documentation in Markdown
```bash
dotnet run -- program.c --format md --output API.md
```

### Create summary report
```bash
dotnet run -- program.c --format report --output report.txt
```

## Output Examples

### JSON Format
Produces structured JSON with complete module metadata:
```json
{
  "Name": "CProgram",
  "Version": "1.0.0",
  "Types": [...],
  "Functions": [...]
}
```

### Text Format
Compact human-readable output:
```
Module: CProgram, Version: 1.0.0
Types (1): Program
Methods: add, multiply, main
```

### Report Format
Formatted statistics and metrics:
```
Module Summary Report

Module Name:        CProgram
Version:            1.0.0

Statistics:
  Total Types:      1
  Total Methods:    3
  Total Instructions: 15
```

## Supported C Features

- Function declarations and definitions
- Variable declarations and assignments
- Type specifiers (int, float, char, void, etc.)
- Control flow statements (if, while, for, return)
- Expressions and operators
- Local variables

## Output Directory Structure

By default, files are saved to the `output/` directory:
```
output/
├── module.json          (JSON format)
├── module.txt           (Text format)
├── module.ir            (IR Code)
├── module.csv           (CSV)
├── module.md            (Markdown)
├── module.yaml          (YAML)
├── module.bson          (Binary)
└── module_report.txt    (Report)
```

## Format Comparison

| Format | Size | Human-Readable | Efficient | Best For |
|--------|------|-----------------|-----------|----------|
| JSON | 4 KB | ✓ | - | Web APIs, tooling |
| Text | 0.3 KB | ✓ | ✓ | Quick overview |
| BSON | 1.9 KB | - | ✓ | Compact storage |
| Report | 0.8 KB | ✓ | ✓ | Summary metrics |
| Markdown | 0.4 KB | ✓ | ✓ | Documentation |
| FOB | - | - | ✓✓ | ObjectIR runtime |

## Error Handling

The compiler provides helpful error messages:

```bash
$ dotnet run -- missing.c
Error: Input file 'missing.c' not found.

$ dotnet run -- program.c --format invalid
Error: Unknown format: invalid
Valid formats are: json, text, ir, bson, fob, csv, md, yaml, report, all
```

## Notes

- FOB format requires a valid entry point. If compilation fails with FOB, try other formats.
- The `--quiet` flag suppresses format confirmation messages but keeps error output.
- Custom output directories are created automatically if they don't exist.
- Text-based formats display to console before saving (unless `--quiet` is used).

## Architecture

The compiler pipeline:
1. **Lexer** - Tokenizes C source code
2. **Parser** - Builds Abstract Syntax Tree (AST)
3. **Compiler** - Converts AST to ObjectIR module
4. **Serializer** - Exports to selected format

## License

Part of the ObjectIR project.
