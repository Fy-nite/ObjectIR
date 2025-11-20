# C Compiler Output Format Options

The C compiler now requires an explicit output format option and defaults to **FOB** format if no option is provided.

## Usage

```bash
dotnet run [format]
```

## Available Formats

| Format | Option | File | Description |
|--------|--------|------|-------------|
| **FOB** | `fob` | `.fob` | Finite Open Bytecode (binary) - **DEFAULT** |
| **JSON** | `json` | `.json` | Structured data (4 KB) |
| **Text** | `text` | `.txt` | Human-readable (0.26 KB) |
| **IR Code** | `ir` | `.ir` | Assembly-like instructions (0.58 KB) |
| **BSON** | `bson` | `.bson` | Binary JSON (1.85 KB) |
| **CSV** | `csv` | `.csv` | Spreadsheet format (0.13 KB) |
| **Markdown** | `md` | `.md` | Documentation (0.37 KB) |
| **YAML** | `yaml` | `.yaml` | Configuration (0.37 KB) |
| **Report** | `report` | `_report.txt` | Module statistics (0.83 KB) |
| **All** | `all` | `.*` | Save to all formats |

## Examples

### Default (FOB)
```bash
dotnet run
# Attempts to generate module.fob
```

### Specific Format
```bash
dotnet run json
# Outputs: module.json (human-readable JSON)

dotnet run text
# Outputs: module.txt (simple text summary)

dotnet run md
# Outputs: module.md (Markdown documentation)

dotnet run report
# Outputs: module_report.txt (statistics and metrics)
```

### All Formats
```bash
dotnet run all
# Generates: module.json, module.txt, module.ir, module.bson,
#            module.csv, module.md, module.yaml, module_report.txt
# (FOB is skipped if it fails due to missing entry point)
```

### Help
```bash
dotnet run invalid
# Shows all available format options
```

## Output Location

All generated files are saved to the `output/` directory:
```
output/
├── module.json          # Structured data (4 KB)
├── module.txt           # Human-readable text (0.26 KB)
├── module.ir            # Assembly-like IR code (0.58 KB)
├── module.bson          # Binary JSON (1.85 KB)
├── module.csv           # Spreadsheet data (0.13 KB)
├── module.md            # Markdown documentation (0.37 KB)
├── module.yaml          # YAML format (0.37 KB)
├── module_report.txt    # Statistics report (0.83 KB)
└── module.fob           # Binary FOB (if successful)
```

## Format Characteristics

### Recommended Formats by Use Case

**Data Interchange & APIs**
- `json` - Most widely supported

**Documentation & Readability**
- `text` - Quick overview
- `md` - Professional documentation
- `report` - Statistics and metrics

**Analysis & Spreadsheets**
- `csv` - Import into Excel/Google Sheets
- `yaml` - Configuration management

**Low-Level Analysis**
- `ir` - Instruction sequences
- `ir` - Assembly-like representation

**Efficient Storage**
- `bson` - 53% smaller than JSON
- `fob` - Binary bytecode (requires entry point)

## Default Behavior

When no format is specified, the compiler defaults to **FOB format**:

```bash
dotnet run
# Equivalent to: dotnet run fob
```

However, FOB format requires a valid entry point in the module. If the compilation fails with FOB, try a different format:

```bash
dotnet run json    # Recommended alternative
dotnet run text    # For quick viewing
dotnet run report  # For metrics
```

## Notes

- **FOB Format**: Requires a valid entry point to be defined. For modules without an entry point, use an alternative format.
- **Binary Formats**: `bson` and `fob` produce binary files that aren't directly human-readable.
- **Text Formats**: All text-based formats output directly to console before saving.
- **All Formats**: Using `dotnet run all` gracefully skips FOB if it fails.
