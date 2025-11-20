using OIC;
using ObjectIR.Core.IR;
using ObjectIR.Core.Serialization;

// Parse command-line arguments
var cliArgs = new CommandLineArgs(args);

// Show help if requested
if (cliArgs.ShowHelp)
{
    PrintHelp();
    return;
}

// Validate required arguments
if (string.IsNullOrEmpty(cliArgs.InputFile))
{
    Console.Error.WriteLine("Error: No input file specified.");
    Console.Error.WriteLine("Usage: oic <input.c> [options]");
    Console.Error.WriteLine("Use --help for more information.");
    return;
}

// Check if input file exists
if (!File.Exists(cliArgs.InputFile))
{
    Console.Error.WriteLine($"Error: Input file '{cliArgs.InputFile}' not found.");
    return;
}

try
{
    Console.WriteLine("=== C to ObjectIR Compiler ===\n");
    
    // Read source code from file
    string cSourceCode = File.ReadAllText(cliArgs.InputFile);
    Console.WriteLine($"Compiling: {cliArgs.InputFile}");
    Console.WriteLine($"Output Format: {cliArgs.Format.ToUpper()}");
    if (!string.IsNullOrEmpty(cliArgs.OutputFile))
        Console.WriteLine($"Output File: {cliArgs.OutputFile}");
    Console.WriteLine(new string('=', 60) + "\n");

    var compiler = new CLanguageCompiler();
    
    // Compile to ObjectIR
    var module = compiler.CompileSource(cSourceCode);
    
    // Determine output directory
    string outputDir = cliArgs.OutputDirectory ?? "output";
    if (!Directory.Exists(outputDir))
        Directory.CreateDirectory(outputDir);

    // Save based on format selection
    if (cliArgs.Format == "all")
    {
        if (!cliArgs.Quiet) Console.WriteLine("Saving to all formats...\n");
        
        // Text formats
        File.WriteAllText(Path.Combine(outputDir, "module.json"), module.DumpJson());
        if (!cliArgs.Quiet) Console.WriteLine($"✓ JSON");
        
        File.WriteAllText(Path.Combine(outputDir, "module.txt"), module.DumpText());
        if (!cliArgs.Quiet) Console.WriteLine($"✓ Text");
        
        File.WriteAllText(Path.Combine(outputDir, "module.ir"), module.Serialize().DumpToIRCode());
        if (!cliArgs.Quiet) Console.WriteLine($"✓ IR Code");
        
        File.WriteAllText(Path.Combine(outputDir, "module.csv"), module.DumpCsv());
        if (!cliArgs.Quiet) Console.WriteLine($"✓ CSV");
        
        File.WriteAllText(Path.Combine(outputDir, "module.md"), module.DumpMarkdown());
        if (!cliArgs.Quiet) Console.WriteLine($"✓ Markdown");
        
        File.WriteAllText(Path.Combine(outputDir, "module.yaml"), module.DumpYaml());
        if (!cliArgs.Quiet) Console.WriteLine($"✓ YAML");
        
        File.WriteAllText(Path.Combine(outputDir, "module_report.txt"), module.GenerateSummaryReport());
        if (!cliArgs.Quiet) Console.WriteLine($"✓ Report");
        
        // Binary formats
        File.WriteAllBytes(Path.Combine(outputDir, "module.bson"), module.DumpBson());
        if (!cliArgs.Quiet) Console.WriteLine($"✓ BSON");
        
        try
        {
            File.WriteAllBytes(Path.Combine(outputDir, "module.fob"), module.DumpFob());
            if (!cliArgs.Quiet) Console.WriteLine($"✓ FOB");
        }
        catch
        {
            if (!cliArgs.Quiet) Console.WriteLine($"⚠ FOB (skipped - requires entry point)");
        }
        
        if (!cliArgs.Quiet) Console.WriteLine($"\nAll files saved to: {Path.GetFullPath(outputDir)}");
    }
    else
    {
        // Single format
        string outputFile = cliArgs.OutputFile ?? GetDefaultFileName(cliArgs.Format, outputDir);
        SaveFormat(module, cliArgs.Format, outputFile, cliArgs.Quiet);
        
        if (!cliArgs.Quiet && cliArgs.Format != "fob" && cliArgs.Format != "bson")
        {
            // Show content for text formats
            Console.WriteLine(new string('-', 60));
            Console.WriteLine(File.ReadAllText(outputFile));
        }
    }

    Console.WriteLine(new string('=', 60));
    Console.WriteLine("Compilation successful!");
}
catch (Exception ex)
{
    Console.Error.WriteLine($"Compilation error: {ex.Message}");
    if (!cliArgs.Quiet)
        Console.Error.WriteLine($"Stack trace: {ex.StackTrace}");
    Environment.Exit(1);
}

// Helper methods
string GetDefaultFileName(string format, string outputDir)
{
    return format switch
    {
        "json" => Path.Combine(outputDir, "module.json"),
        "text" => Path.Combine(outputDir, "module.txt"),
        "ir" => Path.Combine(outputDir, "module.ir"),
        "bson" => Path.Combine(outputDir, "module.bson"),
        "fob" => Path.Combine(outputDir, "module.fob"),
        "csv" => Path.Combine(outputDir, "module.csv"),
        "md" => Path.Combine(outputDir, "module.md"),
        "yaml" => Path.Combine(outputDir, "module.yaml"),
        "report" => Path.Combine(outputDir, "module_report.txt"),
        _ => Path.Combine(outputDir, "module.out")
    };
}

void SaveFormat(Module module, string format, string outputFile, bool quiet)
{
    try
    {
        switch (format)
        {
            case "json":
                File.WriteAllText(outputFile, module.DumpJson());
                if (!quiet) Console.WriteLine($"✓ Saved JSON to: {Path.GetFullPath(outputFile)}");
                break;
            case "text":
                File.WriteAllText(outputFile, module.DumpText());
                if (!quiet) Console.WriteLine($"✓ Saved text to: {Path.GetFullPath(outputFile)}");
                break;
            case "ir":
                File.WriteAllText(outputFile, module.Serialize().DumpToIRCode());
                if (!quiet) Console.WriteLine($"✓ Saved IR code to: {Path.GetFullPath(outputFile)}");
                break;
            case "bson":
                File.WriteAllBytes(outputFile, module.DumpBson());
                if (!quiet) Console.WriteLine($"✓ Saved BSON to: {Path.GetFullPath(outputFile)}");
                break;
            case "fob":
                File.WriteAllBytes(outputFile, module.DumpFob());
                if (!quiet) Console.WriteLine($"✓ Saved FOB to: {Path.GetFullPath(outputFile)}");
                break;
            case "csv":
                File.WriteAllText(outputFile, module.DumpCsv());
                if (!quiet) Console.WriteLine($"✓ Saved CSV to: {Path.GetFullPath(outputFile)}");
                break;
            case "md":
                File.WriteAllText(outputFile, module.DumpMarkdown());
                if (!quiet) Console.WriteLine($"✓ Saved Markdown to: {Path.GetFullPath(outputFile)}");
                break;
            case "yaml":
                File.WriteAllText(outputFile, module.DumpYaml());
                if (!quiet) Console.WriteLine($"✓ Saved YAML to: {Path.GetFullPath(outputFile)}");
                break;
            case "report":
                File.WriteAllText(outputFile, module.GenerateSummaryReport());
                if (!quiet) Console.WriteLine($"✓ Saved report to: {Path.GetFullPath(outputFile)}");
                break;
            default:
                throw new InvalidOperationException($"Unknown format: {format}");
        }
    }
    catch (Exception ex) when (format == "fob")
    {
        Console.Error.WriteLine($"Error: {ex.Message}");
        Console.Error.WriteLine("FOB format requires a valid entry point. Try a different format:");
        Console.Error.WriteLine("  --format json");
        Console.Error.WriteLine("  --format text");
        Environment.Exit(1);
    }
}

void PrintHelp()
{
    Console.WriteLine(@"
C to ObjectIR Compiler
======================

Usage:
  oic <input.c> [options]

Arguments:
  <input.c>              C source file to compile

Options:
  --format <format>      Output format (default: fob)
                         Formats: json, text, ir, bson, fob, csv, md, yaml, report, all
  --output <file>        Output file path (auto-generated if not specified)
  --output-dir <dir>     Output directory (default: ./output)
  --quiet                Suppress verbose output
  --help                 Show this help message

Examples:
  oic main.c
  oic main.c --format json
  oic main.c --format text --output result.txt
  oic main.c --format all --output-dir dist/
  oic main.c --format json --output-dir build/ --quiet

Format Descriptions:
  json      Structured data format (JSON)
  text      Human-readable summary
  ir        Assembly-like IR code
  bson      Binary JSON (compact)
  fob       Finite Open Bytecode (binary) - DEFAULT
  csv       Spreadsheet compatible
  md        Markdown documentation
  yaml      YAML configuration
  report    Module statistics and metrics
  all       Save to all formats at once
");
}

// Command-line argument parser
class CommandLineArgs
{
    public string InputFile { get; set; } = string.Empty;
    public string Format { get; set; } = "fob";
    public string? OutputFile { get; set; }
    public string? OutputDirectory { get; set; }
    public bool Quiet { get; set; }
    public bool ShowHelp { get; set; }

    public CommandLineArgs(string[] args)
    {
        var validFormats = new[] { "json", "text", "ir", "bson", "fob", "csv", "md", "yaml", "report", "all" };

        for (int i = 0; i < args.Length; i++)
        {
            switch (args[i].ToLower())
            {
                case "--help":
                case "-h":
                    ShowHelp = true;
                    break;
                case "--format":
                case "-f":
                    if (i + 1 < args.Length)
                    {
                        string fmt = args[++i].ToLower();
                        if (validFormats.Contains(fmt))
                            Format = fmt;
                        else
                            throw new InvalidOperationException($"Unknown format: {fmt}");
                    }
                    break;
                case "--output":
                case "-o":
                    if (i + 1 < args.Length)
                        OutputFile = args[++i];
                    break;
                case "--output-dir":
                case "-d":
                    if (i + 1 < args.Length)
                        OutputDirectory = args[++i];
                    break;
                case "--quiet":
                case "-q":
                    Quiet = true;
                    break;
                default:
                    if (!args[i].StartsWith("-"))
                    {
                        InputFile = args[i];
                    }
                    break;
            }
        }
    }
}


