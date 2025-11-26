using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace ObjectIR.CSharpFrontend;

/// <summary>
/// Parses and validates command-line arguments for the C# to ObjectIR compiler
/// </summary>
public class CommandLineParser
{
    private const string VERSION = "1.0.0";
    private const string USAGE = @"
Usage: csharp-to-objectir [options] <input-files...>

Options:
  -h, --help              Show this help message
  -v, --version           Show version information
  -o, --output <dir>      Output directory (default: current directory)
  -m, --module <name>     Module name (default: derived from first input file)
  -f, --format <fmt>      Output format: binary, text, json (default: binary)
  --verbosity <level>     Verbosity level: quiet, normal, verbose, debug (default: normal)
  --debug                 Emit debug information
  --no-optimize           Disable IR optimization
  --warnings-as-errors    Treat warnings as errors
  --aggressive-bootstrap  Aggressive bootstrap mode (ignore all errors, use AST)

Examples:
  # Compile single file
  csharp-to-objectir mycode.cs -o bin/

  # Compile multiple files with custom module name
  csharp-to-objectir *.cs -m MyModule -f json

  # Verbose output with debug info
  csharp-to-objectir calculator.cs --verbosity verbose --debug
";

    public CompilerOptions Parse(string[] args)
    {
        var options = new CompilerOptions();

        if (args.Length == 0)
        {
            options.ShowHelp = true;
            return options;
        }

        var inputFiles = new List<string>();

        for (int i = 0; i < args.Length; i++)
        {
            var arg = args[i];

            if (arg == "-h" || arg == "--help")
            {
                options.ShowHelp = true;
            }
            else if (arg == "-v" || arg == "--version")
            {
                options.ShowVersion = true;
            }
            else if (arg == "-o" || arg == "--output")
            {
                if (i + 1 >= args.Length)
                    throw new ArgumentException("--output requires an argument");
                options.OutputDirectory = args[++i];
            }
            else if (arg == "-m" || arg == "--module")
            {
                if (i + 1 >= args.Length)
                    throw new ArgumentException("--module requires an argument");
                options.ModuleName = args[++i];
            }
            else if (arg == "-f" || arg == "--format")
            {
                if (i + 1 >= args.Length)
                    throw new ArgumentException("--format requires an argument");
                var fmt = args[++i].ToLower();
                options.OutputFormat = fmt switch
                {
                    "binary" => OutputFormat.Binary,
                    "text" => OutputFormat.Text,
                    "json" => OutputFormat.Json,
                    _ => throw new ArgumentException($"Unknown format: {fmt}")
                };
            }
            else if (arg == "--verbosity")
            {
                if (i + 1 >= args.Length)
                    throw new ArgumentException("--verbosity requires an argument");
                var level = args[++i].ToLower();
                options.Verbosity = level switch
                {
                    "quiet" => VerbosityLevel.Quiet,
                    "normal" => VerbosityLevel.Normal,
                    "verbose" => VerbosityLevel.Verbose,
                    "debug" => VerbosityLevel.Debug,
                    _ => throw new ArgumentException($"Unknown verbosity level: {level}")
                };
            }
            else if (arg == "--debug")
            {
                options.EmitDebugInfo = true;
            }
            else if (arg == "--no-optimize")
            {
                options.OptimizeIR = false;
            }
            else if (arg == "--warnings-as-errors")
            {
                options.WarningsAsErrors = true;
            }
            else if (arg == "--aggressive-bootstrap")
            {
                options.AggressiveBootstrapMode = true;
            }
            else if (arg.StartsWith("-"))
            {
                throw new ArgumentException($"Unknown option: {arg}");
            }
            else
            {
                inputFiles.Add(arg);
            }
        }

        // Check for help/version - these don't require input files
        if (options.ShowHelp || options.ShowVersion)
        {
            return options;
        }

        // Validate input files (only if not showing help/version)
        if (inputFiles.Count == 0)
            throw new ArgumentException("At least one input file must be specified");

        // Expand wildcards
        var expandedFiles = new List<string>();
        foreach (var pattern in inputFiles)
        {
            var dir = Path.GetDirectoryName(pattern);
            if (string.IsNullOrEmpty(dir))
                dir = ".";
            
            var file = Path.GetFileName(pattern);

            if (file.Contains('*') || file.Contains('?'))
            {
                var matches = Directory.GetFiles(dir, file, SearchOption.TopDirectoryOnly);
                if (matches.Length == 0)
                    throw new ArgumentException($"No files match pattern: {pattern}");
                expandedFiles.AddRange(matches);
            }
            else
            {
                if (!File.Exists(pattern))
                    throw new ArgumentException($"Input file not found: {pattern}");
                expandedFiles.Add(pattern);
            }
        }

        options.InputFiles = expandedFiles;

        // Derive module name if not specified
        if (options.ModuleName == null)
        {
            var firstName = Path.GetFileNameWithoutExtension(options.InputFiles[0]);
            options.ModuleName = firstName;
        }

        // Create output directory if it doesn't exist
        if (!Directory.Exists(options.OutputDirectory))
            Directory.CreateDirectory(options.OutputDirectory);

        return options;
    }

    public static void PrintHelp()
    {
        Console.WriteLine(USAGE);
    }

    public static void PrintVersion()
    {
        Console.WriteLine($"csharp-to-objectir version {VERSION}");
    }
}
