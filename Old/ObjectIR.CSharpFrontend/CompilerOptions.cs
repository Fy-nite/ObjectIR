using System;
using System.Collections.Generic;

namespace ObjectIR.CSharpFrontend;

/// <summary>
/// Options for C# to ObjectIR compilation
/// </summary>
public class CompilerOptions
{
    /// <summary>
    /// Input C# source files to compile
    /// </summary>
    public List<string> InputFiles { get; set; } = new();

    /// <summary>
    /// Output directory for generated ObjectIR modules
    /// </summary>
    public string OutputDirectory { get; set; } = ".";

    /// <summary>
    /// Name of the generated module (if not specified, uses first input file name)
    /// </summary>
    public string? ModuleName { get; set; }

    /// <summary>
    /// Output format: binary, text, or json
    /// </summary>
    public OutputFormat OutputFormat { get; set; } = OutputFormat.Binary;

    /// <summary>
    /// Verbosity level: quiet, normal, verbose, debug
    /// </summary>
    public VerbosityLevel Verbosity { get; set; } = VerbosityLevel.Normal;

    /// <summary>
    /// Whether to emit debug information
    /// </summary>
    public bool EmitDebugInfo { get; set; } = false;

    /// <summary>
    /// Whether to emit optimization markers
    /// </summary>
    public bool OptimizeIR { get; set; } = true;

    /// <summary>
    /// Whether to treat warnings as errors
    /// </summary>
    public bool WarningsAsErrors { get; set; } = false;

    /// <summary>
    /// Show help message
    /// </summary>
    public bool ShowHelp { get; set; } = false;

    /// <summary>
    /// Show version information
    /// </summary>
    public bool ShowVersion { get; set; } = false;

    /// <summary>
    /// Aggressive bootstrap mode: ignore all semantic errors and use AST-based analysis.
    /// Used when self-hosting the compiler (compiling CSharpFrontend itself).
    /// </summary>
    public bool AggressiveBootstrapMode { get; set; } = false;

    /// <summary>
    /// Number of compilation errors encountered
    /// </summary>
    public int ErrorCount { get; set; } = 0;

    /// <summary>
    /// Number of compilation warnings encountered
    /// </summary>
    public int WarningCount { get; set; } = 0;
}

/// <summary>
/// Output format for generated ObjectIR modules
/// </summary>
public enum OutputFormat
{
    /// <summary>
    /// Binary format (default, compact)
    /// </summary>
    Binary,

    /// <summary>
    /// Text format (human-readable IR)
    /// </summary>
    Text,

    /// <summary>
    /// JSON format (structured metadata)
    /// </summary>
    Json
}

/// <summary>
/// Verbosity level for compiler output
/// </summary>
public enum VerbosityLevel
{
    /// <summary>
    /// Quiet: only errors
    /// </summary>
    Quiet = 0,

    /// <summary>
    /// Normal: errors and summary
    /// </summary>
    Normal = 1,

    /// <summary>
    /// Verbose: detailed progress
    /// </summary>
    Verbose = 2,

    /// <summary>
    /// Debug: full trace information
    /// </summary>
    Debug = 3
}
