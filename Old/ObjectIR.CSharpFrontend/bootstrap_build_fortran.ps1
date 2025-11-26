#!/usr/bin/env pwsh
# Bootstrap build script for self-hosted OIFortran compiler
# This script compiles the Fortran compiler's C# source code

param(
    [string]$OutputDir = "bin/Release",
    [string]$ModuleName = "OIFortran",
    [string]$Format = "json",
    [string]$Verbosity = "Normal"
)

# Files to compile, in dependency order
$SourceFiles = @(
    # Core infrastructure
    "../OIFortran/Program.cs",
    
    # Lexical analysis
    "../OIFortran/Compiler/FortranToken.cs",
    "../OIFortran/Compiler/FortranLexer.cs",
    
    # AST and compilation options
    "../OIFortran/Compiler/FortranAST.cs",
    "../OIFortran/Compiler/FortranCompilationOptions.cs",
    
    # Parsing
    "../OIFortran/Compiler/FortranParser.cs",
    "../OIFortran/Compiler/FortranParserV2.cs",
    
    # Language compilation
    "../OIFortran/Compiler/FortranLanguageCompiler.cs",
    "../OIFortran/Compiler/FortranCompiler.cs",
    
    # Intrinsics and I/O
    "../OIFortran/Compiler/FortranIntrinsicConfig.cs",
    "../OIFortran/Compiler/FortranIntrinsicRegistry.cs",
    "../OIFortran/Compiler/FortranIOMapping.cs",
    "../OIFortran/Compiler/FortranIOMapper.cs"
)

# Verify all source files exist
$MissingFiles = @()
foreach ($file in $SourceFiles) {
    if (!(Test-Path $file)) {
        $MissingFiles += $file
    }
}

if ($MissingFiles.Count -gt 0) {
    Write-Host "Error: Missing source files: $($MissingFiles -join ', ')" -ForegroundColor Red
    exit 1
}

# Build the dotnet command
$BuildArgs = @(
    "run", "-c", "Release", "--"
) + $SourceFiles + @(
    "-o", $OutputDir,
    "-m", $ModuleName,
    "-f", $Format,
    "--aggressive-bootstrap",
    "--verbosity", $Verbosity
)

Write-Host "Bootstrap building OIFortran compiler with $($SourceFiles.Count) files..." -ForegroundColor Cyan
Write-Host ""

# Run the compiler
& dotnet @BuildArgs

$ExitCode = $LASTEXITCODE
if ($ExitCode -eq 0) {
    Write-Host ""
    Write-Host "Bootstrap build successful!" -ForegroundColor Green
    Write-Host "Output: $(Resolve-Path $OutputDir)"
} else {
    Write-Host ""
    Write-Host "Bootstrap build failed with exit code $ExitCode" -ForegroundColor Red
}

exit $ExitCode
