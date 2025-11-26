#!/usr/bin/env pwsh
# Bootstrap build script for self-hosted ObjectIR.CSharpFrontend compiler
# This script compiles only the frontend implementation files (excluding AST/Parser/Lexer infrastructure)

param(
    [string]$OutputDir = "bin/Release",
    [string]$ModuleName = "CSharpFrontend",
    [string]$Format = "json",
    [string]$Verbosity = "Normal"
)

# Files to compile, in dependency order
$SourceFiles = @(
    # Core infrastructure
    "Program.cs",
    "CompilerOptions.cs", 
    "CommandLineParser.cs",
    
    # Roslyn integration
    "RoslynCompiler.cs",
    "RoslynToIREmitter.cs",
    
    # Compiler infrastructure  
    "CompilerService.cs",
    "SemanticAnalyzer.cs",
    "MultiPassCompiler.cs",
    
    # Bootstrap support
    "BootstrapManager.cs"
)

# Verify all source files exist
$MissingFiles = @()
foreach ($file in $SourceFiles) {
    if (!(Test-Path $file)) {
        $MissingFiles += $file
    }
}

if ($MissingFiles.Count -gt 0) {
    Write-Error "Error: Missing source files: $($MissingFiles -join ', ')"
    exit 1
}

# Build the dotnet command
$Args = @(
    "run", "-c", "Release", "--"
) + $SourceFiles + @(
    "-o", $OutputDir,
    "-m", $ModuleName,
    "-f", $Format,
    "--aggressive-bootstrap",
    "--verbosity", $Verbosity
)

Write-Host "Bootstrap building CSharpFrontend frontend with $($SourceFiles.Count) files..."
Write-Host "Command: dotnet $($Args -join ' ')"
Write-Host ""

# Run the compiler
& dotnet @Args

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
