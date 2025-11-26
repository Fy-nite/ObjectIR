#!/usr/bin/env pwsh
# IR Linker - combines multiple ObjectIR modules into a single module

param(
    [Parameter(Mandatory=$true)]
    [string[]]$InputFiles,
    
    [Parameter(Mandatory=$true)]
    [string]$OutputFile,
    
    [string]$OutputModuleName = "LinkedCompiler"
)

Write-Host "IR Linker: Combining ObjectIR modules..." -ForegroundColor Cyan

# Load all IR files
$AllTypes = [System.Collections.ArrayList]@()
$UsedNamespaces = [System.Collections.HashSet[string]]@()

foreach ($file in $InputFiles) {
    if (-not (Test-Path $file)) {
        Write-Host "Error: Input file not found: $file" -ForegroundColor Red
        exit 1
    }
    
    Write-Host "  Loading: $file" -ForegroundColor Yellow
    $json = Get-Content $file | ConvertFrom-Json
    
    # Collect all types
    foreach ($type in $json.Types) {
        $AllTypes.Add($type) > $null
        if ($type.Namespace) {
            $UsedNamespaces.Add($type.Namespace) > $null
        }
    }
    
    Write-Host "    - Loaded $($json.Types.Count) types from module '$($json.Name)'" -ForegroundColor Green
}

Write-Host ""
Write-Host "Total types collected: $($AllTypes.Count)" -ForegroundColor Cyan

# Sort types by name for consistency
$SortedTypes = $AllTypes | Sort-Object { $_.Name }

# Create output module
$LinkedModule = @{
    Name = $OutputModuleName
    Version = "1.0.0"
    TypeCount = $SortedTypes.Count
    SourceModules = @($InputFiles | ForEach-Object { Split-Path -Leaf $_ })
    Types = $SortedTypes
}

# Write output
$OutputJson = $LinkedModule | ConvertTo-Json -Depth 100
Set-Content -Path $OutputFile -Value $OutputJson -Encoding UTF8

Write-Host ""
Write-Host "Linked module created: $OutputFile" -ForegroundColor Green
Write-Host "  Total types: $($LinkedModule.TypeCount)" -ForegroundColor Cyan
Write-Host "  Source modules: $($LinkedModule.SourceModules -join ', ')" -ForegroundColor Cyan

# Show file size
$FileSize = (Get-Item $OutputFile).Length
Write-Host "  File size: $([math]::Round($FileSize/1KB, 1)) KB" -ForegroundColor Cyan
