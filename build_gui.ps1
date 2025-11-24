# Build script for ObjectIR GUI Frontend (Windows)
# Usage: .\build_gui.ps1 [Qt_Path] [Generator] [Platform]

param(
    [string]$QtPath = "C:\Qt\6.9.3\msvc2019_64",
    [string]$Generator = "Visual Studio 17 2022",
    [string]$Platform = "x64"
)

$ErrorActionPreference = "Stop"

$ProjectRoot = Split-Path -Parent $MyInvocation.MyCommand.Path
$BuildDir = Join-Path $ProjectRoot "..\..\build\gui-build"
$CmakeSource = $ProjectRoot

Write-Host "==========================================" -ForegroundColor Cyan
Write-Host "ObjectIR GUI Build Script (Windows)" -ForegroundColor Cyan
Write-Host "==========================================" -ForegroundColor Cyan
Write-Host "Source:  $CmakeSource"
Write-Host "Build:   $BuildDir"
Write-Host "Qt Path: $QtPath"
Write-Host "Generator: $Generator"
Write-Host "Platform: $Platform"
Write-Host "==========================================" -ForegroundColor Cyan

# Verify Qt installation
if (-not (Test-Path $QtPath)) {
    Write-Host "ERROR: Qt path not found: $QtPath" -ForegroundColor Red
    Write-Host "Please install Qt6 or specify the correct path:" -ForegroundColor Yellow
    Write-Host "  .\build_gui.ps1 -QtPath 'C:\Qt\6.9.3\msvc2019_64'" -ForegroundColor Yellow
    exit 1
}

# Create build directory
if (-not (Test-Path $BuildDir)) {
    New-Item -ItemType Directory -Path $BuildDir -Force | Out-Null
}

Push-Location $BuildDir

try {
    # Configure
    Write-Host "`nConfiguring CMake..." -ForegroundColor Green
    $configArgs = @(
        "-S", $CmakeSource,
        "-B", ".",
        "-G", $Generator,
        "-A", $Platform,
        "-DBUILD_GUI=ON",
        "-DCMAKE_PREFIX_PATH=$QtPath"
    )
    
    & cmake @configArgs
    if ($LASTEXITCODE -ne 0) {
        Write-Host "CMake configuration failed!" -ForegroundColor Red
        exit 1
    }

    # Build
    Write-Host "`nBuilding GUI executable..." -ForegroundColor Green
    $buildArgs = @(
        "--build", ".",
        "--config", "Release",
        "--target", "objectir_gui"
    )
    
    & cmake @buildArgs
    if ($LASTEXITCODE -ne 0) {
        Write-Host "Build failed!" -ForegroundColor Red
        exit 1
    }

    Write-Host "`n==========================================" -ForegroundColor Cyan
    Write-Host "Build Complete!" -ForegroundColor Green
    Write-Host "==========================================" -ForegroundColor Cyan
    
    $exePath = Join-Path $BuildDir "Release\objectir_gui.exe"
    Write-Host "Output: $exePath"
    
    if (Test-Path $exePath) {
        Write-Host "`nTo run the GUI:" -ForegroundColor Yellow
        Write-Host "  $exePath" -ForegroundColor Yellow
        
        $deployChoice = Read-Host "`nDeploy Qt runtime libraries? (y/n)"
        if ($deployChoice -eq "y" -or $deployChoice -eq "Y") {
            $windeployqt = Join-Path $QtPath "bin\windeployqt.exe"
            if (Test-Path $windeployqt) {
                Write-Host "Running windeployqt..." -ForegroundColor Green
                & $windeployqt (Join-Path $BuildDir "Release\objectir_gui.exe")
                Write-Host "Qt libraries deployed to Release directory" -ForegroundColor Green
            }
        }
    }
}
finally {
    Pop-Location
}
