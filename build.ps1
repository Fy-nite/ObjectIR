# Set paths
$cppRuntimePath = "ObjectIR.CppRuntime"
$unifiedRuntimePath = "ObjectIR.UnifiedRuntime"
$fortranpath = "ObjectIR.Fortran"
$outputPath = "out"

# Build C++ runtime
cd $cppRuntimePath
cmake -S . -B build
cmake --build build 
cd ..

# Build Unified Runtime
cd $unifiedRuntimePath
dotnet build -c Release
cd ..
# Build Fortran Compiler
cd $fortranpath
dotnet build -c Release
cd ..

# Create output folder
if (!(Test-Path $outputPath)) {
    New-Item -ItemType Directory -Path $outputPath | Out-Null
}

# Copy C++ runtime binaries by platform
if ($IsWindows) {
    Copy-Item "$cppRuntimePath/build/*.dll" $outputPath -Recurse -ErrorAction SilentlyContinue
    Copy-Item "$cppRuntimePath/build/*.lib" $outputPath -Recurse -ErrorAction SilentlyContinue
} elseif ($IsLinux) {
    Copy-Item "$cppRuntimePath/build/*.so" $outputPath -Recurse -ErrorAction SilentlyContinue
    Copy-Item "$cppRuntimePath/build/*.a" $outputPath -Recurse -ErrorAction SilentlyContinue
} elseif ($IsMacOS) {
    Copy-Item "$cppRuntimePath/build/*.dylib" $outputPath -Recurse -ErrorAction SilentlyContinue
    Copy-Item "$cppRuntimePath/build/*.a" $outputPath -Recurse -ErrorAction SilentlyContinue
}

# Copy Unified Runtime output (DLL for .NET, works on all platforms)
Copy-Item "$unifiedRuntimePath/bin/Release/net9.0/*.dll" $outputPath -Recurse -ErrorAction SilentlyContinue

# Copy Unified Runtime output
Copy-Item "$unifiedRuntimePath/bin/Release/net9.0/*" $outputPath -Recurse
# Copy Fortran Compiler output
Copy-Item "$fortranpath/bin/Release/net9.0/*" $outputPath -Recurse

# (Optional) Copy additional files (config, docs, etc.)
# Copy-Item "README.md" $outputPath

Write-Host "Build and copy complete. Output in $outputPath"