# ObjectIR GUI Frontend Build Guide

## Overview

The ObjectIR GUI frontend is now a standalone executable that lets you write and execute ObjectIR text IR code directly in a visual editor.

## Features

✅ **Text IR Editor** - Write ObjectIR code in the textual syntax (as seen in `docs/COMPLETE_EXAMPLE.md`)
✅ **Parse & Execute** - Instantly parse and run your IR programs
✅ **File Operations** - Open/save `.ir` and `.ir.txt` files
✅ **Output Console** - See execution results in real-time

## Example ObjectIR Program

```
module HelloWorld

class Program {
    method Main() -> void {
        ldstr "Hello from ObjectIR!"
        call System.Console.WriteLine(string) -> void
        ret
    }
}
```

## Prerequisites

### Required
- **CMake** 3.16+ ([download](https://cmake.org/download/))
- **Qt6** or **Qt5** (Widgets module)
- **C++ Compiler** (MSVC 2019+, GCC, or Clang with C++17 support)

### Installing Qt

#### Option 1: Using Qt Online Installer
1. Download [Qt Online Installer](https://www.qt.io/download-qt-installer)
2. Install Qt6 (recommended) or Qt5
3. Make sure to install the **Desktop** variant with your preferred compiler

#### Option 2: Using Package Manager
```powershell
# Windows with Chocolatey
choco install qt

# macOS with Homebrew
brew install qt6

# Ubuntu/Debian
sudo apt-get install qt6-base-dev

# Fedora
sudo dnf install qt6-devel
```

## Building the GUI

### Step 1: Configure CMake

```powershell
cd D:\ObjectIR

# Create build directory
mkdir build\gui-build
cd build\gui-build

# Configure with GUI enabled and Qt path
# For Qt6 with MSVC:
cmake -S ..\..\src\ObjectIR.CppRuntime `
  -B . `
  -G "Visual Studio 17 2022" `
  -A x64 `
  -DBUILD_GUI=ON `
  -DCMAKE_PREFIX_PATH="C:\Qt\6.9.3\msvc2019_64"

# For Qt5 with MSVC:
# cmake -S ..\..\src\ObjectIR.CppRuntime `
#   -B . `
#   -G "Visual Studio 17 2022" `
#   -A x64 `
#   -DBUILD_GUI=ON `
#   -DCMAKE_PREFIX_PATH="C:\Qt\5.15.x\msvc2019_64"

# For MinGW with Qt6:
# cmake -S ..\..\src\ObjectIR.CppRuntime `
#   -B . `
#   -G "MinGW Makefiles" `
#   -DBUILD_GUI=ON `
#   -DCMAKE_PREFIX_PATH="C:\Qt\6.9.3\mingw_64"
```

**Important:** Replace the `CMAKE_PREFIX_PATH` with your actual Qt installation path.

### Step 2: Build

```powershell
# Using CMake
cmake --build . --config Release --target objectir_gui

# Or using your build system directly
# Visual Studio:
# msbuild objectir_gui.sln /p:Configuration=Release /p:Platform=x64

# Make/Ninja:
# make -j4 objectir_gui
# ninja objectir_gui
```

### Step 3: Run

```powershell
# Windows
.\Release\objectir_gui.exe

# Linux/macOS
./objectir_gui
```

## Finding Your Qt Installation Path

### Windows

```powershell
# Find Qt installation
Get-ChildItem -Path "C:\Qt" -Directory
Get-ChildItem -Path "C:\Qt\6.9.3" -Directory

# Example Qt6 with MSVC:
# C:\Qt\6.9.3\msvc2019_64

# Example Qt5 with MSVC:
# C:\Qt\5.15.13\msvc2019_64
```

### Linux

```bash
# Qt6
/opt/qt/6.x.x/gcc_64
/usr/lib/x86_64-linux-gnu/cmake/Qt6

# Qt5
/opt/qt/5.15.x/gcc
/usr/lib/x86_64-linux-gnu/cmake/Qt5
```

### macOS

```bash
# Qt6
/usr/local/opt/qt6
~/Qt/6.x.x/macos

# Qt5
/usr/local/opt/qt5
~/Qt/5.15.x/macos
```

## Using the GUI

### 1. Writing Code

The editor comes with a sample "Hello World" program:

```
module HelloWorld

class Program {
    method Main() -> void {
        ldstr "Hello from ObjectIR!"
        call System.Console.WriteLine(string) -> void
        ret
    }
}
```

### 2. Opening Files

- Click **"Open File"** to load an existing `.ir` or `.ir.txt` file
- All text IR programs are supported

### 3. Saving Code

- Click **"Save"** to export your program to disk
- Choose `.ir` or `.ir.txt` format

### 4. Running Programs

- Click **"Run"** to parse and execute your ObjectIR code
- Output appears in the console below
- Errors are displayed with details

### 5. Clearing Output

- Click **"Clear Output"** to reset the output pane

## Example Programs

### Simple Arithmetic

```
module Calculator

class Calculator {
    method Add(a: int32, b: int32) -> int32 {
        ldarg a
        ldarg b
        add
        ret
    }
    
    method Main() -> void {
        ldstr "This is a simple calculator"
        call System.Console.WriteLine(string) -> void
        ret
    }
}
```

### Working with Strings

```
module StringExample

class Program {
    method Main() -> void {
        ldstr "First string"
        ldstr "Second string"
        call System.String.Concat(string, string) -> string
        call System.Console.WriteLine(string) -> void
        ret
    }
}
```

## Troubleshooting

### CMake can't find Qt

**Error:**
```
CMake Error: Could not find Qt
```

**Solution:**
Explicitly set `CMAKE_PREFIX_PATH` when configuring:

```powershell
cmake -S src/ObjectIR.CppRuntime -B build\gui-build -DCMAKE_PREFIX_PATH="C:\Qt\6.9.3\msvc2019_64"
```

### Missing MOC files

**Error:**
```
error: cannot open file 'gui_frontend_moc.cpp'
```

**Solution:**
Make sure `CMAKE_AUTOMOC` is enabled in CMakeLists.txt (it is by default).

### Linker errors with Qt libraries

**Error:**
```
error LNK2019: unresolved external symbol
```

**Solution:**
1. Ensure Qt_LIBS is correctly linked in CMakeLists.txt
2. Verify the Qt installation path matches your compiler (MSVC, MinGW, GCC)
3. Rebuild with `cmake --build . --clean-first`

### GUI window doesn't appear

**Solution:**
- Ensure you're running the Release build (not Debug without debug Qt libraries)
- Check that all Qt DLLs are in the PATH or same directory as executable

### Port Qt DLLs to standalone executable

On Windows, you may need to include Qt runtime libraries:

```powershell
# Option 1: Copy Qt DLLs to build directory
$QtBinPath = "C:\Qt\6.9.3\msvc2019_64\bin"
Copy-Item "$QtBinPath\Qt6Core.dll" -Destination Release
Copy-Item "$QtBinPath\Qt6Gui.dll" -Destination Release
Copy-Item "$QtBinPath\Qt6Widgets.dll" -Destination Release

# Option 2: Use Qt's deployment tool (windeployqt)
&"C:\Qt\6.9.3\msvc2019_64\bin\windeployqt.exe" Release\objectir_gui.exe
```

## Development

### Adding New Features

The GUI frontend uses Qt's signal/slot system for event handling. Key files:

- **`src/gui_frontend.cpp`** - Main UI and logic
- **`src/ir_text_parser.cpp`** - Text IR parser
- **`include/ir_text_parser.hpp`** - Parser interface
- **`include/ir_loader.hpp`** - Runtime loader

### Building Without GUI

If you don't want to build the GUI, simply configure without `-DBUILD_GUI=ON`:

```powershell
cmake -S src/ObjectIR.CppRuntime -B build\cpp-runtime -DBUILD_STANDALONE=ON
```

## Next Steps

Once the GUI is built and running:

1. ✅ Use the **Text IR Editor** to write programs in ObjectIR's textual syntax
2. ✅ Execute them immediately with the **Run** button
3. ✅ Save your work to `.ir` files for version control
4. ✅ Explore the examples in `docs/COMPLETE_EXAMPLE.md` and `examples/`

## Performance Notes

- The text IR parser is optimized for small to medium programs
- For very large programs (1000+ methods), consider pre-compiling to FOB format
- The GUI refreshes output in real-time; long-running programs will block the UI

## Contributing

To improve the GUI:

1. Extend the parser in `ir_text_parser.cpp` for better IR support
2. Add syntax highlighting by implementing a custom Qt text formatter
3. Add breakpoint/debugging support via the VirtualMachine
4. Implement project management features

## License

This GUI frontend is part of the ObjectIR project and follows the same license terms.
