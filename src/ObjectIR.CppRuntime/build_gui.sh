#!/bin/bash
# Build script for ObjectIR GUI Frontend

set -e

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
BUILD_DIR="${PROJECT_ROOT}/../../build/gui-build"
CMAKE_SOURCE="${PROJECT_ROOT}"

# Detect OS
if [[ "$OSTYPE" == "msys" || "$OSTYPE" == "win32" ]]; then
    # Windows
    QT_PATH="${1:-C:\\Qt\\6.9.3\\msvc2019_64}"
    GENERATOR="${2:-Visual Studio 17 2022}"
    PLATFORM="${3:-x64}"
    BUILD_TOOL_ARGS="/p:Configuration=Release /p:Platform=$PLATFORM"
else
    # Linux/macOS
    QT_PATH="${1:-/usr/lib/x86_64-linux-gnu/cmake/Qt6}"
    GENERATOR="${2:-Unix Makefiles}"
    BUILD_TOOL_ARGS="-j$(nproc || echo 4)"
fi

echo "=========================================="
echo "ObjectIR GUI Build Script"
echo "=========================================="
echo "Source:  $CMAKE_SOURCE"
echo "Build:   $BUILD_DIR"
echo "Qt Path: $QT_PATH"
echo "Generator: $GENERATOR"
echo "=========================================="

# Create build directory
mkdir -p "$BUILD_DIR"
cd "$BUILD_DIR"

# Configure
echo "Configuring CMake..."
if [[ "$OSTYPE" == "msys" || "$OSTYPE" == "win32" ]]; then
    cmake -S "$CMAKE_SOURCE" \
          -B . \
          -G "$GENERATOR" \
          -A "$PLATFORM" \
          -DBUILD_GUI=ON \
          -DCMAKE_PREFIX_PATH="$QT_PATH"
else
    cmake -S "$CMAKE_SOURCE" \
          -B . \
          -G "$GENERATOR" \
          -DBUILD_GUI=ON \
          -DCMAKE_PREFIX_PATH="$QT_PATH"
fi

# Build
echo "Building GUI..."
if [[ "$OSTYPE" == "msys" || "$OSTYPE" == "win32" ]]; then
    cmake --build . --config Release --target objectir_gui
else
    cmake --build . --config Release --target objectir_gui $BUILD_TOOL_ARGS
fi

echo "=========================================="
echo "Build Complete!"
echo "=========================================="

if [[ "$OSTYPE" == "msys" || "$OSTYPE" == "win32" ]]; then
    echo "Output: $BUILD_DIR\\Release\\objectir_gui.exe"
else
    echo "Output: $BUILD_DIR/objectir_gui"
fi
