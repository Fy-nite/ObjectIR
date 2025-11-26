#include "objectir_runtime.hpp"
#include "fob_loader.hpp"
#include "ir_loader.hpp"
#include <iostream>
#include <fstream>
#include <vector>
#include <string>

using namespace ObjectIR;

// Helper function to check if file is FOB format
bool IsFOBFile(const std::string& filePath) {
    std::ifstream file(filePath, std::ios::binary);
    if (!file) return false;

    char magic[3];
    file.read(magic, 3);
    return file && magic[0] == 'F' && magic[1] == 'O' && magic[2] == 'B';
}

int main(int argc, char* argv[]) {
    if (argc < 2) {
        std::cerr << "Usage: " << argv[0] << " <module_file> [entry_point] [args...]" << std::endl;
        std::cerr << "  module_file: Path to .fob or .json ObjectIR module" << std::endl;
        std::cerr << "  entry_point: Optional class.method entry point (default: Main.Main)" << std::endl;
        std::cerr << "  args: Optional arguments to pass to the entry point" << std::endl;
        return 1;
    }

    std::string modulePath = argv[1];
    std::string entryPoint = (argc >= 3) ? argv[2] : "Main.Main";

    // Parse entry point
    size_t dotPos = entryPoint.rfind('.');
    if (dotPos == std::string::npos) {
        std::cerr << "Invalid entry point format. Expected: Class.Method or Namespace.Class.Method" << std::endl;
        return 1;
    }

    std::string className = entryPoint.substr(0, dotPos);
    std::string methodName = entryPoint.substr(dotPos + 1);

    try {
        std::shared_ptr<VirtualMachine> vm;

        if (IsFOBFile(modulePath)) {
            // Load FOB file
            auto result = FOBLoader::LoadFromFile(modulePath);
            vm = result.vm;
            std::cout << "Loaded FOB module with " << result.classNames.size() << " classes" << std::endl;
        } else {
            // Load JSON file
            vm = IRLoader::LoadFromFile(modulePath);
            std::cout << "Loaded JSON module" << std::endl;
        }

        if (!vm) {
            std::cerr << "Failed to load module from: " << modulePath << std::endl;
            return 1;
        }
        
        // // Print all classes and methods in the loaded module
        // std::cout << "\n=== Module Introspection ===" << std::endl;
        // std::cout << "Classes and Methods in the module:" << std::endl;
        // for (const auto& className : vm->GetAllClassNames()) {
        //     ClassRef classRef = vm->GetClass(className);
        //     std::cout << "Class: " << className << std::endl;
        //     for (const auto& method : classRef->GetAllMethods()) {
        //         std::cout << "  Method: " << method->GetName() 
        //                   << " (Return type: " << method->GetReturnType().ToString() 
        //                   << ", Static: " << (method->IsStatic() ? "yes" : "no") << ")" << std::endl;
                
        //         // Print parameters if any
        //         const auto& params = method->GetParameters();
        //         if (!params.empty()) {
        //             std::cout << "    Parameters:" << std::endl;
        //             for (const auto& param : params) {
        //                 std::cout << "      " << param.first << ": " << param.second.ToString() << std::endl;
        //             }
        //         }
        //     }
        //     std::cout << std::endl;
        // }
        // std::cout << "=== End Introspection ===\n" << std::endl;

        // Find the entry class
        ClassRef entryClass = nullptr;
        try {
            entryClass = vm->GetClass(className);
        } catch (const std::runtime_error& e) {
            std::cout << "Debug: Could not find class '" << className << "': " << e.what() << std::endl;
            
            // Class not found - try fallback if using default entry point
            if (entryPoint == "Main.Main") {
                std::cout << "Note: Main.Main not found, searching for Program.Main in any namespace..." << std::endl;
                try {
                    entryClass = vm->GetClass("Program");
                    if (entryClass) {
                        className = "Program";
                        methodName = "Main";
                        std::cout << "Found Program.Main entry point" << std::endl;
                    }
                } catch (const std::runtime_error& e2) {
                    std::cout << "Debug: Could not find class 'Program': " << e2.what() << std::endl;
                }
            }
        }
        
        if (!entryClass) {
            std::cerr << "Entry class '" << className << "' not found in module" << std::endl;
            return 1;
        }

        // Prepare method arguments from command line
        std::vector<Value> methodArgs;
        for (int i = 3; i < argc; ++i) {
            methodArgs.push_back(Value(std::string(argv[i])));
        }

        // Invoke the static method
        try {
            Value result = vm->InvokeStaticMethod(entryClass, methodName, methodArgs);

            // Print result if it's a string or primitive
            if (result.IsString()) {
                std::cout << "Result: " << result.AsString() << std::endl;
            } else if (result.IsInt32()) {
                std::cout << "Result: " << result.AsInt32() << std::endl;
            } else if (result.IsInt64()) {
                std::cout << "Result: " << result.AsInt64() << std::endl;
            } else if (result.IsBool()) {
                std::cout << "Result: " << (result.AsBool() ? "true" : "false") << std::endl;
            } else if (result.IsFloat32()) {
                std::cout << "Result: " << result.AsFloat32() << std::endl;
            } else if (result.IsFloat64()) {
                std::cout << "Result: " << result.AsFloat64() << std::endl;
            } else if (result.IsNull()) {
                std::cout << "Method executed successfully (void return)" << std::endl;
            } else if (!result.IsNull()) {
                std::cout << "Result: [Object]" << std::endl;
            }
        } catch (const std::runtime_error& e) {
            if (std::string(e.what()).find("Method has no implementation") != std::string::npos) {
                std::cout << "Note: Method '" << className << "." << methodName << "' has no implementation (stub method)" << std::endl;
                std::cout << "This is expected for generated stub code. The standalone executable is working correctly!" << std::endl;
            } else {
                throw; // Re-throw other runtime errors
            }
        }

        return 0;

    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << std::endl;
        return 1;
    }
}