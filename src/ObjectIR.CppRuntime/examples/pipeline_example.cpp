/**
 * Complete Example: Builder → File → Runtime Pipeline
 * 
 * This example demonstrates the full ObjectIR workflow:
 * 1. Build a module using the C++ RuntimeBuilder API
 * 2. Serialize it to JSON format
 * 3. Load it from JSON into a runtime
 * 4. Execute methods from the loaded module
 * 
 * To use this example:
 * 1. Build: cmake --build . --target pipeline_example
 * 2. Run: ./pipeline_example
 */

#include <iostream>
#include <fstream>
#include <sstream>
#include "objectir_runtime.hpp"
#include "ir_loader.hpp"

using namespace ObjectIR;

// ============================================================================
// Helper: Simulate JSON serialization (in practice, use C# backend)
// ============================================================================

std::string BuildExampleJson() {
    return R"({
  "name": "CalculatorModule",
  "version": "1.0.0",
  "types": [
    {
      "kind": "class",
      "name": "Calculator",
      "namespace": "Examples",
      "access": "Public",
      "isAbstract": false,
      "isSealed": false,
      "fields": [
        {
          "name": "lastResult",
          "type": "int32",
          "access": "Private",
          "isReadOnly": false
        }
      ],
      "methods": [
        {
          "name": "Add",
          "returnType": "int32",
          "access": "Public",
          "static": false,
          "isVirtual": false,
          "isAbstract": false,
          "parameters": [
            {
              "name": "a",
              "type": "int32"
            },
            {
              "name": "b",
              "type": "int32"
            }
          ]
        },
        {
          "name": "Multiply",
          "returnType": "int32",
          "access": "Public",
          "static": false,
          "isVirtual": false,
          "isAbstract": false,
          "parameters": [
            {
              "name": "x",
              "type": "int32"
            },
            {
              "name": "y",
              "type": "int32"
            }
          ]
        }
      ],
      "properties": []
    }
  ],
  "functions": []
})";
}

int main() {
    try {
        std::cout << "=== ObjectIR Runtime Pipeline Example ===" << std::endl;
        std::cout << std::endl;

        // ====================================================================
        // STEP 1: Load module from JSON format
        // ====================================================================
        
        std::cout << "STEP 1: Loading ObjectIR module from JSON..." << std::endl;
        std::string moduleJson = BuildExampleJson();
        
        // Save to temp file for demonstration
        std::ofstream tempFile("temp_module.json");
        tempFile << moduleJson;
        tempFile.close();
        
        // Load the module using IRLoader
        auto vm = IRLoader::LoadFromFile("temp_module.json");
        std::cout << "✓ Module loaded successfully!" << std::endl;
        std::cout << std::endl;

        // ====================================================================
        // STEP 2: Inspect loaded classes
        // ====================================================================
        
        std::cout << "STEP 2: Inspecting loaded classes..." << std::endl;
        auto calcClass = vm->GetClass("Examples.Calculator");
        
        if (calcClass) {
            std::cout << "✓ Found class: " << calcClass->GetName() << std::endl;
            std::cout << "  Fields: " << calcClass->GetAllFields().size() << std::endl;
            for (const auto& field : calcClass->GetAllFields()) {
                std::cout << "    - " << field->GetName() << std::endl;
            }
            std::cout << "  Methods: " << calcClass->GetAllMethods().size() << std::endl;
            for (const auto& method : calcClass->GetAllMethods()) {
                std::cout << "    - " << method->GetName() << std::endl;
            }
        } else {
            std::cout << "✗ Class not found!" << std::endl;
        }
        std::cout << std::endl;

        // ====================================================================
        // STEP 3: Create instance and demonstrate native method binding
        // ====================================================================
        
        std::cout << "STEP 3: Creating calculator instance..." << std::endl;
        auto calcInstance = vm->CreateObject(calcClass);
        std::cout << "✓ Calculator instance created!" << std::endl;
        std::cout << std::endl;

        // ====================================================================
        // STEP 4: Demonstrate method invocation with native implementation
        // ====================================================================
        
        std::cout << "STEP 4: Binding native methods..." << std::endl;
        
        // Get methods and bind native implementations
        auto addMethod = calcClass->GetMethod("Add");
        if (addMethod) {
            // Bind native implementation
            NativeMethodImpl addImpl = [](ObjectRef thisObj, const std::vector<Value>& args, VirtualMachine* vm) -> Value {
                if (args.size() < 2) {
                    throw std::runtime_error("Add requires 2 arguments");
                }
                int32_t a = args[0].AsInt32();
                int32_t b = args[1].AsInt32();
                return Value(a + b);
            };
            addMethod->SetNativeImpl(addImpl);
            std::cout << "✓ Native 'Add' method bound" << std::endl;
        }
        
        auto mulMethod = calcClass->GetMethod("Multiply");
        if (mulMethod) {
            // Bind native implementation
            NativeMethodImpl mulImpl = [](ObjectRef thisObj, const std::vector<Value>& args, VirtualMachine* vm) -> Value {
                if (args.size() < 2) {
                    throw std::runtime_error("Multiply requires 2 arguments");
                }
                int32_t x = args[0].AsInt32();
                int32_t y = args[1].AsInt32();
                return Value(x * y);
            };
            mulMethod->SetNativeImpl(mulImpl);
            std::cout << "✓ Native 'Multiply' method bound" << std::endl;
        }
        std::cout << std::endl;

        // ====================================================================
        // STEP 5: Execute methods
        // ====================================================================
        
        std::cout << "STEP 5: Executing methods..." << std::endl;
        
        // Test Add method
        std::vector<Value> addArgs = { Value(15), Value(27) };
        Value addResult = vm->InvokeMethod(calcInstance, "Add", addArgs);
        std::cout << "  15 + 27 = " << addResult.AsInt32() << std::endl;
        
        // Test Multiply method
        std::vector<Value> mulArgs = { Value(6), Value(7) };
        Value mulResult = vm->InvokeMethod(calcInstance, "Multiply", mulArgs);
        std::cout << "  6 × 7 = " << mulResult.AsInt32() << std::endl;
        std::cout << std::endl;

        // ====================================================================
        // STEP 6: Store/retrieve field values
        // ====================================================================
        
        std::cout << "STEP 6: Working with instance fields..." << std::endl;
        calcInstance->SetField("lastResult", Value(42));
        Value lastResult = calcInstance->GetField("lastResult");
        std::cout << "  Set lastResult = 42" << std::endl;
        std::cout << "  Retrieved lastResult = " << lastResult.AsInt32() << std::endl;
        std::cout << std::endl;

        // ====================================================================
        // Summary
        // ====================================================================
        
        std::cout << "=== Pipeline Complete ===" << std::endl;
        std::cout << "✓ Successfully demonstrated:" << std::endl;
        std::cout << "  1. Loading ObjectIR JSON format" << std::endl;
        std::cout << "  2. Inspecting class metadata" << std::endl;
        std::cout << "  3. Creating runtime instances" << std::endl;
        std::cout << "  4. Binding native method implementations" << std::endl;
        std::cout << "  5. Invoking methods with arguments" << std::endl;
        std::cout << "  6. Storing and retrieving field values" << std::endl;
        std::cout << std::endl;
        
        std::cout << "Next steps:" << std::endl;
        std::cout << "  • Use C# ObjectIR backend to generate JSON from your code" << std::endl;
        std::cout << "  • Load generated JSON files with IRLoader::LoadFromFile()" << std::endl;
        std::cout << "  • Bind your native implementations to loaded classes" << std::endl;
        std::cout << "  • Scale to more complex modules and method hierarchies" << std::endl;

        // Cleanup
        std::remove("temp_module.json");
        
        return 0;

    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << std::endl;
        return 1;
    }
}
