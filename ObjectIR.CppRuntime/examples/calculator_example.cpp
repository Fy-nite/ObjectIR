#include "../include/objectir_runtime.hpp"
#include <iostream>

using namespace ObjectIR;

int main() {
    std::cout << "=== ObjectIR C++ Runtime: Calculator Example ===\n\n";
    
    // Create a runtime builder
    RuntimeBuilder builder;
    
    // Define the Calculator class
    builder.Class("Calculator")
        .Field("lastResult", TypeReference::Int32())
        .Field("history", TypeReference::Int32())  // Simplified: just count operations
        
        .Method("Add", TypeReference::Int32(), false)
            .Parameter("a", TypeReference::Int32())
            .Parameter("b", TypeReference::Int32())
            .NativeImpl([](ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine*) {
                if (args.size() < 2) throw std::runtime_error("Add requires 2 arguments");
                int32_t result = args[0].AsInt32() + args[1].AsInt32();
                thisPtr->SetField("lastResult", Value(result));
                
                // Increment history counter
                int32_t count = thisPtr->GetField("history").AsInt32();
                thisPtr->SetField("history", Value(count + 1));
                
                std::cout << args[0].AsInt32() << " + " << args[1].AsInt32() 
                          << " = " << result << std::endl;
                return Value(result);
            })
            .EndMethod()
        
        .Method("Subtract", TypeReference::Int32(), false)
            .Parameter("a", TypeReference::Int32())
            .Parameter("b", TypeReference::Int32())
            .NativeImpl([](ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine*) {
                if (args.size() < 2) throw std::runtime_error("Subtract requires 2 arguments");
                int32_t result = args[0].AsInt32() - args[1].AsInt32();
                thisPtr->SetField("lastResult", Value(result));
                
                int32_t count = thisPtr->GetField("history").AsInt32();
                thisPtr->SetField("history", Value(count + 1));
                
                std::cout << args[0].AsInt32() << " - " << args[1].AsInt32() 
                          << " = " << result << std::endl;
                return Value(result);
            })
            .EndMethod()
        
        .Method("Multiply", TypeReference::Int32(), false)
            .Parameter("a", TypeReference::Int32())
            .Parameter("b", TypeReference::Int32())
            .NativeImpl([](ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine*) {
                if (args.size() < 2) throw std::runtime_error("Multiply requires 2 arguments");
                int32_t result = args[0].AsInt32() * args[1].AsInt32();
                thisPtr->SetField("lastResult", Value(result));
                
                int32_t count = thisPtr->GetField("history").AsInt32();
                thisPtr->SetField("history", Value(count + 1));
                
                std::cout << args[0].AsInt32() << " * " << args[1].AsInt32() 
                          << " = " << result << std::endl;
                return Value(result);
            })
            .EndMethod()
        
        .Method("Divide", TypeReference::Int32(), false)
            .Parameter("a", TypeReference::Int32())
            .Parameter("b", TypeReference::Int32())
            .NativeImpl([](ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine*) {
                if (args.size() < 2) throw std::runtime_error("Divide requires 2 arguments");
                if (args[1].AsInt32() == 0) throw std::runtime_error("Division by zero");
                
                int32_t result = args[0].AsInt32() / args[1].AsInt32();
                thisPtr->SetField("lastResult", Value(result));
                
                int32_t count = thisPtr->GetField("history").AsInt32();
                thisPtr->SetField("history", Value(count + 1));
                
                std::cout << args[0].AsInt32() << " / " << args[1].AsInt32() 
                          << " = " << result << std::endl;
                return Value(result);
            })
            .EndMethod()
        
        .Method("GetLastResult", TypeReference::Int32(), false)
            .NativeImpl([](ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine*) {
                return thisPtr->GetField("lastResult");
            })
            .EndMethod()
        
        .Method("GetOperationCount", TypeReference::Int32(), false)
            .NativeImpl([](ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine*) {
                return thisPtr->GetField("history");
            })
            .EndMethod()
        
        .EndClass();
    
    // Build the runtime
    auto vm = builder.Release();
    
    // Create a Calculator instance
    try {
        auto calcClass = vm->GetClass("Calculator");
        auto calc = vm->CreateObject(calcClass);
        
        // Initialize fields
        calc->SetField("lastResult", Value(0));
        calc->SetField("history", Value(0));
        
        std::cout << "\n--- Testing Calculator ---\n";
        
        // Test operations
        vm->InvokeMethod(calc, "Add", {Value(10), Value(5)});
        vm->InvokeMethod(calc, "Subtract", {Value(20), Value(7)});
        vm->InvokeMethod(calc, "Multiply", {Value(6), Value(4)});
        vm->InvokeMethod(calc, "Divide", {Value(100), Value(5)});
        
        // Get results
        auto lastResult = vm->InvokeMethod(calc, "GetLastResult", {});
        auto opCount = vm->InvokeMethod(calc, "GetOperationCount", {});
        
        std::cout << "\nLast Result: " << lastResult.AsInt32() << std::endl;
        std::cout << "Operations Performed: " << opCount.AsInt32() << std::endl;
        
    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << std::endl;
        return 1;
    }
    
    std::cout << "\n✓ Example completed successfully!\n";
    return 0;
}
