#include "../include/objectir_runtime.hpp"
#include <iostream>
#include <memory>

using namespace ObjectIR;

int main() {
    std::cout << "=== ObjectIR C++ Runtime: TodoApp Example ===\n\n";
    
    // Create a runtime builder
    RuntimeBuilder builder;
    
    // Define TodoItem class
    builder.Class("TodoItem")
        .Field("id", TypeReference::Int32())
        .Field("description", TypeReference::String())
        .Field("isComplete", TypeReference::Bool())
        
        .Method("GetId", TypeReference::Int32(), false)
            .NativeImpl([](ObjectRef thisPtr, const std::vector<Value>&, VirtualMachine*) {
                return thisPtr->GetField("id");
            })
            .EndMethod()
        
        .Method("GetDescription", TypeReference::String(), false)
            .NativeImpl([](ObjectRef thisPtr, const std::vector<Value>&, VirtualMachine*) {
                return thisPtr->GetField("description");
            })
            .EndMethod()
        
        .Method("IsComplete", TypeReference::Bool(), false)
            .NativeImpl([](ObjectRef thisPtr, const std::vector<Value>&, VirtualMachine*) {
                return thisPtr->GetField("isComplete");
            })
            .EndMethod()
        
        .Method("MarkComplete", TypeReference::Void(), false)
            .NativeImpl([](ObjectRef thisPtr, const std::vector<Value>&, VirtualMachine*) {
                thisPtr->SetField("isComplete", Value(true));
                return Value();
            })
            .EndMethod()
        
        .EndClass();
    
    // Define TodoList class
    builder.Class("TodoList")
        .Field("nextId", TypeReference::Int32())
        .Field("itemCount", TypeReference::Int32())
        
        .Method("AddItem", TypeReference::Int32(), false)
            .Parameter("description", TypeReference::String())
            .NativeImpl([](ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
                if (args.empty()) throw std::runtime_error("AddItem requires description argument");
                
                int32_t newId = thisPtr->GetField("nextId").AsInt32();
                auto todoItemClass = vm->GetClass("TodoItem");
                auto item = vm->CreateObject(todoItemClass);
                
                item->SetField("id", Value(newId));
                item->SetField("description", args[0]);
                item->SetField("isComplete", Value(false));
                
                // Increment ID and item count
                thisPtr->SetField("nextId", Value(newId + 1));
                int32_t count = thisPtr->GetField("itemCount").AsInt32();
                thisPtr->SetField("itemCount", Value(count + 1));
                
                std::cout << "Added todo #" << newId << ": \"" << args[0].AsString() << "\"" << std::endl;
                
                return Value(newId);
            })
            .EndMethod()
        
        .Method("GetItemCount", TypeReference::Int32(), false)
            .NativeImpl([](ObjectRef thisPtr, const std::vector<Value>&, VirtualMachine*) {
                return thisPtr->GetField("itemCount");
            })
            .EndMethod()
        
        .EndClass();
    
    // Build the runtime
    auto vm = builder.Release();
    
    // Use the TodoApp
    try {
        std::cout << "\n--- Creating and Managing Todo Items ---\n\n";
        
        auto todoListClass = vm->GetClass("TodoList");
        auto todoList = vm->CreateObject(todoListClass);
        
        // Initialize fields
        todoList->SetField("nextId", Value(1));
        todoList->SetField("itemCount", Value(0));
        
        // Add some todo items
        auto id1 = vm->InvokeMethod(todoList, "AddItem", {Value(std::string("Learn C++"))});
        auto id2 = vm->InvokeMethod(todoList, "AddItem", {Value(std::string("Build ObjectIR runtime"))});
        auto id3 = vm->InvokeMethod(todoList, "AddItem", {Value(std::string("Test OOP features"))});
        
        std::cout << "\nTodo Items Added:\n";
        std::cout << "  - ID " << id1.AsInt32() << ": Learn C++\n";
        std::cout << "  - ID " << id2.AsInt32() << ": Build ObjectIR runtime\n";
        std::cout << "  - ID " << id3.AsInt32() << ": Test OOP features\n";
        
        auto count = vm->InvokeMethod(todoList, "GetItemCount", {});
        std::cout << "\nTotal Items: " << count.AsInt32() << "\n";
        
        std::cout << "\n✓ TodoApp example completed successfully!\n";
        
    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << std::endl;
        return 1;
    }
    
    return 0;
}
