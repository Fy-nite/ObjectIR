#pragma once

#include "objectir_runtime.hpp"
#include <iostream>
#include <memory>

namespace ObjectIR
{
    namespace Examples
    {

        // ============================================================================
        // Advanced Pattern 1: Fluent Interface Builder Pattern
        // ============================================================================

        class BankAccount : public Object
        {
        public:
            static void SetupInRuntime(RuntimeBuilder &builder, VirtualMachine *vm)
            {
                builder.Class("BankAccount")
                    .Field("accountNumber", TypeReference::String())
                    .Field("balance", TypeReference::Int64())
                    .Field("owner", TypeReference::String())

                    .Method("Deposit", TypeReference::Int64(), false)
                    .Parameter("amount", TypeReference::Int64())
                    .NativeImpl([](ObjectRef thisPtr, const std::vector<Value> &args, VirtualMachine *)
                                {
                    if (args.empty()) throw std::runtime_error("Deposit requires amount");
                    int64_t amount = args[0].AsInt64();
                    if (amount < 0) throw std::runtime_error("Amount cannot be negative");
                    
                    int64_t balance = thisPtr->GetField("balance").AsInt64();
                    int64_t newBalance = balance + amount;
                    thisPtr->SetField("balance", Value(newBalance));
                    
                    std::cout << "Deposited: " << amount << ", New balance: " << newBalance << std::endl;
                    return Value(newBalance); })
                    .EndMethod()

                    .Method("Withdraw", TypeReference::Int64(), false)
                    .Parameter("amount", TypeReference::Int64())
                    .NativeImpl([](ObjectRef thisPtr, const std::vector<Value> &args, VirtualMachine *)
                                {
                    if (args.empty()) throw std::runtime_error("Withdraw requires amount");
                    int64_t amount = args[0].AsInt64();
                    
                    int64_t balance = thisPtr->GetField("balance").AsInt64();
                    if (amount > balance) throw std::runtime_error("Insufficient funds");
                    
                    int64_t newBalance = balance - amount;
                    thisPtr->SetField("balance", Value(newBalance));
                    
                    std::cout << "Withdrew: " << amount << ", New balance: " << newBalance << std::endl;
                    return Value(newBalance); })
                    .EndMethod()

                    .Method("GetBalance", TypeReference::Int64(), false)
                    .NativeImpl([](ObjectRef thisPtr, const std::vector<Value> &, VirtualMachine *)
                                { return thisPtr->GetField("balance"); })
                    .EndMethod()

                    .Method("GetAccountInfo", TypeReference::String(), false)
                    .NativeImpl([](ObjectRef thisPtr, const std::vector<Value> &, VirtualMachine *)
                                {
                    std::string owner = thisPtr->GetField("owner").AsString();
                    std::string accountNum = thisPtr->GetField("accountNumber").AsString();
                    int64_t balance = thisPtr->GetField("balance").AsInt64();
                    
                    std::string info = owner + " (" + accountNum + "): $" + std::to_string(balance);
                    return Value(info); })
                    .EndMethod()

                    .EndClass();
            }
        };

        // ============================================================================
        // Advanced Pattern 2: State Machine Pattern
        // ============================================================================

        class TrafficLight : public Object
        {
        public:
            static void SetupInRuntime(RuntimeBuilder &builder, VirtualMachine *)
            {
                builder.Class("TrafficLight")
                    .Field("state", TypeReference::String()) // "RED", "YELLOW", "GREEN"
                    .Field("duration", TypeReference::Int32())

                    .Method("GetState", TypeReference::String(), false)
                    .NativeImpl([](ObjectRef thisPtr, const std::vector<Value> &, VirtualMachine *)
                                { return thisPtr->GetField("state"); })
                    .EndMethod()

                    .Method("ChangeState", TypeReference::String(), false)
                    .NativeImpl([](ObjectRef thisPtr, const std::vector<Value> &, VirtualMachine *)
                                {
                    std::string currentState = thisPtr->GetField("state").AsString();
                    std::string nextState;
                    
                    if (currentState == "RED") {
                        nextState = "GREEN";
                    } else if (currentState == "GREEN") {
                        nextState = "YELLOW";
                    } else if (currentState == "YELLOW") {
                        nextState = "RED";
                    } else {
                        nextState = "RED";
                    }
                    
                    thisPtr->SetField("state", Value(nextState));
                    std::cout << "Light changed: " << currentState << " → " << nextState << std::endl;
                    return Value(nextState); })
                    .EndMethod()

                    .EndClass();
            }
        };

        // ============================================================================
        // Advanced Pattern 3: Delegation Pattern
        // ============================================================================

        class Logger : public Object
        {
        public:
            static void SetupInRuntime(RuntimeBuilder &builder, VirtualMachine *)
            {
                builder.Class("Logger")
                    .Field("level", TypeReference::Int32()) // 0=DEBUG, 1=INFO, 2=WARN, 3=ERROR
                    .Field("prefix", TypeReference::String())

                    .Method("SetLevel", TypeReference::Void(), false)
                    .Parameter("level", TypeReference::Int32())
                    .NativeImpl([](ObjectRef thisPtr, const std::vector<Value> &args, VirtualMachine *)
                                {
                    if (args.empty()) throw std::runtime_error("SetLevel requires level");
                    thisPtr->SetField("level", args[0]);
                    return Value(); })
                    .EndMethod()

                    .Method("Log", TypeReference::Void(), false)
                    .Parameter("message", TypeReference::String())
                    .NativeImpl([](ObjectRef thisPtr, const std::vector<Value> &args, VirtualMachine *)
                                {
                    if (args.empty()) throw std::runtime_error("Log requires message");
                    
                    std::string prefix = thisPtr->GetField("prefix").AsString();
                    std::string message = args[0].AsString();
                    
                    std::cout << "[" << prefix << "] " << message << std::endl;
                    return Value(); })
                    .EndMethod()

                    .EndClass();
            }
        };

        // ============================================================================
        // Advanced Pattern 4: Template Method Pattern
        // ============================================================================

        class DataProcessor : public Object
        {
        public:
            static void SetupInRuntime(RuntimeBuilder &builder, VirtualMachine *)
            {
                builder.Class("DataProcessor")
                    .Field("itemsProcessed", TypeReference::Int32())

                    .Method("Process", TypeReference::Bool(), false)
                    .Parameter("data", TypeReference::String())
                    .NativeImpl([](ObjectRef thisPtr, const std::vector<Value> &args, VirtualMachine *)
                                {
                    if (args.empty()) throw std::runtime_error("Process requires data");
                    
                    // Template method steps:
                    // 1. Validate
                    std::string data = args[0].AsString();
                    bool isValid = !data.empty();
                    
                    if (!isValid) {
                        std::cout << "Validation failed" << std::endl;
                        return Value(false);
                    }
                    
                    // 2. Transform
                    std::string transformed = "[PROCESSED] " + data;
                    std::cout << "Transformed: " << transformed << std::endl;
                    
                    // 3. Store
                    int32_t count = thisPtr->GetField("itemsProcessed").AsInt32();
                    thisPtr->SetField("itemsProcessed", Value(count + 1));
                    
                    return Value(true); })
                    .EndMethod()

                    .Method("GetProcessedCount", TypeReference::Int32(), false)
                    .NativeImpl([](ObjectRef thisPtr, const std::vector<Value> &, VirtualMachine *)
                                { return thisPtr->GetField("itemsProcessed"); })
                    .EndMethod()

                    .EndClass();
            }
        };

        // ============================================================================
        // Advanced Pattern 5: Observer Pattern Simplified
        // ============================================================================

        class Observable : public Object
        {
        public:
            static void SetupInRuntime(RuntimeBuilder &builder, VirtualMachine *)
            {
                builder.Class("Observable")
                    .Field("value", TypeReference::Int32())
                    .Field("lastNotifiedValue", TypeReference::Int32())

                    .Method("SetValue", TypeReference::Void(), false)
                    .Parameter("newValue", TypeReference::Int32())
                    .NativeImpl([](ObjectRef thisPtr, const std::vector<Value> &args, VirtualMachine *)
                                {
                    if (args.empty()) throw std::runtime_error("SetValue requires value");
                    
                    int32_t newVal = args[0].AsInt32();
                    int32_t oldVal = thisPtr->GetField("value").AsInt32();
                    
                    thisPtr->SetField("value", Value(newVal));
                    
                    if (newVal != oldVal) {
                        std::cout << "VALUE CHANGED: " << oldVal << " → " << newVal << std::endl;
                        thisPtr->SetField("lastNotifiedValue", Value(newVal));
                    }
                    
                    return Value(); })
                    .EndMethod()

                    .Method("GetValue", TypeReference::Int32(), false)
                    .NativeImpl([](ObjectRef thisPtr, const std::vector<Value> &, VirtualMachine *)
                                { return thisPtr->GetField("value"); })
                    .EndMethod()

                    .EndClass();
            }
        };

        // ============================================================================
        // Utility: PatternExamples Runner
        // ============================================================================

        class PatternExamples
        {
        public:
            static void RunAll()
            {
                std::cout << "=== ObjectIR C++ Runtime: Advanced Patterns ===\n\n";

                RunBankingExample();
                RunStateMachineExample();
                RunDataProcessingExample();
                RunObservableExample();
            }

        private:
            static void RunBankingExample()
            {
                std::cout << "\n--- Banking Example (Fluent Pattern) ---\n";

                RuntimeBuilder builder;
                auto vm = builder.Release();

                BankAccount::SetupInRuntime(builder, vm.get());

                try
                {
                    auto account = vm->CreateObject("BankAccount");
                    account->SetField("accountNumber", Value(std::string("ACC-12345")));
                    account->SetField("owner", Value(std::string("Alice Smith")));
                    account->SetField("balance", Value(int64_t(1000)));

                    vm->InvokeMethod(account, "Deposit", {Value(int64_t(500))});
                    vm->InvokeMethod(account, "Withdraw", {Value(int64_t(200))});

                    auto info = vm->InvokeMethod(account, "GetAccountInfo", {});
                    std::cout << "Account Info: " << info.AsString() << std::endl;
                }
                catch (const std::exception &e)
                {
                    std::cerr << "Error: " << e.what() << std::endl;
                }
            }

            static void RunStateMachineExample()
            {
                std::cout << "\n--- Traffic Light Example (State Machine Pattern) ---\n";

                RuntimeBuilder builder;
                auto vm = builder.Release();

                TrafficLight::SetupInRuntime(builder, vm.get());

                try
                {
                    auto light = vm->CreateObject("TrafficLight");
                    light->SetField("state", Value(std::string("RED")));
                    light->SetField("duration", Value(int32_t(30)));

                    for (int i = 0; i < 3; ++i)
                    {
                        vm->InvokeMethod(light, "ChangeState", {});
                    }
                }
                catch (const std::exception &e)
                {
                    std::cerr << "Error: " << e.what() << std::endl;
                }
            }

            static void RunDataProcessingExample()
            {
                std::cout << "\n--- Data Processing Example (Template Method Pattern) ---\n";

                RuntimeBuilder builder;
                auto vm = builder.Release();

                DataProcessor::SetupInRuntime(builder, vm.get());
                Logger::SetupInRuntime(builder, vm.get());

                try
                {
                    auto processor = vm->CreateObject("DataProcessor");
                    processor->SetField("itemsProcessed", Value(int32_t(0)));

                    vm->InvokeMethod(processor, "Process", {Value(std::string("data1"))});
                    vm->InvokeMethod(processor, "Process", {Value(std::string("data2"))});
                    vm->InvokeMethod(processor, "Process", {Value(std::string(""))}); // Invalid
                    vm->InvokeMethod(processor, "Process", {Value(std::string("data3"))});

                    auto count = vm->InvokeMethod(processor, "GetProcessedCount", {});
                    std::cout << "Total Processed: " << count.AsInt32() << std::endl;
                }
                catch (const std::exception &e)
                {
                    std::cerr << "Error: " << e.what() << std::endl;
                }
            }

            static void RunObservableExample()
            {
                std::cout << "\n--- Observable Example (Observer Pattern) ---\n";

                RuntimeBuilder builder;
                auto vm = builder.Release();

                Observable::SetupInRuntime(builder, vm.get());

                try
                {
                    auto observable = vm->CreateObject("Observable");
                    observable->SetField("value", Value(int32_t(0)));
                    observable->SetField("lastNotifiedValue", Value(int32_t(0)));

                    vm->InvokeMethod(observable, "SetValue", {Value(int32_t(10))});
                    vm->InvokeMethod(observable, "SetValue", {Value(int32_t(10))}); // No change
                    vm->InvokeMethod(observable, "SetValue", {Value(int32_t(20))});
                }
                catch (const std::exception &e)
                {
                    std::cerr << "Error: " << e.what() << std::endl;
                }
            }
        };

    } // namespace Examples
} // namespace ObjectIR
