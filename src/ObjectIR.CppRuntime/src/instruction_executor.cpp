#include "instruction_executor.hpp"
#include <algorithm>
#include <cmath>
#include <cctype>
#include <iostream>
#include <stdexcept>

namespace ObjectIR {

namespace {

struct BreakSignal : public std::exception {
    const char* what() const noexcept override { return "break"; }
};

struct ContinueSignal : public std::exception {
    const char* what() const noexcept override { return "continue"; }
};

std::string ToLowerInvariant(std::string value) {
    std::transform(value.begin(), value.end(), value.begin(), [](unsigned char c) {
        return static_cast<char>(std::tolower(c));
    });
    return value;
}

Instruction::ConditionData ParseConditionNode(const json& node);
std::vector<Instruction> ParseInstructionArray(const json& node);

bool EqualsIgnoreCase(const std::string& lhs, const std::string& rhs) {
    return ToLowerInvariant(lhs) == ToLowerInvariant(rhs);
}

Value CreateConstantValue(const Instruction& instr) {
    if (instr.constantIsNull) {
        return Value();
    }

    if (!instr.constantType.empty()) {
        auto typeLower = ToLowerInvariant(instr.constantType);

        if (typeLower == "system.string" || typeLower == "string") {
            return Value(instr.constantRawValue);
        }

        if (typeLower == "system.boolean" || typeLower == "bool" || typeLower == "boolean") {
            bool boolValue = instr.constantBool;
            if (instr.constantRawValue.empty()) {
                return Value(boolValue);
            }
            auto valueLower = ToLowerInvariant(instr.constantRawValue);
            if (valueLower == "true" || valueLower == "1") {
                boolValue = true;
            } else if (valueLower == "false" || valueLower == "0") {
                boolValue = false;
            }
            return Value(boolValue);
        }

        if (typeLower == "system.int32" || typeLower == "int32" || typeLower == "int") {
            return Value(static_cast<int32_t>(std::stoi(instr.constantRawValue)));
        }

        if (typeLower == "system.int64" || typeLower == "int64" || typeLower == "long") {
            return Value(static_cast<int64_t>(std::stoll(instr.constantRawValue)));
        }

        if (typeLower == "system.single" || typeLower == "single" || typeLower == "float" || typeLower == "float32") {
            return Value(static_cast<float>(std::stof(instr.constantRawValue)));
        }

        if (typeLower == "system.double" || typeLower == "double" || typeLower == "float64") {
            return Value(static_cast<double>(std::stod(instr.constantRawValue)));
        }
    }

    if (instr.constantBool) {
        return Value(instr.constantBool);
    }

    return Value(instr.constantRawValue);
}

std::string ValueToString(const Value& value) {
    if (value.IsNull()) {
        return "null";
    }
    if (value.IsString()) {
        return value.AsString();
    }
    if (value.IsInt32()) {
        return std::to_string(value.AsInt32());
    }
    if (value.IsInt64()) {
        return std::to_string(value.AsInt64());
    }
    if (value.IsFloat32()) {
        return std::to_string(value.AsFloat32());
    }
    if (value.IsFloat64()) {
        return std::to_string(value.AsFloat64());
    }
    if (value.IsBool()) {
        return value.AsBool() ? "true" : "false";
    }
    if (value.IsObject()) {
        return "<object>";
    }
    return "";
}

Instruction::ConditionData ParseConditionNode(const json& node) {
    Instruction::ConditionData data;

    if (!node.is_object()) {
        throw std::runtime_error("Condition node must be an object");
    }

    auto kindIt = node.find("kind");
    if (kindIt == node.end() || !kindIt->is_string()) {
        throw std::runtime_error("Condition kind missing");
    }

    auto kindStr = kindIt->get<std::string>();
    if (kindStr == "stack") {
        data.kind = ConditionKind::Stack;
    } else if (kindStr == "binary") {
        data.kind = ConditionKind::Binary;
    } else if (kindStr == "expression") {
        data.kind = ConditionKind::Expression;
    } else {
        throw std::runtime_error("Unsupported condition kind: " + kindStr);
    }

    if (data.kind == ConditionKind::Binary) {
        auto opIt = node.find("operation");
        if (opIt == node.end() || !opIt->is_string()) {
            throw std::runtime_error("Binary condition missing operation");
        }
        data.comparisonOp = InstructionExecutor::ParseOpCode(opIt->get<std::string>());
    }

    if (data.kind == ConditionKind::Expression) {
        auto exprIt = node.find("expression");
        if (exprIt != node.end()) {
            data.expressionInstructions.push_back(InstructionExecutor::ParseJsonInstruction(*exprIt));
        }
    }

    return data;
}

std::vector<Instruction> ParseInstructionArray(const json& node) {
    std::vector<Instruction> result;
    if (!node.is_array()) {
        return result;
    }

    result.reserve(node.size());
    for (const auto& element : node) {
        result.push_back(InstructionExecutor::ParseJsonInstruction(element));
    }
    return result;
}

} // namespace

OpCode InstructionExecutor::ParseOpCode(const std::string& opStr) {
    if (opStr == "nop") return OpCode::Nop;
    if (opStr == "dup") return OpCode::Dup;
    if (opStr == "pop") return OpCode::Pop;
    
    if (opStr == "ldarg") return OpCode::LdArg;
    if (opStr == "ldloc") return OpCode::LdLoc;
    if (opStr == "ldfld") return OpCode::LdFld;
    if (opStr == "ldcon" || opStr == "ldc") return OpCode::LdCon;
    if (opStr == "ldstr") return OpCode::LdStr;
    if (opStr == "ldi4" || opStr == "ldi32") return OpCode::LdI4;
    if (opStr == "ldi8" || opStr == "ldi64") return OpCode::LdI8;
    if (opStr == "ldr4") return OpCode::LdR4;
    if (opStr == "ldr8") return OpCode::LdR8;
    if (opStr == "ldtrue") return OpCode::LdTrue;
    if (opStr == "ldfalse") return OpCode::LdFalse;
    if (opStr == "ldnull") return OpCode::LdNull;
    
    if (opStr == "stloc") return OpCode::StLoc;
    if (opStr == "stfld") return OpCode::StFld;
    if (opStr == "starg") return OpCode::StArg;
    
    if (opStr == "add") return OpCode::Add;
    if (opStr == "sub") return OpCode::Sub;
    if (opStr == "mul") return OpCode::Mul;
    if (opStr == "div") return OpCode::Div;
    if (opStr == "rem") return OpCode::Rem;
    if (opStr == "neg") return OpCode::Neg;
    
    if (opStr == "ceq") return OpCode::Ceq;
    if (opStr == "cne") return OpCode::Cne;
    if (opStr == "clt") return OpCode::Clt;
    if (opStr == "cle") return OpCode::Cle;
    if (opStr == "cgt") return OpCode::Cgt;
    if (opStr == "cge") return OpCode::Cge;
    
    if (opStr == "ret") return OpCode::Ret;
    if (opStr == "br") return OpCode::Br;
    if (opStr == "brtrue") return OpCode::BrTrue;
    if (opStr == "brfalse") return OpCode::BrFalse;
    
    if (opStr == "if") return OpCode::If;
    
    if (opStr == "newobj") return OpCode::NewObj;
    if (opStr == "call") return OpCode::Call;
    if (opStr == "callvirt") return OpCode::CallVirt;
    if (opStr == "castclass") return OpCode::CastClass;
    if (opStr == "isinst") return OpCode::IsInst;
    
    if (opStr == "newarr") return OpCode::NewArr;
    if (opStr == "ldelem") return OpCode::LdElem;
    if (opStr == "stelem") return OpCode::StElem;
    if (opStr == "ldlen") return OpCode::LdLen;
    
    if (opStr == "break") return OpCode::Break;
    if (opStr == "continue") return OpCode::Continue;
    if (opStr == "throw") return OpCode::Throw;
    if (opStr == "while") return OpCode::While;
    
    throw std::runtime_error("Unknown opcode: " + opStr);
}

Instruction InstructionExecutor::ParseJsonInstruction(const json& instrJson) {
    Instruction instr;

    std::string opCodeStr = instrJson.value("opCode", "");
    instr.opCode = ParseOpCode(opCodeStr);

    if (!instrJson.contains("operand") || instrJson["operand"].is_null()) {
        return instr;
    }

    const auto& operand = instrJson["operand"];

    switch (instr.opCode) {
        case OpCode::LdArg:
        case OpCode::StArg:
            if (operand.contains("argumentName")) {
                instr.identifier = operand.value("argumentName", "");
            }
            break;

        case OpCode::LdLoc:
        case OpCode::StLoc:
            if (operand.contains("localName")) {
                instr.identifier = operand.value("localName", "");
            }
            break;

        case OpCode::LdCon:
        case OpCode::LdStr:
            instr.hasConstant = true;
            if (operand.contains("type")) {
                instr.constantType = operand.value("type", "");
            }
            if (operand.contains("value")) {
                const auto& valueNode = operand["value"];
                if (valueNode.is_string()) {
                    instr.constantRawValue = valueNode.get<std::string>();
                } else if (valueNode.is_number_integer()) {
                    instr.constantRawValue = std::to_string(valueNode.get<int64_t>());
                } else if (valueNode.is_number_float()) {
                    instr.constantRawValue = std::to_string(valueNode.get<double>());
                } else if (valueNode.is_boolean()) {
                    instr.constantBool = valueNode.get<bool>();
                    instr.constantRawValue = instr.constantBool ? "true" : "false";
                } else if (valueNode.is_null()) {
                    instr.constantIsNull = true;
                }
            } else {
                instr.constantIsNull = true;
            }
            break;

        case OpCode::Call:
        case OpCode::CallVirt:
            if (operand.contains("method")) {
                const auto& methodJson = operand["method"];
                CallTarget target;
                target.declaringType = methodJson.value("declaringType", "");
                target.name = methodJson.value("name", "");
                target.returnType = methodJson.value("returnType", "void");
                if (methodJson.contains("parameterTypes") && methodJson["parameterTypes"].is_array()) {
                    for (const auto& param : methodJson["parameterTypes"]) {
                        target.parameterTypes.push_back(param.get<std::string>());
                    }
                }
                instr.callTarget = std::move(target);
            }
            break;

        case OpCode::While: {
            if (!operand.is_object()) {
                throw std::runtime_error("While instruction operand must be object");
            }
            Instruction::WhileData data;
            if (operand.contains("condition")) {
                data.condition = ParseConditionNode(operand.at("condition"));
            }
            if (operand.contains("body")) {
                data.body = ParseInstructionArray(operand.at("body"));
            }
            instr.whileData = std::move(data);
            break;
        }

        case OpCode::If: {
            if (!operand.is_object()) {
                throw std::runtime_error("If instruction operand must be object");
            }
            Instruction::IfData data;
            if (operand.contains("thenBlock")) {
                data.thenBlock = ParseInstructionArray(operand.at("thenBlock"));
            }
            if (operand.contains("elseBlock")) {
                data.elseBlock = ParseInstructionArray(operand.at("elseBlock"));
            }
            instr.ifData = std::move(data);
            break;
        }

        default:
            if (operand.is_string()) {
                instr.operandString = operand.get<std::string>();
            } else if (operand.is_number_integer()) {
                instr.operandInt = operand.get<int32_t>();
            } else if (operand.is_number_float()) {
                instr.operandDouble = operand.get<double>();
            }
            break;
    }

    return instr;
}

void InstructionExecutor::Execute(
    const Instruction& instr,
    ExecutionContext* context,
    VirtualMachine* vm
) {
    if (!context) {
        throw std::runtime_error("Execution context is null");
    }
    
    switch (instr.opCode) {
        case OpCode::Nop:
            // No operation
            break;
            
        case OpCode::Dup: {
            auto val = context->PeekStack();
            context->PushStack(val);
            break;
        }
        
        case OpCode::Pop: {
            (void)context->PopStack();
            break;
        }

        case OpCode::LdArg: {
            context->PushStack(context->GetArgument(instr.identifier));
            break;
        }

        case OpCode::StArg: {
            auto value = context->PopStack();
            context->SetArgument(instr.identifier, value);
            break;
        }

        case OpCode::LdLoc: {
            context->PushStack(context->GetLocal(instr.identifier));
            break;
        }

        case OpCode::StLoc: {
            auto value = context->PopStack();
            context->SetLocal(instr.identifier, value);
            break;
        }

        case OpCode::LdCon:
        case OpCode::LdStr: {
            context->PushStack(CreateConstantValue(instr));
            break;
        }
        
        case OpCode::LdI4:
            context->PushStack(Value(instr.operandInt));
            break;
            
        case OpCode::LdI8:
            context->PushStack(Value(static_cast<int64_t>(instr.operandInt)));
            break;
            
        case OpCode::LdR4:
            context->PushStack(Value(static_cast<float>(instr.operandDouble)));
            break;
            
        case OpCode::LdR8:
            context->PushStack(Value(instr.operandDouble));
            break;
            
        case OpCode::LdTrue:
            context->PushStack(Value(true));
            break;
            
        case OpCode::LdFalse:
            context->PushStack(Value(false));
            break;
            
        case OpCode::LdNull:
            context->PushStack(Value());
            break;
            
        case OpCode::Add:
            ExecuteAdd(context);
            break;
            
        case OpCode::Sub:
            ExecuteSub(context);
            break;
            
        case OpCode::Mul:
            ExecuteMul(context);
            break;
            
        case OpCode::Div:
            ExecuteDiv(context);
            break;
            
        case OpCode::Rem:
            ExecuteRem(context);
            break;
            
        case OpCode::Neg:
            ExecuteNeg(context);
            break;
        
        case OpCode::Ceq:
            ExecuteCeq(context);
            break;
            
        case OpCode::Cne:
            ExecuteCne(context);
            break;
            
        case OpCode::Clt:
            ExecuteClt(context);
            break;
            
        case OpCode::Cle:
            ExecuteCle(context);
            break;
            
        case OpCode::Cgt:
            ExecuteCgt(context);
            break;
            
        case OpCode::Cge:
            ExecuteCge(context);
            break;
        
        case OpCode::Ret:
            // Return handled at higher level
            break;

        case OpCode::Call:
        case OpCode::CallVirt: {
            if (!instr.callTarget.has_value()) {
                throw std::runtime_error("Call instruction missing target metadata");
            }

            const auto& target = instr.callTarget.value();
            std::vector<Value> callArgs;
            callArgs.reserve(target.parameterTypes.size());
            for (size_t i = 0; i < target.parameterTypes.size(); ++i) {
                callArgs.push_back(context->PopStack());
            }
            std::reverse(callArgs.begin(), callArgs.end());

            auto isVoidReturn = target.returnType.empty() || target.returnType == "void" || target.returnType == "System.Void";

            if (target.declaringType == "System.Console" && target.name == "WriteLine") {
                if (callArgs.empty()) {
                    vm->WriteOutput("\n");
                } else {
                    for (size_t i = 0; i < callArgs.size(); ++i) {
                        if (i > 0) {
                            vm->WriteOutput(" ");
                        }
                        vm->WriteOutput(ValueToString(callArgs[i]));
                    }
                    vm->WriteOutput("\n");
                }
                break;
            }

            Value result;
            if (instr.opCode == OpCode::CallVirt) {
                auto instanceValue = context->PopStack();
                if (!instanceValue.IsObject()) {
                    throw std::runtime_error("CallVirt requires object instance on stack");
                }
                auto instance = instanceValue.AsObject();
                result = vm->InvokeMethod(instance, target.name, callArgs);
            } else {
                auto classRef = vm->GetClass(target.declaringType);
                result = vm->InvokeStaticMethod(classRef, target.name, callArgs);
            }

            if (!isVoidReturn) {
                context->PushStack(result);
            }

            break;
        }
        
        case OpCode::Break:
            throw BreakSignal();

        case OpCode::Continue:
            throw ContinueSignal();

        case OpCode::While: {
            if (!instr.whileData.has_value()) {
                throw std::runtime_error("While instruction missing metadata");
            }

            const auto& whileData = instr.whileData.value();

            while (EvaluateCondition(whileData.condition, context, vm)) {
                try {
                    for (const auto& bodyInstr : whileData.body) {
                        Execute(bodyInstr, context, vm);
                    }
                } catch (const ContinueSignal&) {
                    continue;
                } catch (const BreakSignal&) {
                    break;
                }
            }
            break;
        }

        case OpCode::If: {
            if (!instr.ifData.has_value()) {
                throw std::runtime_error("If instruction missing metadata");
            }

            const auto& ifData = instr.ifData.value();

            // Pop the condition value from the stack
            Value conditionValue = context->PopStack();
            bool condition = conditionValue.AsBool();

            if (condition) {
                // Execute then block
                for (const auto& thenInstr : ifData.thenBlock) {
                    Execute(thenInstr, context, vm);
                }
            } else if (!ifData.elseBlock.empty()) {
                // Execute else block if present
                for (const auto& elseInstr : ifData.elseBlock) {
                    Execute(elseInstr, context, vm);
                }
            }
            break;
        }

        case OpCode::Throw:
            throw std::runtime_error("Instruction not yet implemented: throw");
        
        default:
            throw std::runtime_error("Unknown instruction opcode");
    }
}

Value InstructionExecutor::ExecuteInstructions(
    const std::vector<Instruction>& instructions,
    ObjectRef thisPtr,
    const std::vector<Value>& args,
    ExecutionContext* context,
    VirtualMachine* vm
) {
    context->SetThis(thisPtr);
    context->SetArguments(args);
    
    for (size_t i = 0; i < instructions.size(); ++i) {
        const auto& instr = instructions[i];
        
        if (instr.opCode == OpCode::Ret) {
            try {
                return context->PopStack();
            } catch (...) {
                return Value();
            }
        }
        
        // Special handling for while loops with binary conditions
        // Need to re-execute setup instructions before each condition check
        if (instr.opCode == OpCode::While && instr.whileData.has_value()) {
            const auto& whileData = instr.whileData.value();
            if (whileData.condition.kind == ConditionKind::Binary) {
                // Look back to collect setup instructions (ldloc, ldc immediately before while)
                std::vector<Instruction> setupInstrs;
                int setupIdx = static_cast<int>(i) - 1;
                
                while (setupIdx >= 0) {
                    const auto& prevInstr = instructions[setupIdx];
                    if (prevInstr.opCode == OpCode::LdLoc || 
                        prevInstr.opCode == OpCode::LdCon ||
                        prevInstr.opCode == OpCode::LdI4 ||
                        prevInstr.opCode == OpCode::LdI8 ||
                        prevInstr.opCode == OpCode::LdR4 ||
                        prevInstr.opCode == OpCode::LdR8 ||
                        prevInstr.opCode == OpCode::LdTrue ||
                        prevInstr.opCode == OpCode::LdFalse ||
                        prevInstr.opCode == OpCode::LdNull) {
                        setupInstrs.insert(setupInstrs.begin(), prevInstr);
                        setupIdx--;
                    } else {
                        break;
                    }
                }
                
                // Execute while loop, re-running setup before each condition check
                while (true) {
                    // Re-execute setup to ensure values are on stack for condition check
                    for (const auto& setupInstr : setupInstrs) {
                        Execute(setupInstr, context, vm);
                    }
                    
                    // Evaluate condition (without running setup again)
                    bool cond_result;
                    switch (whileData.condition.kind) {
                        case ConditionKind::Binary: {
                            if (whileData.condition.comparisonOp == OpCode::Nop) {
                                throw std::runtime_error("Binary condition missing comparison operation");
                            }
                            auto right = context->PopStack();
                            auto left = context->PopStack();
                            context->PushStack(left);
                            context->PushStack(right);
                            
                            switch (whileData.condition.comparisonOp) {
                                case OpCode::Ceq: ExecuteCeq(context); break;
                                case OpCode::Cne: ExecuteCne(context); break;
                                case OpCode::Clt: ExecuteClt(context); break;
                                case OpCode::Cle: ExecuteCle(context); break;
                                case OpCode::Cgt: ExecuteCgt(context); break;
                                case OpCode::Cge: ExecuteCge(context); break;
                                default:
                                    throw std::runtime_error("Unsupported comparison opcode in binary condition");
                            }
                            
                            auto result = context->PopStack();
                            cond_result = ValueToBool(result);
                            break;
                        }
                        default:
                            throw std::runtime_error("Expected binary condition in special while handler");
                    }
                    
                    if (!cond_result) {
                        break;
                    }
                    
                    try {
                        for (const auto& bodyInstr : whileData.body) {
                            Execute(bodyInstr, context, vm);
                        }
                    } catch (const ContinueSignal&) {
                        continue;
                    } catch (const BreakSignal&) {
                        break;
                    }
                }
                continue;  // Skip normal Execute call
            }
        }
        
        Execute(instr, context, vm);
    }
    
    // If no explicit return, return last stack value if available
    try {
        return context->PopStack();
    } catch (...) {
        return Value();
    }
}

double InstructionExecutor::ValueToDouble(const Value& v) {
    if (v.IsInt32()) return static_cast<double>(v.AsInt32());
    if (v.IsInt64()) return static_cast<double>(v.AsInt64());
    if (v.IsFloat32()) return static_cast<double>(v.AsFloat32());
    if (v.IsFloat64()) return v.AsFloat64();
    throw std::runtime_error("Cannot convert value to double");
}

int64_t InstructionExecutor::ValueToInt64(const Value& v) {
    if (v.IsInt32()) return static_cast<int64_t>(v.AsInt32());
    if (v.IsInt64()) return v.AsInt64();
    if (v.IsFloat32()) return static_cast<int64_t>(v.AsFloat32());
    if (v.IsFloat64()) return static_cast<int64_t>(v.AsFloat64());
    throw std::runtime_error("Cannot convert value to int64");
}

void InstructionExecutor::ExecuteAdd(ExecutionContext* context) {
    auto b = context->PopStack();
    auto a = context->PopStack();
    
    if (a.IsString() || b.IsString()) {
        // String concatenation
        std::string result = a.AsString() + b.AsString();
        context->PushStack(Value(result));
    } else if (a.IsInt32() && b.IsInt32()) {
        context->PushStack(Value(a.AsInt32() + b.AsInt32()));
    } else if (a.IsInt64() || b.IsInt64()) {
        context->PushStack(Value(ValueToInt64(a) + ValueToInt64(b)));
    } else {
        context->PushStack(Value(ValueToDouble(a) + ValueToDouble(b)));
    }
}

void InstructionExecutor::ExecuteSub(ExecutionContext* context) {
    auto b = context->PopStack();
    auto a = context->PopStack();
    
    if (a.IsInt32() && b.IsInt32()) {
        context->PushStack(Value(a.AsInt32() - b.AsInt32()));
    } else if (a.IsInt64() || b.IsInt64()) {
        context->PushStack(Value(ValueToInt64(a) - ValueToInt64(b)));
    } else {
        context->PushStack(Value(ValueToDouble(a) - ValueToDouble(b)));
    }
}

void InstructionExecutor::ExecuteMul(ExecutionContext* context) {
    auto b = context->PopStack();
    auto a = context->PopStack();
    
    if (a.IsInt32() && b.IsInt32()) {
        context->PushStack(Value(a.AsInt32() * b.AsInt32()));
    } else if (a.IsInt64() || b.IsInt64()) {
        context->PushStack(Value(ValueToInt64(a) * ValueToInt64(b)));
    } else {
        context->PushStack(Value(ValueToDouble(a) * ValueToDouble(b)));
    }
}

void InstructionExecutor::ExecuteDiv(ExecutionContext* context) {
    auto b = context->PopStack();
    auto a = context->PopStack();
    
    if (b.IsInt32() && b.AsInt32() == 0) {
        throw std::runtime_error("Division by zero");
    }
    if (b.IsInt64() && b.AsInt64() == 0) {
        throw std::runtime_error("Division by zero");
    }
    
    if (a.IsInt32() && b.IsInt32()) {
        context->PushStack(Value(a.AsInt32() / b.AsInt32()));
    } else if (a.IsInt64() || b.IsInt64()) {
        context->PushStack(Value(ValueToInt64(a) / ValueToInt64(b)));
    } else {
        context->PushStack(Value(ValueToDouble(a) / ValueToDouble(b)));
    }
}

void InstructionExecutor::ExecuteRem(ExecutionContext* context) {
    auto b = context->PopStack();
    auto a = context->PopStack();
    
    if (a.IsInt32() && b.IsInt32()) {
        context->PushStack(Value(a.AsInt32() % b.AsInt32()));
    } else if (a.IsInt64() || b.IsInt64()) {
        context->PushStack(Value(ValueToInt64(a) % ValueToInt64(b)));
    } else {
        throw std::runtime_error("Modulo operation not supported for floating point");
    }
}

void InstructionExecutor::ExecuteNeg(ExecutionContext* context) {
    auto a = context->PopStack();
    
    if (a.IsInt32()) {
        context->PushStack(Value(-a.AsInt32()));
    } else if (a.IsInt64()) {
        context->PushStack(Value(-a.AsInt64()));
    } else if (a.IsFloat32()) {
        context->PushStack(Value(-a.AsFloat32()));
    } else if (a.IsFloat64()) {
        context->PushStack(Value(-a.AsFloat64()));
    }
}

void InstructionExecutor::ExecuteCeq(ExecutionContext* context) {
    auto b = context->PopStack();
    auto a = context->PopStack();
    
    bool result = false;
    if (a.IsInt32() && b.IsInt32()) {
        result = a.AsInt32() == b.AsInt32();
    } else if ((a.IsInt32() || a.IsInt64()) && (b.IsInt32() || b.IsInt64())) {
        result = ValueToInt64(a) == ValueToInt64(b);
    } else if (a.IsString() && b.IsString()) {
        result = a.AsString() == b.AsString();
    } else if (a.IsBool() && b.IsBool()) {
        result = a.AsBool() == b.AsBool();
    } else {
        result = ValueToDouble(a) == ValueToDouble(b);
    }
    
    context->PushStack(Value(result));
}

void InstructionExecutor::ExecuteCne(ExecutionContext* context) {
    auto b = context->PopStack();
    auto a = context->PopStack();
    
    bool result = false;
    if (a.IsInt32() && b.IsInt32()) {
        result = a.AsInt32() != b.AsInt32();
    } else if ((a.IsInt32() || a.IsInt64()) && (b.IsInt32() || b.IsInt64())) {
        result = ValueToInt64(a) != ValueToInt64(b);
    } else if (a.IsString() && b.IsString()) {
        result = a.AsString() != b.AsString();
    } else if (a.IsBool() && b.IsBool()) {
        result = a.AsBool() != b.AsBool();
    } else {
        result = ValueToDouble(a) != ValueToDouble(b);
    }
    
    context->PushStack(Value(result));
}

void InstructionExecutor::ExecuteClt(ExecutionContext* context) {
    auto b = context->PopStack();
    auto a = context->PopStack();
    
    bool result = false;
    if (a.IsInt32() && b.IsInt32()) {
        result = a.AsInt32() < b.AsInt32();
    } else if ((a.IsInt32() || a.IsInt64()) && (b.IsInt32() || b.IsInt64())) {
        result = ValueToInt64(a) < ValueToInt64(b);
    } else {
        result = ValueToDouble(a) < ValueToDouble(b);
    }
    
    context->PushStack(Value(result));
}

void InstructionExecutor::ExecuteCle(ExecutionContext* context) {
    auto b = context->PopStack();
    auto a = context->PopStack();
    
    bool result = false;
    if (a.IsInt32() && b.IsInt32()) {
        result = a.AsInt32() <= b.AsInt32();
    } else if ((a.IsInt32() || a.IsInt64()) && (b.IsInt32() || b.IsInt64())) {
        result = ValueToInt64(a) <= ValueToInt64(b);
    } else {
        result = ValueToDouble(a) <= ValueToDouble(b);
    }
    
    context->PushStack(Value(result));
}

void InstructionExecutor::ExecuteCgt(ExecutionContext* context) {
    auto b = context->PopStack();
    auto a = context->PopStack();
    
    bool result = false;
    if (a.IsInt32() && b.IsInt32()) {
        result = a.AsInt32() > b.AsInt32();
    } else if ((a.IsInt32() || a.IsInt64()) && (b.IsInt32() || b.IsInt64())) {
        result = ValueToInt64(a) > ValueToInt64(b);
    } else {
        result = ValueToDouble(a) > ValueToDouble(b);
    }
    
    context->PushStack(Value(result));
}

void InstructionExecutor::ExecuteCge(ExecutionContext* context) {
    auto b = context->PopStack();
    auto a = context->PopStack();
    
    bool result = false;
    if (a.IsInt32() && b.IsInt32()) {
        result = a.AsInt32() >= b.AsInt32();
    } else if ((a.IsInt32() || a.IsInt64()) && (b.IsInt32() || b.IsInt64())) {
        result = ValueToInt64(a) >= ValueToInt64(b);
    } else {
        result = ValueToDouble(a) >= ValueToDouble(b);
    }
    
    context->PushStack(Value(result));
}

bool InstructionExecutor::EvaluateCondition(
    const Instruction::ConditionData& condition,
    ExecutionContext* context,
    VirtualMachine* vm
) {
    for (const auto& setupInstr : condition.setupInstructions) {
        Execute(setupInstr, context, vm);
    }

    switch (condition.kind) {
        case ConditionKind::Stack: {
            auto value = context->PopStack();
            return ValueToBool(value);
        }

        case ConditionKind::Binary: {
            if (condition.comparisonOp == OpCode::Nop) {
                throw std::runtime_error("Binary condition missing comparison operation");
            }

            auto right = context->PopStack();
            auto left = context->PopStack();

            context->PushStack(left);
            context->PushStack(right);

            switch (condition.comparisonOp) {
                case OpCode::Ceq: ExecuteCeq(context); break;
                case OpCode::Cne: ExecuteCne(context); break;
                case OpCode::Clt: ExecuteClt(context); break;
                case OpCode::Cle: ExecuteCle(context); break;
                case OpCode::Cgt: ExecuteCgt(context); break;
                case OpCode::Cge: ExecuteCge(context); break;
                default:
                    throw std::runtime_error("Unsupported comparison opcode in binary condition");
            }

            auto result = context->PopStack();
            return ValueToBool(result);
        }

        case ConditionKind::Expression: {
            for (const auto& exprInstr : condition.expressionInstructions) {
                Execute(exprInstr, context, vm);
            }
            auto result = context->PopStack();
            return ValueToBool(result);
        }

        case ConditionKind::None:
        default:
            throw std::runtime_error("Condition kind not supported");
    }
}

bool InstructionExecutor::ValueToBool(const Value& value) {
    if (value.IsBool()) {
        return value.AsBool();
    }
    if (value.IsNull()) {
        return false;
    }
    if (value.IsInt32()) {
        return value.AsInt32() != 0;
    }
    if (value.IsInt64()) {
        return value.AsInt64() != 0;
    }
    if (value.IsFloat32()) {
        return value.AsFloat32() != 0.0f;
    }
    if (value.IsFloat64()) {
        return value.AsFloat64() != 0.0;
    }
    if (value.IsString()) {
        return !value.AsString().empty();
    }
    if (value.IsObject()) {
        return true;
    }
    return false;
}

} // namespace ObjectIR
