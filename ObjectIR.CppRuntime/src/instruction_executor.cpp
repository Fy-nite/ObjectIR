#include "instruction_executor.hpp"
#include <algorithm>
#include <cmath>

namespace ObjectIR {

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
    
    throw std::runtime_error("Unknown opcode: " + opStr);
}

Instruction InstructionExecutor::ParseJsonInstruction(const json& instrJson) {
    Instruction instr;
    
    std::string opCodeStr = instrJson.value("opCode", "");
    instr.opCode = ParseOpCode(opCodeStr);
    
    if (instrJson.contains("operand") && !instrJson["operand"].is_null()) {
        const auto& operand = instrJson["operand"];
        
        if (operand.is_string()) {
            instr.operandStr = operand.get<std::string>();
        } else if (operand.is_number_integer()) {
            instr.operandInt = operand.get<int32_t>();
        } else if (operand.is_number_float()) {
            instr.operandDouble = operand.get<double>();
        } else if (operand.is_object()) {
            // Handle complex operands
            if (operand.contains("value")) {
                if (operand["value"].is_number_integer()) {
                    instr.operandInt = operand["value"].get<int32_t>();
                } else if (operand["value"].is_number_float()) {
                    instr.operandDouble = operand["value"].get<double>();
                } else if (operand["value"].is_string()) {
                    instr.operandStr = operand["value"].get<std::string>();
                }
            }
        }
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
            context->PopStack();
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
            
        case OpCode::LdStr:
            context->PushStack(Value(instr.operandStr));
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
        
        case OpCode::Break:
        case OpCode::Continue:
        case OpCode::Throw:
            throw std::runtime_error("Instruction not yet implemented: " + std::to_string(static_cast<int>(instr.opCode)));
        
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
    
    for (const auto& instr : instructions) {
        if (instr.opCode == OpCode::Ret) {
            // Return the top stack value
            if (!instructions.empty()) {
                return context->PopStack();
            }
            return Value();
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

} // namespace ObjectIR
