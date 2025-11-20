#pragma once

#include "objectir_runtime.hpp"
#include "ir_instruction.hpp"
#include <nlohmann/json.hpp>

namespace ObjectIR {

using json = nlohmann::json;

// ============================================================================
// Instruction Executor - Executes IR instructions at runtime
// ============================================================================

/// Executes IR instructions within an execution context
class OBJECTIR_API InstructionExecutor {
public:
    /// Parse opcode string to enum
    static OpCode ParseOpCode(const std::string& opStr);
    
    /// Parse JSON instruction to Instruction struct
    static Instruction ParseJsonInstruction(const json& instrJson);
    
    /// Execute a single instruction in the given context
    static void Execute(
        const Instruction& instr,
        ExecutionContext* context,
        VirtualMachine* vm
    );
    
    /// Execute a sequence of instructions
    static Value ExecuteInstructions(
        const std::vector<Instruction>& instructions,
        ObjectRef thisPtr,
        const std::vector<Value>& args,
        ExecutionContext* context,
        VirtualMachine* vm
    );
    
private:
    InstructionExecutor() = default;
    
    // Arithmetic instruction handlers
    static void ExecuteAdd(ExecutionContext* context);
    static void ExecuteSub(ExecutionContext* context);
    static void ExecuteMul(ExecutionContext* context);
    static void ExecuteDiv(ExecutionContext* context);
    static void ExecuteRem(ExecutionContext* context);
    static void ExecuteNeg(ExecutionContext* context);
    
    // Comparison instruction handlers
    static void ExecuteCeq(ExecutionContext* context);
    static void ExecuteCne(ExecutionContext* context);
    static void ExecuteClt(ExecutionContext* context);
    static void ExecuteCle(ExecutionContext* context);
    static void ExecuteCgt(ExecutionContext* context);
    static void ExecuteCge(ExecutionContext* context);
    
    // Helper to convert values to double for arithmetic
    static double ValueToDouble(const Value& v);
    static int64_t ValueToInt64(const Value& v);
    static bool EvaluateCondition(
        const Instruction::ConditionData& condition,
        ExecutionContext* context,
        VirtualMachine* vm
    );
    static bool ValueToBool(const Value& value);
};

} // namespace ObjectIR
