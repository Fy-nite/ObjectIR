#pragma once

#include "objectir_runtime.hpp"
#include <nlohmann/json.hpp>

namespace ObjectIR {

using json = nlohmann::json;

// ============================================================================
// Instruction Executor - Executes IR instructions at runtime
// ============================================================================

/// Enum of supported instruction types in the IR
enum class OpCode {
    // Stack operations
    Nop,            // No operation
    Dup,            // Duplicate top stack value
    Pop,            // Pop top stack value
    
    // Load operations
    LdArg,          // Load argument
    LdLoc,          // Load local variable
    LdFld,          // Load field
    LdCon,          // Load constant
    LdStr,          // Load string
    LdI4,           // Load int32
    LdI8,           // Load int64
    LdR4,           // Load float32
    LdR8,           // Load float64
    LdTrue,         // Load true
    LdFalse,        // Load false
    LdNull,         // Load null
    
    // Store operations
    StLoc,          // Store to local variable
    StFld,          // Store to field
    StArg,          // Store to argument
    
    // Arithmetic operations
    Add,            // Add
    Sub,            // Subtract
    Mul,            // Multiply
    Div,            // Divide
    Rem,            // Remainder
    Neg,            // Negate
    
    // Comparison operations
    Ceq,            // Compare equal
    Cne,            // Compare not equal
    Clt,            // Compare less than
    Cle,            // Compare less than or equal
    Cgt,            // Compare greater than
    Cge,            // Compare greater than or equal
    
    // Control flow
    Ret,            // Return
    Br,             // Unconditional branch
    BrTrue,         // Branch if true
    BrFalse,        // Branch if false
    
    // Object operations
    NewObj,         // Create new object
    Call,           // Call method
    CallVirt,       // Call virtual method
    CastClass,      // Cast to class type
    IsInst,         // Check instance type
    
    // Array operations
    NewArr,         // Create new array
    LdElem,         // Load array element
    StElem,         // Store array element
    LdLen,          // Load array length
    
    // Misc
    Break,          // Breakpoint
    Continue,       // Continue
    Throw,          // Throw exception
};

/// Represents a parsed instruction
struct Instruction {
    OpCode opCode;
    std::string operandStr;  // String operand (for references, labels, etc.)
    int32_t operandInt = 0;  // Integer operand
    double operandDouble = 0.0;  // Double operand
};

/// Executes IR instructions within an execution context
class InstructionExecutor {
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
};

} // namespace ObjectIR
