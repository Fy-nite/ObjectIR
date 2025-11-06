#pragma once

#include <string>
#include <vector>
#include <optional>

namespace ObjectIR {

/// Enum of supported instruction types in the IR
enum class OpCode {
    // Stack operations
    Nop,
    Dup,
    Pop,
    
    // Load operations
    LdArg,
    LdLoc,
    LdFld,
    LdCon,
    LdStr,
    LdI4,
    LdI8,
    LdR4,
    LdR8,
    LdTrue,
    LdFalse,
    LdNull,
    
    // Store operations
    StLoc,
    StFld,
    StArg,
    
    // Arithmetic operations
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Neg,
    
    // Comparison operations
    Ceq,
    Cne,
    Clt,
    Cle,
    Cgt,
    Cge,
    
    // Control flow
    Ret,
    Br,
    BrTrue,
    BrFalse,
    
    // Object operations
    NewObj,
    Call,
    CallVirt,
    CastClass,
    IsInst,
    
    // Array operations
    NewArr,
    LdElem,
    StElem,
    LdLen,
    
    // Misc
    Break,
    Continue,
    Throw,
};

/// Data describing a method call target in the IR
struct CallTarget {
    std::string declaringType;
    std::string name;
    std::vector<std::string> parameterTypes;
    std::string returnType;
};

/// Represents a parsed instruction and its operands
struct Instruction {
    OpCode opCode = OpCode::Nop;

    // Generic operands
    std::string operandString;
    int32_t operandInt = 0;
    double operandDouble = 0.0;

    // Constant literal support
    bool hasConstant = false;
    std::string constantType;
    std::string constantRawValue;
    bool constantBool = false;
    bool constantIsNull = false;

    // Metadata for load/store of locals/args
    std::string identifier;

    // Method call target information
    std::optional<CallTarget> callTarget;
};

} // namespace ObjectIR
