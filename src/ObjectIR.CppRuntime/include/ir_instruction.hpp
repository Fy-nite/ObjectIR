#pragma once

#include <string>
#include <vector>
#include <optional>
#include <cstdint>

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
    If,
    
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
    While,
};

/// Represents the kind of structured condition used by high-level control flow
enum class ConditionKind {
    None,
    Stack,
    Binary,
    Expression,
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

    struct ConditionData {
        ConditionKind kind = ConditionKind::None;
        OpCode comparisonOp = OpCode::Nop;
        std::vector<Instruction> setupInstructions;
        std::vector<Instruction> expressionInstructions;

        ConditionData() = default;
        ConditionData(const ConditionData& other)
            : kind(other.kind),
              comparisonOp(other.comparisonOp),
              setupInstructions(other.setupInstructions),
              expressionInstructions(other.expressionInstructions) {}
        ConditionData& operator=(const ConditionData& other) {
            if (this != &other) {
                kind = other.kind;
                comparisonOp = other.comparisonOp;
                setupInstructions = other.setupInstructions;
                expressionInstructions = other.expressionInstructions;
            }
            return *this;
        }
        ConditionData(ConditionData&&) noexcept = default;
        ConditionData& operator=(ConditionData&&) noexcept = default;
    };

    struct WhileData {
        ConditionData condition;
        std::vector<Instruction> body;

        WhileData() = default;
        WhileData(const WhileData& other)
            : condition(other.condition),
              body(other.body) {}
        WhileData& operator=(const WhileData& other) {
            if (this != &other) {
                condition = other.condition;
                body = other.body;
            }
            return *this;
        }
        WhileData(WhileData&&) noexcept = default;
        WhileData& operator=(WhileData&&) noexcept = default;
    };

    struct IfData {
        std::vector<Instruction> thenBlock;
        std::vector<Instruction> elseBlock;

        IfData() = default;
        IfData(const IfData& other)
            : thenBlock(other.thenBlock),
              elseBlock(other.elseBlock) {}
        IfData& operator=(const IfData& other) {
            if (this != &other) {
                thenBlock = other.thenBlock;
                elseBlock = other.elseBlock;
            }
            return *this;
        }
        IfData(IfData&&) noexcept = default;
        IfData& operator=(IfData&&) noexcept = default;
    };

    std::optional<WhileData> whileData;
    std::optional<IfData> ifData;

    Instruction() = default;
    Instruction(const Instruction& other)
        : opCode(other.opCode),
          operandString(other.operandString),
          operandInt(other.operandInt),
          operandDouble(other.operandDouble),
          hasConstant(other.hasConstant),
          constantType(other.constantType),
          constantRawValue(other.constantRawValue),
          constantBool(other.constantBool),
          constantIsNull(other.constantIsNull),
          identifier(other.identifier),
          callTarget(other.callTarget),
          whileData(other.whileData),
          ifData(other.ifData) {}

    Instruction& operator=(const Instruction& other) {
        if (this != &other) {
            opCode = other.opCode;
            operandString = other.operandString;
            operandInt = other.operandInt;
            operandDouble = other.operandDouble;
            hasConstant = other.hasConstant;
            constantType = other.constantType;
            constantRawValue = other.constantRawValue;
            constantBool = other.constantBool;
            constantIsNull = other.constantIsNull;
            identifier = other.identifier;
            callTarget = other.callTarget;
            whileData = other.whileData;
            ifData = other.ifData;
        }
        return *this;
    }

    Instruction(Instruction&&) noexcept = default;
    Instruction& operator=(Instruction&&) noexcept = default;
};

} // namespace ObjectIR
