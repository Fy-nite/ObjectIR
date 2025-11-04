namespace ObjectIR.Core.IR;

/// <summary>
/// OpCodes for all instructions
/// </summary>
public enum OpCode
{
    // Load instructions
    Ldarg,
    Ldloc,
    Ldfld,
    Ldsfld,
    Ldelem,
    Ldlen,
    Ldnull,
    LdcI4,
    LdcI8,
    LdcR4,
    LdcR8,
    Ldstr,
    
    // Store instructions
    Starg,
    Stloc,
    Stfld,
    Stsfld,
    Stelem,
    
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Neg,
    And,
    Or,
    Xor,
    Not,
    Shl,
    Shr,
    
    // Comparison
    Ceq,
    Cgt,
    Clt,
    
    // Control flow
    Br,
    Brtrue,
    Brfalse,
    Beq,
    Bne,
    Bgt,
    Blt,
    Ret,
    
    // Calls
    Call,
    Callvirt,
    Calli,
    Newobj,
    
    // Object operations
    Newarr,
    Castclass,
    Isinst,
    Box,
    Unbox,
    
    // Stack manipulation
    Dup,
    Pop,
    
    // Conversions
    ConvI4,
    ConvI8,
    ConvR4,
    ConvR8,
    ConvU4,
    ConvU8,
    
    // Structured control flow (high-level)
    If,
    While,
    For,
    Switch,
    Try,
    Break,
    Continue,
    Throw
}

public enum ArithmeticOp
{
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Neg,
    And,
    Or,
    Xor,
    Not,
    Shl,
    Shr
}

public enum ComparisonOp
{
    Equal,
    NotEqual,
    Greater,
    GreaterOrEqual,
    Less,
    LessOrEqual
}
