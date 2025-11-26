namespace ObjectIR.Core.IR;

// ============================================================================
// Load Instructions
// ============================================================================

public sealed class LoadArgInstruction : Instruction
{
    public string ArgumentName { get; set; }

    public LoadArgInstruction(string argumentName) : base(OpCode.Ldarg)
    {
        ArgumentName = argumentName;
    }

    public override void Accept(IInstructionVisitor visitor) => visitor.Visit(this);
}

public sealed class LoadLocalInstruction : Instruction
{
    public string LocalName { get; set; }

    public LoadLocalInstruction(string localName) : base(OpCode.Ldloc)
    {
        LocalName = localName;
    }

    public override void Accept(IInstructionVisitor visitor) => visitor.Visit(this);
}

public sealed class LoadFieldInstruction : Instruction
{
    public FieldReference Field { get; set; }

    public LoadFieldInstruction(FieldReference field) : base(OpCode.Ldfld)
    {
        Field = field;
    }

    public override void Accept(IInstructionVisitor visitor) => visitor.Visit(this);
}

public sealed class LoadStaticFieldInstruction : Instruction
{
    public FieldReference Field { get; set; }

    public LoadStaticFieldInstruction(FieldReference field) : base(OpCode.Ldsfld)
    {
        Field = field;
    }

    public override void Accept(IInstructionVisitor visitor) => visitor.Visit(this);
}

public sealed class LoadConstantInstruction : Instruction
{
    public object Value { get; set; }
    public TypeReference Type { get; set; }

    public LoadConstantInstruction(object value, TypeReference type) : base(GetOpCode(type))
    {
        Value = value;
        Type = type;
    }

    private static OpCode GetOpCode(TypeReference type)
    {
        if (type == TypeReference.Int32) return OpCode.LdcI4;
        if (type == TypeReference.Int64) return OpCode.LdcI8;
        if (type == TypeReference.Float32) return OpCode.LdcR4;
        if (type == TypeReference.Float64) return OpCode.LdcR8;
        if (type == TypeReference.String) return OpCode.Ldstr;
        return OpCode.LdcI4;
    }

    public override void Accept(IInstructionVisitor visitor) => visitor.Visit(this);
}

public sealed class LoadNullInstruction : Instruction
{
    public LoadNullInstruction() : base(OpCode.Ldnull) { }

    public override void Accept(IInstructionVisitor visitor) => visitor.Visit(this);
}

// ============================================================================
// Store Instructions
// ============================================================================

public sealed class StoreArgInstruction : Instruction
{
    public string ArgumentName { get; set; }

    public StoreArgInstruction(string argumentName) : base(OpCode.Starg)
    {
        ArgumentName = argumentName;
    }

    public override void Accept(IInstructionVisitor visitor) => visitor.Visit(this);
}

public sealed class StoreLocalInstruction : Instruction
{
    public string LocalName { get; set; }

    public StoreLocalInstruction(string localName) : base(OpCode.Stloc)
    {
        LocalName = localName;
    }

    public override void Accept(IInstructionVisitor visitor) => visitor.Visit(this);
}

public sealed class StoreFieldInstruction : Instruction
{
    public FieldReference Field { get; set; }

    public StoreFieldInstruction(FieldReference field) : base(OpCode.Stfld)
    {
        Field = field;
    }

    public override void Accept(IInstructionVisitor visitor) => visitor.Visit(this);
}

public sealed class StoreStaticFieldInstruction : Instruction
{
    public FieldReference Field { get; set; }

    public StoreStaticFieldInstruction(FieldReference field) : base(OpCode.Stsfld)
    {
        Field = field;
    }

    public override void Accept(IInstructionVisitor visitor) => visitor.Visit(this);
}

// ============================================================================
// Arithmetic and Comparison Instructions
// ============================================================================

public sealed class ArithmeticInstruction : Instruction
{
    public ArithmeticOp Operation { get; set; }

    public ArithmeticInstruction(ArithmeticOp operation) : base(OpCode.Add)
    {
        Operation = operation;
    }

    public override void Accept(IInstructionVisitor visitor) => visitor.Visit(this);
}

public sealed class ComparisonInstruction : Instruction
{
    public ComparisonOp Operation { get; set; }

    public ComparisonInstruction(ComparisonOp operation) : base(OpCode.Ceq)
    {
        Operation = operation;
    }

    public override void Accept(IInstructionVisitor visitor) => visitor.Visit(this);
}

// ============================================================================
// Call Instructions
// ============================================================================

public sealed class CallInstruction : Instruction
{
    public MethodReference Method { get; set; }

    public CallInstruction(MethodReference method) : base(OpCode.Call)
    {
        Method = method;
    }

    public override void Accept(IInstructionVisitor visitor) => visitor.Visit(this);
}

public sealed class CallVirtualInstruction : Instruction
{
    public MethodReference Method { get; set; }

    public CallVirtualInstruction(MethodReference method) : base(OpCode.Callvirt)
    {
        Method = method;
    }

    public override void Accept(IInstructionVisitor visitor) => visitor.Visit(this);
}

// ============================================================================
// Object Instructions
// ============================================================================

public sealed class NewObjectInstruction : Instruction
{
    public TypeReference Type { get; set; }

    public NewObjectInstruction(TypeReference type) : base(OpCode.Newobj)
    {
        Type = type;
    }

    public override void Accept(IInstructionVisitor visitor) => visitor.Visit(this);
}

public sealed class NewArrayInstruction : Instruction
{
    public TypeReference ElementType { get; set; }

    public NewArrayInstruction(TypeReference elementType) : base(OpCode.Newarr)
    {
        ElementType = elementType;
    }

    public override void Accept(IInstructionVisitor visitor) => visitor.Visit(this);
}

public sealed class CastInstruction : Instruction
{
    public TypeReference TargetType { get; set; }

    public CastInstruction(TypeReference targetType) : base(OpCode.Castclass)
    {
        TargetType = targetType;
    }

    public override void Accept(IInstructionVisitor visitor) => visitor.Visit(this);
}

public sealed class IsInstanceInstruction : Instruction
{
    public TypeReference TargetType { get; set; }

    public IsInstanceInstruction(TypeReference targetType) : base(OpCode.Isinst)
    {
        TargetType = targetType;
    }

    public override void Accept(IInstructionVisitor visitor) => visitor.Visit(this);
}

// ============================================================================
// Control Flow Instructions
// ============================================================================

public sealed class ReturnInstruction : Instruction
{
    public Instruction? Value { get; set; }

    public ReturnInstruction(Instruction? value) : base(OpCode.Ret)
    {
        Value = value;
    }

    public override void Accept(IInstructionVisitor visitor) => visitor.Visit(this);
}

public sealed class BreakInstruction : Instruction
{
    public BreakInstruction() : base(OpCode.Break) { }

    public override void Accept(IInstructionVisitor visitor) => visitor.Visit(this);
}

public sealed class ContinueInstruction : Instruction
{
    public ContinueInstruction() : base(OpCode.Continue) { }

    public override void Accept(IInstructionVisitor visitor) => visitor.Visit(this);
}

// ============================================================================
// Stack Manipulation Instructions
// ============================================================================

public sealed class DupInstruction : Instruction
{
    public DupInstruction() : base(OpCode.Dup) { }

    public override void Accept(IInstructionVisitor visitor) => visitor.Visit(this);
}

public sealed class PopInstruction : Instruction
{
    public PopInstruction() : base(OpCode.Pop) { }

    public override void Accept(IInstructionVisitor visitor) => visitor.Visit(this);
}

// ============================================================================
// Conversion Instructions
// ============================================================================

public sealed class ConversionInstruction : Instruction
{
    public TypeReference TargetType { get; set; }

    public ConversionInstruction(TypeReference targetType) : base(OpCode.ConvI4)
    {
        TargetType = targetType;
    }

    public override void Accept(IInstructionVisitor visitor) => visitor.Visit(this);
}

// ============================================================================
// Array Access Instructions
// ============================================================================

public sealed class LoadElementInstruction : Instruction
{
    public LoadElementInstruction() : base(OpCode.Ldelem) { }

    public override void Accept(IInstructionVisitor visitor) => visitor.Visit(this);
}

public sealed class StoreElementInstruction : Instruction
{
    public StoreElementInstruction() : base(OpCode.Stelem) { }

    public override void Accept(IInstructionVisitor visitor) => visitor.Visit(this);
}

// ============================================================================
// Unary Instructions
// ============================================================================

public sealed class UnaryNegateInstruction : Instruction
{
    public UnaryNegateInstruction() : base(OpCode.Neg) { }

    public override void Accept(IInstructionVisitor visitor) => visitor.Visit(this);
}

public sealed class UnaryNotInstruction : Instruction
{
    public UnaryNotInstruction() : base(OpCode.Not) { }

    public override void Accept(IInstructionVisitor visitor) => visitor.Visit(this);
}
