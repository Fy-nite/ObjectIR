namespace ObjectIR.Core.IR;

/// <summary>
/// Base class for all IR instructions
/// </summary>
public abstract class Instruction
{
    public OpCode OpCode { get; }
    public int LineNumber { get; set; }
    public string? SourceFile { get; set; }

    protected Instruction(OpCode opCode)
    {
        OpCode = opCode;
    }

    public abstract void Accept(IInstructionVisitor visitor);
}

/// <summary>
/// Visitor pattern for traversing instructions
/// </summary>
public interface IInstructionVisitor
{
    void Visit(LoadArgInstruction instruction);
    void Visit(LoadLocalInstruction instruction);
    void Visit(LoadFieldInstruction instruction);
    void Visit(LoadStaticFieldInstruction instruction);
    void Visit(LoadConstantInstruction instruction);
    void Visit(LoadNullInstruction instruction);
    void Visit(StoreArgInstruction instruction);
    void Visit(StoreLocalInstruction instruction);
    void Visit(StoreFieldInstruction instruction);
    void Visit(StoreStaticFieldInstruction instruction);
    void Visit(ArithmeticInstruction instruction);
    void Visit(ComparisonInstruction instruction);
    void Visit(CallInstruction instruction);
    void Visit(CallVirtualInstruction instruction);
    void Visit(NewObjectInstruction instruction);
    void Visit(NewArrayInstruction instruction);
    void Visit(CastInstruction instruction);
    void Visit(IsInstanceInstruction instruction);
    void Visit(ReturnInstruction instruction);
    void Visit(DupInstruction instruction);
    void Visit(PopInstruction instruction);
    void Visit(ConversionInstruction instruction);
    void Visit(IfInstruction instruction);
    void Visit(WhileInstruction instruction);
    void Visit(BreakInstruction instruction);
    void Visit(ContinueInstruction instruction);
    void Visit(TryInstruction instruction);
    void Visit(ThrowInstruction instruction);
    void Visit(LoadElementInstruction instruction);
    void Visit(StoreElementInstruction instruction);
    void Visit(UnaryNegateInstruction instruction);
    void Visit(UnaryNotInstruction instruction);
}

/// <summary>
/// List of instructions with helper methods
/// </summary>
public sealed class InstructionList : List<Instruction>
{
    public void Emit(Instruction instruction) => Add(instruction);
    
    public void EmitLoadArg(string name) => Add(new LoadArgInstruction(name));
    public void EmitLoadLocal(string name) => Add(new LoadLocalInstruction(name));
    public void EmitLoadField(FieldReference field) => Add(new LoadFieldInstruction(field));
    public void EmitLoadStaticField(FieldReference field) => Add(new LoadStaticFieldInstruction(field));
    public void EmitLoadConstant(object value, TypeReference type) => Add(new LoadConstantInstruction(value, type));
    public void EmitLoadNull() => Add(new LoadNullInstruction());
    
    public void EmitStoreArg(string name) => Add(new StoreArgInstruction(name));
    public void EmitStoreLocal(string name) => Add(new StoreLocalInstruction(name));
    public void EmitStoreField(FieldReference field) => Add(new StoreFieldInstruction(field));
    public void EmitStoreStaticField(FieldReference field) => Add(new StoreStaticFieldInstruction(field));
    
    public void EmitAdd() => Add(new ArithmeticInstruction(ArithmeticOp.Add));
    public void EmitSub() => Add(new ArithmeticInstruction(ArithmeticOp.Sub));
    public void EmitMul() => Add(new ArithmeticInstruction(ArithmeticOp.Mul));
    public void EmitDiv() => Add(new ArithmeticInstruction(ArithmeticOp.Div));
    public void EmitRem() => Add(new ArithmeticInstruction(ArithmeticOp.Rem));
    
    public void EmitCompareEqual() => Add(new ComparisonInstruction(ComparisonOp.Equal));
    public void EmitCompareGreater() => Add(new ComparisonInstruction(ComparisonOp.Greater));
    public void EmitCompareLess() => Add(new ComparisonInstruction(ComparisonOp.Less));
    
    public void EmitCall(MethodReference method) => Add(new CallInstruction(method));
    public void EmitCallVirtual(MethodReference method) => Add(new CallVirtualInstruction(method));
    
    public void EmitNewObject(TypeReference type) => Add(new NewObjectInstruction(type));
    public void EmitNewArray(TypeReference elementType) => Add(new NewArrayInstruction(elementType));
    
    public void EmitReturn() => Add(new ReturnInstruction(null));
    public void EmitReturn(Instruction value) => Add(new ReturnInstruction(value));
    
    public void EmitDup() => Add(new DupInstruction());
    public void EmitPop() => Add(new PopInstruction());
}
