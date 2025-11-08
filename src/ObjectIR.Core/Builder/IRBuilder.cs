namespace ObjectIR.Core.Builder;

using ObjectIR.Core.IR;

/// <summary>
/// Fluent API for building IR modules
/// </summary>
public sealed class IRBuilder
{
    private Module _module;
    private ClassDefinition? _currentClass;
    private MethodDefinition? _currentMethod;

    public IRBuilder(string moduleName)
    {
        _module = new Module(moduleName);
    }

    public Module Build() => _module;

    // ========================================================================
    // Type Building
    // ========================================================================

    public ClassBuilder Class(string name)
    {
        _currentClass = _module.DefineClass(name);
        return new ClassBuilder(this, _currentClass);
    }

    public InterfaceBuilder Interface(string name)
    {
        var interfaceDef = _module.DefineInterface(name);
        return new InterfaceBuilder(this, interfaceDef);
    }

    public StructBuilder Struct(string name)
    {
        var structDef = _module.DefineStruct(name);
        return new StructBuilder(this, structDef);
    }

    // ========================================================================
    // Internal Methods
    // ========================================================================

    internal void SetCurrentClass(ClassDefinition? classDef) => _currentClass = classDef;
    internal void SetCurrentMethod(MethodDefinition? method) => _currentMethod = method;
}

/// <summary>
/// Builder for class definitions
/// </summary>
public sealed class ClassBuilder
{
    private readonly IRBuilder _builder;
    private readonly ClassDefinition _class;

    internal ClassBuilder(IRBuilder builder, ClassDefinition classDef)
    {
        _builder = builder;
        _class = classDef;
    }

    public ClassBuilder Access(AccessModifier access)
    {
        _class.Access = access;
        return this;
    }

    public ClassBuilder Namespace(string ns)
    {
        _class.Namespace = ns;
        return this;
    }

    public ClassBuilder Extends(TypeReference baseType)
    {
        _class.BaseType = baseType;
        return this;
    }

    public ClassBuilder Implements(params TypeReference[] interfaces)
    {
        _class.Interfaces.AddRange(interfaces);
        return this;
    }

    public ClassBuilder Abstract()
    {
        _class.IsAbstract = true;
        return this;
    }

    public ClassBuilder Sealed()
    {
        _class.IsSealed = true;
        return this;
    }

    public ClassBuilder Generic(params string[] parameters)
    {
        foreach (var param in parameters)
        {
            _class.GenericParameters.Add(new GenericParameter(param));
        }
        return this;
    }

    public FieldBuilder Field(string name, TypeReference type)
    {
        var field = _class.DefineField(name, type);
        return new FieldBuilder(this, field);
    }

    public MethodBuilder Method(string name, TypeReference returnType)
    {
        var method = _class.DefineMethod(name, returnType);
        return new MethodBuilder(this, _builder, method);
    }

    public MethodBuilder Constructor()
    {
        var ctor = _class.DefineConstructor();
        return new MethodBuilder(this, _builder, ctor);
    }

    public IRBuilder EndClass()
    {
        _builder.SetCurrentClass(null);
        return _builder;
    }
}

/// <summary>
/// Builder for interface definitions
/// </summary>
public sealed class InterfaceBuilder
{
    private readonly IRBuilder _builder;
    private readonly InterfaceDefinition _interface;

    internal InterfaceBuilder(IRBuilder builder, InterfaceDefinition interfaceDef)
    {
        _builder = builder;
        _interface = interfaceDef;
    }

    public InterfaceBuilder Access(AccessModifier access)
    {
        _interface.Access = access;
        return this;
    }

    public InterfaceBuilder Namespace(string ns)
    {
        _interface.Namespace = ns;
        return this;
    }

    public InterfaceBuilder Method(string name, TypeReference returnType)
    {
        _interface.DefineMethod(name, returnType);
        return this;
    }

    public IRBuilder EndInterface() => _builder;
}

/// <summary>
/// Builder for struct definitions
/// </summary>
public sealed class StructBuilder
{
    private readonly IRBuilder _builder;
    private readonly StructDefinition _struct;

    internal StructBuilder(IRBuilder builder, StructDefinition structDef)
    {
        _builder = builder;
        _struct = structDef;
    }

    public StructBuilder Access(AccessModifier access)
    {
        _struct.Access = access;
        return this;
    }

    public StructBuilder Field(string name, TypeReference type)
    {
        _struct.DefineField(name, type);
        return this;
    }

    public IRBuilder EndStruct() => _builder;
}

/// <summary>
/// Builder for field definitions
/// </summary>
public sealed class FieldBuilder
{
    private readonly ClassBuilder _classBuilder;
    private readonly FieldDefinition _field;

    internal FieldBuilder(ClassBuilder classBuilder, FieldDefinition field)
    {
        _classBuilder = classBuilder;
        _field = field;
    }

    public FieldBuilder Access(AccessModifier access)
    {
        _field.Access = access;
        return this;
    }

    public FieldBuilder Static()
    {
        _field.IsStatic = true;
        return this;
    }

    public FieldBuilder ReadOnly()
    {
        _field.IsReadOnly = true;
        return this;
    }

    public FieldBuilder InitialValue(object value)
    {
        _field.InitialValue = value;
        return this;
    }

    public ClassBuilder EndField() => _classBuilder;
}

/// <summary>
/// Builder for method definitions
/// </summary>
public sealed class MethodBuilder
{
    private readonly ClassBuilder? _classBuilder;
    private readonly IRBuilder _irBuilder;
    private readonly MethodDefinition _method;

    internal MethodBuilder(ClassBuilder? classBuilder, IRBuilder irBuilder, MethodDefinition method)
    {
        _classBuilder = classBuilder;
        _irBuilder = irBuilder;
        _method = method;
        _irBuilder.SetCurrentMethod(method);
    }

    public MethodBuilder Access(AccessModifier access)
    {
        _method.Access = access;
        return this;
    }

    public MethodBuilder Static()
    {
        _method.IsStatic = true;
        return this;
    }

    public MethodBuilder Virtual()
    {
        _method.IsVirtual = true;
        return this;
    }

    public MethodBuilder Override()
    {
        _method.IsOverride = true;
        return this;
    }

    public MethodBuilder Abstract()
    {
        _method.IsAbstract = true;
        return this;
    }

    public MethodBuilder Parameter(string name, TypeReference type)
    {
        _method.DefineParameter(name, type);
        return this;
    }

    public MethodBuilder Local(string name, TypeReference type)
    {
        _method.DefineLocal(name, type);
        return this;
    }

    public InstructionBuilder Body()
    {
        return new InstructionBuilder(this, _method.Instructions);
    }

    public ClassBuilder? EndMethod()
    {
        _irBuilder.SetCurrentMethod(null);
        return _classBuilder;
    }
}

/// <summary>
/// Builder for emitting instructions
/// </summary>
public sealed class InstructionBuilder
{
    private readonly MethodBuilder _methodBuilder;
    private readonly InstructionList _instructions;

    internal InstructionBuilder(MethodBuilder methodBuilder, InstructionList instructions)
    {
        _methodBuilder = methodBuilder;
        _instructions = instructions;
    }

    // Load/Store
    public InstructionBuilder Ldarg(string name) { _instructions.EmitLoadArg(name); return this; }
    public InstructionBuilder Ldloc(string name) { _instructions.EmitLoadLocal(name); return this; }
    public InstructionBuilder Ldfld(FieldReference field) { _instructions.EmitLoadField(field); return this; }
    public InstructionBuilder Ldsfld(FieldReference field) { _instructions.EmitLoadStaticField(field); return this; }
    public InstructionBuilder LdcI4(int value) { _instructions.EmitLoadConstant(value, TypeReference.Int32); return this; }
    public InstructionBuilder LdcR4(float value) { _instructions.EmitLoadConstant(value, TypeReference.Float32); return this; }
    public InstructionBuilder Ldstr(string value) { _instructions.EmitLoadConstant(value, TypeReference.String); return this; }
    public InstructionBuilder Ldnull() { _instructions.EmitLoadNull(); return this; }

    public InstructionBuilder Stloc(string name) { _instructions.EmitStoreLocal(name); return this; }
    public InstructionBuilder Stfld(FieldReference field) { _instructions.EmitStoreField(field); return this; }

    // Arithmetic
    public InstructionBuilder Add() { _instructions.EmitAdd(); return this; }
    public InstructionBuilder Sub() { _instructions.EmitSub(); return this; }
    public InstructionBuilder Mul() { _instructions.EmitMul(); return this; }
    public InstructionBuilder Div() { _instructions.EmitDiv(); return this; }

    // Comparison
    public InstructionBuilder Ceq() { _instructions.EmitCompareEqual(); return this; }
    public InstructionBuilder Cgt() { _instructions.EmitCompareGreater(); return this; }
    public InstructionBuilder Clt() { _instructions.EmitCompareLess(); return this; }

    // Calls
    public InstructionBuilder Call(MethodReference method) { _instructions.EmitCall(method); return this; }
    public InstructionBuilder Callvirt(MethodReference method) { _instructions.EmitCallVirtual(method); return this; }

    // Object operations
    public InstructionBuilder Newobj(TypeReference type) { _instructions.EmitNewObject(type); return this; }
    public InstructionBuilder Newarr(TypeReference elementType) { _instructions.EmitNewArray(elementType); return this; }

    // Stack
    public InstructionBuilder Dup() { _instructions.EmitDup(); return this; }
    public InstructionBuilder Pop() { _instructions.EmitPop(); return this; }


    // Control flow
    public InstructionBuilder Ret() { _instructions.EmitReturn(); return this; }

    public InstructionBuilder If(Condition condition, Action<InstructionBuilder> thenBlock, Action<InstructionBuilder>? elseBlock = null)
    {
        var ifInst = new IfInstruction(condition);
        var thenBuilder = new InstructionBuilder(_methodBuilder, ifInst.ThenBlock);
        thenBlock(thenBuilder);
        
        if (elseBlock != null)
        {
            ifInst.ElseBlock = new InstructionList();
            var elseBuilder = new InstructionBuilder(_methodBuilder, ifInst.ElseBlock);
            elseBlock(elseBuilder);
        }
        
        _instructions.Add(ifInst);
        return this;
    }

    public InstructionBuilder While(Condition condition, Action<InstructionBuilder> body)
    {
        var whileInst = new WhileInstruction(condition);
        var bodyBuilder = new InstructionBuilder(_methodBuilder, whileInst.Body);
        body(bodyBuilder);
        _instructions.Add(whileInst);
        return this;
    }

    public MethodBuilder EndBody() => _methodBuilder;
}
