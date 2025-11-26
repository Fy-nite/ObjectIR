namespace ObjectIR.Core.IR;

/// <summary>
/// Represents a method or function definition
/// </summary>
public sealed class MethodDefinition
{
    public string Name { get; set; }
    public TypeReference ReturnType { get; set; }
    public List<Parameter> Parameters { get; } = new();
    public List<LocalVariable> Locals { get; } = new();
    public InstructionList Instructions { get; } = new();
    public AccessModifier Access { get; set; } = AccessModifier.Public;
    public bool IsStatic { get; set; }
    public bool IsVirtual { get; set; }
    public bool IsOverride { get; set; }
    public bool IsAbstract { get; set; }
    public bool IsConstructor { get; set; }
    public List<GenericParameter> GenericParameters { get; } = new();
    public MethodReference? ImplementsInterface { get; set; }
    public string? BodySource { get; set; }

    public MethodDefinition(string name, TypeReference returnType)
    {
        Name = name;
        ReturnType = returnType;
    }

    public Parameter DefineParameter(string name, TypeReference type)
    {
        var param = new Parameter(name, type);
        Parameters.Add(param);
        return param;
    }

    public LocalVariable DefineLocal(string name, TypeReference type)
    {
        var local = new LocalVariable(name, type);
        Locals.Add(local);
        return local;
    }

    public MethodReference ToReference(TypeReference declaringType)
    {
        return new MethodReference(
            declaringType,
            Name,
            ReturnType,
            Parameters.Select(p => p.Type).ToList()
        );
    }
}

/// <summary>
/// Represents a standalone function (not in a class)
/// </summary>
public sealed class FunctionDefinition
{
    public string Name { get; set; }
    public TypeReference ReturnType { get; set; }
    public List<Parameter> Parameters { get; } = new();
    public List<LocalVariable> Locals { get; } = new();
    public InstructionList Instructions { get; } = new();
    public List<GenericParameter> GenericParameters { get; } = new();

    public FunctionDefinition(string name, TypeReference returnType)
    {
        Name = name;
        ReturnType = returnType;
    }

    public Parameter DefineParameter(string name, TypeReference type)
    {
        var param = new Parameter(name, type);
        Parameters.Add(param);
        return param;
    }

    public LocalVariable DefineLocal(string name, TypeReference type)
    {
        var local = new LocalVariable(name, type);
        Locals.Add(local);
        return local;
    }
}

/// <summary>
/// Represents a method parameter
/// </summary>
public sealed class Parameter
{
    public string Name { get; set; }
    public TypeReference Type { get; set; }
    public bool IsOut { get; set; }
    public bool IsRef { get; set; }
    public object? DefaultValue { get; set; }

    public Parameter(string name, TypeReference type)
    {
        Name = name;
        Type = type;
    }
}

/// <summary>
/// Represents a local variable in a method
/// </summary>
public sealed class LocalVariable
{
    public string Name { get; set; }
    public TypeReference Type { get; set; }

    public LocalVariable(string name, TypeReference type)
    {
        Name = name;
        Type = type;
    }
}

/// <summary>
/// Represents a reference to a method
/// </summary>
public sealed class MethodReference
{
    public TypeReference DeclaringType { get; }
    public string Name { get; }
    public TypeReference ReturnType { get; }
    public List<TypeReference> ParameterTypes { get; }
    public List<TypeReference> GenericArguments { get; } = new();

    public MethodReference(TypeReference declaringType, string name, TypeReference returnType, List<TypeReference> parameterTypes)
    {
        DeclaringType = declaringType;
        Name = name;
        ReturnType = returnType;
        ParameterTypes = parameterTypes;
    }

    public string GetSignature()
    {
        var genericPart = GenericArguments.Count > 0 
            ? $"<{string.Join(", ", GenericArguments.Select(g => g.GetQualifiedName()))}>"
            : "";
        
        var paramsPart = string.Join(", ", ParameterTypes.Select(p => p.GetQualifiedName()));
        
        return $"{DeclaringType.GetQualifiedName()}.{Name}{genericPart}({paramsPart}) -> {ReturnType.GetQualifiedName()}";
    }

    public override string ToString() => GetSignature();
}

/// <summary>
/// Represents a reference to a field
/// </summary>
public sealed class FieldReference
{
    public TypeReference DeclaringType { get; }
    public string Name { get; }
    public TypeReference FieldType { get; }

    public FieldReference(TypeReference declaringType, string name, TypeReference fieldType)
    {
        DeclaringType = declaringType;
        Name = name;
        FieldType = fieldType;
    }

    public override string ToString() => $"{DeclaringType.GetQualifiedName()}.{Name}";
}
