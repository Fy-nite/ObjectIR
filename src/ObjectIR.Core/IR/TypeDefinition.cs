namespace ObjectIR.Core.IR;

/// <summary>
/// Base class for all type definitions in the IR
/// </summary>
public abstract class TypeDefinition
{
    public string Name { get; set; }
    public string? Namespace { get; set; }
    public AccessModifier Access { get; set; } = AccessModifier.Public;
    public List<GenericParameter> GenericParameters { get; } = new();
    public Dictionary<string, object> Metadata { get; } = new();

    protected TypeDefinition(string name)
    {
        Name = name;
    }

    public string GetQualifiedName()
    {
        var baseName = string.IsNullOrEmpty(Namespace) ? Name : $"{Namespace}.{Name}";
        if (GenericParameters.Count > 0)
        {
            return $"{baseName}<{string.Join(", ", GenericParameters.Select(p => p.Name))}>";
        }
        return baseName;
    }

    public abstract TypeKind Kind { get; }
}

/// <summary>
/// Represents a class type
/// </summary>
public sealed class ClassDefinition : TypeDefinition
{
    public TypeReference? BaseType { get; set; }
    public List<TypeReference> Interfaces { get; } = new();
    public List<FieldDefinition> Fields { get; } = new();
    public List<MethodDefinition> Methods { get; } = new();
    public List<PropertyDefinition> Properties { get; } = new();
    public bool IsAbstract { get; set; }
    public bool IsSealed { get; set; }

    public override TypeKind Kind => TypeKind.Class;

    public ClassDefinition(string name) : base(name) { }

    public FieldDefinition DefineField(string name, TypeReference type)
    {
        var field = new FieldDefinition(name, type);
        Fields.Add(field);
        return field;
    }

    public MethodDefinition DefineMethod(string name, TypeReference returnType)
    {
        var method = new MethodDefinition(name, returnType);
        Methods.Add(method);
        return method;
    }

    public MethodDefinition DefineConstructor()
    {
        var ctor = new MethodDefinition(".ctor", TypeReference.Void)
        {
            IsConstructor = true
        };
        Methods.Add(ctor);
        return ctor;
    }
}

/// <summary>
/// Represents an interface type
/// </summary>
public sealed class InterfaceDefinition : TypeDefinition
{
    public List<TypeReference> BaseInterfaces { get; } = new();
    public List<MethodDefinition> Methods { get; } = new();
    public List<PropertyDefinition> Properties { get; } = new();

    public override TypeKind Kind => TypeKind.Interface;

    public InterfaceDefinition(string name) : base(name) { }

    public MethodDefinition DefineMethod(string name, TypeReference returnType)
    {
        var method = new MethodDefinition(name, returnType) { IsAbstract = true };
        Methods.Add(method);
        return method;
    }
}

/// <summary>
/// Represents a struct (value type)
/// </summary>
public sealed class StructDefinition : TypeDefinition
{
    public List<TypeReference> Interfaces { get; } = new();
    public List<FieldDefinition> Fields { get; } = new();
    public List<MethodDefinition> Methods { get; } = new();

    public override TypeKind Kind => TypeKind.Struct;

    public StructDefinition(string name) : base(name) { }

    public FieldDefinition DefineField(string name, TypeReference type)
    {
        var field = new FieldDefinition(name, type);
        Fields.Add(field);
        return field;
    }

    public MethodDefinition DefineMethod(string name, TypeReference returnType)
    {
        var method = new MethodDefinition(name, returnType);
        Methods.Add(method);
        return method;
    }
}

/// <summary>
/// Represents an enum type
/// </summary>
public sealed class EnumDefinition : TypeDefinition
{
    public TypeReference UnderlyingType { get; set; } = TypeReference.Int32;
    public List<EnumMember> Members { get; } = new();

    public override TypeKind Kind => TypeKind.Enum;

    public EnumDefinition(string name) : base(name) { }

    public void DefineMember(string name, long value)
    {
        Members.Add(new EnumMember(name, value));
    }
}

public enum TypeKind
{
    Class,
    Interface,
    Struct,
    Enum
}

public enum AccessModifier
{
    Public,
    Private,
    Protected,
    Internal
}
