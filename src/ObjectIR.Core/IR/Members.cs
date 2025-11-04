namespace ObjectIR.Core.IR;

/// <summary>
/// Represents a field in a class or struct
/// </summary>
public sealed class FieldDefinition
{
    public string Name { get; set; }
    public TypeReference Type { get; set; }
    public AccessModifier Access { get; set; } = AccessModifier.Private;
    public bool IsStatic { get; set; }
    public bool IsReadOnly { get; set; }
    public object? InitialValue { get; set; }

    public FieldDefinition(string name, TypeReference type)
    {
        Name = name;
        Type = type;
    }
}

/// <summary>
/// Represents a property in a class or interface
/// </summary>
public sealed class PropertyDefinition
{
    public string Name { get; set; }
    public TypeReference Type { get; set; }
    public AccessModifier Access { get; set; } = AccessModifier.Public;
    public MethodDefinition? Getter { get; set; }
    public MethodDefinition? Setter { get; set; }

    public PropertyDefinition(string name, TypeReference type)
    {
        Name = name;
        Type = type;
    }
}

/// <summary>
/// Represents an enum member
/// </summary>
public sealed class EnumMember
{
    public string Name { get; set; }
    public long Value { get; set; }

    public EnumMember(string name, long value)
    {
        Name = name;
        Value = value;
    }
}

/// <summary>
/// Represents a generic parameter
/// </summary>
public sealed class GenericParameter
{
    public string Name { get; set; }
    public List<TypeConstraint> Constraints { get; } = new();

    public GenericParameter(string name)
    {
        Name = name;
    }
}

/// <summary>
/// Represents a constraint on a generic parameter
/// </summary>
public abstract class TypeConstraint
{
    public static TypeConstraint Class() => new ClassConstraint();
    public static TypeConstraint Struct() => new StructConstraint();
    public static TypeConstraint BaseType(TypeReference type) => new BaseTypeConstraint(type);
}

public sealed class ClassConstraint : TypeConstraint { }
public sealed class StructConstraint : TypeConstraint { }
public sealed class BaseTypeConstraint : TypeConstraint
{
    public TypeReference BaseType { get; }
    public BaseTypeConstraint(TypeReference baseType) => BaseType = baseType;
}
