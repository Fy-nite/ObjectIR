namespace ObjectIR.Core.IR;

/// <summary>
/// Represents a reference to a type in the IR
/// </summary>
public sealed class TypeReference
{
    public string Name { get; }
    public string? Namespace { get; }
    public List<TypeReference> GenericArguments { get; }
    public bool IsArray { get; }
    public bool IsPointer { get; }
    public bool IsReference { get; }

    private TypeReference(string name, string? ns = null)
    {
        Name = name;
        Namespace = ns;
        GenericArguments = new();
    }

    private TypeReference(string name, string? ns, List<TypeReference> genericArgs, bool isArray, bool isPointer, bool isReference)
    {
        Name = name;
        Namespace = ns;
        GenericArguments = genericArgs;
        IsArray = isArray;
        IsPointer = isPointer;
        IsReference = isReference;
    }

    public string GetQualifiedName()
    {
        var baseName = string.IsNullOrEmpty(Namespace) ? Name : $"{Namespace}.{Name}";
        
        if (GenericArguments.Count > 0)
        {
            baseName += $"<{string.Join(", ", GenericArguments.Select(g => g.GetQualifiedName()))}>";
        }
        
        if (IsArray) baseName += "[]";
        if (IsPointer) baseName += "*";
        if (IsReference) baseName += "&";
        
        return baseName;
    }

    public TypeReference MakeArrayType()
    {
        return new TypeReference(Name, Namespace, GenericArguments, true, IsPointer, IsReference);
    }

    public TypeReference MakeGenericType(params TypeReference[] args)
    {
        return new TypeReference(Name, Namespace, args.ToList(), IsArray, IsPointer, IsReference);
    }

    public override string ToString() => GetQualifiedName();

    // Primitive types
    public static readonly TypeReference Void = new("void");
    public static readonly TypeReference Bool = new("bool");
    public static readonly TypeReference Int8 = new("int8");
    public static readonly TypeReference UInt8 = new("uint8");
    public static readonly TypeReference Int16 = new("int16");
    public static readonly TypeReference UInt16 = new("uint16");
    public static readonly TypeReference Int32 = new("int32");
    public static readonly TypeReference UInt32 = new("uint32");
    public static readonly TypeReference Int64 = new("int64");
    public static readonly TypeReference UInt64 = new("uint64");
    public static readonly TypeReference Float32 = new("float32");
    public static readonly TypeReference Float64 = new("float64");
    public static readonly TypeReference Char = new("char");
    public static readonly TypeReference String = new("string", "System");

    // Standard library types
    public static TypeReference List(TypeReference elementType) 
        => new TypeReference("List", "System").MakeGenericType(elementType);
    
    public static TypeReference Dict(TypeReference keyType, TypeReference valueType)
        => new TypeReference("Dict", "System").MakeGenericType(keyType, valueType);
    
    public static TypeReference Set(TypeReference elementType)
        => new TypeReference("Set", "System").MakeGenericType(elementType);
    
    public static TypeReference Optional(TypeReference valueType)
        => new TypeReference("Optional", "System").MakeGenericType(valueType);

    public static TypeReference FromName(string qualifiedName)
    {
        // Simple parser for qualified names
        var parts = qualifiedName.Split('.');
        if (parts.Length == 1)
            return new TypeReference(parts[0]);
        
        var name = parts[^1];
        var ns = string.Join(".", parts[..^1]);
        return new TypeReference(name, ns);
    }
}
