namespace ObjectIR.Core.IR;

/// <summary>
/// Represents a complete IR module containing types and functions
/// </summary>
public sealed class Module
{
    public string Name { get; set; } = string.Empty;
    public Version Version { get; set; } = new(1, 0, 0);
    public List<TypeDefinition> Types { get; } = new();
    public List<FunctionDefinition> Functions { get; } = new();
    public Dictionary<string, object> Metadata { get; } = new();

    public Module(string name)
    {
        Name = name;
    }

    public Module(string name, string version)
    {
        Name = name;
        Version = Version.Parse(version);
    }

    public ClassDefinition DefineClass(string name)
    {
        var classDef = new ClassDefinition(name);
        Types.Add(classDef);
        return classDef;
    }

    public InterfaceDefinition DefineInterface(string name)
    {
        var interfaceDef = new InterfaceDefinition(name);
        Types.Add(interfaceDef);
        return interfaceDef;
    }

    public StructDefinition DefineStruct(string name)
    {
        var structDef = new StructDefinition(name);
        Types.Add(structDef);
        return structDef;
    }

    public TypeDefinition? FindType(string qualifiedName)
    {
        return Types.FirstOrDefault(t => t.GetQualifiedName() == qualifiedName);
    }
}
