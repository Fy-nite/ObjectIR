namespace ObjectIR.Core.Serialization;

using ObjectIR.Core.IR;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.Json;
using System.Text.Json.Nodes;

#pragma warning disable CS1591

/// <summary>
/// Provides serialization and dumping capabilities for IR modules
/// </summary>
public sealed class ModuleSerializer
{
    private readonly Module _module;

  public ModuleSerializer(Module module)
 {
        _module = module ?? throw new ArgumentNullException(nameof(module));
    }

 /// <summary>
    /// Dumps the module as a structured object representation
    /// </summary>
    public ModuleData Dump() => DumpModule();

    /// <summary>
    /// Dumps the module as JSON string
    /// </summary>
    public string DumpToJson(bool indented = true)
    {
        var data = DumpModule();
        var options = new JsonSerializerOptions
        {
          WriteIndented = indented,
            DefaultIgnoreCondition = System.Text.Json.Serialization.JsonIgnoreCondition.WhenWritingNull
        };
        return JsonSerializer.Serialize(data, options);
    }

    /// <summary>
    /// Dumps the module as human-readable text
    /// </summary>
    public string DumpToText()
    {
 var sb = new StringBuilder();
   
        sb.AppendLine($"Module: {_module.Name}");
        sb.AppendLine($"Version: {_module.Version}");
        
 if (_module.Metadata.Count > 0)
        {
    sb.AppendLine("\nMetadata:");
 foreach (var (key, value) in _module.Metadata)
            {
       sb.AppendLine($"  {key}: {value}");
        }
   }

        if (_module.Types.Count > 0)
        {
            sb.AppendLine($"\nTypes ({_module.Types.Count}):");
            foreach (var type in _module.Types)
    {
   DumpType(sb, type, indent: 1);
  }
  }

  if (_module.Functions.Count > 0)
        {
            sb.AppendLine($"\nFunctions ({_module.Functions.Count}):");
 foreach (var func in _module.Functions)
            {
     DumpFunction(sb, func, indent: 1);
 }
        }

        return sb.ToString();
  }

    /// <summary>
    /// Dumps the module to an array of type descriptions
    /// </summary>
    public TypeData[] DumpTypes() => _module.Types.Select(t => DumpTypeData(t)).ToArray();

    /// <summary>
    /// Dumps the module to an array of function descriptions
    /// </summary>
    public FunctionData[] DumpFunctions() => _module.Functions.Select(f => DumpFunctionData(f)).ToArray();

    // ============================================================================
    // Private Methods
    // ============================================================================

    private ModuleData DumpModule()
    {
        return new ModuleData
    {
            Name = _module.Name,
       Version = _module.Version.ToString(),
 Metadata = _module.Metadata.ToDictionary(x => x.Key, x => x.Value?.ToString() ?? "null"),
          Types = _module.Types.Select(DumpTypeData).ToArray(),
            Functions = _module.Functions.Select(DumpFunctionData).ToArray()
        };
    }

    private TypeData DumpTypeData(TypeDefinition type)
    {
        return type switch
      {
       ClassDefinition classDef => new TypeData
            {
    Kind = "Class",
          Name = classDef.Name,
       Namespace = classDef.Namespace,
                Access = classDef.Access.ToString(),
   IsAbstract = classDef.IsAbstract,
              IsSealed = classDef.IsSealed,
         BaseType = classDef.BaseType?.GetQualifiedName(),
     Interfaces = classDef.Interfaces.Select(i => i.GetQualifiedName()).ToArray(),
   GenericParameters = classDef.GenericParameters.Select(g => g.Name).ToArray(),
              Fields = classDef.Fields.Select(DumpFieldData).ToArray(),
  Methods = classDef.Methods.Select(DumpMethodData).ToArray(),
     Properties = classDef.Properties.Select(DumpPropertyData).ToArray()
 },
            InterfaceDefinition interfaceDef => new TypeData
  {
     Kind = "Interface",
      Name = interfaceDef.Name,
                Namespace = interfaceDef.Namespace,
    Access = interfaceDef.Access.ToString(),
    BaseInterfaces = interfaceDef.BaseInterfaces.Select(i => i.GetQualifiedName()).ToArray(),
    Methods = interfaceDef.Methods.Select(DumpMethodData).ToArray(),
Properties = interfaceDef.Properties.Select(DumpPropertyData).ToArray()
  },
            StructDefinition structDef => new TypeData
    {
          Kind = "Struct",
    Name = structDef.Name,
      Namespace = structDef.Namespace,
                Access = structDef.Access.ToString(),
         Interfaces = structDef.Interfaces.Select(i => i.GetQualifiedName()).ToArray(),
          GenericParameters = structDef.GenericParameters.Select(g => g.Name).ToArray(),
                Fields = structDef.Fields.Select(DumpFieldData).ToArray(),
      Methods = structDef.Methods.Select(DumpMethodData).ToArray()
 },
            EnumDefinition enumDef => new TypeData
            {
                Kind = "Enum",
                Name = enumDef.Name,
                Namespace = enumDef.Namespace,
                Access = enumDef.Access.ToString(),
                UnderlyingType = enumDef.UnderlyingType.GetQualifiedName()
            },
 _ => new TypeData { Kind = "Unknown", Name = type.Name }
        };
    }

    private FieldData DumpFieldData(FieldDefinition field)
 {
        return new FieldData
        {
         Name = field.Name,
         Type = field.Type.GetQualifiedName(),
    Access = field.Access.ToString(),
            IsStatic = field.IsStatic,
 IsReadOnly = field.IsReadOnly,
        InitialValue = field.InitialValue?.ToString()
      };
    }

    private PropertyData DumpPropertyData(PropertyDefinition property)
    {
        return new PropertyData
        {
Name = property.Name,
            Type = property.Type.GetQualifiedName(),
            Access = property.Access.ToString(),
         HasGetter = property.Getter != null,
            HasSetter = property.Setter != null,
        GetterAccess = property.Getter?.Access.ToString(),
            SetterAccess = property.Setter?.Access.ToString()
        };
  }

    private MethodData DumpMethodData(MethodDefinition method)
    {
        var instructionsNode = JsonNode.Parse(InstructionSerializer.SerializeInstructions(method.Instructions).GetRawText()) ?? new JsonArray();

        return new MethodData
        {
            Name = method.Name,
            ReturnType = method.ReturnType.GetQualifiedName(),
            Access = method.Access.ToString(),
            IsStatic = method.IsStatic,
            IsVirtual = method.IsVirtual,
            IsOverride = method.IsOverride,
            IsAbstract = method.IsAbstract,
            IsConstructor = method.IsConstructor,
            Parameters = method.Parameters.Select(p => new ParameterData
            {
                Name = p.Name,
                Type = p.Type.GetQualifiedName()
            }).ToArray(),
            LocalVariables = method.Locals.Select(l => new LocalVariableData
            {
                Name = l.Name,
                Type = l.Type.GetQualifiedName()
            }).ToArray(),
            InstructionCount = method.Instructions.Count,
            Instructions = instructionsNode
        };
    }

    private FunctionData DumpFunctionData(FunctionDefinition function)
    {
        var instructionsNode = JsonNode.Parse(InstructionSerializer.SerializeInstructions(function.Instructions).GetRawText()) ?? new JsonArray();

        return new FunctionData
        {
            Name = function.Name,
            ReturnType = function.ReturnType.GetQualifiedName(),
            Parameters = function.Parameters.Select(p => new ParameterData
            {
                Name = p.Name,
                Type = p.Type.GetQualifiedName()
            }).ToArray(),
            LocalVariables = function.Locals.Select(l => new LocalVariableData
            {
                Name = l.Name,
                Type = l.Type.GetQualifiedName()
            }).ToArray(),
            InstructionCount = function.Instructions.Count,
            Instructions = instructionsNode
        };
    }

    private void DumpType(StringBuilder sb, TypeDefinition type, int indent = 0)
    {
        var ind = new string(' ', indent * 2);
        
        if (type is ClassDefinition classDef)
        {
 var modifiers = new List<string>();
 if (classDef.IsAbstract) modifiers.Add("abstract");
        if (classDef.IsSealed) modifiers.Add("sealed");
            
            var modifierStr = modifiers.Count > 0 ? string.Join(" ", modifiers) + " " : "";
 sb.AppendLine($"{ind}{modifierStr}class {classDef.GetQualifiedName()}");
 
            if (classDef.BaseType != null)
           sb.AppendLine($"{ind}  : {classDef.BaseType.GetQualifiedName()}");
    
          if (classDef.Interfaces.Count > 0)
        {
         sb.AppendLine($"{ind}  implements: {string.Join(", ", classDef.Interfaces.Select(i => i.GetQualifiedName()))}");
   }
            
   if (classDef.Fields.Count > 0)
   {
     sb.AppendLine($"{ind}  Fields:");
 foreach (var field in classDef.Fields)
           {
      sb.AppendLine($"{ind}    {field.Access} {(field.IsStatic ? "static " : "")}{(field.IsReadOnly ? "readonly " : "")}{field.Type.GetQualifiedName()} {field.Name}");
           }
  }
            
            if (classDef.Methods.Count > 0)
       {
    sb.AppendLine($"{ind}  Methods:");
         foreach (var method in classDef.Methods)
    {
   var methodModifiers = new List<string>();
         if (method.IsStatic) methodModifiers.Add("static");
           if (method.IsVirtual) methodModifiers.Add("virtual");
         if (method.IsOverride) methodModifiers.Add("override");
    if (method.IsAbstract) methodModifiers.Add("abstract");
          
    var methodModStr = methodModifiers.Count > 0 ? string.Join(" ", methodModifiers) + " " : "";
      var methodName = method.IsConstructor ? ".ctor" : method.Name;
        var returnType = method.ReturnType.GetQualifiedName();
          var parameters = string.Join(", ", method.Parameters.Select(p => $"{p.Type.GetQualifiedName()} {p.Name}"));
            
 sb.AppendLine($"{ind}    {method.Access} {methodModStr}{returnType} {methodName}({parameters}) [{method.Instructions.Count} instructions]");
         }
          }
        }
        else if (type is InterfaceDefinition interfaceDef)
      {
  sb.AppendLine($"{ind}interface {interfaceDef.GetQualifiedName()}");
          
    if (interfaceDef.Methods.Count > 0)
            {
       sb.AppendLine($"{ind}  Methods:");
    foreach (var method in interfaceDef.Methods)
                {
         var parameters = string.Join(", ", method.Parameters.Select(p => $"{p.Type.GetQualifiedName()} {p.Name}"));
   sb.AppendLine($"{ind}    {method.ReturnType.GetQualifiedName()} {method.Name}({parameters})");
            }
    }
        }
  else if (type is StructDefinition structDef)
     {
            sb.AppendLine($"{ind}struct {structDef.GetQualifiedName()}");
         
     if (structDef.Fields.Count > 0)
            {
      sb.AppendLine($"{ind}  Fields:");
       foreach (var field in structDef.Fields)
         {
         sb.AppendLine($"{ind}    {field.Access} {field.Type.GetQualifiedName()} {field.Name}");
                }
   }
        }
    }

    private void DumpFunction(StringBuilder sb, FunctionDefinition function, int indent = 0)
    {
        var ind = new string(' ', indent * 2);
  var parameters = string.Join(", ", function.Parameters.Select(p => $"{p.Type.GetQualifiedName()} {p.Name}"));
        sb.AppendLine($"{ind}{function.ReturnType.GetQualifiedName()} {function.Name}({parameters}) [{function.Instructions.Count} instructions]");
    }

    /// <summary>
    /// Loads a module from JSON string
    /// </summary>
    public static Module LoadFromJson(string json)
    {
        var data = JsonSerializer.Deserialize<ModuleData>(json) ?? throw new InvalidOperationException("Failed to deserialize module data");
        return LoadModule(data);
    }

    /// <summary>
    /// Loads a module from ModuleData
    /// </summary>
    public static Module LoadModule(ModuleData data)
    {
        var module = new Module(data.Name)
        {
            Version = Version.Parse(data.Version)
        };

        foreach (var kvp in data.Metadata)
        {
            module.Metadata[kvp.Key] = kvp.Value;
        }

        // Load types
        foreach (var typeData in data.Types)
        {
            var typeDef = LoadType(typeData);
            module.Types.Add(typeDef);
        }

        // Load functions
        foreach (var funcData in data.Functions)
        {
            var funcDef = LoadFunction(funcData);
            module.Functions.Add(funcDef);
        }

        return module;
    }

    private static TypeDefinition LoadType(TypeData typeData)
    {
        TypeDefinition typeDef = typeData.Kind switch
        {
            "Class" => LoadClass(typeData),
            "Interface" => LoadInterface(typeData),
            "Struct" => LoadStruct(typeData),
            "Enum" => LoadEnum(typeData),
            _ => throw new NotSupportedException($"Unknown type kind: {typeData.Kind}")
        };

        // Load generic parameters
        foreach (var gp in typeData.GenericParameters)
        {
            var param = new GenericParameter(gp);
            typeDef.GenericParameters.Add(param);
        }

        return typeDef;
    }

    private static ClassDefinition LoadClass(TypeData typeData)
    {
        var classDef = new ClassDefinition(typeData.Name);
        classDef.Namespace = typeData.Namespace;
        classDef.Access = Enum.Parse<AccessModifier>(typeData.Access);
        classDef.IsAbstract = typeData.IsAbstract;
        classDef.IsSealed = typeData.IsSealed;
        if (typeData.BaseType != null)
        {
            classDef.BaseType = TypeReference.FromName(typeData.BaseType);
        }
        foreach (var iface in typeData.Interfaces)
        {
            classDef.Interfaces.Add(TypeReference.FromName(iface));
        }

        // Load fields
        foreach (var fieldData in typeData.Fields)
        {
            var field = classDef.DefineField(fieldData.Name, TypeReference.FromName(fieldData.Type));
            field.Access = Enum.Parse<AccessModifier>(fieldData.Access);
            field.IsStatic = fieldData.IsStatic;
            field.IsReadOnly = fieldData.IsReadOnly;
            if (fieldData.InitialValue != null)
            {
                field.InitialValue = fieldData.InitialValue;
            }
        }

        // Load properties
        foreach (var propData in typeData.Properties)
        {
            var prop = new PropertyDefinition(propData.Name, TypeReference.FromName(propData.Type));
            prop.Access = Enum.Parse<AccessModifier>(propData.Access);
            classDef.Properties.Add(prop);
        }

        // Load methods
        foreach (var methodData in typeData.Methods)
        {
            var method = classDef.DefineMethod(methodData.Name, TypeReference.FromName(methodData.ReturnType));
            LoadMethodData(method, methodData);
        }

        return classDef;
    }

    private static InterfaceDefinition LoadInterface(TypeData typeData)
    {
        var interfaceDef = new InterfaceDefinition(typeData.Name);
        interfaceDef.Namespace = typeData.Namespace;
        interfaceDef.Access = Enum.Parse<AccessModifier>(typeData.Access);

        foreach (var iface in typeData.BaseInterfaces)
        {
            interfaceDef.BaseInterfaces.Add(TypeReference.FromName(iface));
        }

        // Load properties
        foreach (var propData in typeData.Properties)
        {
            var prop = new PropertyDefinition(propData.Name, TypeReference.FromName(propData.Type));
            prop.Access = Enum.Parse<AccessModifier>(propData.Access);
            interfaceDef.Properties.Add(prop);
        }

        // Load methods
        foreach (var methodData in typeData.Methods)
        {
            var method = interfaceDef.DefineMethod(methodData.Name, TypeReference.FromName(methodData.ReturnType));
            LoadMethodData(method, methodData);
        }

        return interfaceDef;
    }

    private static StructDefinition LoadStruct(TypeData typeData)
    {
        var structDef = new StructDefinition(typeData.Name);
        structDef.Namespace = typeData.Namespace;
        structDef.Access = Enum.Parse<AccessModifier>(typeData.Access);

        foreach (var iface in typeData.Interfaces)
        {
            structDef.Interfaces.Add(TypeReference.FromName(iface));
        }

        // Load fields
        foreach (var fieldData in typeData.Fields)
        {
            var field = structDef.DefineField(fieldData.Name, TypeReference.FromName(fieldData.Type));
            field.Access = Enum.Parse<AccessModifier>(fieldData.Access);
            field.IsStatic = fieldData.IsStatic;
            field.IsReadOnly = fieldData.IsReadOnly;
            if (fieldData.InitialValue != null)
            {
                field.InitialValue = fieldData.InitialValue;
            }
        }

        // Load methods
        foreach (var methodData in typeData.Methods)
        {
            var method = structDef.DefineMethod(methodData.Name, TypeReference.FromName(methodData.ReturnType));
            LoadMethodData(method, methodData);
        }

        return structDef;
    }

    private static EnumDefinition LoadEnum(TypeData typeData)
    {
        var enumDef = new EnumDefinition(typeData.Name);
        enumDef.Namespace = typeData.Namespace;
        enumDef.Access = Enum.Parse<AccessModifier>(typeData.Access);
        enumDef.UnderlyingType = TypeReference.FromName(typeData.UnderlyingType ?? "int32");
        // Note: Members are not serialized
        return enumDef;
    }

    private static FunctionDefinition LoadFunction(FunctionData funcData)
    {
        var func = new FunctionDefinition(funcData.Name, TypeReference.FromName(funcData.ReturnType));

        // Load parameters
        foreach (var paramData in funcData.Parameters)
        {
            func.DefineParameter(paramData.Name, TypeReference.FromName(paramData.Type));
        }

        // Load locals
        foreach (var localData in funcData.LocalVariables)
        {
            func.DefineLocal(localData.Name, TypeReference.FromName(localData.Type));
        }

        if (funcData.Instructions is JsonArray instructionsNode)
        {
            using var instructionsDoc = JsonDocument.Parse(instructionsNode.ToJsonString());
            var instructions = InstructionSerializer.DeserializeInstructions(instructionsDoc.RootElement);
            foreach (var instruction in instructions)
            {
                func.Instructions.Add(instruction);
            }
        }

        // Note: Instructions are not loaded if missing
        return func;
    }

    private static void LoadMethodData(MethodDefinition method, MethodData methodData)
    {
        method.Access = Enum.Parse<AccessModifier>(methodData.Access);
        method.IsStatic = methodData.IsStatic;
        method.IsVirtual = methodData.IsVirtual;
        method.IsOverride = methodData.IsOverride;
        method.IsAbstract = methodData.IsAbstract;
        method.IsConstructor = methodData.IsConstructor;

        // Load parameters
        foreach (var paramData in methodData.Parameters)
        {
            method.DefineParameter(paramData.Name, TypeReference.FromName(paramData.Type));
        }

        // Load locals
        foreach (var localData in methodData.LocalVariables)
        {
            method.DefineLocal(localData.Name, TypeReference.FromName(localData.Type));
        }

        if (methodData.Instructions is JsonArray instructionsNode)
        {
            using var instructionsDoc = JsonDocument.Parse(instructionsNode.ToJsonString());
            var instructions = InstructionSerializer.DeserializeInstructions(instructionsDoc.RootElement);
            foreach (var instruction in instructions)
            {
                method.Instructions.Add(instruction);
            }
        }

        // Instructions remain empty if missing for compatibility
    }

    public static string ToJson(Module module)
    {
        var serializer = new ModuleSerializer(module);
        return serializer.DumpToJson();
    }
}

// ============================================================================
// Data Transfer Objects
// ============================================================================

/// <summary>
/// Represents serialized module data
/// </summary>
public sealed class ModuleData
{
    public string Name { get; set; } = string.Empty;
    public string Version { get; set; } = string.Empty;
    public Dictionary<string, string> Metadata { get; set; } = new();
    public TypeData[] Types { get; set; } = Array.Empty<TypeData>();
    public FunctionData[] Functions { get; set; } = Array.Empty<FunctionData>();
}

/// <summary>
/// Represents serialized type data
/// </summary>
public sealed class TypeData
{
    public string Kind { get; set; } = string.Empty; // "Class", "Interface", "Struct", "Enum"
    public string Name { get; set; } = string.Empty;
    public string? Namespace { get; set; }
    public string Access { get; set; } = string.Empty;
    public bool IsAbstract { get; set; }
    public bool IsSealed { get; set; }
    public string? BaseType { get; set; }
    public string[] Interfaces { get; set; } = Array.Empty<string>();
    public string[] BaseInterfaces { get; set; } = Array.Empty<string>(); // For interfaces
    public string? UnderlyingType { get; set; } // For enums
    public string[] GenericParameters { get; set; } = Array.Empty<string>();
    public FieldData[] Fields { get; set; } = Array.Empty<FieldData>();
    public MethodData[] Methods { get; set; } = Array.Empty<MethodData>();
    public PropertyData[] Properties { get; set; } = Array.Empty<PropertyData>();
}

/// <summary>
/// Represents serialized field data
/// </summary>
public sealed class FieldData
{
    public string Name { get; set; } = string.Empty;
    public string Type { get; set; } = string.Empty;
    public string Access { get; set; } = string.Empty;
    public bool IsStatic { get; set; }
  public bool IsReadOnly { get; set; }
    public string? InitialValue { get; set; }
}

/// <summary>
/// Represents serialized property data
/// </summary>
public sealed class PropertyData
{
    public string Name { get; set; } = string.Empty;
    public string Type { get; set; } = string.Empty;
    public string Access { get; set; } = string.Empty;
    public bool HasGetter { get; set; }
    public bool HasSetter { get; set; }
    public string? GetterAccess { get; set; }
    public string? SetterAccess { get; set; }
}

/// <summary>
/// Represents serialized method data
/// </summary>
public sealed class MethodData
{
  public string Name { get; set; } = string.Empty;
    public string ReturnType { get; set; } = string.Empty;
    public string Access { get; set; } = string.Empty;
    public bool IsStatic { get; set; }
    public bool IsVirtual { get; set; }
    public bool IsOverride { get; set; }
    public bool IsAbstract { get; set; }
    public bool IsConstructor { get; set; }
    public ParameterData[] Parameters { get; set; } = Array.Empty<ParameterData>();
    public LocalVariableData[] LocalVariables { get; set; } = Array.Empty<LocalVariableData>();
    public int InstructionCount { get; set; }
        public JsonNode? Instructions { get; set; }
}

/// <summary>
/// Represents serialized function data
/// </summary>
public sealed class FunctionData
{
    public string Name { get; set; } = string.Empty;
    public string ReturnType { get; set; } = string.Empty;
    public ParameterData[] Parameters { get; set; } = Array.Empty<ParameterData>();
    public LocalVariableData[] LocalVariables { get; set; } = Array.Empty<LocalVariableData>();
    public int InstructionCount { get; set; }
    public JsonNode? Instructions { get; set; }
}

/// <summary>
/// Represents serialized parameter data
/// </summary>
public sealed class ParameterData
{
    public string Name { get; set; } = string.Empty;
    public string Type { get; set; } = string.Empty;
}

/// <summary>
/// Represents serialized local variable data
/// </summary>
public sealed class LocalVariableData
{
    public string Name { get; set; } = string.Empty;
    public string Type { get; set; } = string.Empty;
}

#pragma warning restore CS1591
