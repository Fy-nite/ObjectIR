namespace ObjectIR.Core.Composition;

using ObjectIR.Core.IR;
using System.Collections.Generic;
using System.Linq;

/// <summary>
/// Represents a composition of multiple modules into a single unified module
/// </summary>
public sealed class ModuleComposer
{
    private readonly DependencyResolver _resolver;
    private readonly List<Module> _modules;
    private readonly Dictionary<string, int> _namespaceConflicts;

    public ModuleComposer()
    {
        _resolver = new DependencyResolver();
        _modules = new List<Module>();
        _namespaceConflicts = new Dictionary<string, int>();
    }

    /// <summary>
    /// Adds a module to the composition
    /// </summary>
    public ModuleComposer AddModule(Module module)
    {
        if (module == null) throw new ArgumentNullException(nameof(module));
        if (_modules.Any(m => m.Name == module.Name))
        {
            throw new InvalidOperationException($"Module '{module.Name}' already added");
        }

        _modules.Add(module);
        _resolver.RegisterModule(module);
        return this;
    }

    /// <summary>
    /// Gets all currently composed modules
    /// </summary>
    public IEnumerable<Module> GetModules() => _modules.AsReadOnly();

    /// <summary>
    /// Validates the composition for conflicts and missing dependencies
    /// </summary>
    public CompositionValidation Validate()
    {
        var validation = new CompositionValidation();

        // Check for type name conflicts
        var typeNames = new Dictionary<string, List<(Module module, TypeDefinition type)>>();
        foreach (var module in _modules)
        {
            foreach (var type in module.Types)
            {
                var key = type.Name;
                if (!typeNames.ContainsKey(key))
                {
                    typeNames[key] = new List<(Module, TypeDefinition)>();
                }
                typeNames[key].Add((module, type));
            }
        }

        foreach (var (typeName, occurrences) in typeNames)
        {
            if (occurrences.Count > 1)
            {
                validation.AddError($"Type '{typeName}' is defined in multiple modules: {string.Join(", ", occurrences.Select(o => o.module.Name))}");
            }
        }

        // Check for unresolved dependencies
        foreach (var module in _modules)
        {
            foreach (var type in module.Types)
            {
                ValidateTypeDependencies(type, module, validation);
            }
        }

        return validation;
    }

    /// <summary>
    /// Composes all modules into a single unified module
    /// </summary>
    public Module Compose(string compositeName, string? compositeVersion = null)
    {
        var validation = Validate();
        if (validation.HasErrors)
        {
            throw new InvalidOperationException($"Cannot compose modules with errors:\n{string.Join("\n", validation.Errors)}");
        }

        var composite = new Module(compositeName, compositeVersion ?? "1.0.0");

        // Sort types by dependency order
        var allTypes = _modules.SelectMany(m => m.Types).ToList();
        var sortedTypes = TopologicalSortTypes(allTypes);

        // Add all types to composite module
        foreach (var type in sortedTypes)
        {
            var typeCopy = CloneType(type);
            composite.Types.Add(typeCopy);
        }

        // Add all functions to composite module
        foreach (var module in _modules)
        {
            foreach (var func in module.Functions)
            {
                var funcCopy = CloneFunction(func);
                composite.Functions.Add(funcCopy);
            }
        }

        // Merge metadata
        foreach (var module in _modules)
        {
            foreach (var (key, value) in module.Metadata)
            {
                var compositeKey = $"{module.Name}.{key}";
                composite.Metadata[compositeKey] = value;
            }
        }

        // Add source module information
        composite.Metadata["ComposedFrom"] = string.Join(", ", _modules.Select(m => m.Name));
        composite.Metadata["CompositionTime"] = DateTime.UtcNow.ToString("o");

        return composite;
    }

    /// <summary>
    /// Creates a link between two modules for namespace aliasing
    /// </summary>
    public ModuleComposer LinkNamespace(string sourceNamespace, string targetModule, string targetNamespace)
    {
        // This information would be stored for use during composition
        var key = $"{sourceNamespace}->{targetModule}.{targetNamespace}";
        _namespaceConflicts[key] = 1;
        return this;
    }

    /// <summary>
    /// Gets the dependency graph as an adjacency list
    /// </summary>
    public Dictionary<string, List<string>> GetDependencyGraph()
    {
        var graph = new Dictionary<string, List<string>>();
        var allTypes = _modules.SelectMany(m => m.Types).ToList();

        foreach (var type in allTypes)
        {
            var typeKey = $"{type.Name}";
            if (!graph.ContainsKey(typeKey))
            {
                graph[typeKey] = new List<string>();
            }

            var deps = _resolver.GetDependencies(type);
            foreach (var dep in deps)
            {
                if (dep != type)
                {
                    var depKey = $"{dep.Name}";
                    if (!graph[typeKey].Contains(depKey))
                    {
                        graph[typeKey].Add(depKey);
                    }
                }
            }
        }

        return graph;
    }

    /// <summary>
    /// Generates a report of the composition
    /// </summary>
    public string GenerateReport()
    {
        var lines = new List<string>();
        lines.Add("=== Module Composition Report ===");
        lines.Add($"Total modules: {_modules.Count}");
        lines.Add($"Total types: {_modules.Sum(m => m.Types.Count)}");
        lines.Add($"Total functions: {_modules.Sum(m => m.Functions.Count)}");
        lines.Add("");

        foreach (var module in _modules)
        {
            lines.Add($"Module: {module.Name} (v{module.Version})");
            lines.Add($"  Types: {module.Types.Count}");
            foreach (var type in module.Types)
            {
                lines.Add($"    - {type.Name} ({type.Kind})");
            }
            lines.Add($"  Functions: {module.Functions.Count}");
            lines.Add("");
        }

        return string.Join("\n", lines);
    }

    // ============================================================================
    // Private Methods
    // ============================================================================

    private void ValidateTypeDependencies(TypeDefinition type, Module module, CompositionValidation validation)
    {
        // Only ClassDefinition has BaseType and Interfaces
        if (type is ClassDefinition classDef)
        {
            if (classDef.BaseType != null)
            {
                var baseResolved = _resolver.ResolveType(classDef.BaseType);
                if (baseResolved == null && !IsBuiltInType(classDef.BaseType))
                {
                    validation.AddWarning($"Base class '{classDef.BaseType.Name}' of type '{type.Name}' in module '{module.Name}' not found in composition");
                }
            }

            foreach (var iface in classDef.Interfaces)
            {
                var ifaceResolved = _resolver.ResolveType(iface);
                if (ifaceResolved == null && !IsBuiltInType(iface))
                {
                    validation.AddWarning($"Interface '{iface.Name}' implemented by type '{type.Name}' in module '{module.Name}' not found in composition");
                }
            }
        }
        else if (type is InterfaceDefinition ifaceDef)
        {
            foreach (var baseIface in ifaceDef.BaseInterfaces)
            {
                var baseResolved = _resolver.ResolveType(baseIface);
                if (baseResolved == null && !IsBuiltInType(baseIface))
                {
                    validation.AddWarning($"Base interface '{baseIface.Name}' of interface '{type.Name}' in module '{module.Name}' not found in composition");
                }
            }
        }
        else if (type is StructDefinition structDef)
        {
            foreach (var iface in structDef.Interfaces)
            {
                var ifaceResolved = _resolver.ResolveType(iface);
                if (ifaceResolved == null && !IsBuiltInType(iface))
                {
                    validation.AddWarning($"Interface '{iface.Name}' implemented by struct '{type.Name}' in module '{module.Name}' not found in composition");
                }
            }
        }
    }

    private bool IsBuiltInType(TypeReference typeRef)
    {
        return typeRef.Name is "void" or "bool" or "int32" or "int64" or "float32" or "float64" or "string" or "object";
    }

    private IEnumerable<TypeDefinition> TopologicalSortTypes(List<TypeDefinition> types)
    {
        var sorted = new List<TypeDefinition>();
        var visited = new HashSet<TypeDefinition>();
        var visiting = new HashSet<TypeDefinition>();

        foreach (var type in types)
        {
            if (!visited.Contains(type))
            {
                VisitType(type, sorted, visited, visiting, types);
            }
        }

        return sorted;
    }

    private void VisitType(TypeDefinition type, List<TypeDefinition> sorted, HashSet<TypeDefinition> visited, HashSet<TypeDefinition> visiting, List<TypeDefinition> allTypes)
    {
        if (visited.Contains(type)) return;
        if (visiting.Contains(type)) return;

        visiting.Add(type);

        // Visit base class if this is a ClassDefinition
        if (type is ClassDefinition classDef && classDef.BaseType != null)
        {
            var baseType = allTypes.FirstOrDefault(t => t.Name == classDef.BaseType.Name);
            if (baseType != null && !visited.Contains(baseType))
            {
                VisitType(baseType, sorted, visited, visiting, allTypes);
            }
        }

        visiting.Remove(type);
        visited.Add(type);
        sorted.Add(type);
    }

    private TypeDefinition CloneType(TypeDefinition type)
    {
        return type switch
        {
            ClassDefinition classDef => CloneClass(classDef),
            InterfaceDefinition ifaceDef => CloneInterface(ifaceDef),
            StructDefinition structDef => CloneStruct(structDef),
            EnumDefinition enumDef => CloneEnum(enumDef),
            _ => throw new NotSupportedException($"Type {type.GetType().Name} not supported for cloning")
        };
    }

    private ClassDefinition CloneClass(ClassDefinition classDef)
    {
        var cloned = new ClassDefinition(classDef.Name)
        {
            Access = classDef.Access,
            IsAbstract = classDef.IsAbstract,
            IsSealed = classDef.IsSealed,
            BaseType = classDef.BaseType,
            Namespace = classDef.Namespace
        };

        foreach (var iface in classDef.Interfaces)
        {
            cloned.Interfaces.Add(iface);
        }

        foreach (var field in classDef.Fields)
        {
            cloned.Fields.Add(new FieldDefinition(field.Name, field.Type)
            {
                Access = field.Access,
                IsStatic = field.IsStatic,
                IsReadOnly = field.IsReadOnly
            });
        }

        foreach (var method in classDef.Methods)
        {
            cloned.Methods.Add(CloneMethod(method));
        }

        foreach (var genericParam in classDef.GenericParameters)
        {
            cloned.GenericParameters.Add(genericParam);
        }

        return cloned;
    }

    private InterfaceDefinition CloneInterface(InterfaceDefinition ifaceDef)
    {
        var cloned = new InterfaceDefinition(ifaceDef.Name)
        {
            Access = ifaceDef.Access,
            Namespace = ifaceDef.Namespace
        };

        foreach (var baseIface in ifaceDef.BaseInterfaces)
        {
            cloned.BaseInterfaces.Add(baseIface);
        }

        foreach (var method in ifaceDef.Methods)
        {
            cloned.Methods.Add(CloneMethod(method));
        }

        foreach (var genericParam in ifaceDef.GenericParameters)
        {
            cloned.GenericParameters.Add(genericParam);
        }

        return cloned;
    }

    private StructDefinition CloneStruct(StructDefinition structDef)
    {
        var cloned = new StructDefinition(structDef.Name)
        {
            Access = structDef.Access,
            Namespace = structDef.Namespace
        };

        foreach (var iface in structDef.Interfaces)
        {
            cloned.Interfaces.Add(iface);
        }

        foreach (var field in structDef.Fields)
        {
            cloned.Fields.Add(new FieldDefinition(field.Name, field.Type)
            {
                Access = field.Access,
                IsStatic = field.IsStatic,
                IsReadOnly = field.IsReadOnly
            });
        }

        foreach (var method in structDef.Methods)
        {
            cloned.Methods.Add(CloneMethod(method));
        }

        foreach (var genericParam in structDef.GenericParameters)
        {
            cloned.GenericParameters.Add(genericParam);
        }

        return cloned;
    }

    private EnumDefinition CloneEnum(EnumDefinition enumDef)
    {
        var cloned = new EnumDefinition(enumDef.Name)
        {
            Access = enumDef.Access,
            Namespace = enumDef.Namespace,
            UnderlyingType = enumDef.UnderlyingType
        };

        foreach (var member in enumDef.Members)
        {
            cloned.Members.Add(new EnumMember(member.Name, member.Value));
        }

        return cloned;
    }

    private MethodDefinition CloneMethod(MethodDefinition method)
    {
        var cloned = new MethodDefinition(method.Name, method.ReturnType)
        {
            Access = method.Access,
            IsStatic = method.IsStatic,
            IsVirtual = method.IsVirtual,
            IsOverride = method.IsOverride,
            IsAbstract = method.IsAbstract,
            IsConstructor = method.IsConstructor
        };

        foreach (var param in method.Parameters)
        {
            cloned.Parameters.Add(new Parameter(param.Name, param.Type));
        }

        foreach (var local in method.Locals)
        {
            cloned.Locals.Add(new LocalVariable(local.Name, local.Type));
        }

        foreach (var instruction in method.Instructions)
        {
            cloned.Instructions.Add(instruction);
        }

        foreach (var genericParam in method.GenericParameters)
        {
            cloned.GenericParameters.Add(genericParam);
        }

        return cloned;
    }

    private FunctionDefinition CloneFunction(FunctionDefinition function)
    {
        var cloned = new FunctionDefinition(function.Name, function.ReturnType);

        foreach (var param in function.Parameters)
        {
            cloned.Parameters.Add(new Parameter(param.Name, param.Type));
        }

        foreach (var local in function.Locals)
        {
            cloned.Locals.Add(new LocalVariable(local.Name, local.Type));
        }

        foreach (var instruction in function.Instructions)
        {
            cloned.Instructions.Add(instruction);
        }

        foreach (var genericParam in function.GenericParameters)
        {
            cloned.GenericParameters.Add(genericParam);
        }

        return cloned;
    }
}

/// <summary>
/// Represents validation results for module composition
/// </summary>
public sealed class CompositionValidation
{
    private readonly List<string> _errors = new();
    private readonly List<string> _warnings = new();

    public IReadOnlyList<string> Errors => _errors.AsReadOnly();
    public IReadOnlyList<string> Warnings => _warnings.AsReadOnly();
    public bool HasErrors => _errors.Count > 0;
    public bool HasWarnings => _warnings.Count > 0;

    public void AddError(string message) => _errors.Add(message);
    public void AddWarning(string message) => _warnings.Add(message);

    public override string ToString()
    {
        var lines = new List<string>();
        if (_errors.Count > 0)
        {
            lines.Add("ERRORS:");
            lines.AddRange(_errors.Select(e => $"  - {e}"));
        }
        if (_warnings.Count > 0)
        {
            lines.Add("WARNINGS:");
            lines.AddRange(_warnings.Select(w => $"  - {w}"));
        }
        return string.Join("\n", lines);
    }
}
