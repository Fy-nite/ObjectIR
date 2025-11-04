namespace ObjectIR.Core.Composition;

using ObjectIR.Core.IR;
using System.Collections.Generic;
using System.Linq;

/// <summary>
/// Resolves type and method dependencies across modules
/// </summary>
public sealed class DependencyResolver
{
    private readonly Dictionary<string, Module> _modules;
    private readonly Dictionary<string, TypeDefinition> _typeCache;
    private readonly HashSet<string> _visited;

    public DependencyResolver()
    {
        _modules = new Dictionary<string, Module>();
        _typeCache = new Dictionary<string, TypeDefinition>();
        _visited = new HashSet<string>();
    }

    /// <summary>
    /// Registers a module for dependency resolution
    /// </summary>
    public void RegisterModule(Module module)
    {
        if (module == null) throw new ArgumentNullException(nameof(module));
        _modules[module.Name] = module;

        // Index all types in the module
        foreach (var type in module.Types)
        {
            var fullName = $"{module.Name}.{type.Name}";
            _typeCache[fullName] = type;
        }
    }

    /// <summary>
    /// Resolves a type reference to its definition across registered modules
    /// </summary>
    public TypeDefinition? ResolveType(TypeReference typeRef)
    {
        if (typeRef == null) return null;

        // Check for built-in types
        if (IsBuiltInType(typeRef)) return null;

        // Search for the type in registered modules
        foreach (var type in _typeCache.Values)
        {
            if (type.Name == typeRef.Name)
            {
                return type;
            }
        }

        return null;
    }

    private bool IsBuiltInType(TypeReference typeRef)
    {
        return typeRef.Name is "void" or "bool" or "int32" or "int64" or "float32" or "float64" or "string" or "object";
    }

    /// <summary>
    /// Resolves a method reference to its definition across registered modules
    /// </summary>
    public MethodDefinition? ResolveMethod(MethodReference methodRef)
    {
        if (methodRef == null) return null;

        var declaringType = ResolveType(methodRef.DeclaringType);
        if (declaringType == null) return null;

        // Get methods based on type kind
        var methods = declaringType switch
        {
            ClassDefinition classDef => classDef.Methods,
            InterfaceDefinition ifaceDef => ifaceDef.Methods,
            StructDefinition structDef => structDef.Methods,
            _ => new List<MethodDefinition>()
        };

        return methods.FirstOrDefault(m => m.Name == methodRef.Name);
    }

    /// <summary>
    /// Gets all types that a given type depends on
    /// </summary>
    public IEnumerable<TypeDefinition> GetDependencies(TypeDefinition type)
    {
        var dependencies = new HashSet<TypeDefinition>();
        CollectDependencies(type, dependencies);
        return dependencies;
    }

    /// <summary>
    /// Gets all types that depend on a given type
    /// </summary>
    public IEnumerable<TypeDefinition> GetDependents(TypeDefinition type)
    {
        var dependents = new HashSet<TypeDefinition>();

        foreach (var otherType in _typeCache.Values)
        {
            if (otherType == type) continue;

            var deps = GetDependencies(otherType);
            if (deps.Contains(type))
            {
                dependents.Add(otherType);
            }
        }

        return dependents;
    }

    /// <summary>
    /// Topologically sorts types based on dependencies
    /// </summary>
    public IEnumerable<TypeDefinition> TopologicalSort()
    {
        var sorted = new List<TypeDefinition>();
        var visited = new HashSet<TypeDefinition>();
        var visiting = new HashSet<TypeDefinition>();

        foreach (var type in _typeCache.Values)
        {
            if (!visited.Contains(type))
            {
                Visit(type, sorted, visited, visiting);
            }
        }

        return sorted;
    }

    /// <summary>
    /// Detects circular dependencies
    /// </summary>
    public IEnumerable<List<TypeDefinition>> FindCircularDependencies()
    {
        var cycles = new List<List<TypeDefinition>>();
        var visited = new HashSet<TypeDefinition>();

        foreach (var type in _typeCache.Values)
        {
            if (!visited.Contains(type))
            {
                var path = new List<TypeDefinition>();
                var pathSet = new HashSet<TypeDefinition>();
                FindCycles(type, path, pathSet, visited, cycles);
            }
        }

        return cycles;
    }

    // ============================================================================
    // Private Methods
    // ============================================================================

    private void CollectDependencies(TypeDefinition type, HashSet<TypeDefinition> dependencies)
    {
        if (dependencies.Contains(type)) return;
        dependencies.Add(type);

        if (type is ClassDefinition classDef)
        {
            // Base class dependencies
            if (classDef.BaseType != null)
            {
                var baseType = ResolveType(classDef.BaseType);
                if (baseType != null)
                {
                    CollectDependencies(baseType, dependencies);
                }
            }

            // Interface dependencies
            foreach (var iface in classDef.Interfaces)
            {
                var ifaceType = ResolveType(iface);
                if (ifaceType != null)
                {
                    CollectDependencies(ifaceType, dependencies);
                }
            }

            // Field type dependencies
            foreach (var field in classDef.Fields)
            {
                var fieldType = ResolveType(field.Type);
                if (fieldType != null)
                {
                    CollectDependencies(fieldType, dependencies);
                }
            }

            // Method parameter and return type dependencies
            foreach (var method in classDef.Methods)
            {
                var returnType = ResolveType(method.ReturnType);
                if (returnType != null)
                {
                    CollectDependencies(returnType, dependencies);
                }

                foreach (var param in method.Parameters)
                {
                    var paramType = ResolveType(param.Type);
                    if (paramType != null)
                    {
                        CollectDependencies(paramType, dependencies);
                    }
                }
            }
        }
        else if (type is InterfaceDefinition ifaceDef)
        {
            // Base interface dependencies
            foreach (var baseIface in ifaceDef.BaseInterfaces)
            {
                var baseIfaceType = ResolveType(baseIface);
                if (baseIfaceType != null)
                {
                    CollectDependencies(baseIfaceType, dependencies);
                }
            }

            // Method dependencies
            foreach (var method in ifaceDef.Methods)
            {
                var returnType = ResolveType(method.ReturnType);
                if (returnType != null)
                {
                    CollectDependencies(returnType, dependencies);
                }

                foreach (var param in method.Parameters)
                {
                    var paramType = ResolveType(param.Type);
                    if (paramType != null)
                    {
                        CollectDependencies(paramType, dependencies);
                    }
                }
            }
        }
        else if (type is StructDefinition structDef)
        {
            // Interface dependencies
            foreach (var iface in structDef.Interfaces)
            {
                var ifaceType = ResolveType(iface);
                if (ifaceType != null)
                {
                    CollectDependencies(ifaceType, dependencies);
                }
            }

            // Field type dependencies
            foreach (var field in structDef.Fields)
            {
                var fieldType = ResolveType(field.Type);
                if (fieldType != null)
                {
                    CollectDependencies(fieldType, dependencies);
                }
            }

            // Method dependencies
            foreach (var method in structDef.Methods)
            {
                var returnType = ResolveType(method.ReturnType);
                if (returnType != null)
                {
                    CollectDependencies(returnType, dependencies);
                }

                foreach (var param in method.Parameters)
                {
                    var paramType = ResolveType(param.Type);
                    if (paramType != null)
                    {
                        CollectDependencies(paramType, dependencies);
                    }
                }
            }
        }
    }

    private void Visit(TypeDefinition type, List<TypeDefinition> sorted, HashSet<TypeDefinition> visited, HashSet<TypeDefinition> visiting)
    {
        if (visited.Contains(type)) return;
        if (visiting.Contains(type)) return; // Cycle detection

        visiting.Add(type);

        var deps = GetDependencies(type);
        foreach (var dep in deps)
        {
            if (dep != type)
            {
                Visit(dep, sorted, visited, visiting);
            }
        }

        visiting.Remove(type);
        visited.Add(type);
        sorted.Add(type);
    }

    private void FindCycles(TypeDefinition type, List<TypeDefinition> path, HashSet<TypeDefinition> pathSet, HashSet<TypeDefinition> visited, List<List<TypeDefinition>> cycles)
    {
        if (visited.Contains(type)) return;

        if (pathSet.Contains(type))
        {
            // Found a cycle
            var cycleStart = path.IndexOf(type);
            cycles.Add(path.Skip(cycleStart).ToList());
            return;
        }

        path.Add(type);
        pathSet.Add(type);

        var deps = GetDependencies(type);
        foreach (var dep in deps)
        {
            if (dep != type)
            {
                FindCycles(dep, path, pathSet, visited, cycles);
            }
        }

        path.RemoveAt(path.Count - 1);
        pathSet.Remove(type);
        visited.Add(type);
    }
}
