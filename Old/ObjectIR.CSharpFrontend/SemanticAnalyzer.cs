using System;
using System.Collections.Generic;
using System.Linq;
using ObjectIR.CSharpFrontend.AST;

namespace ObjectIR.CSharpFrontend;

/// <summary>
/// Semantic analysis phase: validates AST and builds symbol tables.
/// Performs:
/// - Symbol table construction (types, members, local variables)
/// - Scope management
/// - Type reference resolution
/// - Method signature validation
/// - Forward reference handling
/// </summary>
public class SemanticAnalyzer
    {
        private readonly CompilationUnit _ast;
        private readonly List<SemanticError> _errors = new();
        private readonly Stack<Scope> _scopes = new();
        
        // Global scope for top-level types
        private Scope _globalScope;
        
        // Maps fully qualified names to type definitions
        private readonly Dictionary<string, TypeSymbol> _typeSymbols = new();
        
        public SemanticAnalyzer(CompilationUnit ast)
        {
            _ast = ast ?? throw new ArgumentNullException(nameof(ast));
        }

        /// <summary>
        /// Performs semantic analysis on the AST.
        /// Returns true if no errors, false otherwise.
        /// </summary>
        public bool Analyze()
        {
            _errors.Clear();
            _typeSymbols.Clear();
            _scopes.Clear();

            // Phase 1: Collect all type definitions in global scope
            _globalScope = new Scope(null, ScopeKind.Module);
            _scopes.Push(_globalScope);
            CollectTypeDefinitions();

            // Phase 2: Validate type references and resolve them
            if (_errors.Count == 0)
            {
                ValidateTypeReferences();
            }

            // Phase 3: Validate members (methods, fields, properties)
            if (_errors.Count == 0)
            {
                ValidateMembers();
            }

            _scopes.Pop();
            return _errors.Count == 0;
        }

        /// <summary>
        /// Gets all semantic errors encountered during analysis.
        /// </summary>
        public IReadOnlyList<SemanticError> GetErrors() => _errors.AsReadOnly();

        /// <summary>
        /// Gets the global symbol table.
        /// </summary>
        public Scope GetGlobalScope() => _globalScope;

        /// <summary>
        /// Gets symbol information for a type.
        /// </summary>
        public TypeSymbol GetTypeSymbol(string fullyQualifiedName)
        {
            _typeSymbols.TryGetValue(fullyQualifiedName, out var symbol);
            return symbol;
        }

        // ==================== PHASE 1: Collect Type Definitions ====================

        private void CollectTypeDefinitions()
        {
            foreach (var ns in _ast.Namespaces)
            {
                var namespaceName = ns.Name;
                var namespaceScope = new Scope(_globalScope, ScopeKind.Namespace);
                namespaceScope.NamespacePath = namespaceName;

                foreach (var type in ns.Types)
                {
                    CollectTypeDefinition(type, namespaceScope, namespaceName);
                }

                _globalScope.DefineNamespace(namespaceName, namespaceScope);
            }

            // Handle global types (outside namespaces)
            foreach (var type in _ast.GlobalTypes)
            {
                CollectTypeDefinition(type, _globalScope, "");
            }
        }

        private void CollectTypeDefinition(TypeDeclaration typeDecl, Scope scope, string namespaceName)
        {
            var qualifiedName = string.IsNullOrEmpty(namespaceName)
                ? typeDecl.Name
                : $"{namespaceName}.{typeDecl.Name}";

            // Check for duplicate type names in same scope
            if (scope.LookupType(typeDecl.Name) != null)
            {
                _errors.Add(new SemanticError(
                    $"Type '{qualifiedName}' is already defined in this scope",
                    typeDecl.Location));
                return;
            }

            // Create type symbol
            var typeSymbol = new TypeSymbol
            {
                Name = typeDecl.Name,
                FullyQualifiedName = qualifiedName,
                Declaration = typeDecl,
                Kind = GetTypeKind(typeDecl),
                AccessLevel = GetAccessLevel(typeDecl.Access),
                IsAbstract = typeDecl.Modifiers?.Contains(Modifier.Abstract) ?? false,
                IsSealed = typeDecl.Modifiers?.Contains(Modifier.Sealed) ?? false,
                IsStatic = typeDecl.Modifiers?.Contains(Modifier.Static) ?? false,
                Location = typeDecl.Location
            };

            // Handle base type reference (only for classes)
            if (typeDecl is ClassDeclaration classDecl && classDecl.BaseClass != null)
            {
                typeSymbol.BaseTypeName = ExtractTypeName(classDecl.BaseClass);
            }

            // Handle interface implementations
            List<TypeReference> interfaces = null;
            if (typeDecl is ClassDeclaration cDecl)
            {
                interfaces = cDecl.Interfaces;
            }
            else if (typeDecl is InterfaceDeclaration iDecl)
            {
                interfaces = iDecl.BaseInterfaces;
            }
            else if (typeDecl is StructDeclaration sDecl)
            {
                interfaces = sDecl.Interfaces;
            }

            if (interfaces != null && interfaces.Count > 0)
            {
                foreach (var iface in interfaces)
                {
                    typeSymbol.InterfaceNames.Add(ExtractTypeName(iface));
                }
            }

            // Create scope for type members
            var typeScope = new Scope(scope, ScopeKind.Type);
            typeScope.TypeSymbol = typeSymbol;

            // Collect members
            List<Member> members = null;
            if (typeDecl is ClassDeclaration cMembersDecl)
                members = cMembersDecl.Members;
            else if (typeDecl is InterfaceDeclaration iMembersDecl)
                members = iMembersDecl.Members;
            else if (typeDecl is StructDeclaration sMembersDecl)
                members = sMembersDecl.Members;

            if (members != null)
            {
                foreach (var member in members)
                {
                    CollectMember(member, typeScope, typeSymbol);
                }
            }

            typeSymbol.MemberScope = typeScope;
            scope.DefineType(typeDecl.Name, typeSymbol);
            _typeSymbols[qualifiedName] = typeSymbol;
        }

        private void CollectMember(Member member, Scope typeScope, TypeSymbol typeSymbol)
        {
            switch (member)
            {
                case FieldDeclaration field:
                    CollectField(field, typeScope, typeSymbol);
                    break;

                case PropertyDeclaration prop:
                    CollectProperty(prop, typeScope, typeSymbol);
                    break;

                case MethodDeclaration method:
                    CollectMethod(method, typeScope, typeSymbol);
                    break;

                case ConstructorDeclaration ctor:
                    CollectConstructor(ctor, typeScope, typeSymbol);
                    break;
            }
        }

        private void CollectField(FieldDeclaration field, Scope typeScope, TypeSymbol typeSymbol)
        {
            var symbol = new MemberSymbol
            {
                Name = field.Name,
                Kind = MemberKind.Field,
                TypeName = ExtractTypeName(field.FieldType),
                AccessLevel = GetAccessLevel(field.Access),
                IsStatic = field.Modifiers?.Contains(Modifier.Static) ?? false,
                IsReadOnly = field.Modifiers?.Contains(Modifier.Readonly) ?? false,
                Declaration = field,
                Location = field.Location
            };

            if (typeScope.LookupMember(field.Name) != null)
            {
                _errors.Add(new SemanticError(
                    $"Member '{field.Name}' is already defined in type '{typeSymbol.Name}'",
                    field.Location));
                return;
            }

            typeScope.DefineMember(field.Name, symbol);
            typeSymbol.Members.Add(symbol);
        }

        private void CollectProperty(PropertyDeclaration prop, Scope typeScope, TypeSymbol typeSymbol)
        {
            var symbol = new MemberSymbol
            {
                Name = prop.Name,
                Kind = MemberKind.Property,
                TypeName = ExtractTypeName(prop.PropertyType),
                AccessLevel = GetAccessLevel(prop.Access),
                IsStatic = prop.Modifiers?.Contains(Modifier.Static) ?? false,
                IsAbstract = prop.Modifiers?.Contains(Modifier.Abstract) ?? false,
                Declaration = prop,
                Location = prop.Location
            };

            if (typeScope.LookupMember(prop.Name) != null)
            {
                _errors.Add(new SemanticError(
                    $"Member '{prop.Name}' is already defined in type '{typeSymbol.Name}'",
                    prop.Location));
                return;
            }

            typeScope.DefineMember(prop.Name, symbol);
            typeSymbol.Members.Add(symbol);
        }

        private void CollectMethod(MethodDeclaration method, Scope typeScope, TypeSymbol typeSymbol)
        {
            var signature = BuildMethodSignature(method);
            var symbol = new MemberSymbol
            {
                Name = method.Name,
                Kind = MemberKind.Method,
                Signature = signature,
                ReturnTypeName = ExtractTypeName(method.ReturnType),
                AccessLevel = GetAccessLevel(method.Access),
                IsStatic = method.Modifiers?.Contains(Modifier.Static) ?? false,
                IsAbstract = method.Modifiers?.Contains(Modifier.Abstract) ?? false,
                IsVirtual = method.Modifiers?.Contains(Modifier.Virtual) ?? false,
                IsOverride = method.Modifiers?.Contains(Modifier.Override) ?? false,
                Declaration = method,
                Location = method.Location
            };

            // Handle method overloading - multiple methods with same name but different signatures
            var existingMember = typeScope.LookupMember(method.Name);
            if (existingMember != null && existingMember.Kind == MemberKind.Method)
            {
                // Allow method overloading - check if signatures differ
                if (existingMember.Signature == signature)
                {
                    _errors.Add(new SemanticError(
                        $"Method '{method.Name}' with signature '{signature}' is already defined in type '{typeSymbol.Name}'",
                        method.Location));
                    return;
                }
                // Overload is OK - store in an overload list
                if (!typeScope.MethodOverloads.ContainsKey(method.Name))
                {
                    typeScope.MethodOverloads[method.Name] = new List<MemberSymbol>();
                }
                typeScope.MethodOverloads[method.Name].Add(symbol);
            }
            else if (existingMember != null)
            {
                _errors.Add(new SemanticError(
                    $"Member '{method.Name}' is already defined as {existingMember.Kind} in type '{typeSymbol.Name}'",
                    method.Location));
                return;
            }
            else
            {
                typeScope.DefineMember(method.Name, symbol);
            }

            typeSymbol.Members.Add(symbol);
        }

        private void CollectConstructor(ConstructorDeclaration ctor, Scope typeScope, TypeSymbol typeSymbol)
        {
            var signature = BuildConstructorSignature(ctor);
            var symbol = new MemberSymbol
            {
                Name = ".ctor",
                Kind = MemberKind.Constructor,
                Signature = signature,
                AccessLevel = GetAccessLevel(ctor.Access),
                Declaration = ctor,
                Location = ctor.Location
            };

            // Check for duplicate constructors
            if (typeScope.MethodOverloads.ContainsKey(".ctor"))
            {
                var existing = typeScope.MethodOverloads[".ctor"]
                    .FirstOrDefault(m => m.Signature == signature);
                if (existing != null)
                {
                    _errors.Add(new SemanticError(
                        $"Constructor with signature '{signature}' is already defined in type '{typeSymbol.Name}'",
                        ctor.Location));
                    return;
                }
            }
            else
            {
                typeScope.MethodOverloads[".ctor"] = new List<MemberSymbol>();
            }

            typeScope.MethodOverloads[".ctor"].Add(symbol);
            typeSymbol.Members.Add(symbol);
        }

        // ==================== PHASE 2: Validate Type References ====================

        private void ValidateTypeReferences()
        {
            foreach (var typeSymbol in _typeSymbols.Values)
            {
                // Validate base type
                if (!string.IsNullOrEmpty(typeSymbol.BaseTypeName) && typeSymbol.BaseTypeName != "object")
                {
                    var baseType = ResolveType(typeSymbol.BaseTypeName);
                    if (baseType == null)
                    {
                        _errors.Add(new SemanticError(
                            $"Base type '{typeSymbol.BaseTypeName}' not found",
                            typeSymbol.Location));
                    }
                    else
                    {
                        typeSymbol.BaseType = baseType;
                    }
                }

                // Validate interface implementations
                foreach (var interfaceName in typeSymbol.InterfaceNames)
                {
                    var interfaceType = ResolveType(interfaceName);
                    if (interfaceType == null)
                    {
                        _errors.Add(new SemanticError(
                            $"Interface '{interfaceName}' not found",
                            typeSymbol.Location));
                    }
                    else if (interfaceType.Kind != TypeKind.Interface)
                    {
                        _errors.Add(new SemanticError(
                            $"Type '{interfaceName}' is not an interface",
                            typeSymbol.Location));
                    }
                    else
                    {
                        typeSymbol.Interfaces.Add(interfaceType);
                    }
                }

                // Validate member types
                foreach (var member in typeSymbol.Members)
                {
                    ValidateMemberType(member, typeSymbol);
                }
            }
        }

        private void ValidateMemberType(MemberSymbol member, TypeSymbol owner)
        {
            string typeName = member.Kind switch
            {
                MemberKind.Field => member.TypeName,
                MemberKind.Property => member.TypeName,
                MemberKind.Method => member.ReturnTypeName,
                _ => null
            };

            if (!string.IsNullOrEmpty(typeName) && !IsPrimitiveType(typeName))
            {
                var resolvedType = ResolveType(typeName);
                if (resolvedType == null)
                {
                    _errors.Add(new SemanticError(
                        $"Type '{typeName}' not found in member '{member.Name}'",
                        member.Location));
                }
            }
        }

        private TypeSymbol ResolveType(string typeName)
        {
            // Handle primitive types
            if (IsPrimitiveType(typeName))
                return null;

            // Handle array notation (e.g., "int[]")
            if (typeName.EndsWith("[]"))
                return ResolveType(typeName.Substring(0, typeName.Length - 2));

            // Handle generic types (e.g., "List<T>")
            if (typeName.Contains("<"))
            {
                var baseName = typeName.Substring(0, typeName.IndexOf('<'));
                return ResolveType(baseName);
            }

            // Direct lookup in global symbols
            if (_typeSymbols.TryGetValue(typeName, out var symbol))
                return symbol;

            // Try with common namespaces
            foreach (var ns in new[] { "System", "System.Collections.Generic" })
            {
                var fullyQualified = $"{ns}.{typeName}";
                if (_typeSymbols.TryGetValue(fullyQualified, out symbol))
                    return symbol;
            }

            return null;
        }

        // ==================== PHASE 3: Validate Members ====================

        private void ValidateMembers()
        {
            foreach (var typeSymbol in _typeSymbols.Values)
            {
                ValidateMethodSignatures(typeSymbol);
                ValidateAbstractMembers(typeSymbol);
            }
        }

        private void ValidateMethodSignatures(TypeSymbol typeSymbol)
        {
            var methods = typeSymbol.Members
                .Where(m => m.Kind == MemberKind.Method)
                .ToList();

            foreach (var method in methods)
            {
                // Check for override validity
                if (method.IsOverride && typeSymbol.BaseType == null)
                {
                    _errors.Add(new SemanticError(
                        $"Method '{method.Name}' cannot override - no base class",
                        method.Location));
                }

                // Check for abstract on non-abstract class
                if (method.IsAbstract && !typeSymbol.IsAbstract)
                {
                    _errors.Add(new SemanticError(
                        $"Non-abstract type '{typeSymbol.Name}' cannot have abstract method '{method.Name}'",
                        method.Location));
                }
            }
        }

        private void ValidateAbstractMembers(TypeSymbol typeSymbol)
        {
            if (typeSymbol.IsAbstract)
                return; // Abstract classes can have unimplemented abstract members

            // For concrete classes, all abstract members from base types must be implemented
            var abstractMembers = GetAbstractMembers(typeSymbol.BaseType);
            var implementedMembers = typeSymbol.Members
                .Where(m => m.Kind == MemberKind.Method)
                .Select(m => m.Name)
                .ToHashSet();

            foreach (var abstractMember in abstractMembers)
            {
                if (!implementedMembers.Contains(abstractMember))
                {
                    _errors.Add(new SemanticError(
                        $"Type '{typeSymbol.Name}' does not implement abstract member '{abstractMember}' from base type",
                        typeSymbol.Location));
                }
            }
        }

        private List<string> GetAbstractMembers(TypeSymbol type)
        {
            var members = new List<string>();
            while (type != null)
            {
                members.AddRange(type.Members
                    .Where(m => m.IsAbstract)
                    .Select(m => m.Name));
                type = type.BaseType;
            }
            return members;
        }

        // ==================== Helper Methods ====================

        private TypeKind GetTypeKind(TypeDeclaration decl) => decl switch
        {
            ClassDeclaration => TypeKind.Class,
            InterfaceDeclaration => TypeKind.Interface,
            StructDeclaration => TypeKind.Struct,
            EnumDeclaration => TypeKind.Enum,
            _ => TypeKind.Unknown
        };

        private AccessLevel GetAccessLevel(AccessModifier access)
        {
            return access switch
            {
                AccessModifier.Public => AccessLevel.Public,
                AccessModifier.Private => AccessLevel.Private,
                AccessModifier.Protected => AccessLevel.Protected,
                AccessModifier.Internal => AccessLevel.Internal,
                AccessModifier.ProtectedInternal => AccessLevel.Protected,
                _ => AccessLevel.Internal
            };
        }

        private string BuildNamespacePath(List<string> names)
        {
            return names == null || names.Count == 0 ? "" : string.Join(".", names);
        }

        private string BuildMethodSignature(MethodDeclaration method)
        {
            var paramTypes = method.Parameters
                ?.Select(p => ExtractTypeName(p.ParameterType))
                .ToList() ?? new List<string>();

            var returnType = ExtractTypeName(method.ReturnType);
            return $"{method.Name}({string.Join(", ", paramTypes)}) -> {returnType}";
        }

        private string BuildConstructorSignature(ConstructorDeclaration ctor)
        {
            var paramTypes = ctor.Parameters
                ?.Select(p => ExtractTypeName(p.ParameterType))
                .ToList() ?? new List<string>();

            return $".ctor({string.Join(", ", paramTypes)})";
        }

        private string ExtractTypeName(TypeReference typeRef)
        {
            if (typeRef == null)
                return "dynamic";

            return typeRef switch
            {
                PredefinedType pt => pt.Name,
                NamedType nt => nt.Name,
                QualifiedType qt => $"{qt.Namespace}.{qt.Name}",
                ArrayType at => $"{ExtractTypeName(at.ElementType)}[]",
                GenericType gt => $"{ExtractTypeName(gt.BaseType)}<{string.Join(",", gt.TypeArguments.Select(ExtractTypeName))}>",
                NullableType nt => $"{ExtractTypeName(nt.ElementType)}?",
                _ => typeRef.ToString()
            };
        }

        private bool IsPrimitiveType(string typeName)
        {
            return typeName switch
            {
                "void" or "int" or "long" or "float" or "double" or "decimal" or
                "bool" or "char" or "byte" or "sbyte" or "short" or "ushort" or
                "uint" or "ulong" or "object" or "string" or "dynamic" => true,
                _ => false
            };
        }
    }

    /// <summary>
    /// Represents a scope in the program (module, namespace, type, method).
    /// Contains symbol definitions and nested scopes.
    /// </summary>
    public class Scope
    {
        private readonly Dictionary<string, TypeSymbol> _types = new();
        private readonly Dictionary<string, MemberSymbol> _members = new();
        private readonly Dictionary<string, VariableSymbol> _variables = new();
        private readonly Dictionary<string, Scope> _namespaces = new();

        public Scope Parent { get; }
        public ScopeKind Kind { get; }
        public string NamespacePath { get; set; }
        public TypeSymbol TypeSymbol { get; set; }
        public Dictionary<string, List<MemberSymbol>> MethodOverloads { get; } = new();

        public Scope(Scope parent, ScopeKind kind)
        {
            Parent = parent;
            Kind = kind;
        }

        public void DefineType(string name, TypeSymbol symbol)
            => _types[name] = symbol;

        public TypeSymbol LookupType(string name)
        {
            if (_types.TryGetValue(name, out var symbol))
                return symbol;
            return Parent?.LookupType(name);
        }

        public void DefineMember(string name, MemberSymbol symbol)
            => _members[name] = symbol;

        public MemberSymbol LookupMember(string name)
        {
            if (_members.TryGetValue(name, out var symbol))
                return symbol;
            return Parent?.LookupMember(name);
        }

        public void DefineVariable(string name, VariableSymbol symbol)
            => _variables[name] = symbol;

        public VariableSymbol LookupVariable(string name)
        {
            if (_variables.TryGetValue(name, out var symbol))
                return symbol;
            return Parent?.LookupVariable(name);
        }

        public void DefineNamespace(string name, Scope scope)
            => _namespaces[name] = scope;

        public Scope LookupNamespace(string name)
        {
            if (_namespaces.TryGetValue(name, out var scope))
                return scope;
            return Parent?.LookupNamespace(name);
        }

        public IEnumerable<TypeSymbol> GetDefinedTypes() => _types.Values;
        public IEnumerable<MemberSymbol> GetDefinedMembers() => _members.Values;
        public IEnumerable<VariableSymbol> GetDefinedVariables() => _variables.Values;
    }

    /// <summary>
    /// Represents a type (class, interface, struct, enum) in the program.
    /// </summary>
    public class TypeSymbol
    {
        public string Name { get; set; }
        public string FullyQualifiedName { get; set; }
        public TypeKind Kind { get; set; }
        public AccessLevel AccessLevel { get; set; }
        public bool IsAbstract { get; set; }
        public bool IsSealed { get; set; }
        public bool IsStatic { get; set; }
        public TypeDeclaration Declaration { get; set; }
        public SourceLocation Location { get; set; }

        // Base type information
        public string BaseTypeName { get; set; }
        public TypeSymbol BaseType { get; set; }

        // Interface information
        public List<string> InterfaceNames { get; } = new();
        public List<TypeSymbol> Interfaces { get; } = new();

        // Members and scope
        public List<MemberSymbol> Members { get; } = new();
        public Scope MemberScope { get; set; }
    }

    /// <summary>
    /// Represents a member (field, property, method, constructor) of a type.
    /// </summary>
    public class MemberSymbol
    {
        public string Name { get; set; }
        public MemberKind Kind { get; set; }
        public string Signature { get; set; }
        public string TypeName { get; set; }
        public string ReturnTypeName { get; set; }
        public AccessLevel AccessLevel { get; set; }
        public bool IsStatic { get; set; }
        public bool IsAbstract { get; set; }
        public bool IsVirtual { get; set; }
        public bool IsOverride { get; set; }
        public bool IsReadOnly { get; set; }
        public Member Declaration { get; set; }
        public SourceLocation Location { get; set; }
    }

    /// <summary>
    /// Represents a local variable or parameter in a method scope.
    /// </summary>
    public class VariableSymbol
    {
        public string Name { get; set; }
        public string TypeName { get; set; }
        public bool IsParameter { get; set; }
        public SourceLocation Location { get; set; }
    }

    /// <summary>
    /// Represents a semantic error found during analysis.
    /// </summary>
    public class SemanticError
    {
        public string Message { get; }
        public SourceLocation Location { get; }

        public SemanticError(string message, SourceLocation location)
        {
            Message = message;
            Location = location;
        }

        public override string ToString()
        {
            if (Location != null)
                return $"Error at {Location.Line}:{Location.Column}: {Message}";
            return $"Error: {Message}";
        }
    }

    /// <summary>
    /// Type category.
    /// </summary>
    public enum TypeKind
    {
        Unknown,
        Class,
        Interface,
        Struct,
        Enum
    }

    /// <summary>
    /// Member category.
    /// </summary>
    public enum MemberKind
    {
        Field,
        Property,
        Method,
        Constructor
    }

    /// <summary>
    /// Scope category.
    /// </summary>
    public enum ScopeKind
    {
        Module,
        Namespace,
        Type,
        Method,
        Block
    }

    /// <summary>
    /// Access level (visibility).
    /// </summary>
    public enum AccessLevel
    {
        Public,
        Private,
        Protected,
        Internal
    }
