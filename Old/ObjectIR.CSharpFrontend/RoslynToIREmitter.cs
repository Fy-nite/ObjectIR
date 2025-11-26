using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using ObjectIR.Core.IR;

namespace ObjectIR.CSharpFrontend;

/// <summary>
/// Converts Roslyn C# AST to ObjectIR module structure.
/// 
/// This emitter bridges between Microsoft.CodeAnalysis (Roslyn) and ObjectIR.Core,
/// mapping C# constructs to IR equivalents:
/// - ClassDeclaration → ClassDefinition
/// - MethodDeclaration → MethodDefinition
/// - FieldDeclaration → FieldDefinition
/// - PropertyDeclaration → PropertyDefinition
/// - InterfaceDeclaration → InterfaceDefinition
/// - etc.
/// </summary>
public class RoslynToIREmitter
{
    private readonly CompilationUnitSyntax _unit;
    private readonly SemanticModel _semanticModel;
    private readonly CSharpCompilation _compilation;
    private readonly Module _module;
    private Dictionary<string, TypeDefinition> _typeCache = new();
    
    /// <summary>
    /// When true, extracts type info directly from AST even if semantic symbols aren't available.
    /// This enables bootstrap compilation where semantic model is incomplete.
    /// </summary>
    public bool AggressiveASTExtraction { get; set; } = false;

    public RoslynToIREmitter(string moduleName, CompilationUnitSyntax unit, SemanticModel semanticModel, CSharpCompilation compilation)
    {
        _unit = unit ?? throw new ArgumentNullException(nameof(unit));
        _semanticModel = semanticModel ?? throw new ArgumentNullException(nameof(semanticModel));
        _compilation = compilation ?? throw new ArgumentNullException(nameof(compilation));
        _module = new Module(moduleName);
    }

    private SemanticModel GetSemanticModelForNode(SyntaxNode node)
    {
        if (node.SyntaxTree == _semanticModel.SyntaxTree)
            return _semanticModel;

        return _compilation.GetSemanticModel(node.SyntaxTree);
    }

    private ITypeSymbol? GetTypeSymbol(TypeSyntax typeSyntax)
    {
        return GetSemanticModelForNode(typeSyntax).GetTypeInfo(typeSyntax).Type;
    }

    private ISymbol? GetDeclaredSymbol(SyntaxNode node)
    {
        return GetSemanticModelForNode(node).GetDeclaredSymbol(node);
    }

    /// <summary>
    /// Emits the complete IR module from the Roslyn AST.
    /// </summary>
    public Module Emit()
    {
        var members = _unit.Members;
        int classCount = 0;
        int interfaceCount = 0;
        int structCount = 0;
        int enumCount = 0;
        int nsCount = 0;
        int recordCount = 0;

        foreach (var member in members)
        {
            switch (member)
            {
                case RecordDeclarationSyntax recordDecl:
                    recordCount++;
                    EmitRecord(recordDecl);
                    break;

                case ClassDeclarationSyntax classDecl:
                    classCount++;
                    EmitClass(classDecl);
                    break;

                case InterfaceDeclarationSyntax interfaceDecl:
                    interfaceCount++;
                    EmitInterface(interfaceDecl);
                    break;

                case StructDeclarationSyntax structDecl:
                    structCount++;
                    EmitStruct(structDecl);
                    break;

                case EnumDeclarationSyntax enumDecl:
                    enumCount++;
                    EmitEnum(enumDecl);
                    break;

                case NamespaceDeclarationSyntax ns:
                    nsCount++;
                    EmitNamespace(ns);
                    break;

                case FileScopedNamespaceDeclarationSyntax fileScopedNs:
                    nsCount++;
                    EmitFileScopedNamespace(fileScopedNs);
                    break;
            }
        }

        // DEBUG: Report what was found
        if (classCount > 0 || interfaceCount > 0 || structCount > 0 || enumCount > 0 || nsCount > 0 || recordCount > 0)
        {
            System.Console.WriteLine($"[EMITTER] Found: {classCount} classes, {recordCount} records, {interfaceCount} interfaces, {structCount} structs, {enumCount} enums, {nsCount} namespaces. Result module has {_module.Types.Count} types");
        }

        return _module;
    }

    /// <summary>
    /// Emits types from a namespace.
    /// </summary>
    private void EmitNamespace(NamespaceDeclarationSyntax ns)
    {
        string namespaceName = ns.Name.ToString();

        foreach (var member in ns.Members)
        {
            switch (member)
            {
                case RecordDeclarationSyntax recordDecl:
                    EmitRecord(recordDecl, namespaceName);
                    break;

                case ClassDeclarationSyntax classDecl:
                    EmitClass(classDecl, namespaceName);
                    break;

                case InterfaceDeclarationSyntax interfaceDecl:
                    EmitInterface(interfaceDecl, namespaceName);
                    break;

                case StructDeclarationSyntax structDecl:
                    EmitStruct(structDecl, namespaceName);
                    break;

                case EnumDeclarationSyntax enumDecl:
                    EmitEnum(enumDecl, namespaceName);
                    break;

                case NamespaceDeclarationSyntax nestedNs:
                    EmitNamespace(nestedNs);
                    break;
            }
        }
    }

    /// <summary>
    /// Emits types from a file-scoped namespace.
    /// </summary>
    private void EmitFileScopedNamespace(FileScopedNamespaceDeclarationSyntax ns)
    {
        string namespaceName = ns.Name.ToString();

        foreach (var member in ns.Members)
        {
            switch (member)
            {
                case RecordDeclarationSyntax recordDecl:
                    EmitRecord(recordDecl, namespaceName);
                    break;

                case ClassDeclarationSyntax classDecl:
                    EmitClass(classDecl, namespaceName);
                    break;

                case InterfaceDeclarationSyntax interfaceDecl:
                    EmitInterface(interfaceDecl, namespaceName);
                    break;

                case StructDeclarationSyntax structDecl:
                    EmitStruct(structDecl, namespaceName);
                    break;

                case EnumDeclarationSyntax enumDecl:
                    EmitEnum(enumDecl, namespaceName);
                    break;
            }
        }
    }

    /// <summary>
    /// Emits a record definition (treated as a class in IR).
    /// </summary>
    private void EmitRecord(RecordDeclarationSyntax recordDecl, string? ns = null)
    {
        var symbol = GetDeclaredSymbol(recordDecl) as INamedTypeSymbol;
        
        // In aggressive mode, proceed even if symbol is null
        if (symbol == null && !AggressiveASTExtraction)
        {
            return;
        }

        var classDef = _module.DefineClass(recordDecl.Identifier.Text);
        
        if (symbol != null)
        {
            classDef.Namespace = ns ?? GetNamespace(symbol);
            _typeCache[symbol.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)] = classDef;
        }
        else if (AggressiveASTExtraction)
        {
            classDef.Namespace = ns ?? "";
        }

        classDef.IsAbstract = recordDecl.Modifiers.Any(m => m.IsKind(SyntaxKind.AbstractKeyword));
        classDef.IsSealed = recordDecl.Modifiers.Any(m => m.IsKind(SyntaxKind.SealedKeyword));
        classDef.Access = GetAccessModifier(recordDecl.Modifiers);

        // Record primary constructor parameters become properties
        if (recordDecl.ParameterList != null)
        {
            foreach (var parameter in recordDecl.ParameterList.Parameters)
            {
                var paramType = parameter.Type != null 
                    ? ConvertType(GetTypeSymbol(parameter.Type)) 
                    : TypeReference.FromName("object");
                
                if (paramType == null && AggressiveASTExtraction && parameter.Type != null)
                {
                    paramType = TypeReference.FromName(parameter.Type.ToString());
                }

                var propDef = new PropertyDefinition(parameter.Identifier.Text, paramType ?? TypeReference.FromName("object"))
                {
                    Access = AccessModifier.Public,
                    // Records auto-generate getter, init-only setter
                    Getter = new MethodDefinition($"get_{parameter.Identifier.Text}", paramType ?? TypeReference.FromName("object"))
                };
                classDef.Properties.Add(propDef);
            }
        }

        // Base class/record
        if (recordDecl.BaseList != null)
        {
            var baseTypes = recordDecl.BaseList.Types.ToList();
            if (baseTypes.Count > 0)
            {
                var baseType = baseTypes[0];
                var baseSymbol = GetTypeSymbol(baseType.Type);
                if (baseSymbol != null)
                {
                    classDef.BaseType = ConvertType(baseSymbol);
                }
                else if (AggressiveASTExtraction && baseType.Type is IdentifierNameSyntax idName)
                {
                    classDef.BaseType = TypeReference.FromName(idName.Identifier.Text);
                }
            }

            // Interfaces
            for (int i = 1; i < baseTypes.Count; i++)
            {
                var interfaceType = baseTypes[i];
                var interfaceSymbol = GetTypeSymbol(interfaceType.Type);
                if (interfaceSymbol != null)
                {
                    classDef.Interfaces.Add(ConvertType(interfaceSymbol));
                }
                else if (AggressiveASTExtraction && interfaceType.Type is IdentifierNameSyntax idInterfaceName)
                {
                    classDef.Interfaces.Add(TypeReference.FromName(idInterfaceName.Identifier.Text));
                }
            }
        }

        // Members
        foreach (var member in recordDecl.Members)
        {
            switch (member)
            {
                case FieldDeclarationSyntax fieldDecl:
                    EmitFields(fieldDecl, classDef);
                    break;

                case PropertyDeclarationSyntax propDecl:
                    EmitProperty(propDecl, classDef);
                    break;

                case MethodDeclarationSyntax methodDecl:
                    EmitMethod(methodDecl, classDef);
                    break;

                case ConstructorDeclarationSyntax ctorDecl:
                    EmitConstructor(ctorDecl, classDef);
                    break;
            }
        }
    }

    /// <summary>
    /// Emits a class definition.
    /// </summary>
    private void EmitClass(ClassDeclarationSyntax classDecl, string? ns = null)
    {
        var symbol = GetDeclaredSymbol(classDecl) as INamedTypeSymbol;
        
        // In aggressive mode, proceed even if symbol is null
        if (symbol == null && !AggressiveASTExtraction)
        {
            // Symbol resolution failed - this is expected in bootstrap mode
            if (!AggressiveASTExtraction)
                return;
        }

        var classDef = _module.DefineClass(classDecl.Identifier.Text);
        
        if (symbol != null)
        {
            classDef.Namespace = ns ?? GetNamespace(symbol);
            _typeCache[symbol.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)] = classDef;
        }
        else if (AggressiveASTExtraction)
        {
            // Use namespace from parameter or empty
            classDef.Namespace = ns ?? "";
        }

        classDef.IsAbstract = classDecl.Modifiers.Any(m => m.IsKind(SyntaxKind.AbstractKeyword));
        classDef.IsSealed = classDecl.Modifiers.Any(m => m.IsKind(SyntaxKind.SealedKeyword));
        classDef.Access = GetAccessModifier(classDecl.Modifiers);

        // Base class
        if (classDecl.BaseList != null)
        {
            var baseTypes = classDecl.BaseList.Types.ToList();
            if (baseTypes.Count > 0)
            {
                var baseType = baseTypes[0];
                var baseSymbol = GetTypeSymbol(baseType.Type);
                if (baseSymbol != null)
                {
                    classDef.BaseType = ConvertType(baseSymbol);
                }
                else if (AggressiveASTExtraction && baseType.Type is IdentifierNameSyntax idName)
                {
                    classDef.BaseType = TypeReference.FromName(idName.Identifier.Text);
                }
            }

            // Interfaces
            for (int i = 1; i < baseTypes.Count; i++)
            {
                var interfaceType = baseTypes[i];
                var interfaceSymbol = GetTypeSymbol(interfaceType.Type);
                if (interfaceSymbol != null)
                {
                    classDef.Interfaces.Add(ConvertType(interfaceSymbol));
                }
                else if (AggressiveASTExtraction && interfaceType.Type is IdentifierNameSyntax idInterfaceName)
                {
                    classDef.Interfaces.Add(TypeReference.FromName(idInterfaceName.Identifier.Text));
                }
            }
        }

        // Members
        foreach (var member in classDecl.Members)
        {
            switch (member)
            {
                case FieldDeclarationSyntax fieldDecl:
                    EmitFields(fieldDecl, classDef);
                    break;

                case PropertyDeclarationSyntax propDecl:
                    EmitProperty(propDecl, classDef);
                    break;

                case MethodDeclarationSyntax methodDecl:
                    EmitMethod(methodDecl, classDef);
                    break;

                case ConstructorDeclarationSyntax ctorDecl:
                    EmitConstructor(ctorDecl, classDef);
                    break;
            }
        }
    }

    /// <summary>
    /// Emits an interface definition.
    /// </summary>
    private void EmitInterface(InterfaceDeclarationSyntax interfaceDecl, string? ns = null)
    {
        var symbol = GetDeclaredSymbol(interfaceDecl) as INamedTypeSymbol;
        
        // In aggressive mode, proceed even if symbol is null
        if (symbol == null && !AggressiveASTExtraction)
            return;

        var interfaceDef = _module.DefineInterface(interfaceDecl.Identifier.Text);
        
        if (symbol != null)
        {
            interfaceDef.Namespace = ns ?? GetNamespace(symbol);
            _typeCache[symbol.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)] = interfaceDef;
        }
        else if (AggressiveASTExtraction)
        {
            interfaceDef.Namespace = ns ?? "";
        }

        interfaceDef.Access = GetAccessModifier(interfaceDecl.Modifiers);

        // Base interfaces
        if (interfaceDecl.BaseList != null)
        {
            foreach (var baseType in interfaceDecl.BaseList.Types)
            {
                var baseSymbol = GetTypeSymbol(baseType.Type);
                if (baseSymbol != null)
                {
                    interfaceDef.BaseInterfaces.Add(ConvertType(baseSymbol));
                }
                else if (AggressiveASTExtraction && baseType.Type is IdentifierNameSyntax idName)
                {
                    interfaceDef.BaseInterfaces.Add(TypeReference.FromName(idName.Identifier.Text));
                }
            }
        }

        // Methods
        foreach (var member in interfaceDecl.Members)
        {
            if (member is MethodDeclarationSyntax methodDecl)
            {
                EmitInterfaceMethod(methodDecl, interfaceDef);
            }
        }
    }

    /// <summary>
    /// Emits a struct definition.
    /// </summary>
    private void EmitStruct(StructDeclarationSyntax structDecl, string? ns = null)
    {
        var symbol = GetDeclaredSymbol(structDecl) as INamedTypeSymbol;
        
        // In aggressive mode, proceed even if symbol is null
        if (symbol == null && !AggressiveASTExtraction)
            return;

        var structDef = _module.DefineStruct(structDecl.Identifier.Text);
        
        if (symbol != null)
        {
            structDef.Namespace = ns ?? GetNamespace(symbol);
            _typeCache[symbol.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)] = structDef;
        }
        else if (AggressiveASTExtraction)
        {
            structDef.Namespace = ns ?? "";
        }

        structDef.Access = GetAccessModifier(structDecl.Modifiers);

        // Interfaces
        if (structDecl.BaseList != null)
        {
            foreach (var baseType in structDecl.BaseList.Types)
            {
                var interfaceSymbol = GetTypeSymbol(baseType.Type);
                if (interfaceSymbol != null)
                {
                    structDef.Interfaces.Add(ConvertType(interfaceSymbol));
                }
                else if (AggressiveASTExtraction && baseType.Type is IdentifierNameSyntax idName)
                {
                    structDef.Interfaces.Add(TypeReference.FromName(idName.Identifier.Text));
                }
            }
        }

        // Members
        foreach (var member in structDecl.Members)
        {
            switch (member)
            {
                case FieldDeclarationSyntax fieldDecl:
                    EmitFields(fieldDecl, structDef);
                    break;

                case PropertyDeclarationSyntax propDecl:
                    EmitProperty(propDecl, structDef);
                    break;

                case MethodDeclarationSyntax methodDecl:
                    EmitMethod(methodDecl, structDef);
                    break;
            }
        }
    }

    /// <summary>
    /// Emits an enum definition.
    /// </summary>
    private void EmitEnum(EnumDeclarationSyntax enumDecl, string? ns = null)
    {
        var enumDef = new EnumDefinition(enumDecl.Identifier.Text);
        enumDef.Namespace = ns ?? "";
        enumDef.Access = GetAccessModifier(enumDecl.Modifiers);

        // Enum members
        long value = 0;
        foreach (var member in enumDecl.Members)
        {
            if (member.EqualsValue != null)
            {
                // Try to extract constant value
                var constValue = GetSemanticModelForNode(member.EqualsValue.Value).GetConstantValue(member.EqualsValue.Value);
                if (constValue.HasValue && constValue.Value is long longVal)
                {
                    value = longVal;
                }
            }

            enumDef.DefineMember(member.Identifier.Text, value);
            value++;
        }

        _module.Types.Add(enumDef);
    }

    /// <summary>
    /// Emits field declarations.
    /// </summary>
    private void EmitFields(FieldDeclarationSyntax fieldDecl, TypeDefinition parent)
    {
        var fieldType = ConvertType(GetTypeSymbol(fieldDecl.Declaration.Type));
        var access = GetAccessModifier(fieldDecl.Modifiers);
        bool isStatic = fieldDecl.Modifiers.Any(m => m.IsKind(SyntaxKind.StaticKeyword));
        bool isReadOnly = fieldDecl.Modifiers.Any(m => m.IsKind(SyntaxKind.ReadOnlyKeyword));

        foreach (var variable in fieldDecl.Declaration.Variables)
        {
            var fieldDef = new FieldDefinition(variable.Identifier.Text, fieldType)
            {
                Access = access,
                IsStatic = isStatic,
                IsReadOnly = isReadOnly
            };

            if (parent is ClassDefinition classDef)
                classDef.Fields.Add(fieldDef);
            else if (parent is StructDefinition structDef)
                structDef.Fields.Add(fieldDef);
        }
    }

    /// <summary>
    /// Emits a property definition.
    /// </summary>
    private void EmitProperty(PropertyDeclarationSyntax propDecl, TypeDefinition parent)
    {
        var propType = ConvertType(GetTypeSymbol(propDecl.Type));
        var propDef = new PropertyDefinition(propDecl.Identifier.Text, propType)
        {
            Access = GetAccessModifier(propDecl.Modifiers)
        };

        // Getter
        if (propDecl.AccessorList?.Accessors.FirstOrDefault(a => a.IsKind(SyntaxKind.GetAccessorDeclaration)) != null)
        {
            propDef.Getter = new MethodDefinition($"get_{propDecl.Identifier.Text}", propType)
            {
                Access = AccessModifier.Public
            };
        }

        // Setter
        if (propDecl.AccessorList?.Accessors.FirstOrDefault(a => a.IsKind(SyntaxKind.SetAccessorDeclaration)) != null)
        {
            propDef.Setter = new MethodDefinition($"set_{propDecl.Identifier.Text}", TypeReference.Void)
            {
                Access = AccessModifier.Public
            };
            propDef.Setter.DefineParameter("value", propType);
        }

        if (parent is ClassDefinition classDef)
            classDef.Properties.Add(propDef);
        // Note: Structs don't support properties in ObjectIR, only fields and methods
    }

    /// <summary>
    /// Emits a method definition.
    /// </summary>
    private void EmitMethod(MethodDeclarationSyntax methodDecl, TypeDefinition parent)
    {
        var returnType = ConvertType(GetTypeSymbol(methodDecl.ReturnType));
        var methodDef = new MethodDefinition(methodDecl.Identifier.Text, returnType)
        {
            Access = GetAccessModifier(methodDecl.Modifiers),
            IsStatic = methodDecl.Modifiers.Any(m => m.IsKind(SyntaxKind.StaticKeyword)),
            IsVirtual = methodDecl.Modifiers.Any(m => m.IsKind(SyntaxKind.VirtualKeyword)),
            IsOverride = methodDecl.Modifiers.Any(m => m.IsKind(SyntaxKind.OverrideKeyword)),
            IsAbstract = methodDecl.Modifiers.Any(m => m.IsKind(SyntaxKind.AbstractKeyword))
        };

        // Parameters
        foreach (var parameter in methodDecl.ParameterList.Parameters)
        {
            var paramType = ConvertType(GetTypeSymbol(parameter.Type!));
            methodDef.DefineParameter(parameter.Identifier.Text, paramType);
        }

        // Compile method body to IR instructions
        if (methodDecl.Body != null)
        {
            var bodySemanticModel = GetSemanticModelForNode(methodDecl);
            var bodyCompiler = new CSharpBodyCompiler(bodySemanticModel);
            bodyCompiler.CompileMethodBody(methodDecl.Body, methodDef);
        }
        else if (AggressiveASTExtraction)
        {
            // Fallback: capture method body as source text for non-abstract methods
            if (!methodDef.IsAbstract)
            {
                methodDef.BodySource = methodDecl.Body?.ToString() ?? "";
            }
        }

        if (parent is ClassDefinition classDef)
            classDef.Methods.Add(methodDef);
        else if (parent is StructDefinition structDef)
            structDef.Methods.Add(methodDef);
    }

    /// <summary>
    /// Emits an interface method definition.
    /// </summary>
    private void EmitInterfaceMethod(MethodDeclarationSyntax methodDecl, InterfaceDefinition parent)
    {
        var returnType = ConvertType(GetTypeSymbol(methodDecl.ReturnType));
        var methodDef = new MethodDefinition(methodDecl.Identifier.Text, returnType)
        {
            IsAbstract = true,
            Access = AccessModifier.Public
        };

        // Parameters
        foreach (var parameter in methodDecl.ParameterList.Parameters)
        {
            var paramType = ConvertType(GetTypeSymbol(parameter.Type!));
            methodDef.DefineParameter(parameter.Identifier.Text, paramType);
        }

        parent.Methods.Add(methodDef);
    }

    /// <summary>
    /// Emits a constructor definition.
    /// </summary>
    private void EmitConstructor(ConstructorDeclarationSyntax ctorDecl, ClassDefinition parent)
    {
        var ctorDef = parent.DefineConstructor();
        ctorDef.Access = GetAccessModifier(ctorDecl.Modifiers);
        ctorDef.IsStatic = ctorDecl.Modifiers.Any(m => m.IsKind(SyntaxKind.StaticKeyword));

        // Parameters
        foreach (var parameter in ctorDecl.ParameterList.Parameters)
        {
            var paramType = ConvertType(GetTypeSymbol(parameter.Type!));
            ctorDef.DefineParameter(parameter.Identifier.Text, paramType);
        }

        // Capture constructor body as source text
        if (ctorDecl.Body != null && AggressiveASTExtraction)
        {
            ctorDef.BodySource = ctorDecl.Body.ToString();
        }
    }

    /// <summary>
    /// Converts a Roslyn ITypeSymbol to ObjectIR TypeReference.
    /// Falls back to "dynamic" or basic type names if symbol resolution fails.
    /// </summary>
    private TypeReference ConvertType(ITypeSymbol? symbol)
    {
        if (symbol == null)
        {
            // In aggressive mode, use dynamic; in normal mode this shouldn't happen
            return AggressiveASTExtraction ? TypeReference.FromName("dynamic") : TypeReference.Void;
        }

        // Handle built-in types
        switch (symbol.SpecialType)
        {
            case SpecialType.System_Void: return TypeReference.Void;
            case SpecialType.System_Boolean: return TypeReference.Bool;
            case SpecialType.System_Byte: return TypeReference.UInt8;
            case SpecialType.System_Int16: return TypeReference.Int16;
            case SpecialType.System_Int32: return TypeReference.Int32;
            case SpecialType.System_Int64: return TypeReference.Int64;
            case SpecialType.System_Single: return TypeReference.Float32;
            case SpecialType.System_Double: return TypeReference.Float64;
            case SpecialType.System_String: return TypeReference.String;
            case SpecialType.System_Object: return TypeReference.FromName("System.Object");
        }

        // Handle arrays
        if (symbol is IArrayTypeSymbol arraySymbol)
        {
            var elementType = ConvertType(arraySymbol.ElementType);
            return elementType.MakeArrayType();
        }

        // Handle generics
        if (symbol is INamedTypeSymbol namedSymbol && namedSymbol.TypeArguments.Length > 0)
        {
            var genericArgs = namedSymbol.TypeArguments.Select(ConvertType).ToArray();
            var qualifiedName = namedSymbol.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
            var angleIndex = qualifiedName.IndexOf('<');
            if (angleIndex > 0)
            {
                var baseTypeName = qualifiedName.Substring(0, angleIndex);
                var baseType = TypeReference.FromName(baseTypeName);
                return baseType.MakeGenericType(genericArgs);
            }
        }

        // Handle custom types - create fully qualified name
        var ns = symbol.ContainingNamespace?.ToString();
        var qualName = string.IsNullOrEmpty(ns) ? symbol.Name : $"{ns}.{symbol.Name}";
        return TypeReference.FromName(qualName);
    }

    /// <summary>
    /// Extracts the namespace from a type symbol.
    /// </summary>
    private string GetNamespace(INamedTypeSymbol symbol)
    {
        return symbol.ContainingNamespace?.ToString() ?? "";
    }

    /// <summary>
    /// Converts Roslyn modifiers to ObjectIR access modifier.
    /// </summary>
    private AccessModifier GetAccessModifier(SyntaxTokenList modifiers)
    {
        if (modifiers.Any(m => m.IsKind(SyntaxKind.PublicKeyword)))
            return AccessModifier.Public;
        if (modifiers.Any(m => m.IsKind(SyntaxKind.ProtectedKeyword)))
            return AccessModifier.Protected;
        if (modifiers.Any(m => m.IsKind(SyntaxKind.PrivateKeyword)))
            return AccessModifier.Private;
        if (modifiers.Any(m => m.IsKind(SyntaxKind.InternalKeyword)))
            return AccessModifier.Internal;

        return AccessModifier.Private; // Default
    }
}

