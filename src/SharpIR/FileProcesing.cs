using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using ObjectIR.Core.IR;
using ObjectIR.Core.Serialization;

namespace SharpIR
{
    public class CSharpParser
    {
        public static void ParseFile(string filePath)
        {
            Console.WriteLine($"Parsing file: {filePath}");
            // Read the file content
            string code = File.ReadAllText(filePath);
            // Parse the code into a SyntaxTree (the AST)
            SyntaxTree tree = CSharpSyntaxTree.ParseText(code);
            // Create compilation for semantic analysis
            var compilation = CSharpCompilation.Create("temp")
                .AddReferences(MetadataReference.CreateFromFile(typeof(object).Assembly.Location))
                .AddReferences(MetadataReference.CreateFromFile(typeof(Console).Assembly.Location))
                .AddSyntaxTrees(tree);
            var semanticModel = compilation.GetSemanticModel(tree);
            // Get the root of the AST
            CompilationUnitSyntax root = (CompilationUnitSyntax)tree.GetRoot();
            // Create ObjectIR module
            var module = new Module(Path.GetFileNameWithoutExtension(filePath));
            // Traverse the AST to extract information and build IR
            var walker = new CodeWalker(semanticModel, module);
            walker.Visit(root);
            // Output ObjectIR
            var serializer = new ModuleSerializer(module);
            Console.WriteLine("ObjectIR Output:");
            Console.WriteLine(serializer.DumpToJson());
            // write the file back to the path +.ir.json
            string outputPath = filePath + ".ir.json";
            File.WriteAllText(outputPath, serializer.DumpToJson());
        }

        private static AccessModifier MapAccess(Microsoft.CodeAnalysis.Accessibility accessibility)
        {
            return accessibility switch
            {
                Microsoft.CodeAnalysis.Accessibility.Public => AccessModifier.Public,
                Microsoft.CodeAnalysis.Accessibility.Private => AccessModifier.Private,
                Microsoft.CodeAnalysis.Accessibility.Protected => AccessModifier.Protected,
                Microsoft.CodeAnalysis.Accessibility.Internal => AccessModifier.Internal,
                _ => AccessModifier.Public
            };
        }

        private static TypeReference MapType(ITypeSymbol typeSymbol)
        {
            if (typeSymbol is IArrayTypeSymbol arrayType)
            {
                return MapType(arrayType.ElementType).MakeArrayType();
            }
            if (typeSymbol is INamedTypeSymbol namedType)
            {
                if (namedType.IsGenericType)
                {
                    // TODO: Handle generics
                    return TypeReference.FromName(namedType.Name);
                }
                // Map built-in types
                switch (namedType.ToDisplayString())
                {
                    case "void": return TypeReference.Void;
                    case "bool": return TypeReference.Bool;
                    case "int": return TypeReference.Int32;
                    case "long": return TypeReference.Int64;
                    case "short": return TypeReference.Int16;
                    case "byte": return TypeReference.UInt8;
                    case "sbyte": return TypeReference.Int8;
                    case "uint": return TypeReference.UInt32;
                    case "ulong": return TypeReference.UInt64;
                    case "ushort": return TypeReference.UInt16;
                    case "float": return TypeReference.Float32;
                    case "double": return TypeReference.Float64;
                    case "char": return TypeReference.Char;
                    case "string": return TypeReference.String;
                    default: return TypeReference.FromName(namedType.ToDisplayString());
                }
            }
            return TypeReference.FromName(typeSymbol.ToDisplayString());
        }

        private class CodeWalker : CSharpSyntaxWalker
        {
            private readonly SemanticModel _semanticModel;
            private readonly Module _module;
            private readonly Dictionary<string, TypeDefinition> _typeMap = new();

            public CodeWalker(SemanticModel semanticModel, Module module)
            {
                _semanticModel = semanticModel;
                _module = module;
            }

            public override void VisitClassDeclaration(ClassDeclarationSyntax node)
            {
                var symbol = _semanticModel.GetDeclaredSymbol(node);
                if (symbol != null)
                {
                    var classDef = _module.DefineClass(symbol.Name);
                    classDef.Namespace = symbol.ContainingNamespace?.ToDisplayString();
                    classDef.Access = MapAccess(symbol.DeclaredAccessibility);
                    _typeMap[symbol.ToDisplayString()] = classDef;
                    // TODO: Handle base types and interfaces
                    // For now, set BodySource for methods
                }
                base.VisitClassDeclaration(node);
            }

            public override void VisitInterfaceDeclaration(InterfaceDeclarationSyntax node)
            {
                var symbol = _semanticModel.GetDeclaredSymbol(node);
                if (symbol != null)
                {
                    var interfaceDef = _module.DefineInterface(symbol.Name);
                    interfaceDef.Namespace = symbol.ContainingNamespace?.ToDisplayString();
                    interfaceDef.Access = MapAccess(symbol.DeclaredAccessibility);
                    _typeMap[symbol.ToDisplayString()] = interfaceDef;
                }
                base.VisitInterfaceDeclaration(node);
            }

            public override void VisitStructDeclaration(StructDeclarationSyntax node)
            {
                var symbol = _semanticModel.GetDeclaredSymbol(node);
                if (symbol != null)
                {
                    var structDef = _module.DefineStruct(symbol.Name);
                    structDef.Namespace = symbol.ContainingNamespace?.ToDisplayString();
                    structDef.Access = MapAccess(symbol.DeclaredAccessibility);
                    _typeMap[symbol.ToDisplayString()] = structDef;
                }
                base.VisitStructDeclaration(node);
            }

            public override void VisitEnumDeclaration(EnumDeclarationSyntax node)
            {
                // TODO: Implement enum handling
                base.VisitEnumDeclaration(node);
            }

            public override void VisitMethodDeclaration(MethodDeclarationSyntax node)
            {
                var symbol = _semanticModel.GetDeclaredSymbol(node);
                if (symbol != null && symbol.ContainingType != null && _typeMap.TryGetValue(symbol.ContainingType.ToDisplayString(), out var typeDef))
                {
                    if (typeDef is ClassDefinition classDef)
                    {
                        MethodDefinition methodDef;
                        if (symbol.MethodKind == MethodKind.Constructor)
                        {
                            methodDef = classDef.DefineConstructor();
                            methodDef.IsConstructor = true;
                        }
                        else
                        {
                            methodDef = classDef.DefineMethod(symbol.Name, CSharpParser.MapType(symbol.ReturnType));
                        }
                        methodDef.Access = MapAccess(symbol.DeclaredAccessibility);
                        methodDef.IsStatic = symbol.IsStatic;
                        methodDef.IsVirtual = symbol.IsVirtual;
                        methodDef.IsOverride = symbol.IsOverride;
                        methodDef.IsAbstract = symbol.IsAbstract;
                        foreach (var param in symbol.Parameters)
                        {
                            methodDef.DefineParameter(param.Name, CSharpParser.MapType(param.Type));
                        }
                        // Compile body if present
                        if (node.Body != null)
                        {
                            CompileBody(methodDef, node.Body);
                        }
                        else if (node.ExpressionBody != null)
                        {
                            // TODO: Handle expression bodies
                        }
                    }
                }
                base.VisitMethodDeclaration(node);
            }

            public override void VisitPropertyDeclaration(PropertyDeclarationSyntax node)
            {
                var symbol = _semanticModel.GetDeclaredSymbol(node);
                if (symbol != null && symbol.ContainingType != null && _typeMap.TryGetValue(symbol.ContainingType.ToDisplayString(), out var typeDef))
                {
                    if (typeDef is ClassDefinition classDef)
                    {
                        var propDef = new PropertyDefinition(symbol.Name, MapType(symbol.Type));
                        propDef.Access = MapAccess(symbol.DeclaredAccessibility);
                        classDef.Properties.Add(propDef);
                        // TODO: Handle getter/setter
                    }
                }
                base.VisitPropertyDeclaration(node);
            }

            public override void VisitFieldDeclaration(FieldDeclarationSyntax node)
            {
                var symbol = _semanticModel.GetDeclaredSymbol(node);
                if (symbol is IFieldSymbol fieldSymbol && fieldSymbol.ContainingType != null && _typeMap.TryGetValue(fieldSymbol.ContainingType.ToDisplayString(), out var typeDef))
                {
                    if (typeDef is ClassDefinition classDef)
                    {
                        foreach (var variable in node.Declaration.Variables)
                        {
                            var fieldDef = classDef.DefineField(variable.Identifier.Text, MapType(fieldSymbol.Type));
                            fieldDef.Access = MapAccess(fieldSymbol.DeclaredAccessibility);
                            fieldDef.IsStatic = fieldSymbol.IsStatic;
                            fieldDef.IsReadOnly = fieldSymbol.IsReadOnly;
                        }
                    }
                }
                base.VisitFieldDeclaration(node);
            }

            private void CompileBody(MethodDefinition method, BlockSyntax body)
            {
                var compiler = new BodyCompiler(_semanticModel, method);
                compiler.Compile(body);
            }

            private class BodyCompiler
            {
                private readonly SemanticModel _semanticModel;
                private readonly MethodDefinition _method;

                public BodyCompiler(SemanticModel semanticModel, MethodDefinition method)
                {
                    _semanticModel = semanticModel;
                    _method = method;
                }

                public void Compile(BlockSyntax body)
                {
                    foreach (var statement in body.Statements)
                    {
                        CompileStatement(statement);
                    }
                }

                private void CompileStatement(StatementSyntax statement)
                {
                    switch (statement)
                    {
                        case ExpressionStatementSyntax exprStmt:
                            CompileExpression(exprStmt.Expression);
                            // Pop the result if not void
                            if (!(exprStmt.Expression is InvocationExpressionSyntax))
                            {
                                _method.Instructions.Add(new PopInstruction());
                            }
                            break;
                        case ReturnStatementSyntax returnStmt:
                            if (returnStmt.Expression != null)
                            {
                                CompileExpression(returnStmt.Expression);
                            }
                            _method.Instructions.Add(new ReturnInstruction(null));
                            break;
                        // TODO: Add more statement types
                        default:
                            // For now, skip
                            break;
                    }
                }

                private void CompileExpression(ExpressionSyntax expression)
                {
                    switch (expression)
                    {
                        case InvocationExpressionSyntax invocation:
                            CompileInvocation(invocation);
                            break;
                        case LiteralExpressionSyntax literal:
                            CompileLiteral(literal);
                            break;
                        case IdentifierNameSyntax identifier:
                            CompileIdentifier(identifier);
                            break;
                        case InterpolatedStringExpressionSyntax interpolated:
                            CompileInterpolatedString(interpolated);
                            break;
                        case AssignmentExpressionSyntax assignment:
                            CompileAssignment(assignment);
                            break;
                        case MemberAccessExpressionSyntax memberAccess:
                            CompileMemberAccess(memberAccess);
                            break;
                        // TODO: Add more expression types
                        default:
                            // For now, load null
                            _method.Instructions.Add(new LoadNullInstruction());
                            break;
                    }
                }

                private void CompileInvocation(InvocationExpressionSyntax invocation)
                {
                    var symbol = _semanticModel.GetSymbolInfo(invocation).Symbol as IMethodSymbol;
                    if (symbol != null)
                    {
                        // Compile arguments
                        foreach (var arg in invocation.ArgumentList.Arguments)
                        {
                            CompileExpression(arg.Expression);
                        }
                        // Emit call
                        var methodRef = new MethodReference(
                            CSharpParser.MapType(symbol.ContainingType),
                            symbol.Name,
                            CSharpParser.MapType(symbol.ReturnType),
                            symbol.Parameters.Select(p => CSharpParser.MapType(p.Type)).ToList()
                        );
                        _method.Instructions.Add(new CallInstruction(methodRef));
                    }
                }

                private void CompileLiteral(LiteralExpressionSyntax literal)
                {
                    var value = _semanticModel.GetConstantValue(literal).Value;
                    var type = CSharpParser.MapType(_semanticModel.GetTypeInfo(literal).Type);
                    _method.Instructions.Add(new LoadConstantInstruction(value, type));
                }

                private void CompileIdentifier(IdentifierNameSyntax identifier)
                {
                    var symbol = _semanticModel.GetSymbolInfo(identifier).Symbol;
                    if (symbol is IParameterSymbol param)
                    {
                        _method.Instructions.Add(new LoadArgInstruction(param.Name));
                    }
                    else if (symbol is ILocalSymbol local)
                    {
                        _method.Instructions.Add(new LoadLocalInstruction(local.Name));
                    }
                    else if (symbol is IFieldSymbol field)
                    {
                        var fieldRef = new FieldReference(CSharpParser.MapType(field.ContainingType), field.Name, CSharpParser.MapType(field.Type));
                        if (field.IsStatic)
                        {
                            _method.Instructions.Add(new LoadStaticFieldInstruction(fieldRef));
                        }
                        else
                        {
                            // Need 'this' on stack
                            _method.Instructions.Add(new LoadArgInstruction("this"));
                            _method.Instructions.Add(new LoadFieldInstruction(fieldRef));
                        }
                    }
                    // TODO: Handle properties
                }

                private void CompileInterpolatedString(InterpolatedStringExpressionSyntax interpolated)
                {
                    var expressions = new List<ExpressionSyntax>();
                    foreach (var content in interpolated.Contents)
                    {
                        if (content is InterpolatedStringTextSyntax text)
                        {
                            // Emit load constant directly
                            _method.Instructions.Add(new LoadConstantInstruction(text.TextToken.ValueText, TypeReference.String));
                        }
                        else if (content is InterpolationSyntax interp)
                        {
                            CompileExpression(interp.Expression);
                        }
                    }
                    // If more than one on stack, call Concat
                    int count = interpolated.Contents.Count(c => c is InterpolatedStringTextSyntax) + interpolated.Contents.Count(c => c is InterpolationSyntax);
                    if (count > 1)
                    {
                        var concatMethod = new MethodReference(TypeReference.String, "Concat", TypeReference.String, 
                            Enumerable.Repeat(TypeReference.String, count).ToList());
                        _method.Instructions.Add(new CallInstruction(concatMethod));
                    }
                }

                private void CompileAssignment(AssignmentExpressionSyntax assignment)
                {
                    // Compile right side
                    CompileExpression(assignment.Right);
                    // Store to left
                    if (assignment.Left is IdentifierNameSyntax identifier)
                    {
                        var symbol = _semanticModel.GetSymbolInfo(identifier).Symbol;
                        if (symbol is IParameterSymbol param)
                        {
                            _method.Instructions.Add(new StoreArgInstruction(param.Name));
                        }
                        else if (symbol is ILocalSymbol local)
                        {
                            _method.Instructions.Add(new StoreLocalInstruction(local.Name));
                        }
                        else if (symbol is IFieldSymbol field)
                        {
                            var fieldRef = new FieldReference(CSharpParser.MapType(field.ContainingType), field.Name, CSharpParser.MapType(field.Type));
                            if (field.IsStatic)
                            {
                                _method.Instructions.Add(new StoreStaticFieldInstruction(fieldRef));
                            }
                            else
                            {
                                // Load this
                                _method.Instructions.Add(new LoadArgInstruction("this"));
                                _method.Instructions.Add(new StoreFieldInstruction(fieldRef));
                            }
                        }
                    }
                    else if (assignment.Left is MemberAccessExpressionSyntax memberAccess)
                    {
                        // Assume this.field
                        if (memberAccess.Expression is ThisExpressionSyntax && memberAccess.Name is IdentifierNameSyntax name)
                        {
                            var symbol = _semanticModel.GetSymbolInfo(memberAccess).Symbol as IFieldSymbol;
                            if (symbol != null)
                            {
                                var fieldRef = new FieldReference(CSharpParser.MapType(symbol.ContainingType), symbol.Name, CSharpParser.MapType(symbol.Type));
                                _method.Instructions.Add(new LoadArgInstruction("this"));
                                _method.Instructions.Add(new StoreFieldInstruction(fieldRef));
                            }
                        }
                    }
                }

                private void CompileMemberAccess(MemberAccessExpressionSyntax memberAccess)
                {
                    // For now, assume field access
                    var symbol = _semanticModel.GetSymbolInfo(memberAccess).Symbol;
                    if (symbol is IFieldSymbol field)
                    {
                        var fieldRef = new FieldReference(CSharpParser.MapType(field.ContainingType), field.Name, CSharpParser.MapType(field.Type));
                        if (field.IsStatic)
                        {
                            _method.Instructions.Add(new LoadStaticFieldInstruction(fieldRef));
                        }
                        else
                        {
                            CompileExpression(memberAccess.Expression);
                            _method.Instructions.Add(new LoadFieldInstruction(fieldRef));
                        }
                    }
                    else if (symbol is IPropertySymbol prop)
                    {
                        // TODO: Handle properties
                        _method.Instructions.Add(new LoadNullInstruction());
                    }
                }
            }
        }
    }
}