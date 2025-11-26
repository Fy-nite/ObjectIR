using System.Collections.Generic;

namespace ObjectIR.CSharpFrontend.AST;

/// <summary>
/// Source location information for error reporting
/// </summary>
public record SourceLocation(int Line, int Column, int Position)
{
    public static SourceLocation Empty => new(1, 1, 0);
}

/// <summary>
/// Base class for all AST nodes
/// </summary>
public abstract record ASTNode(SourceLocation Location);

/// <summary>
/// Compilation unit - top-level node containing namespaces and global statements
/// </summary>
public record CompilationUnit(
    List<UsingDirective> UsingDirectives,
    List<NamespaceDeclaration> Namespaces,
    List<TypeDeclaration> GlobalTypes,
    SourceLocation Location
) : ASTNode(Location);

/// <summary>
/// Using directive (using namespace or alias)
/// </summary>
public record UsingDirective(
    string Name,
    string? Alias = null,
    SourceLocation? Location = null
) : ASTNode(Location ?? SourceLocation.Empty);

/// <summary>
/// Namespace declaration
/// </summary>
public record NamespaceDeclaration(
    string Name,
    List<UsingDirective> UsingDirectives,
    List<TypeDeclaration> Types,
    SourceLocation Location
) : ASTNode(Location);

/// <summary>
/// Base for type declarations
/// </summary>
public abstract record TypeDeclaration(
    string Name,
    AccessModifier Access,
    List<Modifier> Modifiers,
    List<GenericParameter> GenericParameters,
    SourceLocation Location
) : ASTNode(Location);

/// <summary>
/// Class declaration
/// </summary>
public record ClassDeclaration(
    string Name,
    AccessModifier Access,
    List<Modifier> Modifiers,
    List<GenericParameter> GenericParameters,
    TypeReference? BaseClass,
    List<TypeReference> Interfaces,
    List<Member> Members,
    SourceLocation Location
) : TypeDeclaration(Name, Access, Modifiers, GenericParameters, Location);

/// <summary>
/// Interface declaration
/// </summary>
public record InterfaceDeclaration(
    string Name,
    AccessModifier Access,
    List<Modifier> Modifiers,
    List<GenericParameter> GenericParameters,
    List<TypeReference> BaseInterfaces,
    List<Member> Members,
    SourceLocation Location
) : TypeDeclaration(Name, Access, Modifiers, GenericParameters, Location);

/// <summary>
/// Struct declaration
/// </summary>
public record StructDeclaration(
    string Name,
    AccessModifier Access,
    List<Modifier> Modifiers,
    List<GenericParameter> GenericParameters,
    List<TypeReference> Interfaces,
    List<Member> Members,
    SourceLocation Location
) : TypeDeclaration(Name, Access, Modifiers, GenericParameters, Location);

/// <summary>
/// Enum declaration
/// </summary>
public record EnumDeclaration(
    string Name,
    AccessModifier Access,
    List<Modifier> Modifiers,
    List<EnumMember> Members,
    SourceLocation Location
) : TypeDeclaration(Name, Access, Modifiers, [], Location);

/// <summary>
/// Enum member
/// </summary>
public record EnumMember(
    string Name,
    Expression? Initializer,
    SourceLocation Location
) : ASTNode(Location);

/// <summary>
/// Generic type parameter
/// </summary>
public record GenericParameter(
    string Name,
    List<TypeReference> Constraints,
    SourceLocation Location
) : ASTNode(Location);

/// <summary>
/// Access modifiers
/// </summary>
public enum AccessModifier
{
    Public,
    Private,
    Protected,
    Internal,
    ProtectedInternal
}

/// <summary>
/// Type modifiers
/// </summary>
public enum Modifier
{
    Static,
    Abstract,
    Virtual,
    Override,
    Sealed,
    Partial,
    Readonly,
    Const,
    New
}

/// <summary>
/// Base class for members (fields, properties, methods)
/// </summary>
public abstract record Member(
    string Name,
    AccessModifier Access,
    List<Modifier> Modifiers,
    SourceLocation Location
) : ASTNode(Location);

/// <summary>
/// Field declaration
/// </summary>
public record FieldDeclaration(
    string Name,
    AccessModifier Access,
    List<Modifier> Modifiers,
    TypeReference FieldType,
    Expression? Initializer,
    SourceLocation Location
) : Member(Name, Access, Modifiers, Location);

/// <summary>
/// Property declaration
/// </summary>
public record PropertyDeclaration(
    string Name,
    AccessModifier Access,
    List<Modifier> Modifiers,
    TypeReference PropertyType,
    PropertyAccessor? GetAccessor,
    PropertyAccessor? SetAccessor,
    Expression? Initializer,
    SourceLocation Location
) : Member(Name, Access, Modifiers, Location);

/// <summary>
/// Property accessor (getter/setter)
/// </summary>
public record PropertyAccessor(
    bool IsGetter,
    AccessModifier? Access,
    BlockStatement Body,
    SourceLocation Location
) : ASTNode(Location);

/// <summary>
/// Method declaration
/// </summary>
public record MethodDeclaration(
    string Name,
    AccessModifier Access,
    List<Modifier> Modifiers,
    TypeReference ReturnType,
    List<GenericParameter> GenericParameters,
    List<Parameter> Parameters,
    BlockStatement? Body,
    Expression? ExpressionBody,
    SourceLocation Location
) : Member(Name, Access, Modifiers, Location);

/// <summary>
/// Constructor declaration
/// </summary>
public record ConstructorDeclaration(
    string Name,
    AccessModifier Access,
    List<Parameter> Parameters,
    ConstructorInitializer? Initializer,
    BlockStatement Body,
    SourceLocation Location
) : Member(Name, Access, [], Location);

/// <summary>
/// Constructor initializer (: base() or : this())
/// </summary>
public record ConstructorInitializer(
    bool IsBase,
    List<Expression> Arguments,
    SourceLocation Location
) : ASTNode(Location);

/// <summary>
/// Method parameter
/// </summary>
public record Parameter(
    string Name,
    TypeReference ParameterType,
    Expression? DefaultValue,
    ParameterModifier Modifier,
    SourceLocation Location
) : ASTNode(Location);

/// <summary>
/// Parameter modifiers
/// </summary>
public enum ParameterModifier
{
    None,
    Ref,
    Out,
    Params
}

/// <summary>
/// Type reference
/// </summary>
public abstract record TypeReference(SourceLocation Location) : ASTNode(Location);

/// <summary>
/// Predefined type (int, string, etc.)
/// </summary>
public record PredefinedType(
    string Name,
    SourceLocation Location
) : TypeReference(Location);

/// <summary>
/// Named type (class, interface, etc.)
/// </summary>
public record NamedType(
    string Name,
    List<TypeReference>? GenericArguments,
    SourceLocation Location
) : TypeReference(Location);

/// <summary>
/// Fully qualified type reference
/// </summary>
public record QualifiedType(
    string Namespace,
    string Name,
    List<TypeReference>? GenericArguments,
    SourceLocation Location
) : TypeReference(Location);

/// <summary>
/// Array type
/// </summary>
public record ArrayType(
    TypeReference ElementType,
    int Rank,
    SourceLocation Location
) : TypeReference(Location);

/// <summary>
/// Generic type instance
/// </summary>
public record GenericType(
    TypeReference BaseType,
    List<TypeReference> TypeArguments,
    SourceLocation Location
) : TypeReference(Location);

/// <summary>
/// Nullable reference type (C# 8+)
/// </summary>
public record NullableType(
    TypeReference ElementType,
    SourceLocation Location
) : TypeReference(Location);

/// <summary>
/// Base class for statements
/// </summary>
public abstract record Statement(SourceLocation Location) : ASTNode(Location);

/// <summary>
/// Block statement
/// </summary>
public record BlockStatement(
    List<Statement> Statements,
    SourceLocation Location
) : Statement(Location);

/// <summary>
/// Expression statement
/// </summary>
public record ExpressionStatement(
    Expression Expression,
    SourceLocation Location
) : Statement(Location);

/// <summary>
/// Variable declaration statement
/// </summary>
public record VariableDeclarationStatement(
    TypeReference VariableType,
    List<VariableDeclarator> Declarators,
    SourceLocation Location
) : Statement(Location);

/// <summary>
/// Variable declarator
/// </summary>
public record VariableDeclarator(
    string Name,
    Expression? Initializer,
    SourceLocation Location
) : ASTNode(Location);

/// <summary>
/// If statement
/// </summary>
public record IfStatement(
    Expression Condition,
    Statement ThenStatement,
    Statement? ElseStatement,
    SourceLocation Location
) : Statement(Location);

/// <summary>
/// While loop
/// </summary>
public record WhileStatement(
    Expression Condition,
    Statement Body,
    SourceLocation Location
) : Statement(Location);

/// <summary>
/// Do-while loop
/// </summary>
public record DoWhileStatement(
    Statement Body,
    Expression Condition,
    SourceLocation Location
) : Statement(Location);

/// <summary>
/// For loop
/// </summary>
public record ForStatement(
    List<VariableDeclarator>? Initializers,
    Expression? Condition,
    List<Expression>? Incrementors,
    Statement Body,
    SourceLocation Location
) : Statement(Location);

/// <summary>
/// Foreach loop
/// </summary>
public record ForeachStatement(
    string VariableName,
    TypeReference? VariableType,
    Expression Collection,
    Statement Body,
    SourceLocation Location
) : Statement(Location);

/// <summary>
/// Return statement
/// </summary>
public record ReturnStatement(
    Expression? Expression,
    SourceLocation Location
) : Statement(Location);

/// <summary>
/// Break statement
/// </summary>
public record BreakStatement(
    SourceLocation Location
) : Statement(Location);

/// <summary>
/// Continue statement
/// </summary>
public record ContinueStatement(
    SourceLocation Location
) : Statement(Location);

/// <summary>
/// Throw statement
/// </summary>
public record ThrowStatement(
    Expression? Expression,
    SourceLocation Location
) : Statement(Location);

/// <summary>
/// Try statement
/// </summary>
public record TryStatement(
    BlockStatement TryBlock,
    List<CatchClause> CatchClauses,
    BlockStatement? FinallyBlock,
    SourceLocation Location
) : Statement(Location);

/// <summary>
/// Catch clause
/// </summary>
public record CatchClause(
    TypeReference? ExceptionType,
    string? VariableName,
    BlockStatement Body,
    SourceLocation Location
) : ASTNode(Location);

/// <summary>
/// Switch statement
/// </summary>
public record SwitchStatement(
    Expression Expression,
    List<SwitchSection> Sections,
    SourceLocation Location
) : Statement(Location);

/// <summary>
/// Switch section (case or default)
/// </summary>
public record SwitchSection(
    List<SwitchLabel> Labels,
    List<Statement> Statements,
    SourceLocation Location
) : ASTNode(Location);

/// <summary>
/// Switch label (case X: or default:)
/// </summary>
public abstract record SwitchLabel(SourceLocation Location) : ASTNode(Location);

public record CaseLabel(Expression Value, SourceLocation Location) : SwitchLabel(Location);
public record DefaultLabel(SourceLocation Location) : SwitchLabel(Location);

/// <summary>
/// Base class for expressions
/// </summary>
public abstract record Expression(SourceLocation Location) : ASTNode(Location);

/// <summary>
/// Literal expressions
/// </summary>
public record IntegerLiteral(int Value, SourceLocation Location) : Expression(Location);
public record FloatLiteral(float Value, SourceLocation Location) : Expression(Location);
public record StringLiteral(string Value, SourceLocation Location) : Expression(Location);
public record CharLiteral(char Value, SourceLocation Location) : Expression(Location);
public record BooleanLiteral(bool Value, SourceLocation Location) : Expression(Location);
public record NullLiteral(SourceLocation Location) : Expression(Location);

/// <summary>
/// Identifier expression
/// </summary>
public record IdentifierExpression(
    string Name,
    SourceLocation Location
) : Expression(Location);

/// <summary>
/// Binary operation
/// </summary>
public record BinaryExpression(
    Expression Left,
    string Operator,
    Expression Right,
    SourceLocation Location
) : Expression(Location);

/// <summary>
/// Unary operation
/// </summary>
public record UnaryExpression(
    string Operator,
    Expression Operand,
    bool IsPostfix,
    SourceLocation Location
) : Expression(Location);

/// <summary>
/// Assignment expression
/// </summary>
public record AssignmentExpression(
    Expression Target,
    string Operator,
    Expression Value,
    SourceLocation Location
) : Expression(Location);

/// <summary>
/// Method/function call
/// </summary>
public record InvocationExpression(
    Expression Method,
    List<Expression> Arguments,
    SourceLocation Location
) : Expression(Location);

/// <summary>
/// Member access (object.member)
/// </summary>
public record MemberAccessExpression(
    Expression Object,
    string MemberName,
    bool IsNullConditional,
    SourceLocation Location
) : Expression(Location);

/// <summary>
/// Array/index access
/// </summary>
public record IndexerExpression(
    Expression Object,
    List<Expression> Indices,
    SourceLocation Location
) : Expression(Location);

/// <summary>
/// Conditional (ternary) expression
/// </summary>
public record ConditionalExpression(
    Expression Condition,
    Expression TrueExpression,
    Expression FalseExpression,
    SourceLocation Location
) : Expression(Location);

/// <summary>
/// Object creation
/// </summary>
public record ObjectCreationExpression(
    TypeReference Type,
    List<Expression> Arguments,
    List<PropertyInitializer>? PropertyInitializers,
    SourceLocation Location
) : Expression(Location);

/// <summary>
/// Array creation
/// </summary>
public record ArrayCreationExpression(
    TypeReference? ElementType,
    List<Expression>? Dimensions,
    List<Expression>? Initializers,
    SourceLocation Location
) : Expression(Location);

/// <summary>
/// Property initializer for object creation
/// </summary>
public record PropertyInitializer(
    string PropertyName,
    Expression Value,
    SourceLocation Location
) : ASTNode(Location);

/// <summary>
/// Lambda expression
/// </summary>
public record LambdaExpression(
    List<Parameter> Parameters,
    Expression Body,
    SourceLocation Location
) : Expression(Location);

/// <summary>
/// Cast expression
/// </summary>
public record CastExpression(
    TypeReference TargetType,
    Expression Expression,
    SourceLocation Location
) : Expression(Location);

/// <summary>
/// as expression (type conversion with null on failure)
/// </summary>
public record AsExpression(
    Expression Expression,
    TypeReference TargetType,
    SourceLocation Location
) : Expression(Location);

/// <summary>
/// is expression (type check)
/// </summary>
public record IsExpression(
    Expression Expression,
    TypeReference TargetType,
    SourceLocation Location
) : Expression(Location);

/// <summary>
/// typeof expression
/// </summary>
public record TypeOfExpression(
    TypeReference Type,
    SourceLocation Location
) : Expression(Location);

/// <summary>
/// nameof expression (C# 6)
/// </summary>
public record NameOfExpression(
    Expression Expression,
    SourceLocation Location
) : Expression(Location);

/// <summary>
/// String interpolation expression (C# 6)
/// </summary>
public record InterpolatedStringExpression(
    List<InterpolationPart> Parts,
    SourceLocation Location
) : Expression(Location);

/// <summary>
/// Part of an interpolated string
/// </summary>
public abstract record InterpolationPart(SourceLocation Location) : ASTNode(Location);
public record InterpolationText(string Text, SourceLocation Location) : InterpolationPart(Location);
public record InterpolationExpression(Expression Expression, string? Format, SourceLocation Location) : InterpolationPart(Location);

/// <summary>
/// This expression
/// </summary>
public record ThisExpression(SourceLocation Location) : Expression(Location);

/// <summary>
/// Base expression
/// </summary>
public record BaseExpression(SourceLocation Location) : Expression(Location);
