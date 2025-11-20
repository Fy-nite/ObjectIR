namespace OIC;

/// <summary>
/// Abstract Syntax Tree classes for C language
/// </summary>
/// 
// Base classes
public abstract record CNode;
public abstract record CDeclaration : CNode;
public abstract record CStatement : CNode;
public abstract record CExpression : CNode;
public abstract record CSpecifier : CNode;

// Program (root)
public record CProgram(List<CDeclaration> Declarations) : CNode;

// Specifiers
public record CTypeSpecifier(string Name) : CSpecifier;
public record CStorageClass(string Class) : CSpecifier; // auto, static, extern, register
public record CTypeQualifier(string Qualifier) : CSpecifier; // const, volatile, restrict

// Declarators
public record CPointerDeclarator(CDirectDeclarator Declarator, int PointerCount = 1) : CNode;
public record CDirectDeclarator(string? Name, List<CDirectDeclarator>? Parameters = null, CExpression? ArraySize = null, CDirectDeclarator? Parent = null) : CNode;

// Declarations
public record CDeclarationList(List<CSpecifier> Specifiers, List<CInitializer> Declarators) : CDeclaration;
public record CInitializer(CDirectDeclarator Declarator, CExpression? Initializer = null) : CNode;

// Function declaration
public record CFunctionDeclaration(
    string Name,
    List<CSpecifier> ReturnTypeSpecifiers,
    List<CParameter> Parameters,
    CCompoundStatement Body
) : CDeclaration;

public record CParameter(List<CSpecifier> Specifiers, CDirectDeclarator? Declarator = null) : CNode;

// Statements
public record CCompoundStatement(List<CDeclaration> Declarations, List<CStatement> Statements) : CStatement;
public record CExpressionStatement(CExpression? Expression = null) : CStatement;
public record CIfStatement(CExpression Condition, CStatement ThenBranch, CStatement? ElseBranch = null) : CStatement;
public record CWhileStatement(CExpression Condition, CStatement Body) : CStatement;
public record CDoWhileStatement(CStatement Body, CExpression Condition) : CStatement;
public record CForStatement(CNode? Initializer, CExpression? Condition, CExpression? Increment, CStatement Body) : CStatement;
public record CReturnStatement(CExpression? Expression = null) : CStatement;
public record CBreakStatement : CStatement;
public record CContinueStatement : CStatement;
public record CGotoStatement(string Label) : CStatement;
public record CLabelStatement(string Label, CStatement Statement) : CStatement;
public record CSwitchStatement(CExpression Condition, CStatement Body) : CStatement;
public record CCaseStatement(CExpression Value, CStatement Statement) : CStatement;
public record CDefaultStatement(CStatement Statement) : CStatement;
public record CEmptyStatement : CStatement;

// Expressions
public record CIdentifier(string Name) : CExpression;
public record CIntLiteral(long Value) : CExpression;
public record CFloatLiteral(double Value) : CExpression;
public record CStringLiteral(string Value) : CExpression;
public record CCharLiteral(char Value) : CExpression;

public record CBinaryOp(CExpression Left, string Op, CExpression Right) : CExpression;
public record CUnaryOp(string Op, CExpression Operand, bool IsPrefix = true) : CExpression;
public record CAssignment(CExpression Left, string Op, CExpression Right) : CExpression;
public record CConditional(CExpression Condition, CExpression TrueBranch, CExpression FalseBranch) : CExpression;

public record CFunctionCall(CExpression Function, List<CExpression> Arguments) : CExpression;
public record CArrayAccess(CExpression Array, CExpression Index) : CExpression;
public record CMemberAccess(CExpression Struct, string Member, bool IsPointer = false) : CExpression;
public record CCast(List<CSpecifier> Specifiers, CDirectDeclarator? Declarator, CExpression Expression) : CExpression;
public record CSizeofExpr(CExpression Expression) : CExpression;
public record CSizeofType(List<CSpecifier> Specifiers, CDirectDeclarator? Declarator = null) : CExpression;

public record CParenthesized(CExpression Expression) : CExpression;

// Type definitions
public record CStructOrUnion(string Kind, string? Name = null, List<CDeclarationList>? Members = null) : CSpecifier;
public record CEnum(string? Name = null, List<(string Name, CExpression? Value)>? Members = null) : CSpecifier;

// Typedef
public record CTypedef(List<CSpecifier> Specifiers, CDirectDeclarator Declarator) : CDeclaration;
