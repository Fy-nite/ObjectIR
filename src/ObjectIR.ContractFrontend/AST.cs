namespace ObjectIR.ContractFrontend.AST;

public abstract record Node(SourceSpan Span);

public record SourceSpan(int Start, int Length, int Line, int Column)
{
    public static readonly SourceSpan Empty = new(0,0,1,1);
}

public record ContractUnit(List<ContractDecl> Contracts) : Node(SourceSpan.Empty);
public record ContractDecl(string Name, List<FunctionDecl> Functions, SourceSpan Span) : Node(Span);
public record FunctionDecl(string Name, List<ParameterDecl> Parameters, BlockStmt Body, SourceSpan Span) : Node(Span);
public record ParameterDecl(string Name, SourceSpan Span) : Node(Span);

// Statements
public abstract record Stmt(SourceSpan Span) : Node(Span);
public record BlockStmt(List<Stmt> Statements, SourceSpan Span) : Stmt(Span);
public record VarDeclStmt(string Name, Expr? Initializer, SourceSpan Span) : Stmt(Span);
public record ExprStmt(Expr Expression, SourceSpan Span) : Stmt(Span);
public record IfStmt(Expr Condition, BlockStmt Then, BlockStmt? Else, SourceSpan Span) : Stmt(Span);
public record WhileStmt(Expr Condition, BlockStmt Body, SourceSpan Span) : Stmt(Span);
public record ReturnStmt(Expr? Expression, SourceSpan Span) : Stmt(Span);

// Expressions
public abstract record Expr(SourceSpan Span) : Node(Span);
public record IntLiteralExpr(int Value, SourceSpan Span) : Expr(Span);
public record StringLiteralExpr(string Value, SourceSpan Span) : Expr(Span);
public record IdentifierExpr(string Name, SourceSpan Span) : Expr(Span);
public record CallExpr(Expr Target, List<Expr> Arguments, SourceSpan Span) : Expr(Span);
public record BinaryExpr(string Op, Expr Left, Expr Right, SourceSpan Span) : Expr(Span);
