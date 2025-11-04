namespace ObjectIR.Core.Compilers;

using System;
using System.Collections.Generic;

/// <summary>
/// Abstract base for AST nodes
/// </summary>
public abstract class ASTNode { }

/// <summary>
/// Represents a Construct program
/// </summary>
public class Program : ASTNode
{
    public ContractDeclaration Contract { get; set; }

    public Program(ContractDeclaration contract)
    {
        Contract = contract;
    }
}

/// <summary>
/// Represents a contract declaration
/// </summary>
public class ContractDeclaration : ASTNode
{
    public string Name { get; set; }
    public List<FunctionDeclaration> Functions { get; set; }

    public ContractDeclaration(string name, List<FunctionDeclaration> functions)
    {
        Name = name;
        Functions = functions;
    }
}

/// <summary>
/// Represents a function declaration
/// </summary>
public class FunctionDeclaration : ASTNode
{
    public string Name { get; set; }
    public List<Parameter> Parameters { get; set; }
    public TypeAnnotation? ReturnType { get; set; }
    public Block Body { get; set; }

    public FunctionDeclaration(string name, List<Parameter> parameters, TypeAnnotation? returnType, Block body)
    {
        Name = name;
        Parameters = parameters;
        ReturnType = returnType;
        Body = body;
    }
}

/// <summary>
/// Represents a function parameter
/// </summary>
public class Parameter : ASTNode
{
    public string Name { get; set; }
    public TypeAnnotation Type { get; set; }

    public Parameter(string name, TypeAnnotation type)
    {
        Name = name;
        Type = type;
    }
}

/// <summary>
/// Represents a type annotation
/// </summary>
public class TypeAnnotation : ASTNode
{
    public string Name { get; set; }

    public TypeAnnotation(string name)
    {
        Name = name;
    }
}

/// <summary>
/// Represents a block of statements
/// </summary>
public class Block : ASTNode
{
    public List<Statement> Statements { get; set; }

    public Block(List<Statement> statements)
    {
        Statements = statements;
    }
}

/// <summary>
/// Base class for statements
/// </summary>
public abstract class Statement : ASTNode { }

/// <summary>
/// Variable declaration statement
/// </summary>
public class VarDeclaration : Statement
{
    public string Name { get; set; }
    public Expression Initializer { get; set; }

    public VarDeclaration(string name, Expression initializer)
    {
        Name = name;
        Initializer = initializer;
    }
}

/// <summary>
/// Expression statement
/// </summary>
public class ExpressionStatement : Statement
{
    public Expression Expression { get; set; }

    public ExpressionStatement(Expression expression)
    {
        Expression = expression;
    }
}

/// <summary>
/// If statement
/// </summary>
public class IfStatement : Statement
{
    public Expression Condition { get; set; }
    public Block ThenBranch { get; set; }
    public Block? ElseBranch { get; set; }

    public IfStatement(Expression condition, Block thenBranch, Block? elseBranch = null)
    {
        Condition = condition;
        ThenBranch = thenBranch;
        ElseBranch = elseBranch;
    }
}

/// <summary>
/// While statement
/// </summary>
public class WhileStatement : Statement
{
    public Expression Condition { get; set; }
    public Block Body { get; set; }

    public WhileStatement(Expression condition, Block body)
    {
        Condition = condition;
        Body = body;
    }
}

/// <summary>
/// Return statement
/// </summary>
public class ReturnStatement : Statement
{
    public Expression? Value { get; set; }

    public ReturnStatement(Expression? value = null)
    {
        Value = value;
    }
}

/// <summary>
/// Base class for expressions
/// </summary>
public abstract class Expression : ASTNode { }

/// <summary>
/// Integer literal
/// </summary>
public class NumberLiteral : Expression
{
    public int Value { get; set; }

    public NumberLiteral(int value)
    {
        Value = value;
    }
}

/// <summary>
/// String literal
/// </summary>
public class StringLiteral : Expression
{
    public string Value { get; set; }

    public StringLiteral(string value)
    {
        Value = value;
    }
}

/// <summary>
/// Boolean literal
/// </summary>
public class BooleanLiteral : Expression
{
    public bool Value { get; set; }

    public BooleanLiteral(bool value)
    {
        Value = value;
    }
}

/// <summary>
/// Identifier reference
/// </summary>
public class Identifier : Expression
{
    public string Name { get; set; }

    public Identifier(string name)
    {
        Name = name;
    }
}

/// <summary>
/// Binary operation
/// </summary>
public class BinaryOp : Expression
{
    public Expression Left { get; set; }
    public string Operator { get; set; }
    public Expression Right { get; set; }

    public BinaryOp(Expression left, string op, Expression right)
    {
        Left = left;
        Operator = op;
        Right = right;
    }
}

/// <summary>
/// Unary operation
/// </summary>
public class UnaryOp : Expression
{
    public string Operator { get; set; }
    public Expression Operand { get; set; }

    public UnaryOp(string op, Expression operand)
    {
        Operator = op;
        Operand = operand;
    }
}

/// <summary>
/// Function call
/// </summary>
public class FunctionCall : Expression
{
    public string Name { get; set; }
    public List<Expression> Arguments { get; set; }

    public FunctionCall(string name, List<Expression> arguments)
    {
        Name = name;
        Arguments = arguments;
    }
}

/// <summary>
/// Member access (e.g., IO.println)
/// </summary>
public class MemberAccess : Expression
{
    public Expression Object { get; set; }
    public string Member { get; set; }

    public MemberAccess(Expression obj, string member)
    {
        Object = obj;
        Member = member;
    }
}

/// <summary>
/// Assignment expression
/// </summary>
public class Assignment : Expression
{
    public string Name { get; set; }
    public Expression Value { get; set; }

    public Assignment(string name, Expression value)
    {
        Name = name;
        Value = value;
    }
}
