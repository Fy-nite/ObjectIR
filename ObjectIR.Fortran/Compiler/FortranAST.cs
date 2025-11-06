using System.Collections.Generic;

namespace ObjectIR.Fortran.Compiler;

internal sealed class FortranProgram
{
    public string Name { get; }
    public IReadOnlyList<FortranStatement> Statements { get; }

    public FortranProgram(string name, IReadOnlyList<FortranStatement> statements)
    {
        Name = name;
        Statements = statements;
    }
}

internal enum FortranTypeKind
{
    Integer,
    Real,
    Logical,
    Character
}

internal sealed class FortranTypeSpec
{
    public FortranTypeKind Kind { get; }

    public FortranTypeSpec(FortranTypeKind kind)
    {
        Kind = kind;
    }
}

internal abstract class FortranStatement { }

internal sealed class FortranImplicitNoneStatement : FortranStatement { }

internal sealed class FortranDeclarationStatement : FortranStatement
{
    public FortranTypeSpec Type { get; }
    public IReadOnlyList<string> Names { get; }

    public FortranDeclarationStatement(FortranTypeSpec type, IReadOnlyList<string> names)
    {
        Type = type;
        Names = names;
    }
}

internal sealed class FortranAssignmentStatement : FortranStatement
{
    public string Name { get; }
    public FortranExpression Expression { get; }

    public FortranAssignmentStatement(string name, FortranExpression expression)
    {
        Name = name;
        Expression = expression;
    }
}

internal sealed class FortranPrintStatement : FortranStatement
{
    public IReadOnlyList<FortranExpression> Arguments { get; }

    public FortranPrintStatement(IReadOnlyList<FortranExpression> arguments)
    {
        Arguments = arguments;
    }
}

internal sealed class FortranCallStatement : FortranStatement
{
    public string Name { get; }
    public IReadOnlyList<FortranExpression> Arguments { get; }

    public FortranCallStatement(string name, IReadOnlyList<FortranExpression> arguments)
    {
        Name = name;
        Arguments = arguments;
    }
}

internal abstract class FortranExpression { }

internal sealed class FortranCallExpression : FortranExpression
{
    public string Name { get; }
    public IReadOnlyList<FortranExpression> Arguments { get; }

    public FortranCallExpression(string name, IReadOnlyList<FortranExpression> arguments)
    {
        Name = name;
        Arguments = arguments;
    }
}

internal sealed class FortranIntegerLiteralExpression : FortranExpression
{
    public long Value { get; }

    public FortranIntegerLiteralExpression(long value)
    {
        Value = value;
    }
}

internal sealed class FortranRealLiteralExpression : FortranExpression
{
    public double Value { get; }

    public FortranRealLiteralExpression(double value)
    {
        Value = value;
    }
}

internal sealed class FortranStringLiteralExpression : FortranExpression
{
    public string Value { get; }

    public FortranStringLiteralExpression(string value)
    {
        Value = value;
    }
}

internal sealed class FortranLogicalLiteralExpression : FortranExpression
{
    public bool Value { get; }

    public FortranLogicalLiteralExpression(bool value)
    {
        Value = value;
    }
}

internal sealed class FortranIdentifierExpression : FortranExpression
{
    public string Name { get; }

    public FortranIdentifierExpression(string name)
    {
        Name = name;
    }
}

internal enum FortranBinaryOperator
{
    Add,
    Subtract,
    Multiply,
    Divide
}

internal sealed class FortranBinaryExpression : FortranExpression
{
    public FortranExpression Left { get; }
    public FortranBinaryOperator Operator { get; }
    public FortranExpression Right { get; }

    public FortranBinaryExpression(FortranExpression left, FortranBinaryOperator op, FortranExpression right)
    {
        Left = left;
        Operator = op;
        Right = right;
    }
}

internal enum FortranUnaryOperator
{
    Plus,
    Minus
}

internal sealed class FortranUnaryExpression : FortranExpression
{
    public FortranUnaryOperator Operator { get; }
    public FortranExpression Operand { get; }

    public FortranUnaryExpression(FortranUnaryOperator op, FortranExpression operand)
    {
        Operator = op;
        Operand = operand;
    }
}
