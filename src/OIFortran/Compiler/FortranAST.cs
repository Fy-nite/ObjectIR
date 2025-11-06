using System.Collections.Generic;

namespace ObjectIR.Fortran.Compiler;

internal sealed class FortranProgram
{
    public string Name { get; }
    public IReadOnlyList<FortranStatement> Statements { get; }
    public IReadOnlyList<FortranSubroutineDefinition> Subroutines { get; }

    public FortranProgram(string name, IReadOnlyList<FortranStatement> statements, IReadOnlyList<FortranSubroutineDefinition>? subroutines = null)
    {
        Name = name;
        Statements = statements;
        Subroutines = subroutines ?? [];
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

internal sealed class FortranReturnStatement : FortranStatement { }

/// <summary>
/// IF (condition) THEN
///   statements
/// [ELSE IF (condition) THEN
///   statements]
/// [ELSE
///   statements]
/// END IF
/// </summary>
internal sealed class FortranIfStatement : FortranStatement
{
    public FortranExpression Condition { get; }
    public IReadOnlyList<FortranStatement> ThenStatements { get; }
    public IReadOnlyList<(FortranExpression Condition, IReadOnlyList<FortranStatement> Statements)> ElseIfParts { get; }
    public IReadOnlyList<FortranStatement>? ElseStatements { get; }

    public FortranIfStatement(
        FortranExpression condition,
        IReadOnlyList<FortranStatement> thenStatements,
        IReadOnlyList<(FortranExpression, IReadOnlyList<FortranStatement>)>? elseIfParts = null,
        IReadOnlyList<FortranStatement>? elseStatements = null)
    {
        Condition = condition;
        ThenStatements = thenStatements;
        ElseIfParts = elseIfParts ?? [];
        ElseStatements = elseStatements;
    }
}

/// <summary>
/// DO [label] var = start, end [, step]
///   statements
/// END DO [label]
/// </summary>
internal sealed class FortranDoStatement : FortranStatement
{
    public string LoopVariable { get; }
    public FortranExpression Start { get; }
    public FortranExpression End { get; }
    public FortranExpression? Step { get; }
    public IReadOnlyList<FortranStatement> Statements { get; }
    public int? Label { get; } // Optional label for old-style DO

    public FortranDoStatement(
        string loopVariable,
        FortranExpression start,
        FortranExpression end,
        FortranExpression? step,
        IReadOnlyList<FortranStatement> statements,
        int? label = null)
    {
        LoopVariable = loopVariable;
        Start = start;
        End = end;
        Step = step;
        Statements = statements;
        Label = label;
    }
}

/// <summary>
/// DO WHILE (condition)
///   statements
/// END DO
/// </summary>
internal sealed class FortranDoWhileStatement : FortranStatement
{
    public FortranExpression Condition { get; }
    public IReadOnlyList<FortranStatement> Statements { get; }

    public FortranDoWhileStatement(FortranExpression condition, IReadOnlyList<FortranStatement> statements)
    {
        Condition = condition;
        Statements = statements;
    }
}

/// <summary>
/// SELECT CASE (expr)
/// CASE (value1, value2, ...)
///   statements
/// CASE DEFAULT
///   statements
/// END SELECT
/// </summary>
internal sealed class FortranSelectCaseStatement : FortranStatement
{
    public FortranExpression Expression { get; }
    public IReadOnlyList<FortranCaseClause> CaseClauses { get; }
    public IReadOnlyList<FortranStatement>? DefaultStatements { get; }

    public FortranSelectCaseStatement(
        FortranExpression expression,
        IReadOnlyList<FortranCaseClause> caseClauses,
        IReadOnlyList<FortranStatement>? defaultStatements = null)
    {
        Expression = expression;
        CaseClauses = caseClauses;
        DefaultStatements = defaultStatements;
    }
}

internal sealed class FortranCaseClause
{
    public IReadOnlyList<FortranCaseValue> Values { get; }
    public IReadOnlyList<FortranStatement> Statements { get; }

    public FortranCaseClause(IReadOnlyList<FortranCaseValue> values, IReadOnlyList<FortranStatement> statements)
    {
        Values = values;
        Statements = statements;
    }
}

/// <summary>
/// A case value can be a single value, or a range (value1:value2)
/// </summary>
internal abstract class FortranCaseValue { }

internal sealed class FortranCaseSingleValue : FortranCaseValue
{
    public FortranExpression Value { get; }

    public FortranCaseSingleValue(FortranExpression value)
    {
        Value = value;
    }
}

internal sealed class FortranCaseRangeValue : FortranCaseValue
{
    public FortranExpression? Start { get; }
    public FortranExpression? End { get; }

    public FortranCaseRangeValue(FortranExpression? start, FortranExpression? end)
    {
        Start = start;
        End = end;
    }
}

/// <summary>
/// EXIT [label]
/// </summary>
internal sealed class FortranExitStatement : FortranStatement
{
    public int? Label { get; }

    public FortranExitStatement(int? label = null)
    {
        Label = label;
    }
}

/// <summary>
/// CYCLE [label]
/// </summary>
internal sealed class FortranCycleStatement : FortranStatement
{
    public int? Label { get; }

    public FortranCycleStatement(int? label = null)
    {
        Label = label;
    }
}

/// <summary>
/// STOP [code]
/// </summary>
internal sealed class FortranStopStatement : FortranStatement
{
    public FortranExpression? Code { get; }

    public FortranStopStatement(FortranExpression? code = null)
    {
        Code = code;
    }
}

internal sealed class FortranSubroutineDefinition
{
    public string Name { get; }
    public IReadOnlyList<string> Parameters { get; }
    public IReadOnlyList<FortranStatement> Statements { get; }

    public FortranSubroutineDefinition(string name, IReadOnlyList<string> parameters, IReadOnlyList<FortranStatement> statements)
    {
        Name = name;
        Parameters = parameters;
        Statements = statements;
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
    // Arithmetic
    Add,
    Subtract,
    Multiply,
    Divide,
    Power,
    
    // Relational (Fortran style)
    EqFortran,
    NeFortran,
    LtFortran,
    LeFortran,
    GtFortran,
    GeFortran,
    
    // Relational (Modern style)
    EqModern,
    NeModern,
    LtModern,
    LeModern,
    GtModern,
    GeModern,
    
    // Logical
    And,
    Or
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
    Minus,
    Not
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

// ============================================================================
// FORTRAN 90 AST CLASSES
// ============================================================================

/// <summary>
/// Represents a MODULE definition (Fortran 90+)
/// </summary>
internal sealed class FortranModule
{
    public string Name { get; }
    public IReadOnlyList<FortranUseStatement> UseStatements { get; }
    public IReadOnlyList<FortranStatement> Declarations { get; }
    public IReadOnlyList<FortranSubroutineDefinition> Subroutines { get; }
    public IReadOnlyList<FortranFunctionDefinition> Functions { get; }
    public IReadOnlyList<FortranDerivedType> DerivedTypes { get; }
    public IReadOnlyList<FortranInterfaceBlock> Interfaces { get; }

    public FortranModule(
        string name,
        IReadOnlyList<FortranUseStatement>? useStatements = null,
        IReadOnlyList<FortranStatement>? declarations = null,
        IReadOnlyList<FortranSubroutineDefinition>? subroutines = null,
        IReadOnlyList<FortranFunctionDefinition>? functions = null,
        IReadOnlyList<FortranDerivedType>? derivedTypes = null,
        IReadOnlyList<FortranInterfaceBlock>? interfaces = null)
    {
        Name = name;
        UseStatements = useStatements ?? [];
        Declarations = declarations ?? [];
        Subroutines = subroutines ?? [];
        Functions = functions ?? [];
        DerivedTypes = derivedTypes ?? [];
        Interfaces = interfaces ?? [];
    }
}

/// <summary>
/// Represents a USE statement for module imports (Fortran 90+)
/// USE module_name [, ONLY: rename_list]
/// </summary>
internal sealed class FortranUseStatement : FortranStatement
{
    public string ModuleName { get; }
    public IReadOnlyList<string>? OnlyList { get; }
    public bool HasOnly { get; }

    public FortranUseStatement(string moduleName, IReadOnlyList<string>? onlyList = null)
    {
        ModuleName = moduleName;
        OnlyList = onlyList;
        HasOnly = onlyList != null;
    }
}

/// <summary>
/// Represents a FUNCTION definition (Fortran 90+)
/// </summary>
internal sealed class FortranFunctionDefinition
{
    public string Name { get; }
    public FortranTypeSpec ReturnType { get; }
    public IReadOnlyList<FortranFunctionParameter> Parameters { get; }
    public IReadOnlyList<FortranStatement> Statements { get; }
    public IReadOnlyList<FortranSubroutineDefinition> InternalSubroutines { get; }

    public FortranFunctionDefinition(
        string name,
        FortranTypeSpec returnType,
        IReadOnlyList<FortranFunctionParameter>? parameters = null,
        IReadOnlyList<FortranStatement>? statements = null,
        IReadOnlyList<FortranSubroutineDefinition>? internalSubroutines = null)
    {
        Name = name;
        ReturnType = returnType;
        Parameters = parameters ?? [];
        Statements = statements ?? [];
        InternalSubroutines = internalSubroutines ?? [];
    }
}

/// <summary>
/// Represents a parameter in a function or subroutine (Fortran 90+)
/// with INTENT and optional attributes
/// </summary>
internal sealed class FortranFunctionParameter
{
    public string Name { get; }
    public FortranTypeSpec Type { get; }
    public FortranIntentKind? Intent { get; }
    public bool IsOptional { get; }
    public bool IsAllocatable { get; }
    public bool IsPointer { get; }
    public IReadOnlyList<FortranArrayDimension>? Dimensions { get; }

    public FortranFunctionParameter(
        string name,
        FortranTypeSpec type,
        FortranIntentKind? intent = null,
        bool isOptional = false,
        bool isAllocatable = false,
        bool isPointer = false,
        IReadOnlyList<FortranArrayDimension>? dimensions = null)
    {
        Name = name;
        Type = type;
        Intent = intent;
        IsOptional = isOptional;
        IsAllocatable = isAllocatable;
        IsPointer = isPointer;
        Dimensions = dimensions;
    }
}

internal enum FortranIntentKind
{
    In,
    Out,
    InOut
}

/// <summary>
/// Represents a derived type definition (TYPE...END TYPE block)
/// </summary>
internal sealed class FortranDerivedType
{
    public string Name { get; }
    public IReadOnlyList<FortranTypeComponent> Components { get; }
    public bool IsSequence { get; }
    public bool IsPublic { get; }

    public FortranDerivedType(
        string name,
        IReadOnlyList<FortranTypeComponent>? components = null,
        bool isSequence = false,
        bool isPublic = true)
    {
        Name = name;
        Components = components ?? [];
        IsSequence = isSequence;
        IsPublic = isPublic;
    }
}

/// <summary>
/// Represents a component in a derived type
/// </summary>
internal sealed class FortranTypeComponent
{
    public string Name { get; }
    public FortranTypeSpec Type { get; }
    public IReadOnlyList<FortranArrayDimension>? Dimensions { get; }
    public bool IsPointer { get; }
    public bool IsAllocatable { get; }
    public FortranExpression? InitialValue { get; }

    public FortranTypeComponent(
        string name,
        FortranTypeSpec type,
        IReadOnlyList<FortranArrayDimension>? dimensions = null,
        bool isPointer = false,
        bool isAllocatable = false,
        FortranExpression? initialValue = null)
    {
        Name = name;
        Type = type;
        Dimensions = dimensions;
        IsPointer = isPointer;
        IsAllocatable = isAllocatable;
        InitialValue = initialValue;
    }
}

/// <summary>
/// Represents an array dimension specification
/// Examples: 10, 1:10, :, *
/// </summary>
internal abstract class FortranArrayDimension { }

internal sealed class FortranExplicitShapeDimension : FortranArrayDimension
{
    public FortranExpression? Lower { get; }
    public FortranExpression Upper { get; }

    public FortranExplicitShapeDimension(FortranExpression upper, FortranExpression? lower = null)
    {
        Upper = upper;
        Lower = lower;
    }
}

internal sealed class FortranAssumedShapeDimension : FortranArrayDimension
{
    public FortranExpression? Lower { get; }

    public FortranAssumedShapeDimension(FortranExpression? lower = null)
    {
        Lower = lower;
    }
}

internal sealed class FortranAssumedSizeDimension : FortranArrayDimension { }

internal sealed class FortranDeferredShapeDimension : FortranArrayDimension { }

/// <summary>
/// Represents an INTERFACE block (Fortran 90+)
/// </summary>
internal sealed class FortranInterfaceBlock
{
    public string? Name { get; }
    public IReadOnlyList<FortranFunctionDefinition> Functions { get; }
    public IReadOnlyList<FortranSubroutineDefinition> Subroutines { get; }
    public bool IsOperatorInterface { get; }
    public bool IsAssignmentInterface { get; }

    public FortranInterfaceBlock(
        string? name = null,
        IReadOnlyList<FortranFunctionDefinition>? functions = null,
        IReadOnlyList<FortranSubroutineDefinition>? subroutines = null,
        bool isOperatorInterface = false,
        bool isAssignmentInterface = false)
    {
        Name = name;
        Functions = functions ?? [];
        Subroutines = subroutines ?? [];
        IsOperatorInterface = isOperatorInterface;
        IsAssignmentInterface = isAssignmentInterface;
    }
}

/// <summary>
/// Represents an ALLOCATE statement (Fortran 90+)
/// ALLOCATE (array1(size1), array2(size2), ...)
/// </summary>
internal sealed class FortranAllocateStatement : FortranStatement
{
    public IReadOnlyList<FortranAllocationObject> Objects { get; }

    public FortranAllocateStatement(IReadOnlyList<FortranAllocationObject> objects)
    {
        Objects = objects;
    }
}

internal sealed class FortranAllocationObject
{
    public string Name { get; }
    public IReadOnlyList<FortranArrayDimension>? Dimensions { get; }

    public FortranAllocationObject(string name, IReadOnlyList<FortranArrayDimension>? dimensions = null)
    {
        Name = name;
        Dimensions = dimensions;
    }
}

/// <summary>
/// Represents a DEALLOCATE statement (Fortran 90+)
/// DEALLOCATE (array1, array2, ...)
/// </summary>
internal sealed class FortranDeallocateStatement : FortranStatement
{
    public IReadOnlyList<string> Names { get; }

    public FortranDeallocateStatement(IReadOnlyList<string> names)
    {
        Names = names;
    }
}

/// <summary>
/// Represents a NULLIFY statement (Fortran 90+)
/// NULLIFY (ptr1, ptr2, ...)
/// </summary>
internal sealed class FortranNullifyStatement : FortranStatement
{
    public IReadOnlyList<string> Names { get; }

    public FortranNullifyStatement(IReadOnlyList<string> names)
    {
        Names = names;
    }
}

/// <summary>
/// Represents a declaration with Fortran 90 attributes
/// Example: REAL, DIMENSION(:,:), ALLOCATABLE :: matrix
/// </summary>
internal sealed class FortranAttributedDeclarationStatement : FortranStatement
{
    public FortranTypeSpec Type { get; }
    public IReadOnlyList<FortranDeclarationAttribute> Attributes { get; }
    public IReadOnlyList<FortranDeclaredEntity> Entities { get; }

    public FortranAttributedDeclarationStatement(
        FortranTypeSpec type,
        IReadOnlyList<FortranDeclarationAttribute> attributes,
        IReadOnlyList<FortranDeclaredEntity> entities)
    {
        Type = type;
        Attributes = attributes;
        Entities = entities;
    }
}

internal abstract class FortranDeclarationAttribute { }

internal sealed class FortranDimensionAttribute : FortranDeclarationAttribute
{
    public IReadOnlyList<FortranArrayDimension> Dimensions { get; }

    public FortranDimensionAttribute(IReadOnlyList<FortranArrayDimension> dimensions)
    {
        Dimensions = dimensions;
    }
}

internal sealed class FortranParameterAttribute : FortranDeclarationAttribute { }

internal sealed class FortranAllocatableAttribute : FortranDeclarationAttribute { }

internal sealed class FortranPointerAttribute : FortranDeclarationAttribute { }

internal sealed class FortranTargetAttribute : FortranDeclarationAttribute { }

internal sealed class FortranValueAttribute : FortranDeclarationAttribute { }

internal sealed class FortranIntentAttribute : FortranDeclarationAttribute
{
    public FortranIntentKind Intent { get; }

    public FortranIntentAttribute(FortranIntentKind intent)
    {
        Intent = intent;
    }
}

internal sealed class FortranOptionalAttribute : FortranDeclarationAttribute { }

internal sealed class FortranPublicAttribute : FortranDeclarationAttribute { }

internal sealed class FortranPrivateAttribute : FortranDeclarationAttribute { }

internal sealed class FortranProtectedAttribute : FortranDeclarationAttribute { }

internal sealed class FortranDeclaredEntity
{
    public string Name { get; }
    public IReadOnlyList<FortranArrayDimension>? Dimensions { get; }
    public FortranExpression? InitialValue { get; }

    public FortranDeclaredEntity(
        string name,
        IReadOnlyList<FortranArrayDimension>? dimensions = null,
        FortranExpression? initialValue = null)
    {
        Name = name;
        Dimensions = dimensions;
        InitialValue = initialValue;
    }
}

/// <summary>
/// Represents a component reference expression for derived types
/// Example: point%x, vector%start%x
/// </summary>
internal sealed class FortranComponentRefExpression : FortranExpression
{
    public FortranExpression Object { get; }
    public string ComponentName { get; }

    public FortranComponentRefExpression(FortranExpression obj, string componentName)
    {
        Object = obj;
        ComponentName = componentName;
    }
}

/// <summary>
/// Represents an array subscript expression
/// Example: matrix(i,j), array(1:10)
/// </summary>
internal sealed class FortranArraySubscriptExpression : FortranExpression
{
    public FortranExpression Array { get; }
    public IReadOnlyList<FortranSubscript> Subscripts { get; }

    public FortranArraySubscriptExpression(FortranExpression array, IReadOnlyList<FortranSubscript> subscripts)
    {
        Array = array;
        Subscripts = subscripts;
    }
}

internal abstract class FortranSubscript { }

internal sealed class FortranExpressionSubscript : FortranSubscript
{
    public FortranExpression Expression { get; }

    public FortranExpressionSubscript(FortranExpression expression)
    {
        Expression = expression;
    }
}

internal sealed class FortranRangeSubscript : FortranSubscript
{
    public FortranExpression? Lower { get; }
    public FortranExpression? Upper { get; }
    public FortranExpression? Stride { get; }

    public FortranRangeSubscript(FortranExpression? lower = null, FortranExpression? upper = null, FortranExpression? stride = null)
    {
        Lower = lower;
        Upper = upper;
        Stride = stride;
    }
}

/// <summary>
/// Represents a pointer assignment expression (=>)
/// </summary>
internal sealed class FortranPointerAssignmentExpression : FortranExpression
{
    public FortranExpression Pointer { get; }
    public FortranExpression Target { get; }

    public FortranPointerAssignmentExpression(FortranExpression pointer, FortranExpression target)
    {
        Pointer = pointer;
        Target = target;
    }
}

/// <summary>
/// Represents the NULL() intrinsic function
/// </summary>
internal sealed class FortranNullExpression : FortranExpression { }

