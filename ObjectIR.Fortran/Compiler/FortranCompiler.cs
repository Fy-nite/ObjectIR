using System;
using System.Collections.Generic;
using ObjectIR.Core.Builder;
using ObjectIR.Core.IR;

namespace ObjectIR.Fortran.Compiler;

internal sealed class FortranCompiler
{
    private readonly Dictionary<string, TypeReference> _locals = new(StringComparer.OrdinalIgnoreCase);
    private MethodBuilder? _methodBuilder;
    private InstructionBuilder? _instructions;
    private bool _implicitNone;
    private readonly FortranCompilationOptions _options;

    public FortranCompiler(FortranCompilationOptions? options = null)
    {
        _options = options ?? FortranCompilationOptions.Default;
    }

    public Module Compile(FortranProgram program)
    {
        _locals.Clear();
        _implicitNone = false;

        var builder = new IRBuilder(program.Name);
        var classBuilder = builder.Class(program.Name)
            .Namespace(program.Name)
            .Access(AccessModifier.Public);

        _methodBuilder = classBuilder.Method("Main", TypeReference.Void)
            .Access(AccessModifier.Public)
            .Static();
        _instructions = _methodBuilder.Body();

        foreach (var statement in program.Statements)
        {
            EmitStatement(statement);
        }

        _instructions.Ret();
        _instructions.EndBody();
        classBuilder.EndClass();
        return builder.Build();
    }

    private void EmitStatement(FortranStatement statement)
    {
        switch (statement)
        {
            case FortranImplicitNoneStatement:
                _implicitNone = true;
                break;
            case FortranDeclarationStatement declaration:
                EmitDeclaration(declaration);
                break;
            case FortranAssignmentStatement assignment:
                EmitAssignment(assignment);
                break;
            case FortranPrintStatement print:
                EmitPrint(print);
                break;
            case FortranCallStatement call:
                EmitCall(call);
                break;
            default:
                throw new InvalidOperationException($"Unsupported statement type: {statement.GetType().Name}");
        }
    }

    private void EmitDeclaration(FortranDeclarationStatement declaration)
    {
        var type = MapType(declaration.Type);
        foreach (var name in declaration.Names)
        {
            if (_locals.ContainsKey(name))
            {
                throw new InvalidOperationException($"Variable '{name}' is already declared");
            }

            _locals[name] = type;
            _methodBuilder!.Local(name, type);
        }
    }

    private void EmitAssignment(FortranAssignmentStatement assignment)
    {
        var targetType = ResolveVariableType(assignment.Name);
        var expressionType = EmitExpression(assignment.Expression, targetType);
        EnsureAssignable(targetType, expressionType);
        _instructions!.Stloc(assignment.Name);
    }

    private void EmitPrint(FortranPrintStatement print)
    {
        if (print.Arguments.Count == 0)
        {
            _instructions!.Call(ResolveWriteLine(null));
            return;
        }

        if (print.Arguments.Count > 1)
        {
            throw new NotSupportedException("PRINT with multiple arguments is not supported yet");
        }

        var expressionType = EmitExpression(print.Arguments[0]);
        var method = ResolveWriteLine(expressionType);
        _instructions!.Call(method);
    }

    private void EmitCall(FortranCallStatement call)
    {
        if (!_options.Intrinsics.TryGet(call.Name, out var intrinsic))
        {
            throw new InvalidOperationException($"No intrinsic or binding registered for procedure '{call.Name}'");
        }

        if (intrinsic.ReturnsValue)
        {
            throw new InvalidOperationException($"CALL target '{call.Name}' returns a value; use it in an expression instead");
        }

        EmitIntrinsicArguments(intrinsic, call.Arguments);
        EmitIntrinsicInvocation(intrinsic);
    }

    private TypeReference EmitExpression(FortranExpression expression, TypeReference? expectedType = null)
    {
        var targetType = expectedType ?? ResolveExpressionType(expression, expectedType);
        return EmitExpressionWithTarget(expression, targetType);
    }

    private TypeReference EmitExpressionWithTarget(FortranExpression expression, TypeReference targetType)
    {
        switch (expression)
        {
            case FortranIntegerLiteralExpression integerLiteral:
                return EmitIntegerLiteral(integerLiteral, targetType);
            case FortranRealLiteralExpression realLiteral:
                return EmitRealLiteral(realLiteral, targetType);
            case FortranStringLiteralExpression stringLiteral:
                _instructions!.Ldstr(stringLiteral.Value);
                EnsureAssignable(targetType, TypeReference.String);
                return TypeReference.String;
            case FortranLogicalLiteralExpression logicalLiteral:
                _instructions!.LdcI4(logicalLiteral.Value ? 1 : 0);
                EnsureAssignable(targetType, TypeReference.Bool);
                return TypeReference.Bool;
            case FortranIdentifierExpression identifier:
                return EmitIdentifier(identifier, targetType);
            case FortranCallExpression callExpression:
                return EmitCallExpression(callExpression, targetType);
            case FortranBinaryExpression binary:
                return EmitBinaryExpression(binary, targetType);
            case FortranUnaryExpression unary:
                return EmitUnaryExpression(unary, targetType);
            default:
                throw new InvalidOperationException($"Unsupported expression type: {expression.GetType().Name}");
        }
    }

    private TypeReference EmitIntegerLiteral(FortranIntegerLiteralExpression literal, TypeReference targetType)
    {
        if (literal.Value > int.MaxValue || literal.Value < int.MinValue)
        {
            throw new InvalidOperationException("INTEGER literal out of supported range");
        }

        if (TypeEquals(targetType, TypeReference.Int32))
        {
            _instructions!.LdcI4((int)literal.Value);
            return TypeReference.Int32;
        }

        if (TypeEquals(targetType, TypeReference.Float32))
        {
            _instructions!.LdcR4((float)literal.Value);
            return TypeReference.Float32;
        }

        throw new InvalidOperationException($"Cannot assign integer literal to {targetType.GetQualifiedName()}");
    }

    private TypeReference EmitRealLiteral(FortranRealLiteralExpression literal, TypeReference targetType)
    {
        if (!TypeEquals(targetType, TypeReference.Float32))
        {
            throw new InvalidOperationException("Real literals require REAL context");
        }

        _instructions!.LdcR4((float)literal.Value);
        return TypeReference.Float32;
    }

    private TypeReference EmitIdentifier(FortranIdentifierExpression identifier, TypeReference targetType)
    {
        var name = identifier.Name;
        var variableType = ResolveVariableType(name);
        if (!TypeEquals(variableType, targetType))
        {
            throw new InvalidOperationException($"Type mismatch for '{name}'");
        }

        _instructions!.Ldloc(name);
        return variableType;
    }

    private TypeReference EmitBinaryExpression(FortranBinaryExpression expression, TypeReference targetType)
    {
        if (!IsNumeric(targetType))
        {
            throw new InvalidOperationException("Arithmetic operations require numeric types");
        }

        EmitExpressionWithTarget(expression.Left, targetType);
        EmitExpressionWithTarget(expression.Right, targetType);

        switch (expression.Operator)
        {
            case FortranBinaryOperator.Add:
                _instructions!.Add();
                break;
            case FortranBinaryOperator.Subtract:
                _instructions!.Sub();
                break;
            case FortranBinaryOperator.Multiply:
                _instructions!.Mul();
                break;
            case FortranBinaryOperator.Divide:
                _instructions!.Div();
                break;
            default:
                throw new InvalidOperationException($"Unsupported binary operator: {expression.Operator}");
        }

        return targetType;
    }

    private TypeReference EmitUnaryExpression(FortranUnaryExpression expression, TypeReference targetType)
    {
        switch (expression.Operator)
        {
            case FortranUnaryOperator.Plus:
                return EmitExpressionWithTarget(expression.Operand, targetType);
            case FortranUnaryOperator.Minus:
                var operandType = EmitExpressionWithTarget(expression.Operand, targetType);
                if (!IsNumeric(operandType))
                {
                    throw new InvalidOperationException("Unary minus requires numeric operand");
                }

                if (TypeEquals(operandType, TypeReference.Int32))
                {
                    _instructions!.LdcI4(-1);
                    _instructions.Mul();
                }
                else if (TypeEquals(operandType, TypeReference.Float32))
                {
                    _instructions!.LdcR4(-1.0f);
                    _instructions.Mul();
                }
                else
                {
                    throw new InvalidOperationException("Unsupported numeric type for unary minus");
                }

                return operandType;
            default:
                throw new InvalidOperationException($"Unsupported unary operator: {expression.Operator}");
        }
    }

    private TypeReference ResolveExpressionType(FortranExpression expression, TypeReference? expectedType)
    {
        switch (expression)
        {
            case FortranIntegerLiteralExpression:
                return expectedType ?? TypeReference.Int32;
            case FortranRealLiteralExpression:
                return expectedType ?? TypeReference.Float32;
            case FortranLogicalLiteralExpression:
                return TypeReference.Bool;
            case FortranStringLiteralExpression:
                return TypeReference.String;
            case FortranIdentifierExpression identifier:
                var type = ResolveVariableType(identifier.Name);
                if (expectedType != null && !TypeEquals(type, expectedType))
                {
                    throw new InvalidOperationException($"Type mismatch for identifier '{identifier.Name}'");
                }
                return expectedType ?? type;
            case FortranCallExpression callExpression:
                var intrinsic = ResolveIntrinsic(callExpression.Name);
                if (!intrinsic.ReturnsValue)
                {
                    throw new InvalidOperationException($"Procedure '{callExpression.Name}' cannot be used in an expression");
                }
                return intrinsic.Method.ReturnType;
            case FortranUnaryExpression unary:
                return ResolveExpressionType(unary.Operand, expectedType);
            case FortranBinaryExpression binary:
                var leftType = ResolveExpressionType(binary.Left, expectedType);
                var rightType = ResolveExpressionType(binary.Right, expectedType);
                var resolved = expectedType ?? PromoteNumericType(leftType, rightType);
                if (!TypeEquals(leftType, resolved) || !TypeEquals(rightType, resolved))
                {
                    throw new InvalidOperationException("Mixed numeric types are not supported");
                }
                return resolved;
            default:
                throw new InvalidOperationException($"Cannot resolve type for expression {expression.GetType().Name}");
        }
    }

    private TypeReference PromoteNumericType(TypeReference left, TypeReference right)
    {
        if (!IsNumeric(left) || !IsNumeric(right))
        {
            throw new InvalidOperationException("Arithmetic requires numeric operands");
        }

        if (TypeEquals(left, right))
        {
            return left;
        }

        if (IsFloat(left) && IsInt(right))
        {
            return left;
        }

        if (IsFloat(right) && IsInt(left))
        {
            return right;
        }

        throw new InvalidOperationException("Unsupported numeric combination");
    }

    private TypeReference ResolveVariableType(string name)
    {
        if (!_locals.TryGetValue(name, out var type))
        {
            if (_implicitNone)
            {
                throw new InvalidOperationException($"Variable '{name}' is not declared");
            }

            type = InferImplicitType(name);
            _locals[name] = type;
            _methodBuilder!.Local(name, type);
        }

        return type;
    }

    private TypeReference InferImplicitType(string name)
    {
        char first = char.ToUpperInvariant(name[0]);
        if (first >= 'I' && first <= 'N')
        {
            return TypeReference.Int32;
        }
        return TypeReference.Float32;
    }

    private TypeReference MapType(FortranTypeSpec type)
    {
        return type.Kind switch
        {
            FortranTypeKind.Integer => TypeReference.Int32,
            FortranTypeKind.Real => TypeReference.Float32,
            FortranTypeKind.Logical => TypeReference.Bool,
            FortranTypeKind.Character => TypeReference.String,
            _ => TypeReference.String
        };
    }

    private void EnsureAssignable(TypeReference target, TypeReference value)
    {
        if (!TypeEquals(target, value))
        {
            throw new InvalidOperationException($"Cannot assign {value.GetQualifiedName()} to {target.GetQualifiedName()}");
        }
    }

    private bool TypeEquals(TypeReference left, TypeReference right)
    {
        return string.Equals(left.Name, right.Name, StringComparison.OrdinalIgnoreCase)
            && string.Equals(left.Namespace ?? string.Empty, right.Namespace ?? string.Empty, StringComparison.Ordinal);
    }

    private bool IsNumeric(TypeReference type) => IsInt(type) || IsFloat(type);

    private bool IsInt(TypeReference type) => TypeEquals(type, TypeReference.Int32);

    private bool IsFloat(TypeReference type) => TypeEquals(type, TypeReference.Float32);

    private MethodReference ResolveWriteLine(TypeReference? argumentType)
    {
        var declaring = TypeReference.FromName("System.Console");
        var parameters = argumentType == null
            ? new List<TypeReference>()
            : new List<TypeReference> { argumentType };
        return new MethodReference(declaring, "WriteLine", TypeReference.Void, parameters);
    }

    private FortranIntrinsic ResolveIntrinsic(string name)
    {
        if (_options.Intrinsics.TryGet(name, out var intrinsic))
        {
            return intrinsic;
        }

        throw new InvalidOperationException($"No intrinsic or binding registered for '{name}'");
    }

    private TypeReference EmitCallExpression(FortranCallExpression call, TypeReference targetType)
    {
        var intrinsic = ResolveIntrinsic(call.Name);
        if (!intrinsic.ReturnsValue)
        {
            throw new InvalidOperationException($"Procedure '{call.Name}' cannot be used as a function");
        }

        var returnType = intrinsic.Method.ReturnType;
        if (!TypeEquals(returnType, targetType))
        {
            throw new InvalidOperationException($"Function '{call.Name}' returns {returnType.GetQualifiedName()}, but context expects {targetType.GetQualifiedName()}");
        }

        EmitIntrinsicArguments(intrinsic, call.Arguments);
        EmitIntrinsicInvocation(intrinsic);
        return returnType;
    }

    private void EmitIntrinsicArguments(FortranIntrinsic intrinsic, IReadOnlyList<FortranExpression> arguments)
    {
        int index = 0;
        if (intrinsic.CallKind == IntrinsicCallKind.Virtual)
        {
            if (arguments.Count == 0)
            {
                throw new InvalidOperationException($"Call to '{intrinsic.Method.Name}' is missing instance argument");
            }

            var instanceType = intrinsic.Method.DeclaringType;
            EmitExpression(arguments[0], instanceType);
            index = 1;
        }

        var expectedParameters = intrinsic.Method.ParameterTypes;
        if (arguments.Count - index != expectedParameters.Count)
        {
            throw new InvalidOperationException($"Call to '{intrinsic.Method.Name}' expects {expectedParameters.Count} argument(s) but received {arguments.Count - index}");
        }

        for (int i = 0; i < expectedParameters.Count; i++)
        {
            var expectedType = expectedParameters[i];
            EmitExpression(arguments[index + i], expectedType);
        }
    }

    private void EmitIntrinsicInvocation(FortranIntrinsic intrinsic)
    {
        if (intrinsic.CallKind == IntrinsicCallKind.Virtual)
        {
            _instructions!.Callvirt(intrinsic.Method);
        }
        else
        {
            _instructions!.Call(intrinsic.Method);
        }
    }
}
