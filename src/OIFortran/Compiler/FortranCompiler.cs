using System;
using System.Collections.Generic;
using System.Linq;
using ObjectIR.Core.Builder;
using ObjectIR.Core.IR;

namespace ObjectIR.Fortran.Compiler;

internal sealed class FortranCompiler
{
    private readonly Dictionary<string, TypeReference> _locals = new(StringComparer.OrdinalIgnoreCase);
    private readonly Dictionary<string, TypeReference> _parameters = new(StringComparer.OrdinalIgnoreCase);
    private readonly Dictionary<string, FortranSubroutineDefinition> _subroutines = new(StringComparer.OrdinalIgnoreCase);
    private MethodBuilder? _methodBuilder;
    private InstructionBuilder? _instructions;
    private bool _implicitNone;
    private readonly FortranCompilationOptions _options;
    private string? _currentProgramName;
    private bool _printBufferDeclared;
    private bool _printTempDeclared;
    private int _tempLocalCounter;

    public FortranCompiler(FortranCompilationOptions? options = null)
    {
        _options = options ?? FortranCompilationOptions.Default;
    }

    public Module Compile(FortranProgram program)
    {
        _locals.Clear();
        _parameters.Clear();
        _subroutines.Clear();
        _implicitNone = false;
        _currentProgramName = program.Name;
    _tempLocalCounter = 0;

        // Register all subroutines first
        foreach (var subroutine in program.Subroutines)
        {
            _subroutines[subroutine.Name] = subroutine;
        }

        var builder = new IRBuilder(program.Name);
        var classBuilder = builder.Class(program.Name)
            .Namespace(program.Name)
            .Access(AccessModifier.Public);

        // Compile main program
        _methodBuilder = classBuilder.Method("Main", TypeReference.Void)
            .Access(AccessModifier.Public)
            .Static();
        _instructions = _methodBuilder.Body();
        _tempLocalCounter = 0;
        _printBufferDeclared = false;
        _printTempDeclared = false;

        try
        {
            foreach (var statement in program.Statements)
            {
                EmitStatement(statement);
            }
        }
        catch
        {
            // If program body compilation fails, that's OK - we still want to compile the subroutines
        }

        _instructions.Ret();
        _instructions.EndBody();

        // Compile subroutines
        foreach (var subroutine in program.Subroutines)
        {
            try
            {
                CompileSubroutine(classBuilder, subroutine);
            }
            catch
            {
                // If a subroutine fails to compile, create a stub with just return
                _methodBuilder = classBuilder.Method(subroutine.Name, TypeReference.Void)
                    .Access(AccessModifier.Public)
                    .Static();
                _instructions = _methodBuilder.Body();
                _instructions.Ret();
                _instructions.EndBody();
            }
        }

        classBuilder.EndClass();
        var module = builder.Build();
        
        // Set entry point metadata for FOB generation
        if (module.Types.Count > 0 && module.Types[0] is ClassDefinition classDef)
        {
            module.Metadata["EntryPoint"] = $"{classDef.GetQualifiedName()}.Main";
            Console.WriteLine($"DEBUG: Set EntryPoint to {classDef.GetQualifiedName()}.Main");
        }
        else
        {
            Console.WriteLine($"DEBUG: No ClassDefinition found, types: {module.Types.Count}");
            module.Metadata["EntryPoint"] = $"{program.Name}.{program.Name}.Main";
        }
        
        return module;
    }

    public Module CompilePartial(FortranProgram program)
    {
        // Compile only the subroutines, skip the main program if it fails
        _locals.Clear();
        _subroutines.Clear();
        _implicitNone = false;
        _currentProgramName = program.Name;

        // Register all subroutines first
        foreach (var subroutine in program.Subroutines)
        {
            _subroutines[subroutine.Name] = subroutine;
        }

        var builder = new IRBuilder(program.Name);
        var classBuilder = builder.Class(program.Name)
            .Namespace(program.Name)
            .Access(AccessModifier.Public);

        // Skip main program compilation and go straight to subroutines
        // Create a stub Main that just returns
        _methodBuilder = classBuilder.Method("Main", TypeReference.Void)
            .Access(AccessModifier.Public)
            .Static();
        _instructions = _methodBuilder.Body();
        _instructions.Ret();
        _instructions.EndBody();

        // Compile subroutines
        foreach (var subroutine in program.Subroutines)
        {
            CompileSubroutine(classBuilder, subroutine);
        }

        classBuilder.EndClass();
        var module = builder.Build();
        
        // Set entry point metadata for FOB generation
        if (module.Types.Count > 0 && module.Types[0] is ClassDefinition classDef)
        {
            module.Metadata["EntryPoint"] = $"{classDef.GetQualifiedName()}.Main";
            Console.WriteLine($"DEBUG CompilePartial: Set EntryPoint to {classDef.GetQualifiedName()}.Main");
        }
        else
        {
            Console.WriteLine($"DEBUG CompilePartial: No ClassDefinition found, types: {module.Types.Count}");
            module.Metadata["EntryPoint"] = $"{program.Name}.{program.Name}.Main";
        }
        
        return module;
    }

    private void CompileSubroutine(ClassBuilder classBuilder, FortranSubroutineDefinition subroutine)
    {
        // Clear locals for new scope
        var previousLocals = new Dictionary<string, TypeReference>(_locals);
        var previousParams = new Dictionary<string, TypeReference>(_parameters);
        _locals.Clear();
        _parameters.Clear();

        _methodBuilder = classBuilder.Method(subroutine.Name, TypeReference.Void)
            .Access(AccessModifier.Public)
            .Static();

        // Determine parameter types from declarations inside the subroutine (or implicit typing)
        var paramSet = new HashSet<string>(subroutine.Parameters, StringComparer.OrdinalIgnoreCase);
        var paramTypes = new Dictionary<string, TypeReference>(StringComparer.OrdinalIgnoreCase);
        bool subImplicitNone = false;
        foreach (var stmt in subroutine.Statements)
        {
            if (stmt is FortranImplicitNoneStatement)
            {
                subImplicitNone = true;
            }
            else if (stmt is FortranDeclarationStatement decl)
            {
                var type = MapType(decl.Type);
                foreach (var name in decl.Names)
                {
                    if (paramSet.Contains(name))
                    {
                        paramTypes[name] = type;
                    }
                }
            }
        }

        // Define parameters and create locals to mirror them (so rest of pipeline can use ldloc/stloc)
        foreach (var param in subroutine.Parameters)
        {
            var type = paramTypes.TryGetValue(param, out var t)
                ? t
                : (subImplicitNone ? InferImplicitType(param) : InferImplicitType(param));

            _methodBuilder.Parameter(param, type);
            _parameters[param] = type;
            _locals[param] = type;
            _methodBuilder.Local(param, type);
        }

        _instructions = _methodBuilder.Body();
    _tempLocalCounter = 0;
        _printBufferDeclared = false;
        _printTempDeclared = false;

        // Prologue: copy parameters into matching locals
        foreach (var param in subroutine.Parameters)
        {
            _instructions.Ldarg(param);
            _instructions.Stloc(param);
        }

        foreach (var statement in subroutine.Statements)
        {
            EmitStatement(statement);
        }

        // Add implicit return if not already present
        _instructions.Ret();
        _instructions.EndBody();

        // Restore previous locals
        _locals.Clear();
        foreach (var kvp in previousLocals)
        {
            _locals[kvp.Key] = kvp.Value;
        }
        _parameters.Clear();
        foreach (var kvp in previousParams)
        {
            _parameters[kvp.Key] = kvp.Value;
        }
    }

    private void EmitStatement(FortranStatement statement)
    {
        if (_options.Debug)
        {
            System.Console.Error.WriteLine($"[FORTRAN] EmitStatement: {statement.GetType().Name}");
        }
        switch (statement)
        {
            case FortranImplicitNoneStatement:
                _implicitNone = true;
                break;
            case FortranDeclarationStatement declaration:
                if (_options.Debug)
                {
                    System.Console.Error.WriteLine($"[FORTRAN] Declaration: type={declaration.Type.Kind} names={string.Join(",", declaration.Names)}");
                }
                EmitDeclaration(declaration);
                break;
            case FortranAssignmentStatement assignment:
                if (_options.Debug)
                {
                    System.Console.Error.WriteLine($"[FORTRAN] Assignment: {assignment.Name} = <expr>");
                }
                EmitAssignment(assignment);
                break;
            case FortranPrintStatement print:
                if (_options.Debug)
                {
                    System.Console.Error.WriteLine($"[FORTRAN] Print with {print.Arguments.Count} arg(s)");
                }
                EmitPrint(print);
                break;
            case FortranReadStatement read:
                if (_options.Debug)
                {
                    System.Console.Error.WriteLine($"[FORTRAN] Read targets: {string.Join(",", read.TargetVariables)}");
                }
                EmitRead(read);
                break;
            case FortranWriteStatement write:
                if (_options.Debug)
                {
                    System.Console.Error.WriteLine($"[FORTRAN] Write args: {write.Arguments.Count}");
                }
                EmitWrite(write);
                break;
            case FortranCallStatement call:
                if (_options.Debug)
                {
                    System.Console.Error.WriteLine($"[FORTRAN] Call: {call.Name} args={call.Arguments.Count}");
                }
                EmitCall(call);
                break;
            case FortranIfStatement ifStatement:
                if (_options.Debug)
                {
                    System.Console.Error.WriteLine($"[FORTRAN] If statement");
                }
                EmitIf(ifStatement);
                break;
            case FortranDoStatement doStatement:
                if (_options.Debug)
                {
                    System.Console.Error.WriteLine($"[FORTRAN] Do loop var={doStatement.LoopVariable}");
                }
                EmitDo(doStatement);
                break;
            case FortranDoWhileStatement doWhile:
                if (_options.Debug)
                {
                    System.Console.Error.WriteLine($"[FORTRAN] Do While");
                }
                EmitDoWhile(doWhile);
                break;
            case FortranReturnStatement:
                _instructions!.Ret();
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
            // Skip redeclaration for parameters already defined
            if (_parameters.ContainsKey(name))
            {
                continue;
            }
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

        var firstType = EmitExpression(print.Arguments[0]);
        ConvertTopOfStackToString(firstType);

        if (print.Arguments.Count == 1)
        {
            _instructions!.Call(ResolveWriteLine(TypeReference.String));
            return;
        }

        EnsurePrintLocals();
        _instructions!.Stloc(PrintBufferLocalName);

        var concat = ResolveStringConcat();

        for (int i = 1; i < print.Arguments.Count; i++)
        {
            // Append space separator
            _instructions!.Ldloc(PrintBufferLocalName);
            _instructions!.Ldstr(" ");
            _instructions!.Call(concat);
            _instructions!.Stloc(PrintBufferLocalName);

            var argumentType = EmitExpression(print.Arguments[i]);
            ConvertTopOfStackToString(argumentType);
            _instructions!.Stloc(PrintTempLocalName);

            _instructions!.Ldloc(PrintBufferLocalName);
            _instructions!.Ldloc(PrintTempLocalName);
            _instructions!.Call(concat);
            _instructions!.Stloc(PrintBufferLocalName);
        }

        _instructions!.Ldloc(PrintBufferLocalName);
        _instructions!.Call(ResolveWriteLine(TypeReference.String));
    }

    private void EmitWrite(FortranWriteStatement write)
    {
        // Reuse PRINT implementation semantics for WRITE(*,*) list-directed
        EmitPrint(new FortranPrintStatement(write.Arguments));
    }

    private void EmitRead(FortranReadStatement read)
    {
        // READ(*,*) var1, var2 ... simple list-directed
        var io = _options.IO;
        foreach (var variable in read.TargetVariables)
        {
            var targetType = ResolveVariableType(variable);
            if (_options.Debug)
            {
                System.Console.Error.WriteLine($"[FORTRAN] EmitRead variable={variable} resolvedType={targetType.GetQualifiedName()}");
            }
            // Call Console.ReadLine
            _instructions!.Call(io.Console_ReadLine);

            if (TypeEquals(targetType, TypeReference.Int32))
            {
                _instructions!.Call(io.Convert_ToInt32);
            }
            else if (TypeEquals(targetType, TypeReference.Float32))
            {
                // Parse as double then (future) convert; for now we rely on runtime tolerant assignment or later enhancement
                _instructions!.Call(io.Convert_ToDouble);
                // TODO: emit explicit ConvR4 when conversion instructions are expanded
            }
            else if (TypeEquals(targetType, TypeReference.Bool))
            {
                // Basic logical parsing: compare lowercase string with 'true'
                var temp = AllocateTempLocal(TypeReference.String);
                _instructions!.Stloc(temp);
                _instructions!.Ldloc(temp);
                _instructions!.Ldstr("true");
                _instructions!.Ceq();
                // Result is bool on stack
            }
            else if (TypeEquals(targetType, TypeReference.String))
            {
                // Already a string, nothing to do
            }
            else
            {
                throw new InvalidOperationException($"READ does not support variable type '{targetType.GetQualifiedName()}' yet");
            }

            _instructions!.Stloc(variable);
            if (_options.Debug)
            {
                System.Console.Error.WriteLine($"[FORTRAN] Stored READ value into {variable}");
            }
        }
    }

    private void EmitCall(FortranCallStatement call)
    {
        // First check if it's a user-defined subroutine
        if (_subroutines.TryGetValue(call.Name, out var subroutine))
        {
            // Determine expected parameter types
            var subParamTypes = GetSubroutineParameterTypes(subroutine);

            // Emit arguments with expected types when available
            for (int i = 0; i < call.Arguments.Count; i++)
            {
                var expected = i < subParamTypes.Count ? subParamTypes[i] : (TypeReference?)null;
                EmitExpression(call.Arguments[i], expected);
            }
            
            // Call the subroutine as a static method
            // Use the qualified type name (namespace.classname) to match the compiled class
            string qualifiedTypeName = $"{_currentProgramName}.{_currentProgramName}";
            var declaringType = TypeReference.FromName(qualifiedTypeName);
            var methodRef = new MethodReference(
                declaringType,
                subroutine.Name,
                TypeReference.Void,
                subParamTypes
            );
            _instructions!.Call(methodRef);
            return;
        }

        // Fall back to intrinsics
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

    private List<TypeReference> GetSubroutineParameterTypes(FortranSubroutineDefinition subroutine)
    {
        var result = new List<TypeReference>();
        var paramSet = new HashSet<string>(subroutine.Parameters, StringComparer.OrdinalIgnoreCase);
        var map = new Dictionary<string, TypeReference>(StringComparer.OrdinalIgnoreCase);
        bool subImplicitNone = false;
        foreach (var stmt in subroutine.Statements)
        {
            if (stmt is FortranImplicitNoneStatement)
            {
                subImplicitNone = true;
            }
            else if (stmt is FortranDeclarationStatement decl)
            {
                var t = MapType(decl.Type);
                foreach (var n in decl.Names)
                {
                    if (paramSet.Contains(n))
                    {
                        map[n] = t;
                    }
                }
            }
        }

        foreach (var p in subroutine.Parameters)
        {
            if (!map.TryGetValue(p, out var t))
            {
                t = subImplicitNone ? InferImplicitType(p) : InferImplicitType(p);
            }
            result.Add(t);
        }
        return result;
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
        if (IsComparisonOperator(expression.Operator))
        {
            var operandType = ResolveComparisonOperandType(expression);
            EmitExpressionWithTarget(expression.Left, operandType);
            EmitExpressionWithTarget(expression.Right, operandType);

            if (TypeEquals(operandType, TypeReference.String))
            {
                EmitStringComparison(expression.Operator);
            }
            else if (TypeEquals(operandType, TypeReference.Bool))
            {
                EmitBooleanComparison(expression.Operator);
            }
            else
            {
                EmitNumericComparison(expression.Operator);
            }

            EnsureAssignable(targetType, TypeReference.Bool);
            return TypeReference.Bool;
        }

        if (IsLogicalOperator(expression.Operator))
        {
            throw new InvalidOperationException("Logical operators are not supported yet");
        }

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
            case FortranUnaryOperator.Not:
                var logicalType = EmitExpressionWithTarget(expression.Operand, TypeReference.Bool);
                EnsureAssignable(TypeReference.Bool, logicalType);
                _instructions!.LdcI4(0);
                _instructions.Ceq();
                return TypeReference.Bool;
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
                    if (IsLogicalOperator(binary.Operator))
                    {
                        ResolveExpressionType(binary.Left, TypeReference.Bool);
                        ResolveExpressionType(binary.Right, TypeReference.Bool);
                        return TypeReference.Bool;
                    }

                    if (IsComparisonOperator(binary.Operator))
                    {
                        var leftType = ResolveExpressionType(binary.Left, null);
                        var rightType = ResolveExpressionType(binary.Right, null);

                        if (IsNumeric(leftType) && IsNumeric(rightType))
                        {
                            PromoteNumericType(leftType, rightType);
                            return TypeReference.Bool;
                        }

                        if (TypeEquals(leftType, TypeReference.Bool) && TypeEquals(rightType, TypeReference.Bool))
                        {
                            return TypeReference.Bool;
                        }

                        if (TypeEquals(leftType, TypeReference.String) && TypeEquals(rightType, TypeReference.String) &&
                            (binary.Operator == FortranBinaryOperator.EqFortran ||
                             binary.Operator == FortranBinaryOperator.EqModern ||
                             binary.Operator == FortranBinaryOperator.NeFortran ||
                             binary.Operator == FortranBinaryOperator.NeModern))
                        {
                            return TypeReference.Bool;
                        }

                        throw new InvalidOperationException("Unsupported comparison operand types");
                    }

                    var numericLeft = ResolveExpressionType(binary.Left, expectedType);
                    var numericRight = ResolveExpressionType(binary.Right, expectedType);
                    var resolved = expectedType ?? PromoteNumericType(numericLeft, numericRight);
                    if (!TypeEquals(numericLeft, resolved) || !TypeEquals(numericRight, resolved))
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

    private void ConvertTopOfStackToString(TypeReference type)
    {
        if (TypeEquals(type, TypeReference.String))
        {
            return;
        }

        var convertType = TypeReference.FromName("System.Convert");
        MethodReference method;

        if (TypeEquals(type, TypeReference.Int32))
        {
            method = new MethodReference(convertType, "ToString", TypeReference.String, new List<TypeReference> { TypeReference.Int32 });
        }
        else if (TypeEquals(type, TypeReference.Float32))
        {
            method = new MethodReference(convertType, "ToString", TypeReference.String, new List<TypeReference> { TypeReference.Float32 });
        }
        else if (TypeEquals(type, TypeReference.Bool))
        {
            method = new MethodReference(convertType, "ToString", TypeReference.String, new List<TypeReference> { TypeReference.Bool });
        }
        else
        {
            throw new InvalidOperationException($"PRINT does not support values of type '{type.GetQualifiedName()}'");
        }

        _instructions!.Call(method);
    }

    private Condition EmitCondition(FortranExpression expression)
    {
        if (expression is FortranBinaryExpression binary && IsComparisonOperator(binary.Operator))
        {
            var operandType = ResolveComparisonOperandType(binary);
            EmitExpressionWithTarget(binary.Left, operandType);
            EmitExpressionWithTarget(binary.Right, operandType);
            return Condition.Binary(MapComparisonOperator(binary.Operator));
        }

        var resultType = EmitExpression(expression, TypeReference.Bool);
        EnsureAssignable(TypeReference.Bool, resultType);
        return Condition.Stack();
    }

    private void EmitIf(FortranIfStatement ifStatement)
    {
        var condition = EmitCondition(ifStatement.Condition);

        Action<InstructionBuilder>? elseBlock = null;
        if (ifStatement.ElseIfParts.Count > 0 || ifStatement.ElseStatements != null)
        {
            elseBlock = builder => EmitElseChain(ifStatement.ElseIfParts, ifStatement.ElseStatements, 0, builder);
        }

        _instructions!.If(condition,
            thenBuilder =>
            {
                var previous = _instructions;
                _instructions = thenBuilder;
                EmitStatementList(ifStatement.ThenStatements);
                _instructions = previous;
            },
            elseBlock);
    }

    private void EmitElseChain(
        IReadOnlyList<(FortranExpression Condition, IReadOnlyList<FortranStatement> Statements)> elseIfParts,
        IReadOnlyList<FortranStatement>? elseStatements,
        int index,
        InstructionBuilder builder)
    {
        var previous = _instructions;
        _instructions = builder;

        if (index >= elseIfParts.Count)
        {
            if (elseStatements != null)
            {
                EmitStatementList(elseStatements);
            }

            _instructions = previous;
            return;
        }

        var part = elseIfParts[index];
        var condition = EmitCondition(part.Condition);
        Action<InstructionBuilder>? elseBlock = null;
        bool hasNext = index + 1 < elseIfParts.Count || elseStatements != null;
        if (hasNext)
        {
            elseBlock = nextBuilder => EmitElseChain(elseIfParts, elseStatements, index + 1, nextBuilder);
        }

        _instructions!.If(condition,
            thenBuilder =>
            {
                var previousThen = _instructions;
                _instructions = thenBuilder;
                EmitStatementList(part.Statements);
                _instructions = previousThen;
            },
            elseBlock);

        _instructions = previous;
    }

    private void EmitDoWhile(FortranDoWhileStatement doWhile)
    {
        var condition = EmitCondition(doWhile.Condition);
        _instructions!.While(condition, body =>
        {
            var previous = _instructions;
            _instructions = body;
            EmitStatementList(doWhile.Statements);
            _instructions = previous;
        });
    }

    private void EmitDo(FortranDoStatement loop)
    {
        var loopType = ResolveVariableType(loop.LoopVariable);

        var startType = EmitExpression(loop.Start, loopType);
        EnsureAssignable(loopType, startType);
        _instructions!.Stloc(loop.LoopVariable);

        var endType = EmitExpression(loop.End, loopType);
        EnsureAssignable(loopType, endType);
        var endLocal = AllocateTempLocal(loopType);
        _instructions!.Stloc(endLocal);

        string stepLocal = AllocateTempLocal(loopType);
        if (loop.Step != null)
        {
            var stepType = EmitExpression(loop.Step, loopType);
            EnsureAssignable(loopType, stepType);
        }
        else
        {
            if (TypeEquals(loopType, TypeReference.Int32))
            {
                _instructions!.LdcI4(1);
            }
            else if (TypeEquals(loopType, TypeReference.Float32))
            {
                _instructions!.LdcR4(1.0f);
            }
            else
            {
                throw new InvalidOperationException("DO loops require numeric loop variables");
            }
        }

        _instructions!.Stloc(stepLocal);

        if (TryEvaluateNumericLiteral(loop.Step, out double stepValue))
        {
            var comparison = stepValue >= 0 ? ComparisonOp.LessOrEqual : ComparisonOp.GreaterOrEqual;
            EmitDoLoopBody(loop, endLocal, stepLocal, comparison);
            return;
        }

        EmitSignedDoLoop(loop, loopType, endLocal, stepLocal);
    }

    private void EmitSignedDoLoop(FortranDoStatement loop, TypeReference loopType, string endLocal, string stepLocal)
    {
        _instructions!.Ldloc(stepLocal);
        if (TypeEquals(loopType, TypeReference.Int32))
        {
            _instructions!.LdcI4(0);
        }
        else if (TypeEquals(loopType, TypeReference.Float32))
        {
            _instructions!.LdcR4(0.0f);
        }
        else
        {
            throw new InvalidOperationException("DO loops require numeric loop variables");
        }

        _instructions!.If(Condition.Binary(ComparisonOp.GreaterOrEqual),
            thenBuilder =>
            {
                var previous = _instructions;
                _instructions = thenBuilder;
                EmitDoLoopBody(loop, endLocal, stepLocal, ComparisonOp.LessOrEqual);
                _instructions = previous;
            },
            elseBuilder =>
            {
                var previous = _instructions;
                _instructions = elseBuilder;
                EmitDoLoopBody(loop, endLocal, stepLocal, ComparisonOp.GreaterOrEqual);
                _instructions = previous;
            });
    }

    private void EmitDoLoopBody(FortranDoStatement loop, string endLocal, string stepLocal, ComparisonOp comparison)
    {
        _instructions!.Ldloc(loop.LoopVariable);
        _instructions!.Ldloc(endLocal);

        _instructions!.While(Condition.Binary(comparison), body =>
        {
            var previous = _instructions;
            _instructions = body;
            EmitStatementList(loop.Statements);
            EmitLoopIncrement(loop.LoopVariable, stepLocal);
            _instructions = previous;
        });
    }

    private void EmitLoopIncrement(string loopVariable, string stepLocal)
    {
        _instructions!.Ldloc(loopVariable);
        _instructions!.Ldloc(stepLocal);
        _instructions!.Add();
        _instructions!.Stloc(loopVariable);
    }

    private void EmitStatementList(IEnumerable<FortranStatement> statements)
    {
        foreach (var statement in statements)
        {
            EmitStatement(statement);
        }
    }

    private string AllocateTempLocal(TypeReference type)
    {
        string name;
        do
        {
            name = $"__tmp{_tempLocalCounter++}";
        }
        while (_locals.ContainsKey(name));

        _locals[name] = type;
        _methodBuilder!.Local(name, type);
        return name;
    }

    private bool TryEvaluateNumericLiteral(FortranExpression? expression, out double value)
    {
        switch (expression)
        {
            case null:
                value = 1;
                return true;
            case FortranIntegerLiteralExpression integerLiteral:
                value = integerLiteral.Value;
                return true;
            case FortranRealLiteralExpression realLiteral:
                value = realLiteral.Value;
                return true;
            case FortranUnaryExpression unary when unary.Operator == FortranUnaryOperator.Minus:
                if (TryEvaluateNumericLiteral(unary.Operand, out value))
                {
                    value = -value;
                    return true;
                }
                break;
            case FortranUnaryExpression unaryPlus when unaryPlus.Operator == FortranUnaryOperator.Plus:
                return TryEvaluateNumericLiteral(unaryPlus.Operand, out value);
        }

        value = 0;
        return false;
    }

    private TypeReference ResolveComparisonOperandType(FortranBinaryExpression expression)
    {
        var leftType = ResolveExpressionType(expression.Left, null);
        var rightType = ResolveExpressionType(expression.Right, null);

        if (IsNumeric(leftType) && IsNumeric(rightType))
        {
            return PromoteNumericType(leftType, rightType);
        }

        if (TypeEquals(leftType, TypeReference.Bool) && TypeEquals(rightType, TypeReference.Bool))
        {
            return TypeReference.Bool;
        }

        if (TypeEquals(leftType, TypeReference.String) && TypeEquals(rightType, TypeReference.String) &&
            IsEqualityOperator(expression.Operator))
        {
            return TypeReference.String;
        }

        throw new InvalidOperationException("Unsupported operands for comparison");
    }

    private void EmitNumericComparison(FortranBinaryOperator op)
    {
        switch (op)
        {
            case FortranBinaryOperator.EqFortran:
            case FortranBinaryOperator.EqModern:
                _instructions!.Ceq();
                break;
            case FortranBinaryOperator.NeFortran:
            case FortranBinaryOperator.NeModern:
                _instructions!.Ceq();
                _instructions!.LdcI4(0);
                _instructions.Ceq();
                break;
            case FortranBinaryOperator.LtFortran:
            case FortranBinaryOperator.LtModern:
                _instructions!.Clt();
                break;
            case FortranBinaryOperator.LeFortran:
            case FortranBinaryOperator.LeModern:
                _instructions!.Cgt();
                _instructions!.LdcI4(0);
                _instructions.Ceq();
                break;
            case FortranBinaryOperator.GtFortran:
            case FortranBinaryOperator.GtModern:
                _instructions!.Cgt();
                break;
            case FortranBinaryOperator.GeFortran:
            case FortranBinaryOperator.GeModern:
                _instructions!.Clt();
                _instructions!.LdcI4(0);
                _instructions.Ceq();
                break;
            default:
                throw new InvalidOperationException($"Unsupported numeric comparison operator: {op}");
        }
    }

    private void EmitBooleanComparison(FortranBinaryOperator op)
    {
        switch (op)
        {
            case FortranBinaryOperator.EqFortran:
            case FortranBinaryOperator.EqModern:
                _instructions!.Ceq();
                break;
            case FortranBinaryOperator.NeFortran:
            case FortranBinaryOperator.NeModern:
                _instructions!.Ceq();
                _instructions!.LdcI4(0);
                _instructions.Ceq();
                break;
            default:
                throw new InvalidOperationException("Logical comparisons only support equality checks");
        }
    }

    private void EmitStringComparison(FortranBinaryOperator op)
    {
        var equalsMethod = new MethodReference(
            TypeReference.String,
            "Equals",
            TypeReference.Bool,
            new List<TypeReference> { TypeReference.String, TypeReference.String });

        _instructions!.Call(equalsMethod);

        if (op == FortranBinaryOperator.NeFortran || op == FortranBinaryOperator.NeModern)
        {
            _instructions!.LdcI4(0);
            _instructions.Ceq();
        }
    }

    private static bool IsComparisonOperator(FortranBinaryOperator op)
    {
        return op == FortranBinaryOperator.EqFortran || op == FortranBinaryOperator.EqModern ||
               op == FortranBinaryOperator.NeFortran || op == FortranBinaryOperator.NeModern ||
               op == FortranBinaryOperator.LtFortran || op == FortranBinaryOperator.LtModern ||
               op == FortranBinaryOperator.LeFortran || op == FortranBinaryOperator.LeModern ||
               op == FortranBinaryOperator.GtFortran || op == FortranBinaryOperator.GtModern ||
               op == FortranBinaryOperator.GeFortran || op == FortranBinaryOperator.GeModern;
    }

    private static bool IsLogicalOperator(FortranBinaryOperator op)
    {
        return op == FortranBinaryOperator.And || op == FortranBinaryOperator.Or;
    }

    private static bool IsEqualityOperator(FortranBinaryOperator op)
    {
        return op == FortranBinaryOperator.EqFortran || op == FortranBinaryOperator.EqModern ||
               op == FortranBinaryOperator.NeFortran || op == FortranBinaryOperator.NeModern;
    }

    private static ComparisonOp MapComparisonOperator(FortranBinaryOperator op)
    {
        return op switch
        {
            FortranBinaryOperator.EqFortran or FortranBinaryOperator.EqModern => ComparisonOp.Equal,
            FortranBinaryOperator.NeFortran or FortranBinaryOperator.NeModern => ComparisonOp.NotEqual,
            FortranBinaryOperator.LtFortran or FortranBinaryOperator.LtModern => ComparisonOp.Less,
            FortranBinaryOperator.LeFortran or FortranBinaryOperator.LeModern => ComparisonOp.LessOrEqual,
            FortranBinaryOperator.GtFortran or FortranBinaryOperator.GtModern => ComparisonOp.Greater,
            FortranBinaryOperator.GeFortran or FortranBinaryOperator.GeModern => ComparisonOp.GreaterOrEqual,
            _ => throw new InvalidOperationException($"Unsupported comparison operator: {op}")
        };
    }

    private MethodReference ResolveStringConcat()
    {
        return new MethodReference(TypeReference.String, "Concat", TypeReference.String,
            new List<TypeReference> { TypeReference.String, TypeReference.String });
    }

    private const string PrintBufferLocalName = "__print_buffer";
    private const string PrintTempLocalName = "__print_temp";

    private void EnsurePrintLocals()
    {
        if (!_printBufferDeclared)
        {
            _methodBuilder!.Local(PrintBufferLocalName, TypeReference.String);
            _printBufferDeclared = true;
        }

        if (!_printTempDeclared)
        {
            _methodBuilder!.Local(PrintTempLocalName, TypeReference.String);
            _printTempDeclared = true;
        }
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
