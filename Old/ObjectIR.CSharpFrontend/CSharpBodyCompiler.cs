using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using ObjectIR.Core.IR;

namespace ObjectIR.CSharpFrontend
{

/// <summary>
/// Compiles C# method bodies into ObjectIR instructions.
/// Converts statements and expressions from Roslyn AST to IR stack-based operations.
/// </summary>
public class CSharpBodyCompiler
{
    private readonly SemanticModel _semanticModel;
    private readonly InstructionList _instructions = new();
    private readonly Stack<string> _expressionStack = new();
    private readonly Dictionary<string, LocalVariable> _localVariables = new();
    private MethodDefinition? _currentMethod;
    private int _tempVarCounter = 0;

    public CSharpBodyCompiler(SemanticModel semanticModel)
    {
        _semanticModel = semanticModel ?? throw new ArgumentNullException(nameof(semanticModel));
    }

    /// <summary>
    /// Compiles a method body into IR instructions.
    /// </summary>
    public void CompileMethodBody(BlockSyntax? body, MethodDefinition method)
    {
        if (body == null)
            return;

        _instructions.Clear();
        _localVariables.Clear();
        _tempVarCounter = 0;
        _currentMethod = method;

        // Pre-populate locals from method definition
        foreach (var local in method.Locals)
        {
            _localVariables[local.Name] = local;
        }

        try
        {
            var bodyToCompile = body;

            // Ensure the body node belongs to the same syntax tree as the semantic model.
            if (!ReferenceEquals(body.SyntaxTree, _semanticModel.SyntaxTree))
            {
                var targetTree = _semanticModel.SyntaxTree;
                var targetRoot = targetTree?.GetRoot();

                if (targetRoot != null)
                {
                    var mappedNode = targetRoot.FindNode(body.Span, getInnermostNodeForTie: true) as BlockSyntax;
                    if (mappedNode != null)
                    {
                        bodyToCompile = mappedNode;
                    }
                    else
                    {
                        System.Console.WriteLine($"[WARNING] Unable to map method body for '{method.Name}' to semantic tree - skipping body compilation");
                        return;
                    }
                }
            }

            CompileStatements(bodyToCompile.Statements);
            
            // Add implicit return void if not present
            if (method.ReturnType == TypeReference.Void && 
                (_instructions.Count == 0 || !(_instructions.Last() is ReturnInstruction)))
            {
                _instructions.Add(new ReturnInstruction(null));
            }

            // Transfer compiled instructions to method
            method.Instructions.Clear();
            foreach (var instruction in _instructions)
            {
                method.Instructions.Add(instruction);
            }
        }
        catch (Exception ex)
        {
            System.Console.WriteLine($"[WARNING] Failed to compile method body for '{method.Name}': {ex.Message}");
            // Fall back to empty implementation
            method.Instructions.Clear();
        }
        finally
        {
            _currentMethod = null;
        }
    }

    private void CompileStatements(SyntaxList<StatementSyntax> statements)
    {
        foreach (var statement in statements)
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
                break;

            case LocalDeclarationStatementSyntax varDecl:
                CompileLocalDeclaration(varDecl);
                break;

            case IfStatementSyntax ifStmt:
                CompileIfStatement(ifStmt);
                break;

            case WhileStatementSyntax whileStmt:
                CompileWhileStatement(whileStmt);
                break;

            case ForStatementSyntax forStmt:
                CompileForStatement(forStmt);
                break;

            case ReturnStatementSyntax returnStmt:
                CompileReturnStatement(returnStmt);
                break;

            case BlockSyntax block:
                CompileStatements(block.Statements);
                break;

            case TryStatementSyntax tryStmt:
                CompileTryStatement(tryStmt);
                break;

            case ThrowStatementSyntax throwStmt:
                CompileThrowStatement(throwStmt);
                break;

            default:
                // Unsupported statement type - skip
                System.Console.WriteLine($"[DEBUG] Unsupported statement: {statement.GetType().Name}");
                break;
        }
    }

    private void CompileLocalDeclaration(LocalDeclarationStatementSyntax varDecl)
    {
        CompileVariableDeclaration(varDecl.Declaration);
    }

    private void CompileVariableDeclaration(VariableDeclarationSyntax declaration)
    {
        var mappedDeclaration = MapNodeToSemanticTree(declaration) ?? declaration;
        var typeSymbol = TryGetTypeSymbol(mappedDeclaration.Type);
        var varType = ConvertTypeOrFallback(typeSymbol, mappedDeclaration.Type);

        foreach (var variable in mappedDeclaration.Variables)
        {
            var varName = variable.Identifier.Text;

            GetOrCreateLocal(varName, varType);

            if (variable.Initializer != null)
            {
                CompileExpression(variable.Initializer.Value);
                _instructions.Add(new StoreLocalInstruction(varName));
            }
        }
    }

    private LocalVariable GetOrCreateLocal(string name, TypeReference type)
    {
        if (_localVariables.TryGetValue(name, out var existing))
        {
            return existing;
        }

        LocalVariable local;
        if (_currentMethod != null)
        {
            local = _currentMethod.Locals.FirstOrDefault(l => l.Name == name) ??
                _currentMethod.DefineLocal(name, type);
        }
        else
        {
            local = new LocalVariable(name, type);
        }

        _localVariables[name] = local;
        return local;
    }

    private T? MapNodeToSemanticTree<T>(T? node) where T : SyntaxNode
    {
        if (node == null)
            return null;

        var semanticTree = _semanticModel.SyntaxTree;
        if (semanticTree == null || ReferenceEquals(node.SyntaxTree, semanticTree))
            return node;

        if (node.SyntaxTree == null)
            return null;

        var targetRoot = semanticTree.GetRoot();
        return targetRoot.FindNode(node.Span, getInnermostNodeForTie: true) as T;
    }

    private ITypeSymbol? TryGetTypeSymbol(TypeSyntax? typeSyntax)
    {
        if (typeSyntax == null)
            return null;

        var mappedType = MapNodeToSemanticTree(typeSyntax);
        if (mappedType == null)
            return null;

        return _semanticModel.GetTypeInfo(mappedType).Type;
    }

    private void CompileIfStatement(IfStatementSyntax ifStmt)
    {
        var condition = CompileConditionExpression(ifStmt.Condition);
        var ifInstr = new IfInstruction(condition);

        // Save current instructions
        var savedInstructions = _instructions.ToList();
        
        // Compile then block
        _instructions.Clear();
        CompileStatement(ifStmt.Statement);
        var thenBlock = _instructions.ToList();
        
        // Compile else block if present
        InstructionList? elseBlock = null;
        if (ifStmt.Else != null)
        {
            _instructions.Clear();
            CompileStatement(ifStmt.Else.Statement);
            elseBlock = new InstructionList();
            foreach (var instr in _instructions)
            {
                elseBlock.Add(instr);
            }
        }

        // Restore saved instructions and add if  
        _instructions.Clear();
        foreach (var instr in savedInstructions)
        {
            _instructions.Add(instr);
        }
        
        // Add compiled blocks to if instruction
        foreach (var instr in thenBlock)
        {
            ifInstr.ThenBlock.Add(instr);
        }
        if (elseBlock != null)
        {
            ifInstr.ElseBlock = elseBlock;
        }
        
        _instructions.Add(ifInstr);
    }

    private void CompileWhileStatement(WhileStatementSyntax whileStmt)
    {
        var condition = CompileConditionExpression(whileStmt.Condition);
        var whileInstr = new WhileInstruction(condition);

        // Save current instructions
        var savedInstructions = _instructions.ToList();
        
        // Compile body
        _instructions.Clear();
        CompileStatement(whileStmt.Statement);
        var bodyInstructions = _instructions.ToList();
        
        // Restore saved instructions
        _instructions.Clear();
        foreach (var instr in savedInstructions)
        {
            _instructions.Add(instr);
        }
        
        // Add compiled body to while instruction
        foreach (var instr in bodyInstructions)
        {
            whileInstr.Body.Add(instr);
        }
        
        _instructions.Add(whileInstr);
    }

    private void CompileForStatement(ForStatementSyntax forStmt)
    {
        // Initializers (declaration or explicit expressions)
        if (forStmt.Declaration != null)
        {
            CompileVariableDeclaration(forStmt.Declaration);
        }

        foreach (var initializer in forStmt.Initializers)
        {
            CompileExpression(initializer);
        }

        var condition = CompileLoopCondition(forStmt.Condition);
        var loopInstr = new WhileInstruction(condition);

        var savedInstructions = _instructions.ToList();

        _instructions.Clear();
        CompileStatement(forStmt.Statement);

        foreach (var incrementor in forStmt.Incrementors)
        {
            CompileExpression(incrementor);
        }

        var bodyInstructions = _instructions.ToList();

        _instructions.Clear();
        foreach (var instr in savedInstructions)
        {
            _instructions.Add(instr);
        }

        foreach (var instr in bodyInstructions)
        {
            loopInstr.Body.Add(instr);
        }

        _instructions.Add(loopInstr);
    }

    private void CompileReturnStatement(ReturnStatementSyntax returnStmt)
    {
        if (returnStmt.Expression != null)
        {
            CompileExpression(returnStmt.Expression);
        }
        _instructions.Add(new ReturnInstruction(null));
    }

    private void CompileTryStatement(TryStatementSyntax tryStmt)
    {
        var tryInstr = new TryInstruction();

        // Compile try block
        _instructions.Clear();
        CompileStatements(tryStmt.Block.Statements);
        foreach (var instr in _instructions)
        {
            tryInstr.TryBlock.Add(instr);
        }

        // Compile catch blocks
        foreach (var catchClause in tryStmt.Catches)
        {
            var exceptionType = _semanticModel.GetTypeInfo(catchClause.Declaration?.Type ?? 
                SyntaxFactory.IdentifierName("Exception")).Type;
            var exceptionVar = catchClause.Declaration?.Identifier.Text ?? "ex";
            
            var catchInstr = new CatchClause(ConvertType(exceptionType), exceptionVar);
            
            _instructions.Clear();
            CompileStatements(catchClause.Block.Statements);
            foreach (var instr in _instructions)
            {
                catchInstr.Body.Add(instr);
            }

            tryInstr.CatchClauses.Add(catchInstr);
        }

        // Compile finally block
        if (tryStmt.Finally != null)
        {
            var savedFinallyInstructions = _instructions.ToList();
            _instructions.Clear();
            CompileStatements(tryStmt.Finally.Block.Statements);
            var finallyInstructions = new InstructionList();
            foreach (var instr in _instructions)
            {
                finallyInstructions.Add(instr);
            }
            tryInstr.FinallyBlock = finallyInstructions;
            _instructions.Clear();
            foreach (var instr in savedFinallyInstructions)
            {
                _instructions.Add(instr);
            }
        }

        // Add try instruction
        _instructions.Clear();
        _instructions.Add(tryInstr);
    }

    private void CompileThrowStatement(ThrowStatementSyntax throwStmt)
    {
        if (throwStmt.Expression != null)
        {
            CompileExpression(throwStmt.Expression);
        }
        _instructions.Add(new ThrowInstruction());
    }

    private Condition CompileLoopCondition(ExpressionSyntax? condition)
    {
        if (condition == null)
        {
            _instructions.Add(new LoadConstantInstruction(true, TypeReference.Bool));
            return Condition.Stack();
        }

        return CompileConditionExpression(condition);
    }

    private Condition CompileConditionExpression(ExpressionSyntax expression)
    {
        // For now, use a simple stack-based condition
        // A more sophisticated implementation would analyze the condition
        CompileExpression(expression);
        return Condition.Stack();
    }

    private void CompileExpression(ExpressionSyntax expression)
    {
        switch (expression)
        {
            case BinaryExpressionSyntax binExpr:
                CompileBinaryExpression(binExpr);
                break;

            case AssignmentExpressionSyntax assignExpr:
                CompileAssignmentExpression(assignExpr);
                break;

            case IdentifierNameSyntax idName:
                CompileIdentifier(idName);
                break;

            case LiteralExpressionSyntax literal:
                CompileLiteral(literal);
                break;

            case InvocationExpressionSyntax invocation:
                CompileMethodCall(invocation);
                break;

            case MemberAccessExpressionSyntax memberAccess:
                CompileMemberAccess(memberAccess);
                break;

            case ObjectCreationExpressionSyntax objCreation:
                CompileObjectCreation(objCreation);
                break;

            case ArrayCreationExpressionSyntax arrayCreation:
                CompileArrayCreation(arrayCreation);
                break;

            case ElementAccessExpressionSyntax elementAccess:
                CompileElementAccess(elementAccess);
                break;

            case CastExpressionSyntax castExpr:
                CompileCast(castExpr);
                break;

            case ParenthesizedExpressionSyntax parenExpr:
                CompileExpression(parenExpr.Expression);
                break;

            case InterpolatedStringExpressionSyntax interpolatedStr:
                CompileInterpolatedString(interpolatedStr);
                break;

            case PrefixUnaryExpressionSyntax prefixUnary:
                CompilePrefixUnaryExpression(prefixUnary);
                break;

            case PostfixUnaryExpressionSyntax postfixUnary:
                CompilePostfixUnaryExpression(postfixUnary);
                break;

            case ConditionalExpressionSyntax condExpr:
                CompileConditionalExpression(condExpr);
                break;

            case ThisExpressionSyntax _:
                _instructions.Add(new LoadArgInstruction("this"));
                break;

            case DefaultExpressionSyntax defaultExpr:
                CompileDefaultExpression(defaultExpr);
                break;

            case TypeOfExpressionSyntax typeOfExpr:
                CompileTypeOfExpression(typeOfExpr);
                break;

            case ImplicitArrayCreationExpressionSyntax implicitArray:
                CompileImplicitArrayCreation(implicitArray);
                break;

            default:
                System.Console.WriteLine($"[DEBUG] Unsupported expression: {expression.GetType().Name}");
                // Skip unsupported expressions
                break;
        }
    }

    private void CompileBinaryExpression(BinaryExpressionSyntax binExpr)
    {
        if (TryGetComparisonOperator(binExpr.Kind(), out var comparisonOp))
        {
            CompileExpression(binExpr.Left);
            CompileExpression(binExpr.Right);
            _instructions.Add(new ComparisonInstruction(comparisonOp));
            return;
        }

        // Compile left and right operands
        CompileExpression(binExpr.Left);
        CompileExpression(binExpr.Right);

        // Determine operation
        var op = binExpr.Kind() switch
        {
            SyntaxKind.AddExpression => ArithmeticOp.Add,
            SyntaxKind.SubtractExpression => ArithmeticOp.Sub,
            SyntaxKind.MultiplyExpression => ArithmeticOp.Mul,
            SyntaxKind.DivideExpression => ArithmeticOp.Div,
            SyntaxKind.ModuloExpression => ArithmeticOp.Rem,
            SyntaxKind.BitwiseAndExpression => ArithmeticOp.And,
            SyntaxKind.BitwiseOrExpression => ArithmeticOp.Or,
            SyntaxKind.ExclusiveOrExpression => ArithmeticOp.Xor,
            SyntaxKind.LeftShiftExpression => ArithmeticOp.Shl,
            SyntaxKind.RightShiftExpression => ArithmeticOp.Shr,
            _ => ArithmeticOp.Add
        };

        _instructions.Add(new ArithmeticInstruction(op));
    }

    private bool TryGetComparisonOperator(SyntaxKind kind, out ComparisonOp op)
    {
        switch (kind)
        {
            case SyntaxKind.EqualsExpression:
                op = ComparisonOp.Equal;
                return true;
            case SyntaxKind.NotEqualsExpression:
                op = ComparisonOp.NotEqual;
                return true;
            case SyntaxKind.LessThanExpression:
                op = ComparisonOp.Less;
                return true;
            case SyntaxKind.LessThanOrEqualExpression:
                op = ComparisonOp.LessOrEqual;
                return true;
            case SyntaxKind.GreaterThanExpression:
                op = ComparisonOp.Greater;
                return true;
            case SyntaxKind.GreaterThanOrEqualExpression:
                op = ComparisonOp.GreaterOrEqual;
                return true;
            default:
                op = default;
                return false;
        }
    }

    private void CompileAssignmentExpression(AssignmentExpressionSyntax assignExpr)
    {
        CompileExpression(assignExpr.Right);

        switch (assignExpr.Left)
        {
            case IdentifierNameSyntax idName:
                var varName = idName.Identifier.Text;
                if (_localVariables.ContainsKey(varName))
                {
                    _instructions.Add(new StoreLocalInstruction(varName));
                }
                break;

            case MemberAccessExpressionSyntax memberAccess:
                // Store to field
                CompileMemberAccessStore(memberAccess);
                break;

            case ElementAccessExpressionSyntax elementAccess:
                // Store to array element
                CompileElementAccessStore(elementAccess);
                break;
        }
    }

    private void CompileIdentifier(IdentifierNameSyntax idName)
    {
        var name = idName.Identifier.Text;
        
        if (_localVariables.ContainsKey(name))
        {
            _instructions.Add(new LoadLocalInstruction(name));
        }
        else
        {
            // Could be a parameter or static field
            _instructions.Add(new LoadArgInstruction(name));
        }
    }

    private void CompileLiteral(LiteralExpressionSyntax literal)
    {
        var value = literal.Token.Value;
        var type = GetLiteralType(literal.Kind());
        _instructions.Add(new LoadConstantInstruction(value ?? 0, type));
    }

    private void CompileMethodCall(InvocationExpressionSyntax invocation)
    {
        // Compile arguments
        foreach (var arg in invocation.ArgumentList.Arguments)
        {
            CompileExpression(arg.Expression);
        }

        // Get method reference
        var methodSymbol = _semanticModel.GetSymbolInfo(invocation.Expression).Symbol as IMethodSymbol;
        if (methodSymbol != null)
        {
            var methodRef = ConvertMethodSymbol(methodSymbol);
            
            if (methodSymbol.IsStatic)
            {
                _instructions.Add(new CallInstruction(methodRef));
            }
            else
            {
                // Compile the object expression
                if (invocation.Expression is MemberAccessExpressionSyntax memberAccess)
                {
                    CompileExpression(memberAccess.Expression);
                }
                _instructions.Add(new CallVirtualInstruction(methodRef));
            }
        }
    }

    private void CompileMemberAccess(MemberAccessExpressionSyntax memberAccess)
    {
        CompileExpression(memberAccess.Expression);
        
        var symbol = _semanticModel.GetSymbolInfo(memberAccess).Symbol;
        if (symbol is IFieldSymbol fieldSymbol)
        {
            var fieldRef = ConvertFieldSymbol(fieldSymbol);
            if (fieldSymbol.IsStatic)
            {
                _instructions.Add(new LoadStaticFieldInstruction(fieldRef));
            }
            else
            {
                _instructions.Add(new LoadFieldInstruction(fieldRef));
            }
        }
    }

    private void CompileMemberAccessStore(MemberAccessExpressionSyntax memberAccess)
    {
        // Evaluate the object
        CompileExpression(memberAccess.Expression);
        
        var symbol = _semanticModel.GetSymbolInfo(memberAccess).Symbol;
        if (symbol is IFieldSymbol fieldSymbol)
        {
            var fieldRef = ConvertFieldSymbol(fieldSymbol);
            if (fieldSymbol.IsStatic)
            {
                _instructions.Add(new StoreStaticFieldInstruction(fieldRef));
            }
            else
            {
                _instructions.Add(new StoreFieldInstruction(fieldRef));
            }
        }
    }

    private void CompileObjectCreation(ObjectCreationExpressionSyntax objCreation)
    {
        // Compile constructor arguments
        foreach (var arg in objCreation.ArgumentList?.Arguments ?? Enumerable.Empty<ArgumentSyntax>())
        {
            CompileExpression(arg.Expression);
        }

        var typeSymbol = TryGetTypeSymbol(objCreation.Type);
        var typeRef = ConvertTypeOrFallback(typeSymbol, objCreation.Type);
        
        _instructions.Add(new NewObjectInstruction(typeRef));
    }

    private void CompileArrayCreation(ArrayCreationExpressionSyntax arrayCreation)
    {
        // Compile size expressions
        foreach (var size in arrayCreation.Type.RankSpecifiers.First().Sizes)
        {
            CompileExpression(size);
        }

        var elementType = TryGetTypeSymbol(arrayCreation.Type.ElementType);
        var elementTypeRef = ConvertTypeOrFallback(elementType, arrayCreation.Type.ElementType);
        
        _instructions.Add(new NewArrayInstruction(elementTypeRef));
    }

    private void CompileElementAccess(ElementAccessExpressionSyntax elementAccess)
    {
        // Load array
        CompileExpression(elementAccess.Expression);
        
        // Load index
        foreach (var index in elementAccess.ArgumentList.Arguments)
        {
            CompileExpression(index.Expression);
        }

        _instructions.Add(new LoadElementInstruction());
    }

    private void CompileElementAccessStore(ElementAccessExpressionSyntax elementAccess)
    {
        // Load array
        CompileExpression(elementAccess.Expression);
        
        // Load index
        foreach (var index in elementAccess.ArgumentList.Arguments)
        {
            CompileExpression(index.Expression);
        }

        _instructions.Add(new StoreElementInstruction());
    }

    private void CompileCast(CastExpressionSyntax castExpr)
    {
        CompileExpression(castExpr.Expression);
        
        var targetType = TryGetTypeSymbol(castExpr.Type);
        var targetTypeRef = ConvertTypeOrFallback(targetType, castExpr.Type);
        
        _instructions.Add(new CastInstruction(targetTypeRef));
    }

    // Unary expression compilation - commented out for now due to Roslyn syntax issues
    /*
    private void CompileUnaryExpression(UnaryExpressionSyntax unaryExpr)
    {
        CompileExpression(unaryExpr.Operand);
        
        switch (unaryExpr.Kind())
        {
            case SyntaxKind.UnaryPlusExpression:
                // No-op
                break;
            
            case SyntaxKind.UnaryMinusExpression:
                _instructions.Add(new UnaryNegateInstruction());
                break;
            
            case SyntaxKind.BitwiseNotExpression:
                _instructions.Add(new UnaryNotInstruction());
                break;
            
            case SyntaxKind.LogicalNotExpression:
                _instructions.Add(new UnaryNotInstruction());
                break;
        }
    }
    */

    private void CompileInterpolatedString(InterpolatedStringExpressionSyntax interpolatedStr)
    {
        // Compile interpolated strings as String.Concat calls
        // First, collect all parts and compile them
        var parts = new List<InterpolatedStringContentSyntax>(interpolatedStr.Contents);
        
        if (parts.Count == 0)
        {
            // Empty string
            _instructions.Add(new LoadConstantInstruction("", TypeReference.String));
            return;
        }

        // Compile each part onto the stack
        int partCount = 0;
        foreach (var part in parts)
        {
            switch (part)
            {
                case InterpolatedStringTextSyntax textPart:
                    // Literal text - load as string constant
                    _instructions.Add(new LoadConstantInstruction(textPart.TextToken.ValueText, TypeReference.String));
                    partCount++;
                    break;

                case InterpolationSyntax interpolation:
                    // Expression part - compile the expression
                    CompileExpression(interpolation.Expression);
                    // Box/convert to string if needed - call ToString()
                    var toStringMethod = new MethodReference(
                        TypeReference.FromName("System.Object"),
                        "ToString",
                        TypeReference.String,
                        new List<TypeReference>()
                    );
                    _instructions.Add(new CallVirtualInstruction(toStringMethod));
                    partCount++;
                    break;
            }
        }

        // Now concatenate all parts using String.Concat
        if (partCount == 1)
        {
            // Single part, no need to concat
            return;
        }

        // Create parameter types list for String.Concat (all strings)
        var paramTypes = Enumerable.Repeat(TypeReference.String, partCount).ToList();
        
        var concatMethod = new MethodReference(
            TypeReference.String,
            "Concat",
            TypeReference.String,
            paramTypes
        );

        _instructions.Add(new CallInstruction(concatMethod));
    }

    private void CompilePrefixUnaryExpression(PrefixUnaryExpressionSyntax prefixUnary)
    {
        switch (prefixUnary.Kind())
        {
            case SyntaxKind.UnaryPlusExpression:
                // Just compile the operand, unary plus is a no-op
                CompileExpression(prefixUnary.Operand);
                break;

            case SyntaxKind.UnaryMinusExpression:
                // Load 0, then the operand, then subtract
                CompileExpression(prefixUnary.Operand);
                _instructions.Add(new UnaryNegateInstruction());
                break;

            case SyntaxKind.LogicalNotExpression:
                CompileExpression(prefixUnary.Operand);
                _instructions.Add(new UnaryNotInstruction());
                break;

            case SyntaxKind.BitwiseNotExpression:
                CompileExpression(prefixUnary.Operand);
                _instructions.Add(new UnaryNotInstruction());
                break;

            case SyntaxKind.PreIncrementExpression:
                CompileIncrementDecrement(prefixUnary.Operand, true, true);
                break;

            case SyntaxKind.PreDecrementExpression:
                CompileIncrementDecrement(prefixUnary.Operand, false, true);
                break;

            default:
                CompileExpression(prefixUnary.Operand);
                break;
        }
    }

    private void CompilePostfixUnaryExpression(PostfixUnaryExpressionSyntax postfixUnary)
    {
        switch (postfixUnary.Kind())
        {
            case SyntaxKind.PostIncrementExpression:
                CompileIncrementDecrement(postfixUnary.Operand, true, false);
                break;

            case SyntaxKind.PostDecrementExpression:
                CompileIncrementDecrement(postfixUnary.Operand, false, false);
                break;

            default:
                CompileExpression(postfixUnary.Operand);
                break;
        }
    }

    private void CompileIncrementDecrement(ExpressionSyntax operand, bool isIncrement, bool isPrefix)
    {
        // For prefix: increment first, then load
        // For postfix: load first, then increment
        
        if (operand is IdentifierNameSyntax idName)
        {
            var name = idName.Identifier.Text;
            
            if (!isPrefix)
            {
                // Postfix: load original value first
                if (_localVariables.ContainsKey(name))
                    _instructions.Add(new LoadLocalInstruction(name));
                else
                    _instructions.Add(new LoadArgInstruction(name));
            }

            // Load, add/sub 1, store
            if (_localVariables.ContainsKey(name))
                _instructions.Add(new LoadLocalInstruction(name));
            else
                _instructions.Add(new LoadArgInstruction(name));

            _instructions.Add(new LoadConstantInstruction(1, TypeReference.Int32));
            _instructions.Add(new ArithmeticInstruction(isIncrement ? ArithmeticOp.Add : ArithmeticOp.Sub));

            if (_localVariables.ContainsKey(name))
                _instructions.Add(new StoreLocalInstruction(name));
            else
                _instructions.Add(new StoreArgInstruction(name));

            if (isPrefix)
            {
                // Prefix: load the new value
                if (_localVariables.ContainsKey(name))
                    _instructions.Add(new LoadLocalInstruction(name));
                else
                    _instructions.Add(new LoadArgInstruction(name));
            }
        }
        else
        {
            // For other expressions, just compile and skip the increment for now
            CompileExpression(operand);
        }
    }

    private void CompileConditionalExpression(ConditionalExpressionSyntax condExpr)
    {
        // Compile as: condition ? trueValue : falseValue
        // Using high-level IfInstruction structure
        
        // Create a temp variable to store the result
        var tempVarName = $"__ternary_{_tempVarCounter++}";
        var tempVar = new LocalVariable(tempVarName, TypeReference.FromName("object"));
        _localVariables[tempVarName] = tempVar;

        // Compile condition for the IfInstruction
        var condition = CompileConditionExpression(condExpr.Condition);
        var ifInstr = new IfInstruction(condition);

        // Save current instructions
        var savedInstructions = _instructions.ToList();
        
        // Compile then block (true branch)
        _instructions.Clear();
        CompileExpression(condExpr.WhenTrue);
        _instructions.Add(new StoreLocalInstruction(tempVarName));
        var thenBlock = _instructions.ToList();
        
        // Compile else block (false branch)
        _instructions.Clear();
        CompileExpression(condExpr.WhenFalse);
        _instructions.Add(new StoreLocalInstruction(tempVarName));
        var elseBlock = new InstructionList();
        foreach (var instr in _instructions)
        {
            elseBlock.Add(instr);
        }

        // Restore saved instructions and add if  
        _instructions.Clear();
        foreach (var instr in savedInstructions)
        {
            _instructions.Add(instr);
        }
        
        // Add compiled blocks to if instruction
        foreach (var instr in thenBlock)
        {
            ifInstr.ThenBlock.Add(instr);
        }
        ifInstr.ElseBlock = elseBlock;
        
        _instructions.Add(ifInstr);
        
        // Load the result
        _instructions.Add(new LoadLocalInstruction(tempVarName));
    }

    private void CompileDefaultExpression(DefaultExpressionSyntax defaultExpr)
    {
        var typeSymbol = _semanticModel.GetTypeInfo(defaultExpr.Type).Type;
        var typeRef = ConvertType(typeSymbol);
        
        // Load the default value for the type
        if (typeRef == TypeReference.Int32 || typeRef == TypeReference.Int64 ||
            typeRef == TypeReference.Int16 || typeRef == TypeReference.Int8)
        {
            _instructions.Add(new LoadConstantInstruction(0, typeRef));
        }
        else if (typeRef == TypeReference.Float32 || typeRef == TypeReference.Float64)
        {
            _instructions.Add(new LoadConstantInstruction(0.0, typeRef));
        }
        else if (typeRef == TypeReference.Bool)
        {
            _instructions.Add(new LoadConstantInstruction(false, typeRef));
        }
        else if (typeRef == TypeReference.Char)
        {
            _instructions.Add(new LoadConstantInstruction('\0', typeRef));
        }
        else
        {
            // Reference types default to null
            _instructions.Add(new LoadNullInstruction());
        }
    }

    private void CompileTypeOfExpression(TypeOfExpressionSyntax typeOfExpr)
    {
        var typeSymbol = TryGetTypeSymbol(typeOfExpr.Type);
        var typeRef = ConvertTypeOrFallback(typeSymbol, typeOfExpr.Type);
        
        // Load the type reference as a call to Type.GetType with the type name
        // Since there's no LoadTypeInstruction, we emit a call to get the runtime type
        _instructions.Add(new LoadConstantInstruction(typeRef.GetQualifiedName(), TypeReference.String));
        var getTypeMethod = new MethodReference(
            TypeReference.FromName("System.Type"),
            "GetType",
            TypeReference.FromName("System.Type"),
            new List<TypeReference> { TypeReference.String }
        );
        _instructions.Add(new CallInstruction(getTypeMethod));
    }

    private void CompileImplicitArrayCreation(ImplicitArrayCreationExpressionSyntax implicitArray)
    {
        var initializer = implicitArray.Initializer;
        var elementCount = initializer.Expressions.Count;
        
        // Get element type from semantic model
        var typeInfo = _semanticModel.GetTypeInfo(implicitArray);
        var arrayType = typeInfo.Type as IArrayTypeSymbol;
        var elementType = arrayType != null ? ConvertType(arrayType.ElementType) : TypeReference.FromName("object");
        
        // Create the array with the count
        _instructions.Add(new LoadConstantInstruction(elementCount, TypeReference.Int32));
        _instructions.Add(new NewArrayInstruction(elementType));
        
        // Store each element
        for (int i = 0; i < elementCount; i++)
        {
            _instructions.Add(new DupInstruction()); // Duplicate array reference
            _instructions.Add(new LoadConstantInstruction(i, TypeReference.Int32)); // Index
            CompileExpression(initializer.Expressions[i]); // Value
            _instructions.Add(new StoreElementInstruction());
        }
    }

    private TypeReference ConvertTypeOrFallback(ITypeSymbol? symbol, TypeSyntax? fallbackSyntax)
    {
        if (symbol != null)
        {
            return ConvertType(symbol);
        }

        if (fallbackSyntax != null)
        {
            var mapped = MapNodeToSemanticTree(fallbackSyntax) ?? fallbackSyntax;
            return TypeReference.FromName(mapped.ToString());
        }

        return TypeReference.FromName("object");
    }

    private TypeReference GetLiteralType(SyntaxKind kind)
    {
        return kind switch
        {
            SyntaxKind.TrueLiteralExpression or SyntaxKind.FalseLiteralExpression => TypeReference.Bool,
            SyntaxKind.StringLiteralExpression => TypeReference.String,
            SyntaxKind.NumericLiteralExpression => TypeReference.Int32,
            SyntaxKind.CharacterLiteralExpression => TypeReference.Char,
            _ => TypeReference.FromName("object")
        };
    }

    private TypeReference ConvertType(ITypeSymbol? symbol)
    {
        if (symbol == null)
            return TypeReference.FromName("object");

        return symbol.SpecialType switch
        {
            SpecialType.System_Void => TypeReference.Void,
            SpecialType.System_Boolean => TypeReference.Bool,
            SpecialType.System_Char => TypeReference.Char,
            SpecialType.System_SByte => TypeReference.Int8,
            SpecialType.System_Byte => TypeReference.UInt8,
            SpecialType.System_Int16 => TypeReference.Int16,
            SpecialType.System_UInt16 => TypeReference.UInt16,
            SpecialType.System_Int32 => TypeReference.Int32,
            SpecialType.System_UInt32 => TypeReference.UInt32,
            SpecialType.System_Int64 => TypeReference.Int64,
            SpecialType.System_UInt64 => TypeReference.UInt64,
            SpecialType.System_Single => TypeReference.Float32,
            SpecialType.System_Double => TypeReference.Float64,
            SpecialType.System_String => TypeReference.String,
            _ => TypeReference.FromName(symbol.Name)
        };
    }

    private MethodReference ConvertMethodSymbol(IMethodSymbol symbol)
    {
        var declaringType = ConvertType(symbol.ContainingType);
        var returnType = ConvertType(symbol.ReturnType);
        var paramTypes = symbol.Parameters.Select(p => ConvertType(p.Type)).ToList();

        return new MethodReference(declaringType, symbol.Name, returnType, paramTypes);
    }

    private FieldReference ConvertFieldSymbol(IFieldSymbol symbol)
    {
        var declaringType = ConvertType(symbol.ContainingType);
        var fieldType = ConvertType(symbol.Type);

        return new FieldReference(declaringType, symbol.Name, fieldType);
    }
}
}
