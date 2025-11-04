namespace ObjectIR.Core.Compilers;

using System;
using System.Collections.Generic;
using System.Linq;

/// <summary>
/// Parses Construct language tokens into an AST
/// </summary>
public class ConstructParser
{
    private readonly List<Token> _tokens;
    private int _current = 0;

    public ConstructParser(List<Token> tokens)
    {
        _tokens = tokens;
    }

    public Program Parse()
    {
        var contract = ParseContract();
        return new Program(contract);
    }

    private ContractDeclaration ParseContract()
    {
        Consume(TokenType.Contract, "Expected 'Contract'");
        string name = Consume(TokenType.Identifier, "Expected contract name").Text;
        Consume(TokenType.LeftBrace, "Expected '{'");

        var functions = new List<FunctionDeclaration>();
        while (!Check(TokenType.RightBrace) && !IsAtEnd())
        {
            functions.Add(ParseFunctionDeclaration());
        }

        Consume(TokenType.RightBrace, "Expected '}'");
        return new ContractDeclaration(name, functions);
    }

    private FunctionDeclaration ParseFunctionDeclaration()
    {
        Consume(TokenType.Fn, "Expected 'fn'");
        string name = Consume(TokenType.Identifier, "Expected function name").Text;
        Consume(TokenType.LeftParen, "Expected '('");

        var parameters = new List<Parameter>();
        if (!Check(TokenType.RightParen))
        {
            do
            {
                string paramName = Consume(TokenType.Identifier, "Expected parameter name").Text;
                Consume(TokenType.Colon, "Expected ':'");
                string typeName = ParseTypeName();
                parameters.Add(new Parameter(paramName, new TypeAnnotation(typeName)));
            } while (Match(TokenType.Comma));
        }

        Consume(TokenType.RightParen, "Expected ')'");

        TypeAnnotation? returnType = null;
        if (Match(TokenType.Minus) && Match(TokenType.Greater))
        {
            string typeName = ParseTypeName();
            returnType = new TypeAnnotation(typeName);
        }

        Consume(TokenType.LeftBrace, "Expected '{'");
        var body = ParseBlock();
        Consume(TokenType.RightBrace, "Expected '}'");

        return new FunctionDeclaration(name, parameters, returnType, body);
    }

    private Block ParseBlock()
    {
        var statements = new List<Statement>();

        while (!Check(TokenType.RightBrace) && !IsAtEnd())
        {
            statements.Add(ParseStatement());
        }

        return new Block(statements);
    }

    private Statement ParseStatement()
    {
        if (Match(TokenType.Var))
            return ParseVarDeclaration();

        if (Match(TokenType.If))
            return ParseIfStatement();

        if (Match(TokenType.While))
            return ParseWhileStatement();

        if (Match(TokenType.Return))
            return ParseReturnStatement();

        return ParseExpressionStatement();
    }

    private VarDeclaration ParseVarDeclaration()
    {
        string name = Consume(TokenType.Identifier, "Expected variable name").Text;
        Consume(TokenType.Equal, "Expected '='");
        Expression initializer = ParseExpression();
        Consume(TokenType.Semicolon, "Expected ';'");
        return new VarDeclaration(name, initializer);
    }

    private IfStatement ParseIfStatement()
    {
        Consume(TokenType.LeftParen, "Expected '('");
        Expression condition = ParseExpression();
        Consume(TokenType.RightParen, "Expected ')'");
        Consume(TokenType.LeftBrace, "Expected '{'");
        Block thenBranch = ParseBlock();
        Consume(TokenType.RightBrace, "Expected '}'");

        Block? elseBranch = null;
        if (Match(TokenType.Else))
        {
            Consume(TokenType.LeftBrace, "Expected '{'");
            elseBranch = ParseBlock();
            Consume(TokenType.RightBrace, "Expected '}'");
        }

        return new IfStatement(condition, thenBranch, elseBranch);
    }

    private WhileStatement ParseWhileStatement()
    {
        Consume(TokenType.LeftParen, "Expected '('");
        Expression condition = ParseExpression();
        Consume(TokenType.RightParen, "Expected ')'");
        Consume(TokenType.LeftBrace, "Expected '{'");
        Block body = ParseBlock();
        Consume(TokenType.RightBrace, "Expected '}'");

        return new WhileStatement(condition, body);
    }

    private ReturnStatement ParseReturnStatement()
    {
        Expression? value = null;
        if (!Check(TokenType.Semicolon))
        {
            value = ParseExpression();
        }
        Consume(TokenType.Semicolon, "Expected ';'");
        return new ReturnStatement(value);
    }

    private ExpressionStatement ParseExpressionStatement()
    {
        Expression expr = ParseExpression();
        Consume(TokenType.Semicolon, "Expected ';'");
        return new ExpressionStatement(expr);
    }

    private Expression ParseExpression()
    {
        return ParseAssignment();
    }

    private Expression ParseAssignment()
    {
        Expression expr = ParseLogicalOr();

        if (Match(TokenType.Equal))
        {
            if (expr is Identifier id)
            {
                Expression value = ParseAssignment();
                return new Assignment(id.Name, value);
            }
            throw new CompileException("Invalid assignment target");
        }

        return expr;
    }

    private Expression ParseLogicalOr()
    {
        Expression expr = ParseLogicalAnd();

        // Note: Construct language simplified, only has comparison operators
        return expr;
    }

    private Expression ParseLogicalAnd()
    {
        Expression expr = ParseComparison();
        return expr;
    }

    private Expression ParseComparison()
    {
        Expression expr = ParseAddition();

        while (Match(TokenType.EqualEqual, TokenType.BangEqual, TokenType.Less,
                     TokenType.LessEqual, TokenType.Greater, TokenType.GreaterEqual))
        {
            string op = Previous().Text;
            Expression right = ParseAddition();
            expr = new BinaryOp(expr, op, right);
        }

        return expr;
    }

    private Expression ParseAddition()
    {
        Expression expr = ParseMultiplication();

        while (Match(TokenType.Plus, TokenType.Minus))
        {
            string op = Previous().Text;
            Expression right = ParseMultiplication();
            expr = new BinaryOp(expr, op, right);
        }

        return expr;
    }

    private Expression ParseMultiplication()
    {
        Expression expr = ParseUnary();

        while (Match(TokenType.Star, TokenType.Slash))
        {
            string op = Previous().Text;
            Expression right = ParseUnary();
            expr = new BinaryOp(expr, op, right);
        }

        return expr;
    }

    private Expression ParseUnary()
    {
        if (Match(TokenType.Bang, TokenType.Minus))
        {
            string op = Previous().Text;
            Expression expr = ParseUnary();
            return new UnaryOp(op, expr);
        }

        return ParsePostfix();
    }

    private Expression ParsePostfix()
    {
        Expression expr = ParsePrimary();

        while (true)
        {
            if (Match(TokenType.LeftParen))
            {
                // Function call
                if (expr is Identifier id)
                {
                    var args = new List<Expression>();
                    if (!Check(TokenType.RightParen))
                    {
                        do
                        {
                            args.Add(ParseExpression());
                        } while (Match(TokenType.Comma));
                    }
                    Consume(TokenType.RightParen, "Expected ')'");
                    expr = new FunctionCall(id.Name, args);
                }
                else if (expr is MemberAccess member)
                {
                    // Method call on member access
                    var args = new List<Expression>();
                    if (!Check(TokenType.RightParen))
                    {
                        do
                        {
                            args.Add(ParseExpression());
                        } while (Match(TokenType.Comma));
                    }
                    Consume(TokenType.RightParen, "Expected ')'");
                    
                    // Convert member access + call to qualified function call
                    if (member.Object is Identifier obj)
                    {
                        expr = new FunctionCall($"{obj.Name}.{member.Member}", args);
                    }
                }
            }
            else if (Match(TokenType.Dot))
            {
                string member = Consume(TokenType.Identifier, "Expected member name").Text;
                expr = new MemberAccess(expr, member);
            }
            else
            {
                break;
            }
        }

        return expr;
    }

    private Expression ParsePrimary()
    {
        if (Match(TokenType.True))
            return new BooleanLiteral(true);

        if (Match(TokenType.False))
            return new BooleanLiteral(false);

        if (Match(TokenType.Number))
            return new NumberLiteral(int.Parse(Previous().Text));

        if (Match(TokenType.String))
            return new StringLiteral(Previous().Text);

        if (Match(TokenType.Identifier))
            return new Identifier(Previous().Text);

        if (Match(TokenType.IO))
            return new Identifier("IO");

        if (Match(TokenType.LeftParen))
        {
            Expression expr = ParseExpression();
            Consume(TokenType.RightParen, "Expected ')'");
            return expr;
        }

        throw new CompileException($"Unexpected token: {Peek().Text}");
    }

    private string ParseTypeName()
    {
        if (Match(TokenType.TypeInt))
            return "Int";

        if (Match(TokenType.TypeString))
            return "String";

        if (Match(TokenType.TypeBool))
            return "Bool";

        if (Match(TokenType.Identifier))
            return Previous().Text;

        throw new CompileException("Expected type name");
    }

    private bool Match(params TokenType[] types)
    {
        foreach (var type in types)
        {
            if (Check(type))
            {
                Advance();
                return true;
            }
        }
        return false;
    }

    private bool Check(TokenType type)
    {
        if (IsAtEnd())
            return false;
        return Peek().Type == type;
    }

    private Token Advance()
    {
        if (!IsAtEnd())
            _current++;
        return Previous();
    }

    private bool IsAtEnd()
    {
        return Peek().Type == TokenType.EOF;
    }

    private Token Peek()
    {
        return _tokens[_current];
    }

    private Token Previous()
    {
        return _tokens[_current - 1];
    }

    private Token Consume(TokenType type, string message)
    {
        if (Check(type))
            return Advance();

        throw new CompileException($"{message} at line {Peek().Line}: got {Peek().Text}");
    }
}
