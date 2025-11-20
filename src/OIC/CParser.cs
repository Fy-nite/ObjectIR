namespace OIC;

using System.Collections.Generic;
using System.Linq;

/// <summary>
/// Recursive descent parser for C language
/// </summary>
public class CParser
{
    private readonly List<CToken> _tokens;
    private int _pos;

    public CParser(IEnumerable<CToken> tokens)
    {
        _tokens = tokens.ToList();
        _pos = 0;
    }

    public CProgram Parse()
    {
        var declarations = new List<CDeclaration>();
        
        while (!IsAtEnd())
        {
            try
            {
                if (IsDeclaration())
                {
                    var decl = ParseDeclaration();
                    if (decl != null)
                        declarations.Add(decl);
                }
                else if (!IsAtEnd())
                {
                    // Skip unknown tokens
                    Advance();
                }
            }
            catch (Exception ex)
            {
                // DEBUG: Log the exception
                Console.Error.WriteLine($"[PARSE ERROR] {ex.Message}");
                Console.Error.WriteLine($"  at token: {Current().Kind} '{Current().Text}' (line {Current().Line})");
                
                // Skip to next semicolon on error
                while (!IsAtEnd() && Current().Kind != CTokenKind.Semicolon && Current().Kind != CTokenKind.RBrace)
                    Advance();
                if (!IsAtEnd()) Advance();
            }
        }

        return new CProgram(declarations);
    }

    // Declaration parsing
    private bool IsDeclaration()
    {
        var kind = Current().Kind;
        return kind switch
        {
            CTokenKind.KwVoid or CTokenKind.KwInt or CTokenKind.KwChar or CTokenKind.KwFloat or
            CTokenKind.KwDouble or CTokenKind.KwShort or CTokenKind.KwLong or CTokenKind.KwUnsigned or
            CTokenKind.KwSigned or CTokenKind.KwStatic or CTokenKind.KwExtern or CTokenKind.KwAuto or
            CTokenKind.KwRegister or CTokenKind.KwConst or CTokenKind.KwVolatile or
            CTokenKind.KwStruct or CTokenKind.KwUnion or CTokenKind.KwEnum or
            CTokenKind.KwTypedef => true,
            _ => false,
        };
    }

    private CDeclaration ParseDeclaration()
    {
        if (Match(CTokenKind.KwTypedef))
        {
            var specifiers = ParseDeclarationSpecifiers();
            var declarator = ParseDeclarator();
            Consume(CTokenKind.Semicolon, "Expected ';' after typedef");
            return new CTypedef(specifiers, (CDirectDeclarator)declarator);
        }

        var specifiers2 = ParseDeclarationSpecifiers();

        // Check if this is a function by looking ahead
        var saved = _pos;
        
        // Skip * tokens (pointers)
        while (Check(CTokenKind.Star)) Advance();
        
        // Expect identifier
        if (Check(CTokenKind.Identifier)) 
        {
            Advance();
        }
        
        // If followed by (, it's a function
        if (Check(CTokenKind.LParen))
        {
            _pos = saved;
            return ParseFunctionDeclaration(specifiers2);
        }

        _pos = saved;

        var declarators = new List<CInitializer>();
        
        if (!Check(CTokenKind.Semicolon))
        {
            declarators.Add(ParseInitDeclarator());
            while (Match(CTokenKind.Comma))
            {
                declarators.Add(ParseInitDeclarator());
            }
        }

        Consume(CTokenKind.Semicolon, "Expected ';' after declaration");
        return new CDeclarationList(specifiers2, declarators);
    }

    private List<CSpecifier> ParseDeclarationSpecifiers()
    {
        var specifiers = new List<CSpecifier>();

        while (true)
        {
            var kind = Current().Kind;
            if (kind is CTokenKind.KwStatic or CTokenKind.KwExtern or CTokenKind.KwAuto or CTokenKind.KwRegister)
            {
                specifiers.Add(new CStorageClass(Current().Text));
                Advance();
            }
            else if (kind is CTokenKind.KwConst or CTokenKind.KwVolatile or CTokenKind.KwRestrict)
            {
                specifiers.Add(new CTypeQualifier(Current().Text));
                Advance();
            }
            else if (kind == CTokenKind.KwStruct || kind == CTokenKind.KwUnion)
            {
                specifiers.Add(ParseStructOrUnion());
            }
            else if (kind == CTokenKind.KwEnum)
            {
                specifiers.Add(ParseEnum());
            }
            else if (IsTypeSpecifier(kind))
            {
                specifiers.Add(new CTypeSpecifier(Current().Text));
                Advance();
            }
            else
            {
                break;
            }
        }

        return specifiers.Count > 0 ? specifiers : new List<CSpecifier> { new CTypeSpecifier("int") };
    }

    private bool IsTypeSpecifier(CTokenKind kind)
    {
        return kind switch
        {
            CTokenKind.KwVoid or CTokenKind.KwChar or CTokenKind.KwShort or CTokenKind.KwInt or
            CTokenKind.KwLong or CTokenKind.KwFloat or CTokenKind.KwDouble or
            CTokenKind.KwSigned or CTokenKind.KwUnsigned => true,
            // Don't treat identifiers as type specifiers - let them be declarators
            _ => false,
        };
    }

    private CStructOrUnion ParseStructOrUnion()
    {
        var kind = Current().Text; // "struct" or "union"
        Advance();

        string? name = null;
        List<CDeclarationList>? members = null;

        if (Check(CTokenKind.Identifier))
        {
            name = Current().Text;
            Advance();
        }

        if (Match(CTokenKind.LBrace))
        {
            members = new List<CDeclarationList>();
            while (!Check(CTokenKind.RBrace) && !IsAtEnd())
            {
                var memberSpecifiers = ParseDeclarationSpecifiers();
                var declarators = new List<CInitializer>();

                if (!Check(CTokenKind.Semicolon))
                {
                    declarators.Add(ParseInitDeclarator());
                    while (Match(CTokenKind.Comma))
                    {
                        declarators.Add(ParseInitDeclarator());
                    }
                }

                Consume(CTokenKind.Semicolon, "Expected ';' after member declaration");
                members.Add(new CDeclarationList(memberSpecifiers, declarators));
            }
            Consume(CTokenKind.RBrace, "Expected '}' after struct/union members");
        }

        return new CStructOrUnion(kind, name, members);
    }

    private CEnum ParseEnum()
    {
        Advance(); // consume 'enum'

        string? name = null;
        if (Check(CTokenKind.Identifier))
        {
            name = Current().Text;
            Advance();
        }

        List<(string, CExpression?)>? members = null;
        if (Match(CTokenKind.LBrace))
        {
            members = new List<(string, CExpression?)>();
            do
            {
                if (Check(CTokenKind.Identifier))
                {
                    var memberName = Current().Text;
                    Advance();

                    CExpression? value = null;
                    if (Match(CTokenKind.Assign))
                    {
                        value = ParseExpression();
                    }

                    members.Add((memberName, value));
                }
            } while (Match(CTokenKind.Comma) && !Check(CTokenKind.RBrace));

            Consume(CTokenKind.RBrace, "Expected '}' after enum members");
        }

        return new CEnum(name, members);
    }

    private bool CheckFunction()
    {
        // Simple heuristic: if we see identifier, optional pointer, then ( and { nearby, it's likely a function
        var saved = _pos;
        try
        {
            // Skip type specifiers and qualifiers
            while (!IsAtEnd() && IsTypeSpecifier(Current().Kind))
                Advance();

            // Skip pointer stars
            while (!IsAtEnd() && Current().Kind == CTokenKind.Star)
                Advance();

            // Look for identifier ( 
            if (Check(CTokenKind.Identifier))
            {
                Advance();
                // Next must be lparen
                if (Check(CTokenKind.LParen))
                {
                    // Skip to matching rparen
                    Advance(); // skip (
                    var parenDepth = 1;
                    while (!IsAtEnd() && parenDepth > 0)
                    {
                        if (Current().Kind == CTokenKind.LParen) parenDepth++;
                        else if (Current().Kind == CTokenKind.RParen) parenDepth--;
                        Advance();
                    }
                    // Now check if there's a {
                    return Check(CTokenKind.LBrace);
                }
            }
            return false;
        }
        finally
        {
            _pos = saved;
        }
    }

    private CFunctionDeclaration ParseFunctionDeclaration(List<CSpecifier> returnTypeSpecifiers)
    {
        var name = Consume(CTokenKind.Identifier, "Expected function name").Text;
        
        Consume(CTokenKind.LParen, "Expected '(' after function name");
        var parameters = ParseParameterList();
        Consume(CTokenKind.RParen, "Expected ')' after parameters");

        CCompoundStatement body;
        if (Check(CTokenKind.LBrace))
        {
            body = ParseCompoundStatement();
        }
        else
        {
            body = new CCompoundStatement(new List<CDeclaration>(), new List<CStatement>());
        }

        return new CFunctionDeclaration(name, returnTypeSpecifiers, parameters, body);
    }

    private List<CParameter> ParseParameterList()
    {
        var parameters = new List<CParameter>();

        if (Check(CTokenKind.RParen) || Check(CTokenKind.Ellipsis))
            return parameters;

        do
        {
            var specifiers = ParseDeclarationSpecifiers();
            CDirectDeclarator? declarator = null;

            if (!Check(CTokenKind.Comma) && !Check(CTokenKind.RParen))
            {
                declarator = (CDirectDeclarator)ParseDeclarator();
            }

            parameters.Add(new CParameter(specifiers, declarator));
        } while (Match(CTokenKind.Comma));

        return parameters;
    }

    private CInitializer ParseInitDeclarator()
    {
        var declarator = ParseDeclarator();
        CExpression? initializer = null;

        if (Match(CTokenKind.Assign))
        {
            initializer = ParseExpression();
        }

        return new CInitializer((CDirectDeclarator)declarator, initializer);
    }

    private CNode ParseDeclarator()
    {
        var pointerCount = 0;
        while (Match(CTokenKind.Star))
            pointerCount++;

        var directDeclarator = ParseDirectDeclarator();

        if (pointerCount > 0)
            return new CPointerDeclarator((CDirectDeclarator)directDeclarator, pointerCount);

        return directDeclarator;
    }

    private CNode ParseDirectDeclarator()
    {
        CDirectDeclarator declarator;

        if (Check(CTokenKind.Identifier))
        {
            var name = Current().Text;
            Advance();
            declarator = new CDirectDeclarator(name);
        }
        else if (Match(CTokenKind.LParen))
        {
            var decl = ParseDeclarator();
            Consume(CTokenKind.RParen, "Expected ')' after declarator");
            declarator = (CDirectDeclarator)decl;
        }
        else
        {
            declarator = new CDirectDeclarator(null);
        }

        while (true)
        {
            if (Match(CTokenKind.LBracket))
            {
                CExpression? size = null;
                if (!Check(CTokenKind.RBracket))
                {
                    size = ParseExpression();
                }
                Consume(CTokenKind.RBracket, "Expected ']' after array size");
                declarator = new CDirectDeclarator(declarator.Name, ArraySize: size, Parent: declarator);
            }
            else if (Match(CTokenKind.LParen))
            {
                var parameters = ParseParameterList();
                Consume(CTokenKind.RParen, "Expected ')' after parameters");
                // For now, we skip parameter handling in direct declarator
                declarator = declarator with { Parent = declarator };
            }
            else
            {
                break;
            }
        }

        return declarator;
    }

    // Statement parsing
    private CCompoundStatement ParseCompoundStatement()
    {
        Consume(CTokenKind.LBrace, "Expected '{'");

        var declarations = new List<CDeclaration>();
        var statements = new List<CStatement>();

        while (!Check(CTokenKind.RBrace) && !IsAtEnd())
        {
            if (IsDeclaration())
            {
                declarations.Add(ParseDeclaration());
            }
            else
            {
                statements.Add(ParseStatement());
            }
        }

        Consume(CTokenKind.RBrace, "Expected '}'");
        return new CCompoundStatement(declarations, statements);
    }

    private CStatement ParseStatement()
    {
        return Current().Kind switch
        {
            CTokenKind.LBrace => ParseCompoundStatement(),
            CTokenKind.KwIf => ParseIfStatement(),
            CTokenKind.KwWhile => ParseWhileStatement(),
            CTokenKind.KwDo => ParseDoWhileStatement(),
            CTokenKind.KwFor => ParseForStatement(),
            CTokenKind.KwReturn => ParseReturnStatement(),
            CTokenKind.KwBreak => ParseBreakStatement(),
            CTokenKind.KwContinue => ParseContinueStatement(),
            CTokenKind.KwGoto => ParseGotoStatement(),
            CTokenKind.KwSwitch => ParseSwitchStatement(),
            CTokenKind.KwCase => ParseCaseStatement(),
            CTokenKind.KwDefault => ParseDefaultStatement(),
            CTokenKind.Semicolon => ParseEmptyStatement(),
            CTokenKind.Identifier when Peek().Kind == CTokenKind.Colon => ParseLabelStatement(),
            _ => ParseExpressionStatement(),
        };
    }

    private CStatement ParseIfStatement()
    {
        Consume(CTokenKind.KwIf, "Expected 'if'");
        Consume(CTokenKind.LParen, "Expected '(' after 'if'");
        var condition = ParseExpression();
        Consume(CTokenKind.RParen, "Expected ')' after condition");

        var thenBranch = ParseStatement();
        CStatement? elseBranch = null;

        if (Match(CTokenKind.KwElse))
        {
            elseBranch = ParseStatement();
        }

        return new CIfStatement(condition, thenBranch, elseBranch);
    }

    private CStatement ParseWhileStatement()
    {
        Consume(CTokenKind.KwWhile, "Expected 'while'");
        Consume(CTokenKind.LParen, "Expected '(' after 'while'");
        var condition = ParseExpression();
        Consume(CTokenKind.RParen, "Expected ')' after condition");

        var body = ParseStatement();
        return new CWhileStatement(condition, body);
    }

    private CStatement ParseDoWhileStatement()
    {
        Consume(CTokenKind.KwDo, "Expected 'do'");
        var body = ParseStatement();
        Consume(CTokenKind.KwWhile, "Expected 'while' after do body");
        Consume(CTokenKind.LParen, "Expected '(' after 'while'");
        var condition = ParseExpression();
        Consume(CTokenKind.RParen, "Expected ')' after condition");
        Consume(CTokenKind.Semicolon, "Expected ';' after do-while");

        return new CDoWhileStatement(body, condition);
    }

    private CStatement ParseForStatement()
    {
        Consume(CTokenKind.KwFor, "Expected 'for'");
        Consume(CTokenKind.LParen, "Expected '(' after 'for'");

        CNode? initializer = null;
        if (IsDeclaration())
        {
            initializer = ParseDeclaration();
        }
        else if (!Check(CTokenKind.Semicolon))
        {
            initializer = ParseExpression();
            Consume(CTokenKind.Semicolon, "Expected ';' after for initializer");
        }
        else
        {
            Advance();
        }

        CExpression? condition = null;
        if (!Check(CTokenKind.Semicolon))
        {
            condition = ParseExpression();
        }
        Consume(CTokenKind.Semicolon, "Expected ';' after for condition");

        CExpression? increment = null;
        if (!Check(CTokenKind.RParen))
        {
            increment = ParseExpression();
        }
        Consume(CTokenKind.RParen, "Expected ')' after for clauses");

        var body = ParseStatement();
        return new CForStatement(initializer, condition, increment, body);
    }

    private CStatement ParseReturnStatement()
    {
        Consume(CTokenKind.KwReturn, "Expected 'return'");
        CExpression? expr = null;

        if (!Check(CTokenKind.Semicolon))
        {
            expr = ParseExpression();
        }

        Consume(CTokenKind.Semicolon, "Expected ';' after return statement");
        return new CReturnStatement(expr);
    }

    private CStatement ParseBreakStatement()
    {
        Consume(CTokenKind.KwBreak, "Expected 'break'");
        Consume(CTokenKind.Semicolon, "Expected ';' after break");
        return new CBreakStatement();
    }

    private CStatement ParseContinueStatement()
    {
        Consume(CTokenKind.KwContinue, "Expected 'continue'");
        Consume(CTokenKind.Semicolon, "Expected ';' after continue");
        return new CContinueStatement();
    }

    private CStatement ParseGotoStatement()
    {
        Consume(CTokenKind.KwGoto, "Expected 'goto'");
        var label = Consume(CTokenKind.Identifier, "Expected label name").Text;
        Consume(CTokenKind.Semicolon, "Expected ';' after goto");
        return new CGotoStatement(label);
    }

    private CStatement ParseSwitchStatement()
    {
        Consume(CTokenKind.KwSwitch, "Expected 'switch'");
        Consume(CTokenKind.LParen, "Expected '(' after 'switch'");
        var condition = ParseExpression();
        Consume(CTokenKind.RParen, "Expected ')' after condition");

        var body = ParseStatement();
        return new CSwitchStatement(condition, body);
    }

    private CStatement ParseCaseStatement()
    {
        Consume(CTokenKind.KwCase, "Expected 'case'");
        var value = ParseExpression();
        Consume(CTokenKind.Colon, "Expected ':' after case value");
        var statement = ParseStatement();
        return new CCaseStatement(value, statement);
    }

    private CStatement ParseDefaultStatement()
    {
        Consume(CTokenKind.KwDefault, "Expected 'default'");
        Consume(CTokenKind.Colon, "Expected ':' after 'default'");
        var statement = ParseStatement();
        return new CDefaultStatement(statement);
    }

    private CStatement ParseEmptyStatement()
    {
        Consume(CTokenKind.Semicolon, "Expected ';'");
        return new CEmptyStatement();
    }

    private CStatement ParseLabelStatement()
    {
        var label = Consume(CTokenKind.Identifier, "Expected label").Text;
        Consume(CTokenKind.Colon, "Expected ':' after label");
        var statement = ParseStatement();
        return new CLabelStatement(label, statement);
    }

    private CStatement ParseExpressionStatement()
    {
        CExpression? expr = null;

        if (!Check(CTokenKind.Semicolon))
        {
            expr = ParseExpression();
        }

        Consume(CTokenKind.Semicolon, "Expected ';' after expression statement");
        return new CExpressionStatement(expr);
    }

    // Expression parsing (simplified, without full precedence handling)
    private CExpression ParseExpression()
    {
        return ParseAssignment();
    }

    private CExpression ParseAssignment()
    {
        var expr = ParseConditional();

        if (IsAssignmentOp())
        {
            var op = Current().Text;
            Advance();
            var right = ParseAssignment();
            return new CAssignment(expr, op, right);
        }

        return expr;
    }

    private bool IsAssignmentOp()
    {
        return Current().Kind switch
        {
            CTokenKind.Assign or CTokenKind.PlusAssign or CTokenKind.MinusAssign or
            CTokenKind.StarAssign or CTokenKind.SlashAssign or CTokenKind.PercentAssign or
            CTokenKind.AmpAssign or CTokenKind.PipeAssign or CTokenKind.CaretAssign or
            CTokenKind.LeftShiftAssign or CTokenKind.RightShiftAssign => true,
            _ => false,
        };
    }

    private CExpression ParseConditional()
    {
        var expr = ParseLogicalOr();

        if (Match(CTokenKind.Question))
        {
            var trueBranch = ParseExpression();
            Consume(CTokenKind.Colon, "Expected ':' in ternary operator");
            var falseBranch = ParseConditional();
            return new CConditional(expr, trueBranch, falseBranch);
        }

        return expr;
    }

    private CExpression ParseLogicalOr()
    {
        var expr = ParseLogicalAnd();

        while (Match(CTokenKind.LogicalOr))
        {
            var op = Previous().Text;
            var right = ParseLogicalAnd();
            expr = new CBinaryOp(expr, op, right);
        }

        return expr;
    }

    private CExpression ParseLogicalAnd()
    {
        var expr = ParseBitwiseOr();

        while (Match(CTokenKind.LogicalAnd))
        {
            var op = Previous().Text;
            var right = ParseBitwiseOr();
            expr = new CBinaryOp(expr, op, right);
        }

        return expr;
    }

    private CExpression ParseBitwiseOr()
    {
        var expr = ParseBitwiseXor();

        while (Check(CTokenKind.Pipe) && Peek().Kind != CTokenKind.Pipe)
        {
            Advance();
            var op = Previous().Text;
            var right = ParseBitwiseXor();
            expr = new CBinaryOp(expr, op, right);
        }

        return expr;
    }

    private CExpression ParseBitwiseXor()
    {
        var expr = ParseBitwiseAnd();

        while (Check(CTokenKind.Caret))
        {
            Advance();
            var op = Previous().Text;
            var right = ParseBitwiseAnd();
            expr = new CBinaryOp(expr, op, right);
        }

        return expr;
    }

    private CExpression ParseBitwiseAnd()
    {
        var expr = ParseEquality();

        while (Check(CTokenKind.Amp) && Peek().Kind != CTokenKind.LogicalAnd)
        {
            Advance();
            var op = Previous().Text;
            var right = ParseEquality();
            expr = new CBinaryOp(expr, op, right);
        }

        return expr;
    }

    private CExpression ParseEquality()
    {
        var expr = ParseRelational();

        while (Current().Kind is CTokenKind.Equal or CTokenKind.NotEqual)
        {
            var op = Current().Text;
            Advance();
            var right = ParseRelational();
            expr = new CBinaryOp(expr, op, right);
        }

        return expr;
    }

    private CExpression ParseRelational()
    {
        var expr = ParseShift();

        while (Current().Kind is CTokenKind.LessThan or CTokenKind.LessThanEq or
                                 CTokenKind.GreaterThan or CTokenKind.GreaterThanEq)
        {
            var op = Current().Text;
            Advance();
            var right = ParseShift();
            expr = new CBinaryOp(expr, op, right);
        }

        return expr;
    }

    private CExpression ParseShift()
    {
        var expr = ParseAdditive();

        while (Current().Kind is CTokenKind.LeftShift or CTokenKind.RightShift)
        {
            var op = Current().Text;
            Advance();
            var right = ParseAdditive();
            expr = new CBinaryOp(expr, op, right);
        }

        return expr;
    }

    private CExpression ParseAdditive()
    {
        var expr = ParseMultiplicative();

        while (Current().Kind is CTokenKind.Plus or CTokenKind.Minus)
        {
            var op = Current().Text;
            Advance();
            var right = ParseMultiplicative();
            expr = new CBinaryOp(expr, op, right);
        }

        return expr;
    }

    private CExpression ParseMultiplicative()
    {
        var expr = ParseUnary();

        while (Current().Kind is CTokenKind.Star or CTokenKind.Slash or CTokenKind.Percent)
        {
            var op = Current().Text;
            Advance();
            var right = ParseUnary();
            expr = new CBinaryOp(expr, op, right);
        }

        return expr;
    }

    private CExpression ParseUnary()
    {
        if (Current().Kind is CTokenKind.Increment or CTokenKind.Decrement or
                              CTokenKind.Bang or CTokenKind.Tilde or
                              CTokenKind.Plus or CTokenKind.Minus or
                              CTokenKind.Amp or CTokenKind.Star or
                              CTokenKind.KwSizeof)
        {
            var op = Current().Text;
            Advance();
            var operand = ParseUnary();
            return new CUnaryOp(op, operand, true);
        }

        return ParsePostfix();
    }

    private CExpression ParsePostfix()
    {
        var expr = ParsePrimary();

        while (true)
        {
            if (Match(CTokenKind.LBracket))
            {
                var index = ParseExpression();
                Consume(CTokenKind.RBracket, "Expected ']'");
                expr = new CArrayAccess(expr, index);
            }
            else if (Match(CTokenKind.LParen))
            {
                var args = new List<CExpression>();
                if (!Check(CTokenKind.RParen))
                {
                    args.Add(ParseExpression());
                    while (Match(CTokenKind.Comma))
                    {
                        args.Add(ParseExpression());
                    }
                }
                Consume(CTokenKind.RParen, "Expected ')' after arguments");
                expr = new CFunctionCall(expr, args);
            }
            else if (Match(CTokenKind.Dot))
            {
                var member = Consume(CTokenKind.Identifier, "Expected member name").Text;
                expr = new CMemberAccess(expr, member, false);
            }
            else if (Match(CTokenKind.Arrow))
            {
                var member = Consume(CTokenKind.Identifier, "Expected member name").Text;
                expr = new CMemberAccess(expr, member, true);
            }
            else if (Current().Kind is CTokenKind.Increment or CTokenKind.Decrement)
            {
                var op = Current().Text;
                Advance();
                expr = new CUnaryOp(op, expr, false);
            }
            else
            {
                break;
            }
        }

        return expr;
    }

    private CExpression ParsePrimary()
    {
        if (Match(CTokenKind.IntLiteral))
        {
            var value = long.Parse(Previous().Text);
            return new CIntLiteral(value);
        }

        if (Match(CTokenKind.FloatLiteral))
        {
            var value = double.Parse(Previous().Text);
            return new CFloatLiteral(value);
        }

        if (Match(CTokenKind.StringLiteral))
        {
            return new CStringLiteral(Previous().Text);
        }

        if (Match(CTokenKind.CharLiteral))
        {
            var value = Previous().Text;
            var charVal = value.Length > 0 ? value[0] : '\0';
            return new CCharLiteral(charVal);
        }

        if (Match(CTokenKind.Identifier))
        {
            return new CIdentifier(Previous().Text);
        }

        if (Match(CTokenKind.LParen))
        {
            var expr = ParseExpression();
            Consume(CTokenKind.RParen, "Expected ')' after expression");
            return new CParenthesized(expr);
        }

        // Fallback
        return new CIdentifier(Current().Text);
    }

    // Helper methods
    private CToken Current() => _pos < _tokens.Count ? _tokens[_pos] : _tokens[_tokens.Count - 1];
    private CToken Previous() => _tokens[_pos - 1];
    private CToken Peek() => _pos + 1 < _tokens.Count ? _tokens[_pos + 1] : Current();

    private bool IsAtEnd() => _pos >= _tokens.Count || _tokens[_pos].Kind == CTokenKind.EOF;

    private bool Check(CTokenKind kind) => !IsAtEnd() && Current().Kind == kind;

    private bool Match(CTokenKind kind)
    {
        if (!Check(kind)) return false;
        Advance();
        return true;
    }

    private CToken Consume(CTokenKind kind, string message)
    {
        if (Check(kind))
        {
            var token = Current();
            Advance();
            return token;
        }

        throw new Exception($"{message} (Got {Current().Kind})");
    }

    private void Advance()
    {
        if (!IsAtEnd()) _pos++;
    }

    private void SkipToMatching(CTokenKind open, CTokenKind close)
    {
        var depth = 0;
        while (!IsAtEnd())
        {
            if (Current().Kind == open) depth++;
            else if (Current().Kind == close)
            {
                if (depth == 0) break;
                depth--;
            }
            Advance();
        }
    }
}
