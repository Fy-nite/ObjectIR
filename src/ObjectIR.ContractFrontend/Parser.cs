using ObjectIR.ContractFrontend.AST;

namespace ObjectIR.ContractFrontend;

public class Parser
{
    private readonly List<Token> _tokens;
    private int _idx;

    public Parser(IEnumerable<Token> tokens) => _tokens = tokens.ToList();

    private Token Current => _idx < _tokens.Count ? _tokens[_idx] : _tokens[^1];
    private bool At(TokenKind kind) => Current.Kind == kind;
    private Token Consume(TokenKind kind, string message)
    { if (!At(kind)) throw new Exception($"Parse error at {Current.Line}:{Current.Column} expected {kind} {message}"); return _tokens[_idx++]; }
    private bool Try(TokenKind kind){ if (At(kind)){ _idx++; return true;} return false; }

    public ContractUnit Parse()
    {
        var contracts = new List<ContractDecl>();
        while (!At(TokenKind.EOF))
        {
            if (At(TokenKind.Contract)) contracts.Add(ParseContract()); else _idx++; // skip unexpected
        }
        return new ContractUnit(contracts);
    }

    private ContractDecl ParseContract()
    {
        var kw = Consume(TokenKind.Contract, "");
        var name = Consume(TokenKind.Identifier, "contract name");
        Consume(TokenKind.LBrace, "after contract name");
        var funcs = new List<FunctionDecl>();
        while (!At(TokenKind.RBrace) && !At(TokenKind.EOF))
        {
            if (At(TokenKind.Fn)) funcs.Add(ParseFunction()); else _idx++; // skip
        }
        Consume(TokenKind.RBrace, "close contract");
        return new ContractDecl(name.Text, funcs, new SourceSpan(kw.Position, name.Position+name.Length-kw.Position, kw.Line, kw.Column));
    }

    private FunctionDecl ParseFunction()
    {
        var fn = Consume(TokenKind.Fn, "");
        var name = Consume(TokenKind.Identifier, "function name");
        Consume(TokenKind.LParen, "after function name");
        var parameters = new List<ParameterDecl>();
        if (!At(TokenKind.RParen))
        {
            do {
                var p = Consume(TokenKind.Identifier, "parameter name");
                parameters.Add(new ParameterDecl(p.Text, new SourceSpan(p.Position,p.Length,p.Line,p.Column)));
            } while (Try(TokenKind.Comma));
        }
        Consume(TokenKind.RParen, ")");
        var body = ParseBlock();
        return new FunctionDecl(name.Text, parameters, body, new SourceSpan(fn.Position, body.Span.Start+body.Span.Length-fn.Position, fn.Line, fn.Column));
    }

    private BlockStmt ParseBlock()
    {
        var lbrace = Consume(TokenKind.LBrace, "block start");
        var stmts = new List<Stmt>();
        while (!At(TokenKind.RBrace) && !At(TokenKind.EOF))
        {
            stmts.Add(ParseStatement());
        }
        var r = Consume(TokenKind.RBrace, "block end");
        return new BlockStmt(stmts, new SourceSpan(lbrace.Position, r.Position+r.Length-lbrace.Position, lbrace.Line, lbrace.Column));
    }

    private Stmt ParseStatement()
    {
        if (At(TokenKind.Var)) return ParseVarDecl();
        if (At(TokenKind.If)) return ParseIf();
        if (At(TokenKind.While)) return ParseWhile();
        if (At(TokenKind.Return)) return ParseReturn();
        return ParseExprStatement();
    }

    private Stmt ParseVarDecl()
    {
        var varTok = Consume(TokenKind.Var, "");
        var name = Consume(TokenKind.Identifier, "var name");
        Expr? init = null;
        if (Try(TokenKind.Assign)) init = ParseExpression();
        Consume(TokenKind.Semicolon, "; after var decl");
        return new VarDeclStmt(name.Text, init, new SourceSpan(varTok.Position, name.Position+name.Length-varTok.Position, varTok.Line, varTok.Column));
    }

    private Stmt ParseIf()
    {
        var kw = Consume(TokenKind.If, "");
        Consume(TokenKind.LParen, "after if");
        var cond = ParseExpression();
        Consume(TokenKind.RParen, ") after if condition");
        var thenBlock = ParseBlock();
        BlockStmt? elseBlock = null;
        if (Try(TokenKind.Else))
        {
            if (At(TokenKind.LBrace)) elseBlock = ParseBlock();
            else // single statement wrapped
            {
                var single = ParseStatement();
                elseBlock = new BlockStmt(new List<Stmt>{single}, single.Span);
            }
        }
        return new IfStmt(cond, thenBlock, elseBlock, new SourceSpan(kw.Position, thenBlock.Span.Start+thenBlock.Span.Length-kw.Position, kw.Line, kw.Column));
    }

    private Stmt ParseWhile()
    {
        var kw = Consume(TokenKind.While, "");
        Consume(TokenKind.LParen, "after while");
        var cond = ParseExpression();
        Consume(TokenKind.RParen, ") after while condition");
        var body = ParseBlock();
        return new WhileStmt(cond, body, new SourceSpan(kw.Position, body.Span.Start+body.Span.Length-kw.Position, kw.Line, kw.Column));
    }

    private Stmt ParseReturn()
    {
        var kw = Consume(TokenKind.Return, "");
        Expr? expr = null;
        if (!At(TokenKind.Semicolon)) expr = ParseExpression();
        Consume(TokenKind.Semicolon, "; after return");
        return new ReturnStmt(expr, new SourceSpan(kw.Position, kw.Length + (expr?.Span.Length ?? 0), kw.Line, kw.Column));
    }

    private Stmt ParseExprStatement()
    {
        var expr = ParseExpression();
        Consume(TokenKind.Semicolon, "; after expression");
        return new ExprStmt(expr, expr.Span);
    }

    // Expression grammar precedence: + - < <= ==
    private Expr ParseExpression() => ParseEquality();
    private Expr ParseEquality()
    {
        var left = ParseRelational();
        while (At(TokenKind.EqEq))
        {
            var op = Current; _idx++;
            var right = ParseRelational();
            left = new BinaryExpr(op.Text, left, right, left.Span);
        }
        return left;
    }
    private Expr ParseRelational()
    {
        var left = ParseAdditive();
        while (At(TokenKind.Less) || At(TokenKind.LessEq))
        {
            var op = Current; _idx++;
            var right = ParseAdditive();
            left = new BinaryExpr(op.Text, left, right, left.Span);
        }
        return left;
    }
    private Expr ParseAdditive()
    {
        var left = ParsePrimary();
        while (At(TokenKind.Plus) || At(TokenKind.Minus))
        {
            var op = Current; _idx++;
            var right = ParsePrimary();
            left = new BinaryExpr(op.Text, left, right, left.Span);
        }
        return left;
    }
    private Expr ParsePrimary()
    {
        if (At(TokenKind.Int)) { var t = Current; _idx++; return new IntLiteralExpr(int.Parse(t.Text), new SourceSpan(t.Position,t.Length,t.Line,t.Column)); }
        if (At(TokenKind.String)) { var t = Current; _idx++; return new StringLiteralExpr(t.Text, new SourceSpan(t.Position,t.Length,t.Line,t.Column)); }
        if (At(TokenKind.Identifier))
        {
            var id = Current; _idx++;
            Expr expr = new IdentifierExpr(id.Text, new SourceSpan(id.Position,id.Length,id.Line,id.Column));
            if (At(TokenKind.LParen))
            {
                _idx++; // (
                var args = new List<Expr>();
                if (!At(TokenKind.RParen))
                {
                    do { args.Add(ParseExpression()); } while (Try(TokenKind.Comma));
                }
                Consume(TokenKind.RParen, ") after call arguments");
                expr = new CallExpr(expr, args, expr.Span);
            }
            return expr;
        }
        if (Try(TokenKind.LParen))
        {
            var inner = ParseExpression();
            Consume(TokenKind.RParen, ") after grouping");
            return inner;
        }
        var fallback = Current; _idx++;
        return new IdentifierExpr("<error>", new SourceSpan(fallback.Position,fallback.Length,fallback.Line,fallback.Column));
    }
}
