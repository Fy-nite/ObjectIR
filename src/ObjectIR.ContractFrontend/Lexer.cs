using System.Text;

namespace ObjectIR.ContractFrontend;

public enum TokenKind
{
    EOF,
    Identifier,
    Int,
    String,
    LParen, RParen, LBrace, RBrace, Comma, Semicolon, Colon,
    Plus, Minus, Less, LessEq, EqEq, Assign,
    Contract, Fn, Var, If, Else, While, Return,
}

public record Token(TokenKind Kind, string Text, int Position, int Length, int Line, int Column);

public class Lexer
{
    private readonly string _src;
    private int _pos;
    private int _line = 1;
    private int _col = 1;

    private static readonly Dictionary<string, TokenKind> _keywords = new()
    {
        ["Contract"] = TokenKind.Contract,
        ["fn"] = TokenKind.Fn,
        ["var"] = TokenKind.Var,
        ["if"] = TokenKind.If,
        ["else"] = TokenKind.Else,
        ["while"] = TokenKind.While,
        ["return"] = TokenKind.Return,
    };

    public Lexer(string src) => _src = src;

    public IEnumerable<Token> Lex()
    {
        Token t;
        do { t = NextToken(); yield return t; } while (t.Kind != TokenKind.EOF);
    }

    private Token NextToken()
    {
        SkipWhitespaceAndComments();
        if (Eof) return Make(TokenKind.EOF, string.Empty, 0);
        var ch = Current;
        if (char.IsLetter(ch) || ch == '_') return LexIdentifier();
        if (char.IsDigit(ch)) return LexInt();
        if (ch == '"') return LexString();
        return ch switch
        {
            '(' => Single(TokenKind.LParen),
            ')' => Single(TokenKind.RParen),
            '{' => Single(TokenKind.LBrace),
            '}' => Single(TokenKind.RBrace),
            ',' => Single(TokenKind.Comma),
            ';' => Single(TokenKind.Semicolon),
            ':' => Single(TokenKind.Colon),
            '+' => Single(TokenKind.Plus),
            '-' => Single(TokenKind.Minus),
            '=' => Match('=') ? Make(TokenKind.EqEq, "==", 2) : Single(TokenKind.Assign),
            '<' => Match('=') ? Make(TokenKind.LessEq, "<=", 2) : Single(TokenKind.Less),
            _ => Single(TokenKind.EOF) // error fallback
        };
    }

    private Token LexIdentifier()
    {
        var start = _pos; var line = _line; var col = _col;
        while (!Eof && (char.IsLetterOrDigit(Current) || Current == '_' || Current=='.')) Advance();
        var text = _src[start.._pos];
        if (_keywords.TryGetValue(text, out var kw)) return new Token(kw, text, start, _pos-start, line, col);
        return new Token(TokenKind.Identifier, text, start, _pos-start, line, col);
    }

    private Token LexInt()
    {
        var start = _pos; var line = _line; var col = _col;
        while (!Eof && char.IsDigit(Current)) Advance();
        var text = _src[start.._pos];
        return new Token(TokenKind.Int, text, start, _pos-start, line, col);
    }

    private Token LexString()
    {
        var start = _pos; var line = _line; var col = _col;
        Advance(); // opening quote
        var sb = new StringBuilder();
        while (!Eof && Current != '"')
        {
            if (Current == '\\')
            {
                Advance();
                if (Eof) break;
                var esc = Current; Advance();
                sb.Append(esc switch
                {
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    '"' => '"',
                    '\\' => '\\',
                    _ => esc
                });
            }
            else { sb.Append(Current); Advance(); }
        }
        if (!Eof && Current == '"') Advance();
        var text = sb.ToString();
        return new Token(TokenKind.String, text, start, _pos-start, line, col);
    }

    private void SkipWhitespaceAndComments()
    {
        while (!Eof)
        {
            if (char.IsWhiteSpace(Current)) { Advance(); continue; }
            if (Current=='/' && Peek=='/') { while (!Eof && Current!='\n') Advance(); continue; }
            break;
        }
    }

    private Token Single(TokenKind kind)
    { var ch = Current; var line=_line; var col=_col; Advance(); return new Token(kind, ch.ToString(), _pos-1,1,line,col); }
    private Token Make(TokenKind kind,string text,int len) => new(kind,text,_pos-len,len,_line,_col-len);
    private bool Match(char c){ if (!Eof && Peek==c){ Advance(); Advance(); return true;} return false; }
    private void Advance(){ if (Eof) return; if (Current=='\n'){ _line++; _col=1;} else _col++; _pos++; }
    private char Current => _pos < _src.Length ? _src[_pos] : '\0';
    private char Peek => _pos+1 < _src.Length ? _src[_pos+1] : '\0';
    private bool Eof => _pos >= _src.Length;
}
