using System.Text;

namespace OIC;

public enum CTokenKind
{
    EOF,
    Identifier,
    IntLiteral,
    FloatLiteral,
    StringLiteral,
    CharLiteral,
    
    // Keywords
    KwAuto,
    KwBreak,
    KwCase,
    KwChar,
    KwConst,
    KwContinue,
    KwDefault,
    KwDo,
    KwDouble,
    KwElse,
    KwEnum,
    KwExtern,
    KwFloat,
    KwFor,
    KwGoto,
    KwIf,
    KwInline,
    KwInt,
    KwLong,
    KwRegister,
    KwRestrict,
    KwReturn,
    KwShort,
    KwSigned,
    KwSizeof,
    KwStatic,
    KwStruct,
    KwTypedef,
    KwUnion,
    KwUnsigned,
    KwVoid,
    KwVolatile,
    KwWhile,
    KwSwitch,
    
    // Operators and Punctuation
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Semicolon,
    Comma,
    Dot,
    Arrow,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Amp,
    Pipe,
    Caret,
    Tilde,
    Bang,
    Question,
    Colon,
    Assign,
    PlusAssign,
    MinusAssign,
    StarAssign,
    SlashAssign,
    PercentAssign,
    AmpAssign,
    PipeAssign,
    CaretAssign,
    LeftShift,
    RightShift,
    LeftShiftAssign,
    RightShiftAssign,
    Increment,
    Decrement,
    LogicalAnd,
    LogicalOr,
    LessThan,
    LessThanEq,
    GreaterThan,
    GreaterThanEq,
    Equal,
    NotEqual,
    Ellipsis,
}

public record CToken(CTokenKind Kind, string Text, int Position, int Length, int Line, int Column);

public class CLexer
{
    private readonly string _src;
    private int _pos;
    private int _line = 1;
    private int _col = 1;

    private static readonly Dictionary<string, CTokenKind> _keywords = new()
    {
        ["auto"] = CTokenKind.KwAuto,
        ["break"] = CTokenKind.KwBreak,
        ["case"] = CTokenKind.KwCase,
        ["char"] = CTokenKind.KwChar,
        ["const"] = CTokenKind.KwConst,
        ["continue"] = CTokenKind.KwContinue,
        ["default"] = CTokenKind.KwDefault,
        ["do"] = CTokenKind.KwDo,
        ["double"] = CTokenKind.KwDouble,
        ["else"] = CTokenKind.KwElse,
        ["enum"] = CTokenKind.KwEnum,
        ["extern"] = CTokenKind.KwExtern,
        ["float"] = CTokenKind.KwFloat,
        ["for"] = CTokenKind.KwFor,
        ["goto"] = CTokenKind.KwGoto,
        ["if"] = CTokenKind.KwIf,
        ["inline"] = CTokenKind.KwInline,
        ["int"] = CTokenKind.KwInt,
        ["long"] = CTokenKind.KwLong,
        ["register"] = CTokenKind.KwRegister,
        ["restrict"] = CTokenKind.KwRestrict,
        ["return"] = CTokenKind.KwReturn,
        ["short"] = CTokenKind.KwShort,
        ["signed"] = CTokenKind.KwSigned,
        ["sizeof"] = CTokenKind.KwSizeof,
        ["static"] = CTokenKind.KwStatic,
        ["struct"] = CTokenKind.KwStruct,
        ["typedef"] = CTokenKind.KwTypedef,
        ["union"] = CTokenKind.KwUnion,
        ["unsigned"] = CTokenKind.KwUnsigned,
        ["void"] = CTokenKind.KwVoid,
        ["volatile"] = CTokenKind.KwVolatile,
        ["while"] = CTokenKind.KwWhile,
        ["switch"] = CTokenKind.KwSwitch,
    };

    public CLexer(string src) => _src = src;

    public IEnumerable<CToken> Lex()
    {
        CToken t;
        do
        {
            t = NextToken();
            yield return t;
        } while (t.Kind != CTokenKind.EOF);
    }

    private bool Eof => _pos >= _src.Length;
    private char Current => Eof ? '\0' : _src[_pos];
    private char Peek(int offset = 1) => _pos + offset >= _src.Length ? '\0' : _src[_pos + offset];

    private void Advance()
    {
        if (!Eof)
        {
            if (_src[_pos] == '\n')
            {
                _line++;
                _col = 1;
            }
            else
            {
                _col++;
            }
            _pos++;
        }
    }

    private bool Match(char expected)
    {
        if (Current != expected) return false;
        Advance();
        return true;
    }

    private CToken Make(CTokenKind kind, string text, int length)
    {
        var start = _pos - length;
        return new CToken(kind, text, start, length, _line, _col - length);
    }

    private CToken Single(CTokenKind kind)
    {
        var ch = Current;
        Advance();
        return Make(kind, ch.ToString(), 1);
    }

    private void SkipWhitespace()
    {
        while (!Eof && char.IsWhiteSpace(Current))
            Advance();
    }

    private void SkipLineComment()
    {
        // Skip //
        Advance();
        Advance();
        while (!Eof && Current != '\n')
            Advance();
    }

    private void SkipBlockComment()
    {
        // Skip /*
        Advance();
        Advance();
        while (!Eof)
        {
            if (Current == '*' && Peek() == '/')
            {
                Advance();
                Advance();
                break;
            }
            Advance();
        }
    }

    private CToken NextToken()
    {
        SkipWhitespace();

        // Handle comments
        while (!Eof && Current == '/')
        {
            if (Peek() == '/')
            {
                SkipLineComment();
                SkipWhitespace();
            }
            else if (Peek() == '*')
            {
                SkipBlockComment();
                SkipWhitespace();
            }
            else
            {
                break;
            }
        }

        if (Eof) return Make(CTokenKind.EOF, string.Empty, 0);

        var ch = Current;

        // Identifiers and keywords
        if (char.IsLetter(ch) || ch == '_')
            return LexIdentifier();

        // Numbers
        if (char.IsDigit(ch))
            return LexNumber();

        // Strings
        if (ch == '"')
            return LexString();

        // Character literals
        if (ch == '\'')
            return LexChar();

        // Multi-character operators and single-character tokens
        return ch switch
        {
            '(' => Single(CTokenKind.LParen),
            ')' => Single(CTokenKind.RParen),
            '{' => Single(CTokenKind.LBrace),
            '}' => Single(CTokenKind.RBrace),
            '[' => Single(CTokenKind.LBracket),
            ']' => Single(CTokenKind.RBracket),
            ';' => Single(CTokenKind.Semicolon),
            ',' => Single(CTokenKind.Comma),
            '?' => Single(CTokenKind.Question),
            '~' => Single(CTokenKind.Tilde),
            '^' => LexCaretOrCaretAssign(),
            '%' => LexPercentOrPercentAssign(),
            ':' => Single(CTokenKind.Colon),
            '+' => LexPlusFamily(),
            '-' => LexMinusFamily(),
            '*' => LexStarOrStarAssign(),
            '/' => LexSlashOrSlashAssign(),
            '&' => LexAmpFamily(),
            '|' => LexPipeFamily(),
            '!' => LexBangOrNotEqual(),
            '=' => LexEqualFamily(),
            '<' => LexLessFamily(),
            '>' => LexGreaterFamily(),
            '.' => LexDotOrEllipsis(),
            _ => Single(CTokenKind.EOF), // error fallback
        };
    }

    private CToken LexIdentifier()
    {
        var start = _pos;
        var line = _line;
        var col = _col;

        while (!Eof && (char.IsLetterOrDigit(Current) || Current == '_'))
            Advance();

        var text = _src[start.._pos];
        var length = _pos - start;

        if (_keywords.TryGetValue(text, out var kw))
            return Make(kw, text, length);

        return Make(CTokenKind.Identifier, text, length);
    }

    private CToken LexNumber()
    {
        var start = _pos;
        var line = _line;
        var col = _col;

        // Handle hex, octal, or decimal
        if (Current == '0' && (Peek() == 'x' || Peek() == 'X'))
        {
            Advance();
            Advance();
            while (!Eof && char.IsDigit(Current) || (char.IsLetter(Current) && "abcdefABCDEF".Contains(Current)))
                Advance();
        }
        else
        {
            while (!Eof && char.IsDigit(Current))
                Advance();

            // Check for float
            if (!Eof && Current == '.' && char.IsDigit(Peek()))
            {
                Advance();
                while (!Eof && char.IsDigit(Current))
                    Advance();
            }

            // Handle exponent
            if (!Eof && (Current == 'e' || Current == 'E'))
            {
                Advance();
                if (!Eof && (Current == '+' || Current == '-'))
                    Advance();
                while (!Eof && char.IsDigit(Current))
                    Advance();
            }
        }

        // Suffixes (f, l, u, etc.)
        while (!Eof && char.IsLetter(Current))
            Advance();

        var text = _src[start.._pos];
        var length = _pos - start;
        var kind = text.Contains('.') || text.Contains('e') || text.Contains('E')
            ? CTokenKind.FloatLiteral
            : CTokenKind.IntLiteral;

        return Make(kind, text, length);
    }

    private CToken LexString()
    {
        var start = _pos;
        var line = _line;
        var col = _col;

        Advance(); // opening quote
        var sb = new StringBuilder();

        while (!Eof && Current != '"')
        {
            if (Current == '\\')
            {
                Advance();
                if (!Eof)
                {
                    var esc = Current;
                    Advance();
                    sb.Append(esc switch
                    {
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        '\\' => '\\',
                        '"' => '"',
                        _ => esc,
                    });
                }
            }
            else
            {
                sb.Append(Current);
                Advance();
            }
        }

        if (!Eof) Advance(); // closing quote

        var text = _src[start.._pos];
        var length = _pos - start;
        return Make(CTokenKind.StringLiteral, text, length);
    }

    private CToken LexChar()
    {
        var start = _pos;
        var line = _line;
        var col = _col;

        Advance(); // opening quote
        var sb = new StringBuilder();

        while (!Eof && Current != '\'')
        {
            if (Current == '\\')
            {
                Advance();
                if (!Eof)
                {
                    var esc = Current;
                    Advance();
                    sb.Append(esc switch
                    {
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        '\\' => '\\',
                        '\'' => '\'',
                        _ => esc,
                    });
                }
            }
            else
            {
                sb.Append(Current);
                Advance();
            }
        }

        if (!Eof) Advance(); // closing quote

        var text = _src[start.._pos];
        var length = _pos - start;
        return Make(CTokenKind.CharLiteral, text, length);
    }

    private CToken LexPlusFamily()
    {
        Advance();
        if (Current == '+')
        {
            Advance();
            return Make(CTokenKind.Increment, "++", 2);
        }
        if (Current == '=')
        {
            Advance();
            return Make(CTokenKind.PlusAssign, "+=", 2);
        }
        return Make(CTokenKind.Plus, "+", 1);
    }

    private CToken LexMinusFamily()
    {
        Advance();
        if (Current == '-')
        {
            Advance();
            return Make(CTokenKind.Decrement, "--", 2);
        }
        if (Current == '=')
        {
            Advance();
            return Make(CTokenKind.MinusAssign, "-=", 2);
        }
        if (Current == '>')
        {
            Advance();
            return Make(CTokenKind.Arrow, "->", 2);
        }
        return Make(CTokenKind.Minus, "-", 1);
    }

    private CToken LexStarOrStarAssign()
    {
        Advance();
        if (Current == '=')
        {
            Advance();
            return Make(CTokenKind.StarAssign, "*=", 2);
        }
        return Make(CTokenKind.Star, "*", 1);
    }

    private CToken LexSlashOrSlashAssign()
    {
        Advance();
        if (Current == '=')
        {
            Advance();
            return Make(CTokenKind.SlashAssign, "/=", 2);
        }
        return Make(CTokenKind.Slash, "/", 1);
    }

    private CToken LexPercentOrPercentAssign()
    {
        Advance();
        if (Current == '=')
        {
            Advance();
            return Make(CTokenKind.PercentAssign, "%=", 2);
        }
        return Make(CTokenKind.Percent, "%", 1);
    }

    private CToken LexAmpFamily()
    {
        Advance();
        if (Current == '&')
        {
            Advance();
            return Make(CTokenKind.LogicalAnd, "&&", 2);
        }
        if (Current == '=')
        {
            Advance();
            return Make(CTokenKind.AmpAssign, "&=", 2);
        }
        return Make(CTokenKind.Amp, "&", 1);
    }

    private CToken LexPipeFamily()
    {
        Advance();
        if (Current == '|')
        {
            Advance();
            return Make(CTokenKind.LogicalOr, "||", 2);
        }
        if (Current == '=')
        {
            Advance();
            return Make(CTokenKind.PipeAssign, "|=", 2);
        }
        return Make(CTokenKind.Pipe, "|", 1);
    }

    private CToken LexCaretOrCaretAssign()
    {
        Advance();
        if (Current == '=')
        {
            Advance();
            return Make(CTokenKind.CaretAssign, "^=", 2);
        }
        return Make(CTokenKind.Caret, "^", 1);
    }

    private CToken LexBangOrNotEqual()
    {
        Advance();
        if (Current == '=')
        {
            Advance();
            return Make(CTokenKind.NotEqual, "!=", 2);
        }
        return Make(CTokenKind.Bang, "!", 1);
    }

    private CToken LexEqualFamily()
    {
        Advance();
        if (Current == '=')
        {
            Advance();
            return Make(CTokenKind.Equal, "==", 2);
        }
        return Make(CTokenKind.Assign, "=", 1);
    }

    private CToken LexLessFamily()
    {
        Advance();
        if (Current == '<')
        {
            Advance();
            if (Current == '=')
            {
                Advance();
                return Make(CTokenKind.LeftShiftAssign, "<<=", 3);
            }
            return Make(CTokenKind.LeftShift, "<<", 2);
        }
        if (Current == '=')
        {
            Advance();
            return Make(CTokenKind.LessThanEq, "<=", 2);
        }
        return Make(CTokenKind.LessThan, "<", 1);
    }

    private CToken LexGreaterFamily()
    {
        Advance();
        if (Current == '>')
        {
            Advance();
            if (Current == '=')
            {
                Advance();
                return Make(CTokenKind.RightShiftAssign, ">>=", 3);
            }
            return Make(CTokenKind.RightShift, ">>", 2);
        }
        if (Current == '=')
        {
            Advance();
            return Make(CTokenKind.GreaterThanEq, ">=", 2);
        }
        return Make(CTokenKind.GreaterThan, ">", 1);
    }

    private CToken LexDotOrEllipsis()
    {
        Advance();
        if (Current == '.' && Peek() == '.')
        {
            Advance();
            Advance();
            return Make(CTokenKind.Ellipsis, "...", 3);
        }
        return Make(CTokenKind.Dot, ".", 1);
    }
}
