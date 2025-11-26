using System;
using System.Collections.Generic;

namespace ObjectIR.CSharpFrontend;

/// <summary>
/// Token kinds for C# 6 lexer
/// </summary>
public enum TokenKind
{
    // Special
    EOF,
    Whitespace,
    Comment,

    // Literals
    IntLiteral,
    FloatLiteral,
    StringLiteral,
    CharLiteral,
    BoolLiteral,

    // Identifiers
    Identifier,

    // Keywords
    Namespace,
    Using,
    Class,
    Interface,
    Struct,
    Enum,
    Void,
    Public,
    Private,
    Protected,
    Internal,
    Static,
    Abstract,
    Virtual,
    Override,
    Sealed,
    Partial,
    Readonly,
    Const,
    New,
    This,
    Base,
    Null,
    True,
    False,
    Return,
    If,
    Else,
    For,
    Foreach,
    While,
    Do,
    Break,
    Continue,
    Switch,
    Case,
    Default,
    Try,
    Catch,
    Finally,
    Throw,
    Out,
    RefKeyword,
    Params,
    Get,
    Set,
    Add,
    Remove,
    Where,

    // Operators
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Ampersand,
    Pipe,
    Caret,
    Tilde,
    Bang,
    Question,
    Colon,
    DoubleColon,
    Semicolon,
    Comma,
    Dot,
    Arrow,
    FatArrow,
    DoubleDot,
    DoubleAmpersand,
    DoublePipe,
    EqualEqual,
    BangEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    PlusEqual,
    MinusEqual,
    StarEqual,
    SlashEqual,
    PercentEqual,
    AmpersandEqual,
    PipeEqual,
    CaretEqual,
    LeftShift,
    RightShift,
    LeftShiftEqual,
    RightShiftEqual,
    QuestionDot,
    DoubleMinus,
    DoublePlus,
    PlusPlus,
    MinusMinus,
    Assign,

    // Delimiters
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    LessThan,
    GreaterThan,
}

/// <summary>
/// Represents a single token in the source code
/// </summary>
public record Token(
    TokenKind Kind,
    string Text,
    int Position,
    int Line,
    int Column,
    object? Value = null
)
{
    public override string ToString() =>
        $"{Kind}({Text}) @{Line}:{Column}";
}

/// <summary>
/// Lexer for C# 6 source code
/// </summary>
public class CSharpLexer
{
    private readonly string _source;
    private int _position = 0;
    private int _line = 1;
    private int _column = 1;

    private static readonly Dictionary<string, TokenKind> Keywords = new()
    {
        ["namespace"] = TokenKind.Namespace,
        ["using"] = TokenKind.Using,
        ["class"] = TokenKind.Class,
        ["interface"] = TokenKind.Interface,
        ["struct"] = TokenKind.Struct,
        ["enum"] = TokenKind.Enum,
        ["void"] = TokenKind.Void,
        ["public"] = TokenKind.Public,
        ["private"] = TokenKind.Private,
        ["protected"] = TokenKind.Protected,
        ["internal"] = TokenKind.Internal,
        ["static"] = TokenKind.Static,
        ["abstract"] = TokenKind.Abstract,
        ["virtual"] = TokenKind.Virtual,
        ["override"] = TokenKind.Override,
        ["sealed"] = TokenKind.Sealed,
        ["partial"] = TokenKind.Partial,
        ["readonly"] = TokenKind.Readonly,
        ["const"] = TokenKind.Const,
        ["new"] = TokenKind.New,
        ["this"] = TokenKind.This,
        ["base"] = TokenKind.Base,
        ["null"] = TokenKind.Null,
        ["true"] = TokenKind.True,
        ["false"] = TokenKind.False,
        ["return"] = TokenKind.Return,
        ["if"] = TokenKind.If,
        ["else"] = TokenKind.Else,
        ["for"] = TokenKind.For,
        ["foreach"] = TokenKind.Foreach,
        ["while"] = TokenKind.While,
        ["do"] = TokenKind.Do,
        ["break"] = TokenKind.Break,
        ["continue"] = TokenKind.Continue,
        ["switch"] = TokenKind.Switch,
        ["case"] = TokenKind.Case,
        ["default"] = TokenKind.Default,
        ["try"] = TokenKind.Try,
        ["catch"] = TokenKind.Catch,
        ["finally"] = TokenKind.Finally,
        ["throw"] = TokenKind.Throw,
        ["out"] = TokenKind.Out,
        ["ref"] = TokenKind.RefKeyword,
        ["params"] = TokenKind.Params,
        ["get"] = TokenKind.Get,
        ["set"] = TokenKind.Set,
        ["add"] = TokenKind.Add,
        ["remove"] = TokenKind.Remove,
        ["where"] = TokenKind.Where,
    };

    public CSharpLexer(string source)
    {
        _source = source ?? throw new ArgumentNullException(nameof(source));
    }

    /// <summary>
    /// Tokenize the entire source and return all tokens
    /// </summary>
    public List<Token> Tokenize()
    {
        var tokens = new List<Token>();
        Token token;
        do
        {
            token = NextToken();
            // Skip whitespace and comments in the token stream
            if (token.Kind != TokenKind.Whitespace && token.Kind != TokenKind.Comment)
            {
                tokens.Add(token);
            }
        } while (token.Kind != TokenKind.EOF);
        return tokens;
    }

    /// <summary>
    /// Get the next token from the source
    /// </summary>
    private Token NextToken()
    {
        SkipWhitespace();

        if (IsAtEnd)
            return MakeToken(TokenKind.EOF, "");

        // Comments
        if (Current == '/' && Peek() == '/')
        {
            return ScanLineComment();
        }

        if (Current == '/' && Peek() == '*')
        {
            return ScanBlockComment();
        }

        // String literals
        if (Current == '"')
            return ScanString();

        // Char literals
        if (Current == '\'')
            return ScanChar();

        // Numbers
        if (char.IsDigit(Current))
            return ScanNumber();

        // Identifiers and keywords
        if (char.IsLetter(Current) || Current == '_')
            return ScanIdentifierOrKeyword();

        // Operators and delimiters
        return ScanOperatorOrDelimiter();
    }

    private Token ScanLineComment()
    {
        var start = _position;
        Advance(); // /
        Advance(); // /
        while (!IsAtEnd && Current != '\n')
            Advance();
        return MakeToken(TokenKind.Comment, _source[start.._position]);
    }

    private Token ScanBlockComment()
    {
        var start = _position;
        Advance(); // /
        Advance(); // *
        while (!IsAtEnd && !(Current == '*' && Peek() == '/'))
        {
            if (Current == '\n')
            {
                _line++;
                _column = 0;
            }
            Advance();
        }
        if (!IsAtEnd)
        {
            Advance(); // *
            Advance(); // /
        }
        return MakeToken(TokenKind.Comment, _source[start.._position]);
    }

    private Token ScanString()
    {
        var start = _position;
        var startCol = _column;
        Advance(); // opening quote
        var chars = new System.Text.StringBuilder();

        while (!IsAtEnd && Current != '"')
        {
            if (Current == '\\' && Peek() == '"')
            {
                chars.Append('"');
                Advance();
                Advance();
            }
            else if (Current == '\\')
            {
                chars.Append(Current);
                Advance();
                if (!IsAtEnd)
                {
                    chars.Append(Current);
                    Advance();
                }
            }
            else
            {
                if (Current == '\n')
                {
                    _line++;
                    _column = 0;
                }
                chars.Append(Current);
                Advance();
            }
        }

        if (IsAtEnd)
            throw new LexerException($"Unterminated string at {_line}:{startCol}");

        Advance(); // closing quote
        return MakeToken(TokenKind.StringLiteral, _source[start.._position], chars.ToString());
    }

    private Token ScanChar()
    {
        var start = _position;
        Advance(); // opening quote
        var c = Current;
        Advance();
        if (Current != '\'')
            throw new LexerException($"Unterminated char literal at {_line}:{_column}");
        Advance(); // closing quote
        return MakeToken(TokenKind.CharLiteral, _source[start.._position], c);
    }

    private Token ScanNumber()
    {
        var start = _position;
        while (!IsAtEnd && char.IsDigit(Current))
            Advance();

        if (!IsAtEnd && Current == '.' && char.IsDigit(Peek()))
        {
            Advance(); // .
            while (!IsAtEnd && char.IsDigit(Current))
                Advance();
            var text = _source[start.._position];
            return MakeToken(TokenKind.FloatLiteral, text, float.Parse(text));
        }

        var intText = _source[start.._position];
        return MakeToken(TokenKind.IntLiteral, intText, int.Parse(intText));
    }

    private Token ScanIdentifierOrKeyword()
    {
        var start = _position;
        while (!IsAtEnd && (char.IsLetterOrDigit(Current) || Current == '_'))
            Advance();

        var text = _source[start.._position];
        var kind = Keywords.TryGetValue(text, out var keywordKind)
            ? keywordKind
            : TokenKind.Identifier;

        return MakeToken(kind, text);
    }

    private Token ScanOperatorOrDelimiter()
    {
        var ch = Current;
        var start = _position;
        Advance();

        if (ch == '(') return MakeToken(TokenKind.LeftParen, "(");
        if (ch == ')') return MakeToken(TokenKind.RightParen, ")");
        if (ch == '{') return MakeToken(TokenKind.LeftBrace, "{");
        if (ch == '}') return MakeToken(TokenKind.RightBrace, "}");
        if (ch == '[') return MakeToken(TokenKind.LeftBracket, "[");
        if (ch == ']') return MakeToken(TokenKind.RightBracket, "]");
        if (ch == ';') return MakeToken(TokenKind.Semicolon, ";");
        if (ch == ',') return MakeToken(TokenKind.Comma, ",");
        if (ch == '~') return MakeToken(TokenKind.Tilde, "~");
        if (ch == ':') return MakeToken(TokenKind.Colon, ":");
        if (ch == '.') return MakeToken(TokenKind.Dot, ".");

        if (ch == '+')
        {
            if (Current == '=') { Advance(); return MakeToken(TokenKind.PlusEqual, "+="); }
            return MakeToken(TokenKind.Plus, "+");
        }

        if (ch == '-')
        {
            if (Current == '=') { Advance(); return MakeToken(TokenKind.MinusEqual, "-="); }
            return MakeToken(TokenKind.Minus, "-");
        }

        if (ch == '*')
        {
            if (Current == '=') { Advance(); return MakeToken(TokenKind.StarEqual, "*="); }
            return MakeToken(TokenKind.Star, "*");
        }

        if (ch == '/')
        {
            if (Current == '=') { Advance(); return MakeToken(TokenKind.SlashEqual, "/="); }
            return MakeToken(TokenKind.Slash, "/");
        }

        if (ch == '%')
        {
            if (Current == '=') { Advance(); return MakeToken(TokenKind.PercentEqual, "%="); }
            return MakeToken(TokenKind.Percent, "%");
        }

        if (ch == '&')
        {
            if (Current == '&') { Advance(); return MakeToken(TokenKind.DoubleAmpersand, "&&"); }
            if (Current == '=') { Advance(); return MakeToken(TokenKind.AmpersandEqual, "&="); }
            return MakeToken(TokenKind.Ampersand, "&");
        }

        if (ch == '|')
        {
            if (Current == '|') { Advance(); return MakeToken(TokenKind.DoublePipe, "||"); }
            if (Current == '=') { Advance(); return MakeToken(TokenKind.PipeEqual, "|="); }
            return MakeToken(TokenKind.Pipe, "|");
        }

        if (ch == '^')
        {
            if (Current == '=') { Advance(); return MakeToken(TokenKind.CaretEqual, "^="); }
            return MakeToken(TokenKind.Caret, "^");
        }

        if (ch == '!')
        {
            if (Current == '=') { Advance(); return MakeToken(TokenKind.BangEqual, "!="); }
            return MakeToken(TokenKind.Bang, "!");
        }

        if (ch == '?')
        {
            if (Current == '.') { Advance(); return MakeToken(TokenKind.QuestionDot, "?."); }
            if (Current == ':') { Advance(); return MakeToken(TokenKind.DoubleColon, "?:"); }
            return MakeToken(TokenKind.Question, "?");
        }

        if (ch == '<')
        {
            if (Current == '<')
            {
                Advance();
                if (Current == '=') { Advance(); return MakeToken(TokenKind.LeftShiftEqual, "<<="); }
                return MakeToken(TokenKind.LeftShift, "<<");
            }
            if (Current == '=') { Advance(); return MakeToken(TokenKind.LessEqual, "<="); }
            return MakeToken(TokenKind.Less, "<");
        }

        if (ch == '>')
        {
            if (Current == '>')
            {
                Advance();
                if (Current == '=') { Advance(); return MakeToken(TokenKind.RightShiftEqual, ">>="); }
                return MakeToken(TokenKind.RightShift, ">>");
            }
            if (Current == '=') { Advance(); return MakeToken(TokenKind.GreaterEqual, ">="); }
            return MakeToken(TokenKind.Greater, ">");
        }

        if (ch == '=')
        {
            if (Current == '=') { Advance(); return MakeToken(TokenKind.EqualEqual, "=="); }
            if (Current == '>') { Advance(); return MakeToken(TokenKind.FatArrow, "=>"); }
            return MakeToken(TokenKind.Assign, "=");
        }

        throw new LexerException($"Unexpected character '{ch}' at {_line}:{_column - 1}");
    }

    private void SkipWhitespace()
    {
        while (!IsAtEnd && char.IsWhiteSpace(Current))
        {
            if (Current == '\n')
            {
                _line++;
                _column = 0;
            }
            Advance();
        }
    }

    private char Current => IsAtEnd ? '\0' : _source[_position];
    private char Peek(int offset = 1) => _position + offset >= _source.Length ? '\0' : _source[_position + offset];
    private bool IsAtEnd => _position >= _source.Length;

    private void Advance()
    {
        if (!IsAtEnd)
        {
            _position++;
            _column++;
        }
    }

    private Token MakeToken(TokenKind kind, string text, object? value = null)
    {
        var token = new Token(kind, text, _position - text.Length, _line, _column - text.Length, value);
        return token;
    }
}

/// <summary>
/// Exception thrown by the lexer
/// </summary>
public class LexerException : Exception
{
    public LexerException(string message) : base(message) { }
}
