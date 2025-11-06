using System;
using System.Collections.Generic;
using System.Text;

namespace ObjectIR.Fortran.Compiler;

internal sealed class FortranLexer
{
    private readonly string _source;
    private int _position;

    private static readonly Dictionary<string, FortranTokenKind> _keywords = new(StringComparer.OrdinalIgnoreCase)
    {
        ["program"] = FortranTokenKind.KeywordProgram,
        ["end"] = FortranTokenKind.KeywordEnd,
        ["subroutine"] = FortranTokenKind.KeywordSubroutine,
        ["return"] = FortranTokenKind.KeywordReturn,
        ["implicit"] = FortranTokenKind.KeywordImplicit,
        ["none"] = FortranTokenKind.KeywordNone,
        ["integer"] = FortranTokenKind.KeywordInteger,
        ["real"] = FortranTokenKind.KeywordReal,
        ["logical"] = FortranTokenKind.KeywordLogical,
        ["character"] = FortranTokenKind.KeywordCharacter,
        ["print"] = FortranTokenKind.KeywordPrint,
        ["call"] = FortranTokenKind.KeywordCall
    };

    public FortranLexer(string source)
    {
        _source = source ?? string.Empty;
    }

    public IReadOnlyList<FortranToken> Tokenize()
    {
        var tokens = new List<FortranToken>();

        while (true)
        {
            var token = NextToken();
            tokens.Add(token);
            if (token.Kind == FortranTokenKind.EndOfFile)
            {
                break;
            }
        }

        return tokens;
    }

    private FortranToken NextToken()
    {
        SkipTrivia();
        if (IsEnd)
        {
            return new FortranToken(FortranTokenKind.EndOfFile, string.Empty, _position);
        }

        int start = _position;
        char current = Peek();

        if (current == '.' && TryLexLogicalLiteral(out var logicalToken))
        {
            return logicalToken;
        }

        if (char.IsLetter(current) || current == '_')
        {
            return LexIdentifier(start);
        }

        if (char.IsDigit(current) || (current == '.' && char.IsDigit(Peek(1))))
        {
            return LexNumber(start);
        }

        if (current == '\'' || current == '"')
        {
            return LexString(start);
        }

        switch (current)
        {
            case ':':
                if (Peek(1) == ':')
                {
                    Advance(2);
                    return new FortranToken(FortranTokenKind.DoubleColon, "::", start);
                }
                break;
            case ',':
                Advance();
                return new FortranToken(FortranTokenKind.Comma, ",", start);
            case '*':
                Advance();
                return new FortranToken(FortranTokenKind.Star, "*", start);
            case '=':
                Advance();
                return new FortranToken(FortranTokenKind.Equals, "=", start);
            case '+':
                Advance();
                return new FortranToken(FortranTokenKind.Plus, "+", start);
            case '-':
                Advance();
                return new FortranToken(FortranTokenKind.Minus, "-", start);
            case '/':
                Advance();
                return new FortranToken(FortranTokenKind.Slash, "/", start);
            case '(':
                Advance();
                return new FortranToken(FortranTokenKind.LParen, "(", start);
            case ')':
                Advance();
                return new FortranToken(FortranTokenKind.RParen, ")", start);
        }

        Advance();
        return new FortranToken(FortranTokenKind.Identifier, current.ToString(), start);
    }

    private FortranToken LexIdentifier(int start)
    {
        var builder = new StringBuilder();

        while (!IsEnd && (char.IsLetterOrDigit(Peek()) || Peek() == '_'))
        {
            builder.Append(Peek());
            Advance();
        }

        string text = builder.ToString();
        if (_keywords.TryGetValue(text, out var keywordKind))
        {
            return new FortranToken(keywordKind, text, start);
        }

        return new FortranToken(FortranTokenKind.Identifier, text, start);
    }

    private FortranToken LexNumber(int start)
    {
        bool isReal = false;
        while (char.IsDigit(Peek()))
        {
            Advance();
        }

        if (Peek() == '.')
        {
            if (char.IsDigit(Peek(1)))
            {
                isReal = true;
                Advance();
                while (char.IsDigit(Peek()))
                {
                    Advance();
                }
            }
        }

        if (Peek() == 'e' || Peek() == 'E' || Peek() == 'd' || Peek() == 'D')
        {
            isReal = true;
            Advance();
            if (Peek() == '+' || Peek() == '-')
            {
                Advance();
            }
            while (char.IsDigit(Peek()))
            {
                Advance();
            }
        }

        string text = _source[start.._position];
        return isReal
            ? new FortranToken(FortranTokenKind.RealLiteral, text, start)
            : new FortranToken(FortranTokenKind.IntegerLiteral, text, start);
    }

    private FortranToken LexString(int start)
    {
        char quote = Peek();
        Advance();
        var builder = new StringBuilder();

        while (!IsEnd)
        {
            char current = Peek();
            Advance();

            if (current == quote)
            {
                if (!IsEnd && Peek() == quote)
                {
                    builder.Append(quote);
                    Advance();
                    continue;
                }
                break;
            }

            builder.Append(current);
        }

        return new FortranToken(FortranTokenKind.StringLiteral, builder.ToString(), start);
    }

    private bool TryLexLogicalLiteral(out FortranToken token)
    {
        token = default!;
        if (Matches('.','t','r','u','e','.'))
        {
            int start = _position;
            Advance(6);
            token = new FortranToken(FortranTokenKind.KeywordTrue, ".true.", start);
            return true;
        }

        if (Matches('.','f','a','l','s','e','.'))
        {
            int start = _position;
            Advance(7);
            token = new FortranToken(FortranTokenKind.KeywordFalse, ".false.", start);
            return true;
        }

        return false;
    }

    private bool Matches(params char[] text)
    {
        if (_position + text.Length > _source.Length)
        {
            return false;
        }

        for (int i = 0; i < text.Length; i++)
        {
            if (char.ToLowerInvariant(_source[_position + i]) != char.ToLowerInvariant(text[i]))
            {
                return false;
            }
        }

        return true;
    }

    private void SkipTrivia()
    {
        while (!IsEnd)
        {
            char current = Peek();
            if (char.IsWhiteSpace(current))
            {
                Advance();
                continue;
            }

            if (current == '!')
            {
                Advance();
                while (!IsEnd && Peek() != '\n')
                {
                    Advance();
                }
                continue;
            }

            if (current == '&')
            {
                Advance();
                continue;
            }

            break;
        }
    }

    private char Peek(int offset = 0)
    {
        if (_position + offset >= _source.Length)
        {
            return '\0';
        }

        return _source[_position + offset];
    }

    private void Advance(int count = 1)
    {
        _position = Math.Min(_position + count, _source.Length);
    }

    private bool IsEnd => _position >= _source.Length;
}
