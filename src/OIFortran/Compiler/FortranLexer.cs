using System;
using System.Collections.Generic;
using System.Text;

namespace ObjectIR.Fortran.Compiler;

public sealed class FortranLexer
{
    private readonly string _source;
    private int _position;

    private static readonly Dictionary<string, FortranTokenKind> _keywords = new(StringComparer.OrdinalIgnoreCase)
    {
        // Control flow
        ["program"] = FortranTokenKind.KeywordProgram,
        ["end"] = FortranTokenKind.KeywordEnd,
        ["subroutine"] = FortranTokenKind.KeywordSubroutine,
        ["return"] = FortranTokenKind.KeywordReturn,
        ["if"] = FortranTokenKind.KeywordIf,
        ["then"] = FortranTokenKind.KeywordThen,
        ["else"] = FortranTokenKind.KeywordElse,
        ["elseif"] = FortranTokenKind.KeywordElseif,
        ["else if"] = FortranTokenKind.KeywordElseif,
        ["endif"] = FortranTokenKind.KeywordEndif,
        ["end if"] = FortranTokenKind.KeywordEndif,
        ["do"] = FortranTokenKind.KeywordDo,
        ["enddo"] = FortranTokenKind.KeywordEnddo,
        ["end do"] = FortranTokenKind.KeywordEnddo,
        ["select"] = FortranTokenKind.KeywordSelectCase,
        ["case"] = FortranTokenKind.KeywordCase,
        ["endselect"] = FortranTokenKind.KeywordEndselect,
        ["end select"] = FortranTokenKind.KeywordEndselect,
        ["stop"] = FortranTokenKind.KeywordStop,
        ["cycle"] = FortranTokenKind.KeywordCycle,
        ["exit"] = FortranTokenKind.KeywordExit,
        
        // Types and declarations
        ["implicit"] = FortranTokenKind.KeywordImplicit,
        ["none"] = FortranTokenKind.KeywordNone,
        ["integer"] = FortranTokenKind.KeywordInteger,
        ["real"] = FortranTokenKind.KeywordReal,
        ["complex"] = FortranTokenKind.KeywordComplex,
        ["logical"] = FortranTokenKind.KeywordLogical,
        ["character"] = FortranTokenKind.KeywordCharacter,
        ["len"] = FortranTokenKind.KeywordLen,
        ["parameter"] = FortranTokenKind.KeywordParameter,
        ["dimension"] = FortranTokenKind.KeywordDimension,
        ["allocatable"] = FortranTokenKind.KeywordAllocatable,
        ["pointer"] = FortranTokenKind.KeywordPointer,
        ["target"] = FortranTokenKind.KeywordTarget,
        ["value"] = FortranTokenKind.KeywordValue,
        ["intent"] = FortranTokenKind.KeywordIntent,
        ["optional"] = FortranTokenKind.KeywordOptional,
        ["public"] = FortranTokenKind.KeywordPublic,
        ["private"] = FortranTokenKind.KeywordPrivate,
        ["protected"] = FortranTokenKind.KeywordProtected,
        
        // Fortran 90 Module System
        ["module"] = FortranTokenKind.KeywordModule,
        ["use"] = FortranTokenKind.KeywordUse,
        ["only"] = FortranTokenKind.KeywordOnly,
        ["contains"] = FortranTokenKind.KeywordContains,
        ["interface"] = FortranTokenKind.KeywordInterface,
        ["operator"] = FortranTokenKind.KeywordOperator,
        ["assignment"] = FortranTokenKind.KeywordAssignment,
        ["function"] = FortranTokenKind.KeywordFunction,
        ["type"] = FortranTokenKind.KeywordType,
        ["endtype"] = FortranTokenKind.KeywordEndType,
        ["end type"] = FortranTokenKind.KeywordEndType,
        ["endmodule"] = FortranTokenKind.KeywordEndModule,
        ["end module"] = FortranTokenKind.KeywordEndModule,
        ["endfunction"] = FortranTokenKind.KeywordEndFunction,
        ["end function"] = FortranTokenKind.KeywordEndFunction,
        ["endsubroutine"] = FortranTokenKind.KeywordEndSubroutine,
        ["end subroutine"] = FortranTokenKind.KeywordEndSubroutine,
        ["endinterface"] = FortranTokenKind.KeywordEndInterface,
        ["end interface"] = FortranTokenKind.KeywordEndInterface,
        ["endprogram"] = FortranTokenKind.KeywordEndProgram,
        ["end program"] = FortranTokenKind.KeywordEndProgram,
        
        // Derived Types
        ["sequence"] = FortranTokenKind.KeywordSequence,
        
        // Memory Management
        ["allocate"] = FortranTokenKind.KeywordAllocate,
        ["deallocate"] = FortranTokenKind.KeywordDeallocate,
        ["nullify"] = FortranTokenKind.KeywordNullify,
        ["allocated"] = FortranTokenKind.KeywordAllocated,
        ["associated"] = FortranTokenKind.KeywordAssociated,
        ["null"] = FortranTokenKind.KeywordNull,
        
        // I/O
        ["print"] = FortranTokenKind.KeywordPrint,
        ["call"] = FortranTokenKind.KeywordCall,
        ["read"] = FortranTokenKind.KeywordRead,
        ["write"] = FortranTokenKind.KeywordWrite,
        ["format"] = FortranTokenKind.KeywordFormat,
        ["open"] = FortranTokenKind.KeywordOpen,
        ["close"] = FortranTokenKind.KeywordClose,
        
        // Logical constants
        [".true."] = FortranTokenKind.KeywordTrue,
        [".false."] = FortranTokenKind.KeywordFalse,
        [".eqv."] = FortranTokenKind.EqvFortran,
        [".neqv."] = FortranTokenKind.NeqvFortran,
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

        // Handle logical literals and operators starting with '.'
        if (current == '.' && TryLexLogicalOperator(out var operatorToken))
        {
            return operatorToken;
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
                Advance();
                return new FortranToken(FortranTokenKind.Colon, ":", start);
            case ',':
                Advance();
                return new FortranToken(FortranTokenKind.Comma, ",", start);
            case '*':
                if (Peek(1) == '*')
                {
                    Advance(2);
                    return new FortranToken(FortranTokenKind.Power, "**", start);
                }
                Advance();
                return new FortranToken(FortranTokenKind.Star, "*", start);
            case '=':
                if (Peek(1) == '>')
                {
                    Advance(2);
                    return new FortranToken(FortranTokenKind.Arrow, "=>", start);
                }
                if (Peek(1) == '=')
                {
                    Advance(2);
                    return new FortranToken(FortranTokenKind.EqModern, "==", start);
                }
                Advance();
                return new FortranToken(FortranTokenKind.Equals, "=", start);
            case '+':
                Advance();
                return new FortranToken(FortranTokenKind.Plus, "+", start);
            case '-':
                Advance();
                return new FortranToken(FortranTokenKind.Minus, "-", start);
            case '/':
                if (Peek(1) == '=')
                {
                    Advance(2);
                    return new FortranToken(FortranTokenKind.NeModern, "/=", start);
                }
                if (Peek(1) == '/')
                {
                    Advance(2);
                    return new FortranToken(FortranTokenKind.DoubleSlash, "//", start);
                }
                Advance();
                return new FortranToken(FortranTokenKind.Slash, "/", start);
            case '<':
                if (Peek(1) == '=')
                {
                    Advance(2);
                    return new FortranToken(FortranTokenKind.LeModern, "<=", start);
                }
                Advance();
                return new FortranToken(FortranTokenKind.LtModern, "<", start);
            case '>':
                if (Peek(1) == '=')
                {
                    Advance(2);
                    return new FortranToken(FortranTokenKind.GeModern, ">=", start);
                }
                Advance();
                return new FortranToken(FortranTokenKind.GtModern, ">", start);
            case '(':
                Advance();
                return new FortranToken(FortranTokenKind.LParen, "(", start);
            case ')':
                Advance();
                return new FortranToken(FortranTokenKind.RParen, ")", start);
            case '[':
                Advance();
                return new FortranToken(FortranTokenKind.LBracket, "[", start);
            case ']':
                Advance();
                return new FortranToken(FortranTokenKind.RBracket, "]", start);
            case '{':
                Advance();
                return new FortranToken(FortranTokenKind.LBrace, "{", start);
            case '}':
                Advance();
                return new FortranToken(FortranTokenKind.RBrace, "}", start);
            case '%':
                Advance();
                return new FortranToken(FortranTokenKind.Percent, "%", start);
            case '.':
                Advance();
                return new FortranToken(FortranTokenKind.Dot, ".", start);
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
        
        // Check if this is the first word of a multi-word keyword
        if (text.Equals("SELECT", StringComparison.OrdinalIgnoreCase))
        {
            int savedPosition = _position;
            SkipTrivia();
            if (!IsEnd && (char.IsLetter(Peek()) || Peek() == '_'))
            {
                var nextStart = _position;
                var nextBuilder = new StringBuilder();
                while (!IsEnd && (char.IsLetterOrDigit(Peek()) || Peek() == '_'))
                {
                    nextBuilder.Append(Peek());
                    Advance();
                }
                string nextText = nextBuilder.ToString();
                if (nextText.Equals("CASE", StringComparison.OrdinalIgnoreCase))
                {
                    return new FortranToken(FortranTokenKind.KeywordSelectCase, "SELECT CASE", start);
                }
            }
            // Restore if not "SELECT CASE"
            _position = savedPosition;
        }
        
        if (text.Equals("END", StringComparison.OrdinalIgnoreCase))
        {
            int savedPosition = _position;
            SkipTrivia();
            if (!IsEnd && (char.IsLetter(Peek()) || Peek() == '_'))
            {
                var nextStart = _position;
                var nextBuilder = new StringBuilder();
                while (!IsEnd && (char.IsLetterOrDigit(Peek()) || Peek() == '_'))
                {
                    nextBuilder.Append(Peek());
                    Advance();
                }
                string nextText = nextBuilder.ToString();
                if (nextText.Equals("SELECT", StringComparison.OrdinalIgnoreCase))
                {
                    return new FortranToken(FortranTokenKind.KeywordEndselect, "END SELECT", start);
                }
                if (nextText.Equals("IF", StringComparison.OrdinalIgnoreCase))
                {
                    return new FortranToken(FortranTokenKind.KeywordEndif, "END IF", start);
                }
                if (nextText.Equals("DO", StringComparison.OrdinalIgnoreCase))
                {
                    return new FortranToken(FortranTokenKind.KeywordEnddo, "END DO", start);
                }
                if (nextText.Equals("MODULE", StringComparison.OrdinalIgnoreCase))
                {
                    return new FortranToken(FortranTokenKind.KeywordEndModule, "END MODULE", start);
                }
                if (nextText.Equals("FUNCTION", StringComparison.OrdinalIgnoreCase))
                {
                    return new FortranToken(FortranTokenKind.KeywordEndFunction, "END FUNCTION", start);
                }
                if (nextText.Equals("SUBROUTINE", StringComparison.OrdinalIgnoreCase))
                {
                    return new FortranToken(FortranTokenKind.KeywordEndSubroutine, "END SUBROUTINE", start);
                }
                if (nextText.Equals("TYPE", StringComparison.OrdinalIgnoreCase))
                {
                    return new FortranToken(FortranTokenKind.KeywordEndType, "END TYPE", start);
                }
                if (nextText.Equals("INTERFACE", StringComparison.OrdinalIgnoreCase))
                {
                    return new FortranToken(FortranTokenKind.KeywordEndInterface, "END INTERFACE", start);
                }
                if (nextText.Equals("PROGRAM", StringComparison.OrdinalIgnoreCase))
                {
                    return new FortranToken(FortranTokenKind.KeywordEndProgram, "END PROGRAM", start);
                }
            }
            // Restore if not a recognized compound END
            _position = savedPosition;
        }

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

    private bool TryLexLogicalOperator(out FortranToken token)
    {
        token = default!;
        int start = _position;

        // Try to match Fortran-style operators (.EQ., .LT., .AND., etc.)
        if (Matches('.', 't', 'r', 'u', 'e', '.'))
        {
            Advance(6);
            token = new FortranToken(FortranTokenKind.KeywordTrue, ".true.", start);
            return true;
        }

        if (Matches('.', 'f', 'a', 'l', 's', 'e', '.'))
        {
            Advance(7);
            token = new FortranToken(FortranTokenKind.KeywordFalse, ".false.", start);
            return true;
        }

        // Relational operators
        if (Matches('.', 'e', 'q', '.'))
        {
            Advance(4);
            token = new FortranToken(FortranTokenKind.EqFortran, ".eq.", start);
            return true;
        }

        if (Matches('.', 'n', 'e', '.'))
        {
            Advance(4);
            token = new FortranToken(FortranTokenKind.NeFortran, ".ne.", start);
            return true;
        }

        if (Matches('.', 'l', 't', '.'))
        {
            Advance(4);
            token = new FortranToken(FortranTokenKind.LtFortran, ".lt.", start);
            return true;
        }

        if (Matches('.', 'l', 'e', '.'))
        {
            Advance(4);
            token = new FortranToken(FortranTokenKind.LeFortran, ".le.", start);
            return true;
        }

        if (Matches('.', 'g', 't', '.'))
        {
            Advance(4);
            token = new FortranToken(FortranTokenKind.GtFortran, ".gt.", start);
            return true;
        }

        if (Matches('.', 'g', 'e', '.'))
        {
            Advance(4);
            token = new FortranToken(FortranTokenKind.GeFortran, ".ge.", start);
            return true;
        }

        // Logical operators
        if (Matches('.', 'a', 'n', 'd', '.'))
        {
            Advance(5);
            token = new FortranToken(FortranTokenKind.AndFortran, ".and.", start);
            return true;
        }

        if (Matches('.', 'o', 'r', '.'))
        {
            Advance(4);
            token = new FortranToken(FortranTokenKind.OrFortran, ".or.", start);
            return true;
        }

        if (Matches('.', 'n', 'o', 't', '.'))
        {
            Advance(5);
            token = new FortranToken(FortranTokenKind.NotFortran, ".not.", start);
            return true;
        }

        if (Matches('.', 'e', 'q', 'v', '.'))
        {
            Advance(5);
            token = new FortranToken(FortranTokenKind.EqvFortran, ".eqv.", start);
            return true;
        }

        if (Matches('.', 'n', 'e', 'q', 'v', '.'))
        {
            Advance(6);
            token = new FortranToken(FortranTokenKind.NeqvFortran, ".neqv.", start);
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
