namespace ObjectIR.Fortran.Compiler;

internal enum FortranTokenKind
{
    EndOfFile,
    Identifier,
    IntegerLiteral,
    RealLiteral,
    StringLiteral,
    KeywordProgram,
    KeywordEnd,
    KeywordImplicit,
    KeywordNone,
    KeywordInteger,
    KeywordReal,
    KeywordLogical,
    KeywordCharacter,
    KeywordPrint,
    KeywordCall,
    KeywordTrue,
    KeywordFalse,
    DoubleColon,
    Comma,
    Star,
    Equals,
    Plus,
    Minus,
    Slash,
    LParen,
    RParen
}

internal sealed class FortranToken
{
    public FortranTokenKind Kind { get; }
    public string Text { get; }
    public int Position { get; }

    public FortranToken(FortranTokenKind kind, string text, int position)
    {
        Kind = kind;
        Text = text;
        Position = position;
    }

    public override string ToString() => $"{Kind}: {Text}";
}
