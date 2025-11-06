namespace ObjectIR.Fortran.Compiler;

public enum FortranTokenKind
{
    // End of file and error
    EndOfFile,
    
    // Literals and identifiers
    Identifier,
    IntegerLiteral,
    RealLiteral,
    StringLiteral,
    
    // Keywords - Control Flow
    KeywordProgram,
    KeywordEnd,
    KeywordSubroutine,
    KeywordReturn,
    KeywordIf,
    KeywordThen,
    KeywordElse,
    KeywordElseif,
    KeywordEndif,
    KeywordDo,
    KeywordEnddo,
    KeywordSelectCase,
    KeywordCase,
    KeywordEndselect,
    KeywordStop,
    KeywordCycle,
    KeywordExit,
    
    // Keywords - Types and declarations
    KeywordImplicit,
    KeywordNone,
    KeywordInteger,
    KeywordReal,
    KeywordLogical,
    KeywordCharacter,
    KeywordLen,
    KeywordParameter,
    KeywordComplex,
    KeywordDimension,
    KeywordAllocatable,
    KeywordPointer,
    KeywordTarget,
    KeywordValue,
    KeywordIntent,
    KeywordOptional,
    KeywordPublic,
    KeywordPrivate,
    KeywordProtected,
    
    // Keywords - Fortran 90 Module System
    KeywordModule,
    KeywordUse,
    KeywordOnly,
    KeywordContains,
    KeywordInterface,
    KeywordOperator,
    KeywordAssignment,
    KeywordFunction,
    KeywordType,
    KeywordEndType,
    KeywordEndModule,
    KeywordEndFunction,
    KeywordEndSubroutine,
    KeywordEndInterface,
    KeywordEndProgram,
    
    // Keywords - Fortran 90 Derived Types
    KeywordSequence,
    
    // Keywords - Fortran 90 Memory Management
    KeywordAllocate,
    KeywordDeallocate,
    KeywordNullify,
    KeywordAllocated,
    KeywordAssociated,
    KeywordNull,
    
    // Keywords - I/O
    KeywordPrint,
    KeywordCall,
    KeywordRead,
    KeywordWrite,
    KeywordFormat,
    KeywordOpen,
    KeywordClose,
    
    // Keywords - Logical constants
    KeywordTrue,
    KeywordFalse,
    
    // Delimiters
    DoubleColon,
    Comma,
    LParen,
    RParen,
    LBracket,           // [
    RBracket,           // ]
    LBrace,             // {
    RBrace,             // }
    Percent,            // % (component access)
    Colon,              // : (array subscript ranges)
    
    // Arithmetic operators
    Plus,
    Minus,
    Star,
    Slash,
    Power,              // ** or ^
    DoubleSlash,        // // (string concatenation)
    
    // Relational operators (Fortran style)
    EqFortran,          // .EQ.
    NeFortran,          // .NE.
    LtFortran,          // .LT.
    LeFortran,          // .LE.
    GtFortran,          // .GT.
    GeFortran,          // .GE.
    
    // Relational operators (Fortran 90 style)
    EqModern,           // ==
    NeModern,           // /=
    LtModern,           // <
    LeModern,           // <=
    GtModern,           // >
    GeModern,           // >=
    
    // Logical operators
    AndFortran,         // .AND.
    OrFortran,          // .OR.
    NotFortran,         // .NOT.
    EqvFortran,         // .EQV.
    NeqvFortran,        // .NEQV.
    
    // Assignment and other operators
    Equals,
    Arrow,              // => (pointer assignment)
    Dot,                // . (for logical operators and field access)
}

public sealed class FortranToken
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
