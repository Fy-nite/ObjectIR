using Xunit;
using ObjectIR.Fortran.Compiler;
using System.Collections.Generic;
using System.Linq;

namespace ObjectIR.CSharpTests;

/// <summary>
/// Unit tests for FortranLexer - verifies tokenization of Fortran source
/// </summary>
public class FortranLexerTests
{
    private FortranLexer CreateLexer(string source) => new(source);

    private void AssertTokens(string source, params FortranTokenKind[] expectedKinds)
    {
        var lexer = CreateLexer(source);
        var tokens = lexer.Tokenize();
        var kinds = tokens.Where(t => t.Kind != FortranTokenKind.EndOfFile).Select(t => t.Kind).ToList();
        
        Assert.Equal(expectedKinds.Length, kinds.Count);
        for (int i = 0; i < expectedKinds.Length; i++)
        {
            Assert.Equal(expectedKinds[i], kinds[i]);
        }
    }

    #region Keywords - Control Flow

    [Fact]
    public void Tokenize_IfThenElseEndif()
    {
        AssertTokens(
            "IF (x > 0) THEN\nELSE\nENDIF",
            FortranTokenKind.KeywordIf,
            FortranTokenKind.LParen,
            FortranTokenKind.Identifier,
            FortranTokenKind.GtModern,
            FortranTokenKind.IntegerLiteral,
            FortranTokenKind.RParen,
            FortranTokenKind.KeywordThen,
            FortranTokenKind.KeywordElse,
            FortranTokenKind.KeywordEndif
        );
    }

    [Fact]
    public void Tokenize_IfElseifElseEndif()
    {
        AssertTokens(
            "IF (x) THEN\nELSE IF (y) THEN\nENDIF",
            FortranTokenKind.KeywordIf,
            FortranTokenKind.LParen,
            FortranTokenKind.Identifier,
            FortranTokenKind.RParen,
            FortranTokenKind.KeywordThen,
            FortranTokenKind.KeywordElse,
            FortranTokenKind.KeywordIf,
            FortranTokenKind.LParen,
            FortranTokenKind.Identifier,
            FortranTokenKind.RParen,
            FortranTokenKind.KeywordThen,
            FortranTokenKind.KeywordEndif
        );
    }

    [Fact]
    public void Tokenize_DoLoops()
    {
        AssertTokens(
            "DO i = 1, 10\nENDDO",
            FortranTokenKind.KeywordDo,
            FortranTokenKind.Identifier,
            FortranTokenKind.Equals,
            FortranTokenKind.IntegerLiteral,
            FortranTokenKind.Comma,
            FortranTokenKind.IntegerLiteral,
            FortranTokenKind.KeywordEnddo
        );
    }

    [Fact]
    public void Tokenize_LoopControl()
    {
        AssertTokens(
            "EXIT\nCYCLE",
            FortranTokenKind.KeywordExit,
            FortranTokenKind.KeywordCycle
        );
    }

    [Fact]
    public void Tokenize_SelectCase()
    {
        AssertTokens(
            "SELECT CASE (x)\nCASE (1)\nEND SELECT",
            FortranTokenKind.KeywordSelectCase,
            FortranTokenKind.LParen,
            FortranTokenKind.Identifier,
            FortranTokenKind.RParen,
            FortranTokenKind.KeywordCase,
            FortranTokenKind.LParen,
            FortranTokenKind.IntegerLiteral,
            FortranTokenKind.RParen,
            FortranTokenKind.KeywordEndselect
        );
    }

    [Fact]
    public void Tokenize_StopStatement()
    {
        AssertTokens(
            "STOP 1",
            FortranTokenKind.KeywordStop,
            FortranTokenKind.IntegerLiteral
        );
    }

    #endregion

    #region Arithmetic Operators

    [Fact]
    public void Tokenize_BasicArithmetic()
    {
        AssertTokens(
            "a + b - c * d / e",
            FortranTokenKind.Identifier,
            FortranTokenKind.Plus,
            FortranTokenKind.Identifier,
            FortranTokenKind.Minus,
            FortranTokenKind.Identifier,
            FortranTokenKind.Star,
            FortranTokenKind.Identifier,
            FortranTokenKind.Slash,
            FortranTokenKind.Identifier
        );
    }

    [Fact]
    public void Tokenize_Power()
    {
        AssertTokens(
            "x ** 2",
            FortranTokenKind.Identifier,
            FortranTokenKind.Power,
            FortranTokenKind.IntegerLiteral
        );
    }

    #endregion

    #region Relational Operators - Fortran Style

    [Fact]
    public void Tokenize_FortranRelationalOperators()
    {
        AssertTokens(
            "a .EQ. b .NE. c .LT. d",
            FortranTokenKind.Identifier,
            FortranTokenKind.EqFortran,
            FortranTokenKind.Identifier,
            FortranTokenKind.NeFortran,
            FortranTokenKind.Identifier,
            FortranTokenKind.LtFortran,
            FortranTokenKind.Identifier
        );
    }

    [Fact]
    public void Tokenize_FortranRelationalOperatorsComplete()
    {
        AssertTokens(
            "a .LE. b .GT. c .GE. d",
            FortranTokenKind.Identifier,
            FortranTokenKind.LeFortran,
            FortranTokenKind.Identifier,
            FortranTokenKind.GtFortran,
            FortranTokenKind.Identifier,
            FortranTokenKind.GeFortran,
            FortranTokenKind.Identifier
        );
    }

    #endregion

    #region Relational Operators - Modern Style

    [Fact]
    public void Tokenize_ModernRelationalOperators()
    {
        AssertTokens(
            "a == b /= c < d",
            FortranTokenKind.Identifier,
            FortranTokenKind.EqModern,
            FortranTokenKind.Identifier,
            FortranTokenKind.NeModern,
            FortranTokenKind.Identifier,
            FortranTokenKind.LtModern,
            FortranTokenKind.Identifier
        );
    }

    [Fact]
    public void Tokenize_ModernRelationalOperatorsComplete()
    {
        AssertTokens(
            "a <= b > c >= d",
            FortranTokenKind.Identifier,
            FortranTokenKind.LeModern,
            FortranTokenKind.Identifier,
            FortranTokenKind.GtModern,
            FortranTokenKind.Identifier,
            FortranTokenKind.GeModern,
            FortranTokenKind.Identifier
        );
    }

    #endregion

    #region Logical Operators

    [Fact]
    public void Tokenize_LogicalOperators()
    {
        AssertTokens(
            "a .AND. b .OR. c .NOT. d",
            FortranTokenKind.Identifier,
            FortranTokenKind.AndFortran,
            FortranTokenKind.Identifier,
            FortranTokenKind.OrFortran,
            FortranTokenKind.Identifier,
            FortranTokenKind.NotFortran,
            FortranTokenKind.Identifier
        );
    }

    #endregion

    #region Logical Literals

    [Fact]
    public void Tokenize_LogicalTrue()
    {
        AssertTokens(
            ".TRUE.",
            FortranTokenKind.KeywordTrue
        );
    }

    [Fact]
    public void Tokenize_LogicalFalse()
    {
        AssertTokens(
            ".FALSE.",
            FortranTokenKind.KeywordFalse
        );
    }

    [Fact]
    public void Tokenize_LogicalInContext()
    {
        AssertTokens(
            "IF (.TRUE.) THEN x = .FALSE. ENDIF",
            FortranTokenKind.KeywordIf,
            FortranTokenKind.LParen,
            FortranTokenKind.KeywordTrue,
            FortranTokenKind.RParen,
            FortranTokenKind.KeywordThen,
            FortranTokenKind.Identifier,
            FortranTokenKind.Equals,
            FortranTokenKind.KeywordFalse,
            FortranTokenKind.KeywordEndif
        );
    }

    #endregion

    #region Complex Expressions

    [Fact]
    public void Tokenize_ConditionWithLogicalAnd()
    {
        AssertTokens(
            "IF (x .LT. 10 .AND. y .GT. 5) THEN",
            FortranTokenKind.KeywordIf,
            FortranTokenKind.LParen,
            FortranTokenKind.Identifier,
            FortranTokenKind.LtFortran,
            FortranTokenKind.IntegerLiteral,
            FortranTokenKind.AndFortran,
            FortranTokenKind.Identifier,
            FortranTokenKind.GtFortran,
            FortranTokenKind.IntegerLiteral,
            FortranTokenKind.RParen,
            FortranTokenKind.KeywordThen
        );
    }

    [Fact]
    public void Tokenize_ConditionWithLogicalOr()
    {
        AssertTokens(
            "IF (x .EQ. 0 .OR. y .EQ. 0) THEN",
            FortranTokenKind.KeywordIf,
            FortranTokenKind.LParen,
            FortranTokenKind.Identifier,
            FortranTokenKind.EqFortran,
            FortranTokenKind.IntegerLiteral,
            FortranTokenKind.OrFortran,
            FortranTokenKind.Identifier,
            FortranTokenKind.EqFortran,
            FortranTokenKind.IntegerLiteral,
            FortranTokenKind.RParen,
            FortranTokenKind.KeywordThen
        );
    }

    [Fact]
    public void Tokenize_ConditionWithLogicalNot()
    {
        AssertTokens(
            "IF (.NOT. flag) THEN",
            FortranTokenKind.KeywordIf,
            FortranTokenKind.LParen,
            FortranTokenKind.NotFortran,
            FortranTokenKind.Identifier,
            FortranTokenKind.RParen,
            FortranTokenKind.KeywordThen
        );
    }

    #endregion

    #region Type Keywords

    [Fact]
    public void Tokenize_TypeDeclarations()
    {
        AssertTokens(
            "INTEGER REAL LOGICAL CHARACTER",
            FortranTokenKind.KeywordInteger,
            FortranTokenKind.KeywordReal,
            FortranTokenKind.KeywordLogical,
            FortranTokenKind.KeywordCharacter
        );
    }

    [Fact]
    public void Tokenize_ImplicitNone()
    {
        AssertTokens(
            "IMPLICIT NONE",
            FortranTokenKind.KeywordImplicit,
            FortranTokenKind.KeywordNone
        );
    }

    [Fact]
    public void Tokenize_Parameter()
    {
        AssertTokens(
            "INTEGER, PARAMETER :: PI = 3",
            FortranTokenKind.KeywordInteger,
            FortranTokenKind.Comma,
            FortranTokenKind.KeywordParameter,
            FortranTokenKind.DoubleColon,
            FortranTokenKind.Identifier,
            FortranTokenKind.Equals,
            FortranTokenKind.IntegerLiteral
        );
    }

    #endregion

    #region Program Structure

    [Fact]
    public void Tokenize_SimpleProgram()
    {
        AssertTokens(
            "PROGRAM test END PROGRAM",
            FortranTokenKind.KeywordProgram,
            FortranTokenKind.Identifier,
            FortranTokenKind.KeywordEnd,
            FortranTokenKind.KeywordProgram
        );
    }

    [Fact]
    public void Tokenize_SubroutineDefinition()
    {
        AssertTokens(
            "SUBROUTINE test (x, y) END SUBROUTINE",
            FortranTokenKind.KeywordSubroutine,
            FortranTokenKind.Identifier,
            FortranTokenKind.LParen,
            FortranTokenKind.Identifier,
            FortranTokenKind.Comma,
            FortranTokenKind.Identifier,
            FortranTokenKind.RParen,
            FortranTokenKind.KeywordEnd,
            FortranTokenKind.KeywordSubroutine
        );
    }

    #endregion

    #region Literals

    [Fact]
    public void Tokenize_IntegerLiterals()
    {
        AssertTokens(
            "0 1 42 -99",
            FortranTokenKind.IntegerLiteral,
            FortranTokenKind.IntegerLiteral,
            FortranTokenKind.IntegerLiteral,
            FortranTokenKind.Minus,
            FortranTokenKind.IntegerLiteral
        );
    }

    [Fact]
    public void Tokenize_RealLiterals()
    {
        AssertTokens(
            "3.14 2.0 1.5e-2 2.5d0",
            FortranTokenKind.RealLiteral,
            FortranTokenKind.RealLiteral,
            FortranTokenKind.RealLiteral,
            FortranTokenKind.RealLiteral
        );
    }

    [Fact]
    public void Tokenize_StringLiterals()
    {
        AssertTokens(
            "'hello' \"world\"",
            FortranTokenKind.StringLiteral,
            FortranTokenKind.StringLiteral
        );
    }

    #endregion

    #region Case Insensitivity

    [Fact]
    public void Tokenize_KeywordsCaseInsensitive()
    {
        var tokens1 = CreateLexer("if then else endif").Tokenize();
        var tokens2 = CreateLexer("IF THEN ELSE ENDIF").Tokenize();
        var tokens3 = CreateLexer("If Then Else EndIf").Tokenize();

        var kinds1 = tokens1.Where(t => t.Kind != FortranTokenKind.EndOfFile).Select(t => t.Kind).ToList();
        var kinds2 = tokens2.Where(t => t.Kind != FortranTokenKind.EndOfFile).Select(t => t.Kind).ToList();
        var kinds3 = tokens3.Where(t => t.Kind != FortranTokenKind.EndOfFile).Select(t => t.Kind).ToList();

        Assert.Equal(kinds1, kinds2);
        Assert.Equal(kinds2, kinds3);
    }

    #endregion

    #region Edge Cases

    [Fact]
    public void Tokenize_EmptySource()
    {
        var tokens = CreateLexer("").Tokenize();
        Assert.Single(tokens);
        Assert.Equal(FortranTokenKind.EndOfFile, tokens[0].Kind);
    }

    [Fact]
    public void Tokenize_CommentsIgnored()
    {
        AssertTokens(
            "x = 5 ! This is a comment\ny = 10",
            FortranTokenKind.Identifier,
            FortranTokenKind.Equals,
            FortranTokenKind.IntegerLiteral,
            FortranTokenKind.Identifier,
            FortranTokenKind.Equals,
            FortranTokenKind.IntegerLiteral
        );
    }

    [Fact]
    public void Tokenize_WhitespaceHandled()
    {
        var tokens1 = CreateLexer("a+b").Tokenize();
        var tokens2 = CreateLexer("a + b").Tokenize();
        var tokens3 = CreateLexer("a  +  b").Tokenize();

        var kinds1 = tokens1.Where(t => t.Kind != FortranTokenKind.EndOfFile).Select(t => t.Kind).ToList();
        var kinds2 = tokens2.Where(t => t.Kind != FortranTokenKind.EndOfFile).Select(t => t.Kind).ToList();
        var kinds3 = tokens3.Where(t => t.Kind != FortranTokenKind.EndOfFile).Select(t => t.Kind).ToList();

        Assert.Equal(kinds1, kinds2);
        Assert.Equal(kinds2, kinds3);
    }

    #endregion
}
