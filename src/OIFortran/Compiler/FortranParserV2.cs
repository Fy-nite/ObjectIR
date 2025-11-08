using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;

namespace ObjectIR.Fortran.Compiler;

/// <summary>
/// Resilient Fortran parser with strong error recovery.
/// Key improvements:
/// - Statement types are recognized first before attempting to parse details
/// - Each statement type has isolated parsing logic
/// - Errors are localized and don't cascade
/// - Skipping malformed statements doesn't consume unrelated tokens
/// </summary>
internal sealed class FortranParserV2
{
    private readonly List<FortranToken> _tokens;
    private int _position = 0;

    public FortranParserV2(IReadOnlyList<FortranToken> tokens, bool debug = false)
    {
        _tokens = new List<FortranToken>(tokens);
        _debug = debug;
    }

    public FortranProgram ParseProgram()
    {
        try
        {
            // Skip MODULE declarations if present - not fully supporting yet
            while (Check(FortranTokenKind.KeywordModule))
            {
                SkipToNextSubroutineOrEnd();
            }

            // Expect PROGRAM keyword
            if (!Match(FortranTokenKind.KeywordProgram))
            {
                throw new ParseException("Expected 'program' at start");
            }

            string name = Consume(FortranTokenKind.Identifier, "Expected program name").Text;
            
            var statements = ParseStatementBlockUntil(
                FortranTokenKind.KeywordEnd,
                FortranTokenKind.KeywordEndProgram);

            // Consume END PROGRAM
            if (!Match(FortranTokenKind.KeywordEndProgram))
            {
                if (Match(FortranTokenKind.KeywordEnd))
                {
                    Match(FortranTokenKind.KeywordProgram); // optional
                }
            }

            // Optional program name after END
            if (Check(FortranTokenKind.Identifier))
            {
                Advance();
            }

            // Parse subroutines
            var subroutines = new List<FortranSubroutineDefinition>();
            while (Check(FortranTokenKind.KeywordSubroutine))
            {
                try
                {
                    var sub = ParseSubroutine();
                    if (sub != null)
                    {
                        subroutines.Add(sub);
                    }
                }
                catch
                {
                    SkipToNextSubroutineOrEnd();
                }
            }

            if (_debug)
            {
                System.Console.Error.WriteLine($"[PARSER] Program '{name}' statements={statements.Count} subroutines={subroutines.Count}");
            }
            return new FortranProgram(name, statements, subroutines);
        }
        catch (ParseException ex)
        {
            throw new InvalidOperationException($"Parse error: {ex.Message}");
        }
    }

    /// <summary>
    /// Parse a block of statements until hitting a terminator keyword.
    /// Very resilient - skips malformed statements without cascade failure.
    /// </summary>
    private List<FortranStatement> ParseStatementBlockUntil(
        params FortranTokenKind[] terminators)
    {
        var statements = new List<FortranStatement>();

        while (!IsAtEnd && !IsTerminator(terminators))
        {
            // Try to recognize and parse the next statement
            FortranStatement? statement = null;

            try
            {
                // Check statement type by lookahead
                statement = TryParseStatement();
            }
            catch (Exception ex)
            {
                // Log error and skip to next statement
                if (_debug)
                {
                    System.Console.Error.WriteLine($"[PARSER] Error at token {Current.Kind}: {ex.Message}");
                }
            }

            if (statement != null)
            {
                statements.Add(statement);
            }
            else
            {
                // If we couldn't parse anything, skip one token to avoid infinite loop
                if (!IsTerminator(terminators) && !IsAtEnd)
                {
                    if (_debug)
                    {
                        System.Console.Error.WriteLine($"[PARSER] Skipping token: {Current.Kind} '{Current.Text}'");
                    }
                    Advance();
                }
            }
        }

        return statements;
    }

    /// <summary>
    /// Try to parse any recognized statement type.
    /// Returns null if current token isn't a statement keyword.
    /// </summary>
    private FortranStatement? TryParseStatement()
    {
        // Check each statement type explicitly
        if (Check(FortranTokenKind.KeywordImplicit))
        {
            return ParseImplicitStatement();
        }

        if (IsTypeSpecifier(Current.Kind))
        {
            return TryParseDeclaration();
        }

        if (Check(FortranTokenKind.KeywordAllocate))
        {
            return ParseAllocateStatement();
        }

        if (Check(FortranTokenKind.KeywordDeallocate))
        {
            return ParseDeallocateStatement();
        }

        if (Check(FortranTokenKind.KeywordPrint))
        {
            return ParsePrintStatement();
        }

        if (Check(FortranTokenKind.KeywordRead))
        {
            return ParseReadStatement();
        }

        if (Check(FortranTokenKind.KeywordWrite))
        {
            return ParseWriteStatement();
        }

        if (Check(FortranTokenKind.KeywordCall))
        {
            return ParseCallStatement();
        }

        if (Check(FortranTokenKind.KeywordIf))
        {
            return ParseIfStatement();
        }

        if (Check(FortranTokenKind.KeywordDo))
        {
            return ParseDoStatement();
        }

        if (Check(FortranTokenKind.KeywordReturn))
        {
            Advance();
            return new FortranReturnStatement();
        }

        if (Check(FortranTokenKind.Identifier))
        {
            return TryParseAssignmentOrCall();
        }

        // Not a recognized statement
        return null;
    }

    private FortranStatement? ParseImplicitStatement()
    {
        if (!Match(FortranTokenKind.KeywordImplicit))
            return null;

        if (Match(FortranTokenKind.KeywordNone))
        {
            return new FortranImplicitNoneStatement();
        }

        // Skip other implicit statements for now
        SkipToStatementEnd();
        return null;
    }

    private FortranStatement? TryParseDeclaration()
    {
        int checkpoint = _position;
        try
        {
            var typeKind = Current.Kind;
            if (!IsTypeSpecifier(typeKind))
                return null;

            // Consume the base type token (INTEGER/REAL/LOGICAL/CHARACTER)
            Advance();

            // Handle CHARACTER(...) kind/len and other type parameter lists
            // e.g. CHARACTER(len=20) or CHARACTER(LEN=*)
            if (Check(FortranTokenKind.LParen))
            {
                SkipParenthesizedContent();
            }

            // Optional attribute list before '::', e.g. CHARACTER(len=20), INTENT(IN) :: name
            // Conservatively skip tokens until '::' if attributes are present
            if (Check(FortranTokenKind.Comma))
            {
                while (!IsAtEnd)
                {
                    if (Check(FortranTokenKind.DoubleColon))
                    {
                        Advance();
                        break;
                    }
                    if (Check(FortranTokenKind.LParen))
                    {
                        SkipParenthesizedContent();
                        continue;
                    }
                    if (Check(FortranTokenKind.Comma))
                    {
                        Advance();
                        continue;
                    }
                    // Advance over attribute identifiers/keywords
                    Advance();
                }
            }

            var names = new List<string>();

            // If '::' appears without attributes, consume it
            if (Check(FortranTokenKind.DoubleColon))
            {
                Advance();
            }

            // Parse variable list
            do
            {
                // Accept both Identifier and keyword tokens as variable names
                // (lexer is overly aggressive with keywords)
                if (Check(FortranTokenKind.Identifier) || IsContextualKeyword(Current.Kind))
                {
                    names.Add(Advance().Text);
                    
                    // Handle array notation
                    if (Check(FortranTokenKind.LParen))
                    {
                        int depth = 1;
                        Advance();
                        while (depth > 0 && !IsAtEnd)
                        {
                            if (Check(FortranTokenKind.LParen)) depth++;
                            else if (Check(FortranTokenKind.RParen)) depth--;
                            Advance();
                        }
                    }
                }
            } while (Match(FortranTokenKind.Comma) && !IsAtEnd);

            if (names.Count == 0)
            {
                _position = checkpoint;
                return null;
            }

            var type = MapTokenToType(typeKind);
            return new FortranDeclarationStatement(type, names);
        }
        catch
        {
            _position = checkpoint;
            return null;
        }
    }

    private FortranStatement? ParseAllocateStatement()
    {
        if (!Match(FortranTokenKind.KeywordAllocate))
            return null;

        SkipParenthesizedContent();
        return null;
    }

    private FortranStatement? ParseDeallocateStatement()
    {
        if (!Match(FortranTokenKind.KeywordDeallocate))
            return null;

        SkipParenthesizedContent();
        return null;
    }

    private FortranStatement? ParsePrintStatement()
    {
        if (!Match(FortranTokenKind.KeywordPrint))
            return null;

        if (!Match(FortranTokenKind.Star))
        {
            SkipToStatementEnd();
            return new FortranPrintStatement(new List<FortranExpression>());
        }

        if (!Match(FortranTokenKind.Comma))
        {
            SkipToStatementEnd();
            return new FortranPrintStatement(new List<FortranExpression>());
        }

        var args = ParseExpressionList();
        return new FortranPrintStatement(args);
    }

    private FortranStatement? ParseReadStatement()
    {
        if (!Match(FortranTokenKind.KeywordRead))
            return null;

        // Control list: READ ( *, * ) ...
        if (Match(FortranTokenKind.LParen))
        {
            // Be permissive: ensure at least one '*' exists, ignore commas/spaces until ')'
            bool sawStar = false;
            int parenDepth = 1;
            while (parenDepth > 0 && !IsAtEnd)
            {
                if (Check(FortranTokenKind.Star)) sawStar = true;
                if (Match(FortranTokenKind.LParen)) { parenDepth++; continue; }
                if (Match(FortranTokenKind.RParen)) { parenDepth--; continue; }
                Advance();
            }
            if (!sawStar && _debug)
            {
                System.Console.Error.WriteLine("[PARSER] READ control list missing '*'");
            }
        }

        // Parse variable list
        var variables = new List<string>();
        do
        {
            // Accept both Identifier and keyword tokens as variable names
            if (Check(FortranTokenKind.Identifier) || IsContextualKeyword(Current.Kind))
            {
                variables.Add(Advance().Text);
            }
        } while (Match(FortranTokenKind.Comma) && !IsAtEnd);

        return new FortranReadStatement(variables);
    }

    private FortranStatement? ParseWriteStatement()
    {
        if (!Match(FortranTokenKind.KeywordWrite))
            return null;

        if (!Match(FortranTokenKind.LParen))
        {
            SkipToStatementEnd();
            return new FortranWriteStatement(new List<FortranExpression>());
        }

        if (!Match(FortranTokenKind.Star) || !Match(FortranTokenKind.Comma) || !Match(FortranTokenKind.Star))
        {
            SkipParenthesizedContent();
            return new FortranWriteStatement(new List<FortranExpression>());
        }

        if (!Match(FortranTokenKind.RParen))
        {
            SkipParenthesizedContent();
            return new FortranWriteStatement(new List<FortranExpression>());
        }

        var args = ParseExpressionList();
        return new FortranWriteStatement(args);
    }

    private FortranStatement? ParseCallStatement()
    {
        if (!Match(FortranTokenKind.KeywordCall))
            return null;

        if (!Check(FortranTokenKind.Identifier))
        {
            SkipToStatementEnd();
            return null;
        }

        string name = Advance().Text;
        var args = new List<FortranExpression>();

        if (Match(FortranTokenKind.LParen))
        {
            args = ParseExpressionList();
            if (!Match(FortranTokenKind.RParen))
            {
                SkipParenthesizedContent();
            }
        }

        return new FortranCallStatement(name, args);
    }

    private FortranStatement? ParseIfStatement()
    {
        if (!Match(FortranTokenKind.KeywordIf))
            return null;

        if (!Match(FortranTokenKind.LParen))
        {
            SkipToStatementEnd();
            return null;
        }

        var condition = ParseExpression();

        if (!Match(FortranTokenKind.RParen))
        {
            SkipParenthesizedContent();
        }

        if (!Match(FortranTokenKind.KeywordThen))
        {
            SkipToStatementEnd();
            return null;
        }

        var thenBlock = ParseStatementBlockUntil(
            FortranTokenKind.KeywordElseif,
            FortranTokenKind.KeywordElse,
            FortranTokenKind.KeywordEndif,
            FortranTokenKind.KeywordEnd);

        var elseIfParts = new List<(FortranExpression, IReadOnlyList<FortranStatement>)>();
        while (Check(FortranTokenKind.KeywordElseif))
        {
            Advance();
            if (!Match(FortranTokenKind.LParen))
                break;

            var elseIfCond = ParseExpression();
            if (!Match(FortranTokenKind.RParen))
                SkipParenthesizedContent();
            if (!Match(FortranTokenKind.KeywordThen))
                break;

            var elseIfBlock = ParseStatementBlockUntil(
                FortranTokenKind.KeywordElseif,
                FortranTokenKind.KeywordElse,
                FortranTokenKind.KeywordEndif,
                FortranTokenKind.KeywordEnd);

            elseIfParts.Add((elseIfCond, elseIfBlock));
        }

        IReadOnlyList<FortranStatement>? elseBlock = null;
        if (Match(FortranTokenKind.KeywordElse))
        {
            elseBlock = ParseStatementBlockUntil(
                FortranTokenKind.KeywordEndif,
                FortranTokenKind.KeywordEnd);
        }

        if (!Match(FortranTokenKind.KeywordEndif))
        {
            Match(FortranTokenKind.KeywordEnd);
        }

        return new FortranIfStatement(condition, thenBlock, elseIfParts, elseBlock);
    }

    private FortranStatement? ParseDoStatement()
    {
        if (!Match(FortranTokenKind.KeywordDo))
            return null;

        // DO WHILE variant
        if (CheckIdentifier("while"))
        {
            Advance();
            if (!Match(FortranTokenKind.LParen))
            {
                SkipToStatementEnd();
                return null;
            }

            var condition = ParseExpression();

            if (!Match(FortranTokenKind.RParen))
                SkipParenthesizedContent();

            var body = ParseStatementBlockUntil(
                FortranTokenKind.KeywordEnddo,
                FortranTokenKind.KeywordEnd);

            Match(FortranTokenKind.KeywordEnddo);
            Match(FortranTokenKind.KeywordEnd);

            return new FortranDoWhileStatement(condition, body);
        }

        // Standard DO loop
        if (!Check(FortranTokenKind.Identifier))
        {
            SkipToStatementEnd();
            return null;
        }

        string loopVar = Advance().Text;

        if (!Match(FortranTokenKind.Equals))
        {
            SkipToStatementEnd();
            return null;
        }

        var start = ParseExpression();

        if (!Match(FortranTokenKind.Comma))
        {
            SkipToStatementEnd();
            return null;
        }

        var end = ParseExpression();

        FortranExpression? step = null;
        if (Match(FortranTokenKind.Comma))
        {
            step = ParseExpression();
        }

        var statements = ParseStatementBlockUntil(
            FortranTokenKind.KeywordEnddo,
            FortranTokenKind.KeywordEnd);

        if (!Match(FortranTokenKind.KeywordEnddo))
        {
            Match(FortranTokenKind.KeywordEnd);
        }

        return new FortranDoStatement(loopVar, start, end, step, statements);
    }

    private FortranStatement? TryParseAssignmentOrCall()
    {
        int checkpoint = _position;

        if (!Check(FortranTokenKind.Identifier))
            return null;

        string name = Advance().Text;

        // Could be array indexing or function call
        if (Check(FortranTokenKind.LParen))
        {
            int depth = 1;
            Advance();
            while (depth > 0 && !IsAtEnd)
            {
                if (Check(FortranTokenKind.LParen)) depth++;
                else if (Check(FortranTokenKind.RParen)) depth--;
                Advance();
            }

            // If followed by assignment, parse assignment
            if (Check(FortranTokenKind.Equals))
            {
                _position = checkpoint;
                return null; // Complex assignment - skip for now
            }

            // Otherwise, might be a function call
            _position = checkpoint;
            return null;
        }

        // Check for assignment
        if (Match(FortranTokenKind.Equals))
        {
            var expr = ParseExpression();
            return new FortranAssignmentStatement(name, expr);
        }

        // Reset - not an assignment
        _position = checkpoint;
        return null;
    }

    private FortranSubroutineDefinition? ParseSubroutine()
    {
        if (!Match(FortranTokenKind.KeywordSubroutine))
            return null;

        if (!Check(FortranTokenKind.Identifier))
            return null;

        string name = Advance().Text;
        var parameters = new List<string>();

        if (Match(FortranTokenKind.LParen))
        {
            do
            {
                if (Check(FortranTokenKind.Identifier))
                {
                    parameters.Add(Advance().Text);
                }
            } while (Match(FortranTokenKind.Comma) && !IsAtEnd);

            Match(FortranTokenKind.RParen);
        }

        var statements = ParseStatementBlockUntil(
            FortranTokenKind.KeywordEndSubroutine,
            FortranTokenKind.KeywordEnd);

        if (!Match(FortranTokenKind.KeywordEndSubroutine))
        {
            Match(FortranTokenKind.KeywordEnd);
        }

        return new FortranSubroutineDefinition(name, parameters, statements);
    }


    // ====================================================================
    // Expression Parsing (Simplified - can be enhanced)
    // ====================================================================

    private List<FortranExpression> ParseExpressionList()
    {
        var expressions = new List<FortranExpression>();

        do
        {
            try
            {
                expressions.Add(ParseExpression());
            }
            catch
            {
                break;
            }
        } while (Match(FortranTokenKind.Comma) && !IsAtEnd);

        return expressions;
    }

    private FortranExpression ParseExpression()
    {
        return ParseLogicalOr();
    }

    private FortranExpression ParseLogicalOr()
    {
        var expr = ParseLogicalAnd();

        while (Match(FortranTokenKind.OrFortran))
        {
            var right = ParseLogicalAnd();
            expr = new FortranBinaryExpression(expr, FortranBinaryOperator.Or, right);
        }

        return expr;
    }

    private FortranExpression ParseLogicalAnd()
    {
        var expr = ParseComparison();

        while (Match(FortranTokenKind.AndFortran))
        {
            var right = ParseComparison();
            expr = new FortranBinaryExpression(expr, FortranBinaryOperator.And, right);
        }

        return expr;
    }

    private FortranExpression ParseComparison()
    {
        var expr = ParseAdditive();

        while (true)
        {
            if (Match(FortranTokenKind.EqFortran))
            {
                var right = ParseAdditive();
                expr = new FortranBinaryExpression(expr, FortranBinaryOperator.EqFortran, right);
            }
            else if (Match(FortranTokenKind.EqModern))
            {
                var right = ParseAdditive();
                expr = new FortranBinaryExpression(expr, FortranBinaryOperator.EqModern, right);
            }
            else if (Match(FortranTokenKind.LtFortran))
            {
                var right = ParseAdditive();
                expr = new FortranBinaryExpression(expr, FortranBinaryOperator.LtFortran, right);
            }
            else if (Match(FortranTokenKind.LtModern))
            {
                var right = ParseAdditive();
                expr = new FortranBinaryExpression(expr, FortranBinaryOperator.LtModern, right);
            }
            else if (Match(FortranTokenKind.LeFortran))
            {
                var right = ParseAdditive();
                expr = new FortranBinaryExpression(expr, FortranBinaryOperator.LeFortran, right);
            }
            else if (Match(FortranTokenKind.LeModern))
            {
                var right = ParseAdditive();
                expr = new FortranBinaryExpression(expr, FortranBinaryOperator.LeModern, right);
            }
            else if (Match(FortranTokenKind.GtFortran))
            {
                var right = ParseAdditive();
                expr = new FortranBinaryExpression(expr, FortranBinaryOperator.GtFortran, right);
            }
            else if (Match(FortranTokenKind.GtModern))
            {
                var right = ParseAdditive();
                expr = new FortranBinaryExpression(expr, FortranBinaryOperator.GtModern, right);
            }
            else if (Match(FortranTokenKind.GeFortran))
            {
                var right = ParseAdditive();
                expr = new FortranBinaryExpression(expr, FortranBinaryOperator.GeFortran, right);
            }
            else if (Match(FortranTokenKind.GeModern))
            {
                var right = ParseAdditive();
                expr = new FortranBinaryExpression(expr, FortranBinaryOperator.GeModern, right);
            }
            else
            {
                break;
            }
        }

        return expr;
    }

    private FortranExpression ParseAdditive()
    {
        var expr = ParseMultiplicative();

        while (Match(FortranTokenKind.Plus) || Match(FortranTokenKind.Minus))
        {
            var op = Previous.Kind == FortranTokenKind.Plus
                ? FortranBinaryOperator.Add
                : FortranBinaryOperator.Subtract;
            var right = ParseMultiplicative();
            expr = new FortranBinaryExpression(expr, op, right);
        }

        return expr;
    }

    private FortranExpression ParseMultiplicative()
    {
        var expr = ParseUnary();

        while (Match(FortranTokenKind.Star) || Match(FortranTokenKind.Slash))
        {
            var op = Previous.Kind == FortranTokenKind.Star
                ? FortranBinaryOperator.Multiply
                : FortranBinaryOperator.Divide;
            var right = ParseUnary();
            expr = new FortranBinaryExpression(expr, op, right);
        }

        return expr;
    }

    private FortranExpression ParseUnary()
    {
        if (Match(FortranTokenKind.Minus))
        {
            var operand = ParseUnary();
            return new FortranUnaryExpression(FortranUnaryOperator.Minus, operand);
        }

        if (Match(FortranTokenKind.Plus))
        {
            return ParseUnary();
        }

        return ParsePrimary();
    }

    private FortranExpression ParsePrimary()
    {
        // Integer literal
        if (Check(FortranTokenKind.IntegerLiteral))
        {
            long value = long.Parse(Advance().Text, CultureInfo.InvariantCulture);
            return new FortranIntegerLiteralExpression(value);
        }

        // Real literal
        if (Check(FortranTokenKind.RealLiteral))
        {
            double value = double.Parse(Advance().Text, CultureInfo.InvariantCulture);
            return new FortranRealLiteralExpression(value);
        }

        // String literal
        if (Check(FortranTokenKind.StringLiteral))
        {
            string value = Advance().Text;
            return new FortranStringLiteralExpression(value);
        }

        // Logical literals
        if (Match(FortranTokenKind.KeywordTrue))
        {
            return new FortranLogicalLiteralExpression(true);
        }

        if (Match(FortranTokenKind.KeywordFalse))
        {
            return new FortranLogicalLiteralExpression(false);
        }

        // Identifier or function call (accept contextual keywords as identifiers)
        if (IsIdentifierOrContextualKeyword())
        {
            string name = Advance().Text;

            if (Match(FortranTokenKind.LParen))
            {
                var args = ParseExpressionList();
                Match(FortranTokenKind.RParen);
                return new FortranCallExpression(name, args);
            }

            return new FortranIdentifierExpression(name);
        }

        // Parenthesized expression
        if (Match(FortranTokenKind.LParen))
        {
            var expr = ParseExpression();
            Match(FortranTokenKind.RParen);
            return expr;
        }

        throw new ParseException($"Unexpected token in expression: {Current.Kind}");
    }

    // ====================================================================
    // Helpers
    // ====================================================================

    private bool IsTerminator(FortranTokenKind[] terminators)
    {
        foreach (var term in terminators)
        {
            if (Check(term))
                return true;
        }
        return IsAtEnd;
    }

    private void SkipToStatementEnd()
    {
        while (!IsAtEnd && !IsStatementBoundary(Current.Kind))
        {
            Advance();
        }
    }

    private void SkipParenthesizedContent()
    {
        if (!Match(FortranTokenKind.LParen))
            return;

        int depth = 1;
        while (depth > 0 && !IsAtEnd)
        {
            if (Check(FortranTokenKind.LParen)) depth++;
            else if (Check(FortranTokenKind.RParen)) depth--;
            Advance();
        }
    }

    private void SkipToNextSubroutineOrEnd()
    {
        while (!IsAtEnd && !Check(FortranTokenKind.KeywordSubroutine) &&
               !Check(FortranTokenKind.KeywordEnd) && !Check(FortranTokenKind.KeywordEndProgram))
        {
            Advance();
        }
    }

    private bool IsStatementBoundary(FortranTokenKind kind)
    {
        return kind == FortranTokenKind.KeywordImplicit
            || kind == FortranTokenKind.KeywordPrint
            || kind == FortranTokenKind.KeywordRead
            || kind == FortranTokenKind.KeywordWrite
            || kind == FortranTokenKind.KeywordCall
            || kind == FortranTokenKind.KeywordAllocate
            || kind == FortranTokenKind.KeywordDeallocate
            || kind == FortranTokenKind.KeywordDo
            || kind == FortranTokenKind.KeywordIf
            || kind == FortranTokenKind.KeywordElse
            || kind == FortranTokenKind.KeywordElseif
            || kind == FortranTokenKind.KeywordEnd
            || kind == FortranTokenKind.KeywordEnddo
            || kind == FortranTokenKind.KeywordEndif
            || kind == FortranTokenKind.KeywordEndProgram
            || kind == FortranTokenKind.KeywordEndSubroutine
            || kind == FortranTokenKind.KeywordReturn
            || kind == FortranTokenKind.KeywordContains
            || kind == FortranTokenKind.KeywordEndModule
            || kind == FortranTokenKind.KeywordModule;
    }

    private bool IsTypeSpecifier(FortranTokenKind kind)
    {
        return kind == FortranTokenKind.KeywordInteger
            || kind == FortranTokenKind.KeywordReal
            || kind == FortranTokenKind.KeywordLogical
            || kind == FortranTokenKind.KeywordCharacter;
    }

    /// <summary>
    /// Check if a keyword token can be used as a variable name in this context.
    /// The lexer is too aggressive with keywords, so we allow them as identifiers in many places.
    /// </summary>
    private bool IsContextualKeyword(FortranTokenKind kind)
    {
        return kind == FortranTokenKind.KeywordValue
            || kind == FortranTokenKind.KeywordIntent
            || kind == FortranTokenKind.KeywordOptional
            || kind == FortranTokenKind.KeywordTarget
            || kind == FortranTokenKind.KeywordAllocatable
            || kind == FortranTokenKind.KeywordParameter
            || kind == FortranTokenKind.KeywordDimension
            || kind == FortranTokenKind.KeywordPublic
            || kind == FortranTokenKind.KeywordPrivate
            || kind == FortranTokenKind.KeywordProtected;
    }

    private FortranTypeSpec MapTokenToType(FortranTokenKind kind)
    {
        return kind switch
        {
            FortranTokenKind.KeywordInteger => new FortranTypeSpec(FortranTypeKind.Integer),
            FortranTokenKind.KeywordReal => new FortranTypeSpec(FortranTypeKind.Real),
            FortranTokenKind.KeywordLogical => new FortranTypeSpec(FortranTypeKind.Logical),
            FortranTokenKind.KeywordCharacter => new FortranTypeSpec(FortranTypeKind.Character),
            _ => throw new ParseException($"Unknown type: {kind}")
        };
    }

    private bool CheckIdentifier(string text)
    {
        return Check(FortranTokenKind.Identifier) &&
               string.Equals(Current.Text, text, StringComparison.OrdinalIgnoreCase);
    }

    private bool Check(FortranTokenKind kind)
    {
        if (IsAtEnd)
            return kind == FortranTokenKind.EndOfFile;
        return Current.Kind == kind;
    }

    private bool IsIdentifierOrContextualKeyword()
    {
        return Check(FortranTokenKind.Identifier) || IsContextualKeyword(Current.Kind);
    }

    private bool Match(FortranTokenKind kind)
    {
        if (Check(kind))
        {
            Advance();
            return true;
        }
        return false;
    }

    private FortranToken Advance()
    {
        if (!IsAtEnd)
            _position++;
        return Previous;
    }

    private FortranToken Consume(FortranTokenKind kind, string message)
    {
        if (Check(kind))
            return Advance();
        throw new ParseException(message);
    }

    private bool IsAtEnd => Current.Kind == FortranTokenKind.EndOfFile;
    private FortranToken Current => _tokens[_position];
    private FortranToken Previous => _tokens[_position - 1];
    private readonly bool _debug;

    private class ParseException : Exception
    {
        public ParseException(string message) : base(message) { }
    }
}
