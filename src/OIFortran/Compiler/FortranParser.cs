using System;
using System.Collections.Generic;
using System.Globalization;

namespace ObjectIR.Fortran.Compiler;

internal sealed class FortranParseException : Exception
{
    public FortranParseException(string message) : base(message) { }
}

internal sealed class FortranParser
{
    private readonly IReadOnlyList<FortranToken> _tokens;
    private int _position;

    public FortranParser(IReadOnlyList<FortranToken> tokens)
    {
        _tokens = tokens;
    }

    public FortranProgram ParseProgram()
    {
        // Check if this is a MODULE or PROGRAM
        if (Check(FortranTokenKind.KeywordModule))
        {
            // Parse as module but wrap in a program-like structure for backward compatibility
            var module = ParseModule();
            // Convert FortranModule to FortranProgram by wrapping it
            // After parsing module, check if there's a PROGRAM following
            if (Check(FortranTokenKind.KeywordProgram))
            {
                // Parse the program that follows - for now just skip it
                SkipProgram();
            }
            return new FortranProgram(module.Name, module.Declarations, module.Subroutines);
        }

        Consume(FortranTokenKind.KeywordProgram, "Expected 'program' or 'module' at start of source");
        string name = Consume(FortranTokenKind.Identifier, "Expected program name").Text;

        var statements = new List<FortranStatement>();
        while (!Check(FortranTokenKind.KeywordEnd) &&
               !Check(FortranTokenKind.KeywordEndProgram) &&
               !IsAtEnd)
        {
            try
            {
                var statement = TryParseExecutableStatement(allowReturn: false);
                if (statement != null)
                {
                    statements.Add(statement);
                    continue;
                }

                SkipUnknownStatement();
            }
            catch
            {
                SkipUnknownStatement();
            }
        }

        // Handle both "END PROGRAM" (single token) or "END" followed by "PROGRAM"
        if (Match(FortranTokenKind.KeywordEndProgram))
        {
            // Good - consumed the combined token
        }
        else
        {
            Consume(FortranTokenKind.KeywordEnd, "Expected 'end' to close program");
            Match(FortranTokenKind.KeywordProgram);
        }
        
        if (Check(FortranTokenKind.Identifier))
        {
            Advance();
        }

        // Parse any subroutines after the main program
        var subroutines = new List<FortranSubroutineDefinition>();
        while (Check(FortranTokenKind.KeywordSubroutine))
        {
            subroutines.Add(ParseSubroutine());
        }

        Consume(FortranTokenKind.EndOfFile, "Unexpected tokens after end program");
        return new FortranProgram(name, statements, subroutines);
    }

    private void SkipProgram()
    {
        // Skip PROGRAM name ... END PROGRAM
        Consume(FortranTokenKind.KeywordProgram, "Expected 'program'");
        Consume(FortranTokenKind.Identifier, "Expected program name");
        
        // Skip everything until END PROGRAM
        while (!IsAtEnd)
        {
            if (Match(FortranTokenKind.KeywordEndProgram))
            {
                // Optional program name after END PROGRAM
                if (Check(FortranTokenKind.Identifier))
                {
                    Advance();
                }
                break;
            }
            else if (Check(FortranTokenKind.KeywordEnd))
            {
                Advance();
                if (Check(FortranTokenKind.KeywordProgram))
                {
                    Advance();
                    // Optional program name after END PROGRAM
                    if (Check(FortranTokenKind.Identifier))
                    {
                        Advance();
                    }
                    break;
                }
            }
            else
            {
                Advance();
            }
        }
    }

    private void SkipFunction()
    {
        // Skip FUNCTION ... END FUNCTION
        Consume(FortranTokenKind.KeywordFunction, "Expected 'function'");
        Consume(FortranTokenKind.Identifier, "Expected function name");
        
        // Skip parameters if present
        if (Match(FortranTokenKind.LParen))
        {
            int parenLevel = 1;
            while (parenLevel > 0 && !IsAtEnd)
            {
                if (Check(FortranTokenKind.LParen))
                {
                    parenLevel++;
                }
                else if (Check(FortranTokenKind.RParen))
                {
                    parenLevel--;
                }
                Advance();
            }
        }
        
        // Skip everything until END FUNCTION
        while (!IsAtEnd)
        {
            if (Match(FortranTokenKind.KeywordEndFunction))
            {
                // Optional function name after END FUNCTION
                if (Check(FortranTokenKind.Identifier))
                {
                    Advance();
                }
                break;
            }
            else
            {
                Advance();
            }
        }
    }

    private FortranSubroutineDefinition ParseSubroutine()
    {
        Consume(FortranTokenKind.KeywordSubroutine, "Expected 'subroutine'");
        string name = Consume(FortranTokenKind.Identifier, "Expected subroutine name").Text;

        var parameters = new List<string>();
        if (Match(FortranTokenKind.LParen))
        {
            if (!Check(FortranTokenKind.RParen))
            {
                parameters.Add(Consume(FortranTokenKind.Identifier, "Expected parameter name").Text);
                while (Match(FortranTokenKind.Comma))
                {
                    parameters.Add(Consume(FortranTokenKind.Identifier, "Expected parameter name").Text);
                }
            }
            Consume(FortranTokenKind.RParen, "Expected ')' after parameters");
        }

        var statements = new List<FortranStatement>();
        while (!Check(FortranTokenKind.KeywordEnd) &&
               !Check(FortranTokenKind.KeywordEndSubroutine) &&
               !IsAtEnd)
        {
            try
            {
                var statement = TryParseExecutableStatement(allowReturn: true);
                if (statement != null)
                {
                    statements.Add(statement);
                    continue;
                }

                SkipUnknownStatement();
            }
            catch
            {
                SkipUnknownStatement();
            }
        }

        // Consume END SUBROUTINE (compound token)
        if (Check(FortranTokenKind.KeywordEndSubroutine))
        {
            Consume(FortranTokenKind.KeywordEndSubroutine, "Expected 'end subroutine'");
            // Optional subroutine name
            if (Check(FortranTokenKind.Identifier))
            {
                Advance();
            }
        }
        else if (Check(FortranTokenKind.KeywordEnd))
        {
            // Fall back to generic END handling for non-compound END SUBROUTINE
            Advance(); // consume END
            // Try to consume SUBROUTINE keyword if present
            Match(FortranTokenKind.KeywordSubroutine);
            // Optional subroutine name
            if (Check(FortranTokenKind.Identifier))
            {
                Advance();
            }
        }

        return new FortranSubroutineDefinition(name, parameters, statements);
    }

    private FortranSubroutineDefinition ParseFunctionAsSubroutine()
    {
        // Parse a FUNCTION as a stub - don't try to parse the body since it's complex
        Consume(FortranTokenKind.KeywordFunction, "Expected 'function'");
        string name = Consume(FortranTokenKind.Identifier, "Expected function name").Text;

        var parameters = new List<string>();
        if (Match(FortranTokenKind.LParen))
        {
            if (!Check(FortranTokenKind.RParen))
            {
                parameters.Add(Consume(FortranTokenKind.Identifier, "Expected parameter name").Text);
                while (Match(FortranTokenKind.Comma))
                {
                    parameters.Add(Consume(FortranTokenKind.Identifier, "Expected parameter name").Text);
                }
            }
            Consume(FortranTokenKind.RParen, "Expected ')' after parameters");
        }

        // Skip everything until END FUNCTION without trying to parse the body
        int depth = 0;
        while (!IsAtEnd)
        {
            if (Check(FortranTokenKind.KeywordEndFunction))
            {
                break;
            }
            // Track nested blocks
            if (Check(FortranTokenKind.KeywordDo) || Check(FortranTokenKind.KeywordIf))
            {
                depth++;
            }
            else if (Check(FortranTokenKind.KeywordEnddo) || Check(FortranTokenKind.KeywordEndif))
            {
                depth--;
            }
            Advance();
        }

        Consume(FortranTokenKind.KeywordEndFunction, "Expected 'end function'");
        if (Check(FortranTokenKind.Identifier))
        {
            Advance();
        }

        // Return empty body - just a stub with parameters
        return new FortranSubroutineDefinition(name, parameters, new List<FortranStatement>());
    }
    private FortranStatement ParseDeclaration()
    {
        var type = ParseTypeSpec();
        Match(FortranTokenKind.DoubleColon);

        var names = new List<string>();
        do
        {
            string name = Consume(FortranTokenKind.Identifier, "Expected identifier in declaration").Text;
            names.Add(name);
        }
        while (Match(FortranTokenKind.Comma));

        return new FortranDeclarationStatement(type, names);
    }

    private FortranDeclarationStatement? TryParseLooseDeclaration()
    {
        int start = _position;
        try
        {
            var type = ParseTypeSpec();
            SkipDeclarationAttributes();
            Match(FortranTokenKind.DoubleColon);

            var names = ParseDeclarationNameList();
            if (names.Count == 0)
            {
                _position = start;
                return null;
            }

            return new FortranDeclarationStatement(type, names);
        }
        catch
        {
            _position = start;
            return null;
        }
    }

    private void SkipDeclarationAttributes()
    {
        while (Check(FortranTokenKind.Comma))
        {
            int saved = _position;
            Advance(); // consume comma

            if (!IsDeclarationAttribute(Current.Kind))
            {
                _position = saved;
                break;
            }

            Advance(); // consume attribute keyword

            if (Check(FortranTokenKind.LParen))
            {
                SkipBalancedParentheses();
            }
        }
    }

    private bool IsDeclarationAttribute(FortranTokenKind kind)
    {
        return kind == FortranTokenKind.KeywordAllocatable
            || kind == FortranTokenKind.KeywordDimension
            || kind == FortranTokenKind.KeywordIntent
            || kind == FortranTokenKind.KeywordPointer
            || kind == FortranTokenKind.KeywordTarget
            || kind == FortranTokenKind.KeywordOptional
            || kind == FortranTokenKind.KeywordValue
            || kind == FortranTokenKind.KeywordParameter
            || kind == FortranTokenKind.KeywordPublic
            || kind == FortranTokenKind.KeywordPrivate
            || kind == FortranTokenKind.KeywordProtected;
    }

    private List<string> ParseDeclarationNameList()
    {
        var names = new List<string>();

        while (Check(FortranTokenKind.Identifier))
        {
            string name = Advance().Text;
            names.Add(name);

            SkipPostNameSpecifiers();

            if (!Match(FortranTokenKind.Comma))
            {
                break;
            }
        }

        return names;
    }

    private void SkipPostNameSpecifiers()
    {
        // Skip array or function argument specifiers (e.g., (:,:))
        while (Check(FortranTokenKind.LParen))
        {
            SkipBalancedParentheses();
        }

        // Skip initialization expressions (name = expr or name => target)
        if (Match(FortranTokenKind.Equals) || Match(FortranTokenKind.Arrow))
        {
            SkipExpressionUntilDeclarationTerminator();
        }
    }

    private void SkipBalancedParentheses()
    {
        if (!Match(FortranTokenKind.LParen))
        {
            return;
        }

        int depth = 1;
        while (depth > 0 && !IsAtEnd)
        {
            if (Check(FortranTokenKind.LParen))
            {
                depth++;
            }
            else if (Check(FortranTokenKind.RParen))
            {
                depth--;
            }
            Advance();
        }
    }

    private void SkipExpressionUntilDeclarationTerminator()
    {
        int depth = 0;
        while (!IsAtEnd)
        {
            if (Check(FortranTokenKind.LParen))
            {
                depth++;
                Advance();
                continue;
            }

            if (Check(FortranTokenKind.RParen))
            {
                if (depth == 0)
                {
                    break;
                }
                depth--;
                Advance();
                continue;
            }

            if (depth == 0)
            {
                if (Check(FortranTokenKind.Comma) || IsDeclarationTerminator(Current.Kind))
                {
                    break;
                }
            }

            Advance();
        }
    }

    private bool IsDeclarationTerminator(FortranTokenKind kind)
    {
        return kind == FortranTokenKind.KeywordImplicit
            || kind == FortranTokenKind.KeywordPrint
            || kind == FortranTokenKind.KeywordCall
            || kind == FortranTokenKind.KeywordAllocate
            || kind == FortranTokenKind.KeywordDeallocate
            || kind == FortranTokenKind.KeywordDo
            || kind == FortranTokenKind.KeywordIf
            || kind == FortranTokenKind.KeywordEnd
            || kind == FortranTokenKind.KeywordEndProgram
            || kind == FortranTokenKind.KeywordContains
            || kind == FortranTokenKind.KeywordSubroutine
            || kind == FortranTokenKind.KeywordFunction;
    }

    private FortranAssignmentStatement? ParseAssignment()
    {
        string name = Consume(FortranTokenKind.Identifier, "Expected identifier").Text;
        
        // Check if this is actually an array reference or subroutine call, not an assignment
        if (Check(FortranTokenKind.LParen))
        {
            // This might be a subroutine call like data(n,m) or array indexing
            // Skip it entirely - don't try to parse it
            int depth = 1;
            Advance(); // consume LParen
            while (depth > 0 && !IsAtEnd)
            {
                if (Check(FortranTokenKind.LParen)) depth++;
                else if (Check(FortranTokenKind.RParen)) depth--;
                Advance();
            }
            // If this was followed by assignment we currently don't support it
            // Treat it as unrecognized for now.
            return null;
        }
        
        if (!Check(FortranTokenKind.Equals))
        {
            // Not an assignment - skip this line
            while (!IsAtEnd && Current.Kind != FortranTokenKind.KeywordImplicit &&
                   Current.Kind != FortranTokenKind.KeywordPrint &&
                   Current.Kind != FortranTokenKind.KeywordRead &&
                   Current.Kind != FortranTokenKind.KeywordWrite &&
                   Current.Kind != FortranTokenKind.KeywordCall &&
                   Current.Kind != FortranTokenKind.KeywordAllocate &&
                   Current.Kind != FortranTokenKind.KeywordDeallocate &&
                   Current.Kind != FortranTokenKind.KeywordDo &&
                   Current.Kind != FortranTokenKind.KeywordIf &&
                   Current.Kind != FortranTokenKind.KeywordEnd)
            {
                Advance();
            }
            return null;
        }
        
        Consume(FortranTokenKind.Equals, "Expected '=' in assignment");
        var expression = ParseExpression();
        return new FortranAssignmentStatement(name, expression);
    }

    private FortranStatement? TryParseExecutableStatement(bool allowReturn)
    {
        if (Match(FortranTokenKind.KeywordImplicit))
        {
            if (Match(FortranTokenKind.KeywordNone))
            {
                return new FortranImplicitNoneStatement();
            }

            return null;
        }

        if (IsTypeSpecifier(Current.Kind))
        {
            var declaration = TryParseLooseDeclaration();
            if (declaration != null)
            {
                return declaration;
            }

            return null;
        }

        if (Match(FortranTokenKind.KeywordAllocate))
        {
            SkipParenthesizedStatement();
            return null;
        }

        if (Match(FortranTokenKind.KeywordDeallocate))
        {
            SkipParenthesizedStatement();
            return null;
        }

        if (Match(FortranTokenKind.KeywordPrint))
        {
            return ParsePrintStatement();
        }

        if (Match(FortranTokenKind.KeywordRead))
        {
            return ParseReadStatement();
        }

        if (Match(FortranTokenKind.KeywordWrite))
        {
            return ParseWriteStatement();
        }

        if (Match(FortranTokenKind.KeywordCall))
        {
            return ParseCallStatement();
        }

        if (Match(FortranTokenKind.KeywordIf))
        {
            return ParseIfConstruct(allowReturn);
        }

        if (Match(FortranTokenKind.KeywordDo))
        {
            return ParseDoConstruct(allowReturn);
        }

        if (allowReturn && Match(FortranTokenKind.KeywordReturn))
        {
            return new FortranReturnStatement();
        }

        if (Check(FortranTokenKind.Identifier))
        {
            return ParseAssignment();
        }

        return null;
    }

    private void SkipParenthesizedStatement()
    {
        if (!Match(FortranTokenKind.LParen))
        {
            return;
        }

        int depth = 1;
        while (depth > 0 && !IsAtEnd)
        {
            if (Check(FortranTokenKind.LParen))
            {
                depth++;
            }
            else if (Check(FortranTokenKind.RParen))
            {
                depth--;
            }
            Advance();
        }
    }

    private List<FortranStatement> ParseStatementBlock(bool allowReturn, params FortranTokenKind[] terminators)
    {
        var statements = new List<FortranStatement>();
        while (!IsAtEnd && !IsCurrentTerminator(terminators))
        {
            try
            {
                var statement = TryParseExecutableStatement(allowReturn);
                if (statement != null)
                {
                    statements.Add(statement);
                    continue;
                }

                SkipUnknownStatement();
            }
            catch
            {
                SkipUnknownStatement();
            }
        }

        return statements;
    }

    private FortranStatement ParseDoConstruct(bool allowReturn)
    {
        int? label = null;
        if (Check(FortranTokenKind.IntegerLiteral))
        {
            label = int.Parse(Advance().Text, CultureInfo.InvariantCulture);
            Match(FortranTokenKind.Comma);
        }

        if (CheckIdentifier("while"))
        {
            Advance();
            Consume(FortranTokenKind.LParen, "Expected '(' after DO WHILE");
            var condition = ParseExpression();
            Consume(FortranTokenKind.RParen, "Expected ')' after DO WHILE condition");

            var body = ParseStatementBlock(allowReturn, FortranTokenKind.KeywordEnddo, FortranTokenKind.KeywordEnd);
            ConsumeEndDo();

            return new FortranDoWhileStatement(condition, body);
        }

        string loopVariable = Consume(FortranTokenKind.Identifier, "Expected loop variable name").Text;
        Consume(FortranTokenKind.Equals, "Expected '=' in DO assignment");
        var start = ParseExpression();
        Consume(FortranTokenKind.Comma, "Expected ',' after loop start expression");
        var end = ParseExpression();

        FortranExpression? step = null;
        if (Match(FortranTokenKind.Comma))
        {
            step = ParseExpression();
        }

        var statements = ParseStatementBlock(allowReturn, FortranTokenKind.KeywordEnddo, FortranTokenKind.KeywordEnd);
        ConsumeEndDo();

        if (label.HasValue && Check(FortranTokenKind.IntegerLiteral))
        {
            Advance();
        }

        return new FortranDoStatement(loopVariable, start, end, step, statements, label);
    }

    private FortranIfStatement ParseIfConstruct(bool allowReturn)
    {
        Consume(FortranTokenKind.LParen, "Expected '(' after IF");
        var condition = ParseExpression();
        Consume(FortranTokenKind.RParen, "Expected ')' after IF condition");
        Consume(FortranTokenKind.KeywordThen, "Expected THEN after IF condition");

        var thenStatements = ParseStatementBlock(allowReturn,
            FortranTokenKind.KeywordElse,
            FortranTokenKind.KeywordElseif,
            FortranTokenKind.KeywordEndif,
            FortranTokenKind.KeywordEnd);

        var elseIfParts = new List<(FortranExpression Condition, IReadOnlyList<FortranStatement> Statements)>();
        while (Match(FortranTokenKind.KeywordElseif))
        {
            Consume(FortranTokenKind.LParen, "Expected '(' after ELSEIF");
            var elseIfCondition = ParseExpression();
            Consume(FortranTokenKind.RParen, "Expected ')' after ELSEIF condition");
            Consume(FortranTokenKind.KeywordThen, "Expected THEN after ELSEIF condition");

            var elseIfStatements = ParseStatementBlock(allowReturn,
                FortranTokenKind.KeywordElse,
                FortranTokenKind.KeywordElseif,
                FortranTokenKind.KeywordEndif,
                FortranTokenKind.KeywordEnd);
            elseIfParts.Add((elseIfCondition, elseIfStatements));
        }

        IReadOnlyList<FortranStatement>? elseStatements = null;
        if (Match(FortranTokenKind.KeywordElse))
        {
            elseStatements = ParseStatementBlock(allowReturn,
                FortranTokenKind.KeywordEndif,
                FortranTokenKind.KeywordEnd);
        }

        ConsumeEndIf();

        return new FortranIfStatement(condition, thenStatements, elseIfParts, elseStatements);
    }

    private void ConsumeEndDo()
    {
        if (Match(FortranTokenKind.KeywordEnddo))
        {
            return;
        }

        Consume(FortranTokenKind.KeywordEnd, "Expected 'end do'");
        Match(FortranTokenKind.KeywordDo);
    }

    private void ConsumeEndIf()
    {
        if (Match(FortranTokenKind.KeywordEndif))
        {
            return;
        }

        Consume(FortranTokenKind.KeywordEnd, "Expected 'end if'");
        Match(FortranTokenKind.KeywordIf);
    }

    private void SkipUnknownStatement()
    {
        int start = _position;
        while (!IsAtEnd && !IsStatementBoundary(Current.Kind))
        {
            Advance();
        }

        if (start == _position && !IsAtEnd)
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
            || kind == FortranTokenKind.KeywordContains;
    }

    private bool IsCurrentTerminator(FortranTokenKind[] terminators)
    {
        foreach (var terminator in terminators)
        {
            if (terminator == FortranTokenKind.KeywordEnd)
            {
                if (Check(FortranTokenKind.KeywordEnd))
                {
                    return true;
                }
            }
            else if (Check(terminator))
            {
                return true;
            }
        }

        return false;
    }

    private bool CheckIdentifier(string text)
    {
        return Check(FortranTokenKind.Identifier) &&
               string.Equals(Current.Text, text, StringComparison.OrdinalIgnoreCase);
    }

    private FortranStatement ParsePrintStatement()
    {
        if (!Match(FortranTokenKind.Star))
        {
            // Not standard PRINT *, skip
            while (!IsAtEnd && Current.Kind != FortranTokenKind.KeywordPrint &&
                   Current.Kind != FortranTokenKind.KeywordCall &&
                   Current.Kind != FortranTokenKind.KeywordDo &&
                   Current.Kind != FortranTokenKind.KeywordEnd)
            {
                Advance();
            }
            return new FortranPrintStatement(new List<FortranExpression>());
        }

        if (!Match(FortranTokenKind.Comma))
        {
            // Not standard format, skip
            while (!IsAtEnd && Current.Kind != FortranTokenKind.KeywordPrint &&
                   Current.Kind != FortranTokenKind.KeywordCall &&
                   Current.Kind != FortranTokenKind.KeywordDo &&
                   Current.Kind != FortranTokenKind.KeywordEnd)
            {
                Advance();
            }
            return new FortranPrintStatement(new List<FortranExpression>());
        }

        var arguments = new List<FortranExpression>();
        
        try
        {
            arguments.Add(ParseExpression());

            while (Match(FortranTokenKind.Comma))
            {
                try
                {
                    arguments.Add(ParseExpression());
                }
                catch
                {
                    break;
                }
            }
        }
        catch
        {
            // If expression parsing fails, just skip
        }

        return new FortranPrintStatement(arguments);
    }

    private FortranStatement ParseReadStatement()
    {
        // Support only list-directed READ(*,*) var1, var2, ...
        // Best-effort parse; if malformed, skip statement.
        try
        {
            if (!Match(FortranTokenKind.LParen))
            {
                SkipParenthesizedStatement();
                return new FortranReadStatement(new List<string>());
            }

            // Expect '*'
            if (!Match(FortranTokenKind.Star))
            {
                SkipParenthesizedStatement();
                return new FortranReadStatement(new List<string>());
            }

            // Expect ','
            if (!Match(FortranTokenKind.Comma))
            {
                SkipParenthesizedStatement();
                return new FortranReadStatement(new List<string>());
            }

            // Expect '*'
            if (!Match(FortranTokenKind.Star))
            {
                SkipParenthesizedStatement();
                return new FortranReadStatement(new List<string>());
            }

            Consume(FortranTokenKind.RParen, "Expected ')' after READ(*,*)");

            var variables = new List<string>();
            // Parse variable list: identifier[, identifier]*
            do
            {
                if (!Check(FortranTokenKind.Identifier))
                {
                    break;
                }
                variables.Add(Advance().Text);
            }
            while (Match(FortranTokenKind.Comma));

            return new FortranReadStatement(variables);
        }
        catch
        {
            // On error, skip remainder
            return new FortranReadStatement(new List<string>());
        }
    }

    private FortranStatement ParseWriteStatement()
    {
        // Treat WRITE(*,*) expr-list as PRINT *, expr-list
        try
        {
            if (!Match(FortranTokenKind.LParen))
            {
                SkipParenthesizedStatement();
                return new FortranWriteStatement(new List<FortranExpression>());
            }

            if (!Match(FortranTokenKind.Star) || !Match(FortranTokenKind.Comma) || !Match(FortranTokenKind.Star))
            {
                SkipParenthesizedStatement();
                return new FortranWriteStatement(new List<FortranExpression>());
            }

            Consume(FortranTokenKind.RParen, "Expected ')' after WRITE(*,*)");

            var arguments = new List<FortranExpression>();
            try
            {
                arguments.Add(ParseExpression());
                while (Match(FortranTokenKind.Comma))
                {
                    try { arguments.Add(ParseExpression()); } catch { break; }
                }
            }
            catch { }

            return new FortranWriteStatement(arguments);
        }
        catch
        {
            return new FortranWriteStatement(new List<FortranExpression>());
        }
    }

    private FortranStatement ParseCallStatement()
    {
        string name = Consume(FortranTokenKind.Identifier, "Expected procedure name").Text;
        var arguments = new List<FortranExpression>();

        try
        {
            if (Match(FortranTokenKind.LParen))
            {
                arguments.AddRange(ParseParenthesizedArgumentList());
            }
        }
        catch
        {
            // If argument parsing fails, just skip
        }

        return new FortranCallStatement(name, arguments);
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
                continue;
            }

            if (Match(FortranTokenKind.EqModern))
            {
                var right = ParseAdditive();
                expr = new FortranBinaryExpression(expr, FortranBinaryOperator.EqModern, right);
                continue;
            }

            if (Match(FortranTokenKind.NeFortran))
            {
                var right = ParseAdditive();
                expr = new FortranBinaryExpression(expr, FortranBinaryOperator.NeFortran, right);
                continue;
            }

            if (Match(FortranTokenKind.NeModern))
            {
                var right = ParseAdditive();
                expr = new FortranBinaryExpression(expr, FortranBinaryOperator.NeModern, right);
                continue;
            }

            if (Match(FortranTokenKind.LtFortran))
            {
                var right = ParseAdditive();
                expr = new FortranBinaryExpression(expr, FortranBinaryOperator.LtFortran, right);
                continue;
            }

            if (Match(FortranTokenKind.LtModern))
            {
                var right = ParseAdditive();
                expr = new FortranBinaryExpression(expr, FortranBinaryOperator.LtModern, right);
                continue;
            }

            if (Match(FortranTokenKind.LeFortran))
            {
                var right = ParseAdditive();
                expr = new FortranBinaryExpression(expr, FortranBinaryOperator.LeFortran, right);
                continue;
            }

            if (Match(FortranTokenKind.LeModern))
            {
                var right = ParseAdditive();
                expr = new FortranBinaryExpression(expr, FortranBinaryOperator.LeModern, right);
                continue;
            }

            if (Match(FortranTokenKind.GtFortran))
            {
                var right = ParseAdditive();
                expr = new FortranBinaryExpression(expr, FortranBinaryOperator.GtFortran, right);
                continue;
            }

            if (Match(FortranTokenKind.GtModern))
            {
                var right = ParseAdditive();
                expr = new FortranBinaryExpression(expr, FortranBinaryOperator.GtModern, right);
                continue;
            }

            if (Match(FortranTokenKind.GeFortran))
            {
                var right = ParseAdditive();
                expr = new FortranBinaryExpression(expr, FortranBinaryOperator.GeFortran, right);
                continue;
            }

            if (Match(FortranTokenKind.GeModern))
            {
                var right = ParseAdditive();
                expr = new FortranBinaryExpression(expr, FortranBinaryOperator.GeModern, right);
                continue;
            }

            break;
        }

        return expr;
    }

    private FortranExpression ParseAdditive()
    {
        var expr = ParseMultiplicative();
        while (true)
        {
            if (Match(FortranTokenKind.Plus))
            {
                var right = ParseMultiplicative();
                expr = new FortranBinaryExpression(expr, FortranBinaryOperator.Add, right);
                continue;
            }

            if (Match(FortranTokenKind.Minus))
            {
                var right = ParseMultiplicative();
                expr = new FortranBinaryExpression(expr, FortranBinaryOperator.Subtract, right);
                continue;
            }

            break;
        }

        return expr;
    }

    private FortranExpression ParseMultiplicative()
    {
        var expr = ParseUnary();
        while (true)
        {
            if (Match(FortranTokenKind.Star))
            {
                var right = ParseUnary();
                expr = new FortranBinaryExpression(expr, FortranBinaryOperator.Multiply, right);
                continue;
            }

            if (Match(FortranTokenKind.Slash))
            {
                var right = ParseUnary();
                expr = new FortranBinaryExpression(expr, FortranBinaryOperator.Divide, right);
                continue;
            }

            break;
        }

        return expr;
    }

    private FortranExpression ParseUnary()
    {
        if (Match(FortranTokenKind.Plus))
        {
            var operand = ParseUnary();
            return new FortranUnaryExpression(FortranUnaryOperator.Plus, operand);
        }

        if (Match(FortranTokenKind.Minus))
        {
            var operand = ParseUnary();
            return new FortranUnaryExpression(FortranUnaryOperator.Minus, operand);
        }

        if (Match(FortranTokenKind.NotFortran))
        {
            var operand = ParseUnary();
            return new FortranUnaryExpression(FortranUnaryOperator.Not, operand);
        }

        return ParsePrimary();
    }

    private FortranExpression ParsePrimary()
    {
        if (Match(FortranTokenKind.IntegerLiteral))
        {
            string text = Previous.Text;
            long value = long.Parse(text, CultureInfo.InvariantCulture);
            return new FortranIntegerLiteralExpression(value);
        }

        if (Match(FortranTokenKind.RealLiteral))
        {
            string text = Previous.Text.Replace('d', 'e').Replace('D', 'E');
            double value = double.Parse(text, CultureInfo.InvariantCulture);
            return new FortranRealLiteralExpression(value);
        }

        if (Match(FortranTokenKind.StringLiteral))
        {
            return new FortranStringLiteralExpression(Previous.Text);
        }

        if (Match(FortranTokenKind.KeywordTrue))
        {
            return new FortranLogicalLiteralExpression(true);
        }

        if (Match(FortranTokenKind.KeywordFalse))
        {
            return new FortranLogicalLiteralExpression(false);
        }

        if (Check(FortranTokenKind.Identifier))
        {
            var identifier = Advance();
            if (Match(FortranTokenKind.LParen))
            {
                var args = ParseParenthesizedArgumentList();
                return new FortranCallExpression(identifier.Text, args);
            }

            return new FortranIdentifierExpression(identifier.Text);
        }

        if (Match(FortranTokenKind.LParen))
        {
            var expr = ParseExpression();
            Consume(FortranTokenKind.RParen, "Expected ')' after expression");
            return expr;
        }

        throw Error(Current, "Expected expression");
    }

    private FortranTypeSpec ParseTypeSpec()
    {
        if (Match(FortranTokenKind.KeywordInteger))
        {
            return new FortranTypeSpec(FortranTypeKind.Integer);
        }
        if (Match(FortranTokenKind.KeywordReal))
        {
            return new FortranTypeSpec(FortranTypeKind.Real);
        }
        if (Match(FortranTokenKind.KeywordLogical))
        {
            return new FortranTypeSpec(FortranTypeKind.Logical);
        }
        if (Match(FortranTokenKind.KeywordCharacter))
        {
            return new FortranTypeSpec(FortranTypeKind.Character);
        }

        throw Error(Current, "Expected type specifier");
    }

    private bool IsTypeSpecifier(FortranTokenKind kind)
    {
        return kind == FortranTokenKind.KeywordInteger
            || kind == FortranTokenKind.KeywordReal
            || kind == FortranTokenKind.KeywordLogical
            || kind == FortranTokenKind.KeywordCharacter;
    }

    private List<FortranExpression> ParseParenthesizedArgumentList()
    {
        var arguments = new List<FortranExpression>();

        if (!Check(FortranTokenKind.RParen))
        {
            arguments.Add(ParseExpression());
            while (Match(FortranTokenKind.Comma))
            {
                arguments.Add(ParseExpression());
            }
        }

        Consume(FortranTokenKind.RParen, "Expected ')' after arguments");
        return arguments;
    }

    private FortranToken Consume(FortranTokenKind kind, string message)
    {
        if (Check(kind))
        {
            return Advance();
        }

        throw Error(Current, message);
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

    private bool Check(FortranTokenKind kind)
    {
        if (IsAtEnd)
        {
            return kind == FortranTokenKind.EndOfFile;
        }

        return Current.Kind == kind;
    }

    private FortranToken Advance()
    {
        if (!IsAtEnd)
        {
            _position++;
        }
        return Previous;
    }

    private bool IsAtEnd => Current.Kind == FortranTokenKind.EndOfFile;

    private FortranToken Current => _tokens[_position];

    private FortranModule ParseModule()
    {
        Consume(FortranTokenKind.KeywordModule, "Expected 'module'");
        string name = Consume(FortranTokenKind.Identifier, "Expected module name").Text;

        var useStatements = new List<FortranUseStatement>();
        var declarations = new List<FortranStatement>();
        var subroutines = new List<FortranSubroutineDefinition>();

        // Parse module contents
        while (!Check(FortranTokenKind.KeywordContains) && 
               !Check(FortranTokenKind.KeywordEndModule) && 
               !IsAtEnd)
        {
            if (Match(FortranTokenKind.KeywordUse))
            {
                useStatements.Add(ParseUseStatement());
                continue;
            }

            if (Match(FortranTokenKind.KeywordImplicit))
            {
                Consume(FortranTokenKind.KeywordNone, "Expected 'none' after 'implicit'");
                declarations.Add(new FortranImplicitNoneStatement());
                continue;
            }

            // Handle TYPE definition: type :: name ... end type
            if (Check(FortranTokenKind.KeywordType))
            {
                Advance(); // consume TYPE
                // Skip the type definition and find END TYPE
                int depth = 1;
                while (depth > 0 && !IsAtEnd)
                {
                    if (Check(FortranTokenKind.KeywordEndType))
                        depth--;
                    if (Check(FortranTokenKind.KeywordType))
                        depth++;
                    Advance();
                }
                continue;
            }

            if (IsTypeSpecifier(Current.Kind))
            {
                // Skip Fortran 90 declarations in module (they might have complex attributes)
                Advance(); // consume type
                
                // Skip tokens until next statement
                int depth = 0;
                while (!IsAtEnd && Current.Kind != FortranTokenKind.KeywordContains &&
                       Current.Kind != FortranTokenKind.KeywordEndModule)
                {
                    if (Current.Kind == FortranTokenKind.LParen)
                        depth++;
                    else if (Current.Kind == FortranTokenKind.RParen)
                        depth--;
                    
                    if (depth < 0 || (depth == 0 && Current.Kind == FortranTokenKind.KeywordImplicit))
                        break;
                    Advance();
                }
                continue;
            }

            // Skip unknown tokens to avoid infinite loop
            Advance();
        }

        // Parse CONTAINS section (procedures inside module)
        if (Match(FortranTokenKind.KeywordContains))
        {
            // Parse procedures (SUBROUTINE and FUNCTION) in the CONTAINS section
            while (!Check(FortranTokenKind.KeywordEndModule) && !IsAtEnd)
            {
                try
                {
                    if (Check(FortranTokenKind.KeywordSubroutine))
                    {
                        subroutines.Add(ParseSubroutine());
                    }
                    else if (Check(FortranTokenKind.KeywordFunction))
                    {
                        // Parse function as a subroutine-like object
                        subroutines.Add(ParseFunctionAsSubroutine());
                    }
                    // Handle functions with complex return types like "type(vector_type) function"
                    else if ((IsTypeSpecifier(Current.Kind) || Check(FortranTokenKind.KeywordType)) && 
                             _position + 1 < _tokens.Count)
                    {
                        // Look ahead to see if this is a function or subroutine definition
                        bool isFunctionOrSub = false;
                        int lookAhead = _position + 1;
                        
                        // Skip through the return type specification
                        // For "type(vector_type) function", skip "type" and "(vector_type)"
                        // For "real function", skip "real"
                        if (Check(FortranTokenKind.KeywordType) && 
                            lookAhead < _tokens.Count && 
                            _tokens[lookAhead].Kind == FortranTokenKind.LParen)
                        {
                            // type(...) - skip to find FUNCTION or SUBROUTINE
                            lookAhead++; // skip LParen
                            int depth = 1;
                            while (lookAhead < _tokens.Count && depth > 0)
                            {
                                if (_tokens[lookAhead].Kind == FortranTokenKind.LParen) depth++;
                                else if (_tokens[lookAhead].Kind == FortranTokenKind.RParen) depth--;
                                lookAhead++;
                            }
                        }
                        else
                        {
                            lookAhead++; // skip type specifier
                        }
                        
                        // Check if next token is FUNCTION or SUBROUTINE
                        if (lookAhead < _tokens.Count && 
                            (_tokens[lookAhead].Kind == FortranTokenKind.KeywordFunction ||
                             _tokens[lookAhead].Kind == FortranTokenKind.KeywordSubroutine))
                        {
                            isFunctionOrSub = true;
                        }
                        
                        if (isFunctionOrSub)
                        {
                            // Skip the return type to get to FUNCTION/SUBROUTINE
                            if (Check(FortranTokenKind.KeywordType) && 
                                _position + 1 < _tokens.Count && 
                                _tokens[_position + 1].Kind == FortranTokenKind.LParen)
                            {
                                Advance(); // consume TYPE
                                Advance(); // consume LParen
                                int depth = 1;
                                while (depth > 0 && !IsAtEnd)
                                {
                                    if (Check(FortranTokenKind.LParen)) depth++;
                                    else if (Check(FortranTokenKind.RParen)) depth--;
                                    Advance();
                                }
                            }
                            else
                            {
                                Advance(); // skip type specifier
                            }
                            
                            // Now parse FUNCTION or SUBROUTINE
                            if (Check(FortranTokenKind.KeywordFunction))
                            {
                                subroutines.Add(ParseFunctionAsSubroutine());
                            }
                            else if (Check(FortranTokenKind.KeywordSubroutine))
                            {
                                subroutines.Add(ParseSubroutine());
                            }
                        }
                        else
                        {
                            Advance();
                        }
                    }
                    else if (Check(FortranTokenKind.KeywordEndModule))
                    {
                        break;
                    }
                    else
                    {
                        Advance();
                    }
                }
                catch
                {
                    // If anything fails in the CONTAINS section, skip to END MODULE
                    while (!Check(FortranTokenKind.KeywordEndModule) && !Check(FortranTokenKind.KeywordEndSubroutine) && 
                           !Check(FortranTokenKind.KeywordEndFunction) && !IsAtEnd)
                    {
                        Advance();
                    }
                }
            }
        }

        // Try to consume END MODULE, but don't fail if not present
        if (Check(FortranTokenKind.KeywordEndModule))
        {
            Advance();
        }
        // Optional module name
        if (Check(FortranTokenKind.Identifier))
        {
            Advance();
        }

        return new FortranModule(name, useStatements, declarations, subroutines);
    }

    private FortranUseStatement ParseUseStatement()
    {
        // USE module_name [, ONLY: name_list]
        string moduleName = Consume(FortranTokenKind.Identifier, "Expected module name").Text;

        IReadOnlyList<string>? onlyList = null;
        if (Match(FortranTokenKind.Comma))
        {
            if (Match(FortranTokenKind.KeywordOnly))
            {
                // Optional colon after ONLY
                Match(FortranTokenKind.Colon);
                
                var names = new List<string>();
                names.Add(Consume(FortranTokenKind.Identifier, "Expected name in ONLY list").Text);

                while (Match(FortranTokenKind.Comma))
                {
                    names.Add(Consume(FortranTokenKind.Identifier, "Expected name").Text);
                }

                onlyList = names;
            }
        }

        return new FortranUseStatement(moduleName, onlyList);
    }

    private FortranToken Previous => _tokens[_position - 1];

    private FortranParseException Error(FortranToken token, string message)
    {
        return new FortranParseException($"Parse error at position {token.Position}: {message}");
    }
}
