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
                if (Match(FortranTokenKind.KeywordImplicit))
                {
                    Consume(FortranTokenKind.KeywordNone, "Expected 'none' after 'implicit'");
                    statements.Add(new FortranImplicitNoneStatement());
                    continue;
                }

                if (IsTypeSpecifier(Current.Kind))
                {
                    var declaration = TryParseLooseDeclaration();
                    if (declaration != null)
                    {
                        statements.Add(declaration);
                        continue;
                    }

                    // Fallback: if parsing failed, skip the tokens defensively
                    Advance();
                    continue;
                }

                if (Match(FortranTokenKind.KeywordAllocate))
                {
                    // Skip allocate statements
                    // allocate(array(...))
                    Consume(FortranTokenKind.LParen, "Expected '(' after allocate");
                    int depth = 1;
                    while (depth > 0 && !IsAtEnd)
                    {
                        if (Check(FortranTokenKind.LParen)) depth++;
                        else if (Check(FortranTokenKind.RParen)) depth--;
                        Advance();
                    }
                    continue;
                }

                if (Match(FortranTokenKind.KeywordDeallocate))
                {
                    // Skip deallocate statements
                    Consume(FortranTokenKind.LParen, "Expected '(' after deallocate");
                    int depth = 1;
                    while (depth > 0 && !IsAtEnd)
                    {
                        if (Check(FortranTokenKind.LParen)) depth++;
                        else if (Check(FortranTokenKind.RParen)) depth--;
                        Advance();
                    }
                    continue;
                }

                if (Match(FortranTokenKind.KeywordPrint))
                {
                    statements.Add(ParsePrintStatement());
                    continue;
                }

                if (Match(FortranTokenKind.KeywordCall))
                {
                    statements.Add(ParseCallStatement());
                    continue;
                }

                if (Match(FortranTokenKind.KeywordDo))
                {
                    // Skip DO loops
                    int doNest = 1;
                    while (doNest > 0 && !IsAtEnd)
                    {
                        if (Check(FortranTokenKind.KeywordDo)) doNest++;
                        else if (Check(FortranTokenKind.KeywordEnddo)) doNest--;
                        Advance();
                    }
                    continue;
                }

                if (Check(FortranTokenKind.Identifier))
                {
                    statements.Add(ParseAssignment());
                    continue;
                }

                // Skip any other tokens to be resilient
                Advance();
            }
            catch
            {
                // If anything fails, skip to next statement
                while (!IsAtEnd && Current.Kind != FortranTokenKind.KeywordImplicit &&
                       Current.Kind != FortranTokenKind.KeywordPrint &&
                       Current.Kind != FortranTokenKind.KeywordCall &&
                       Current.Kind != FortranTokenKind.KeywordAllocate &&
                       Current.Kind != FortranTokenKind.KeywordDeallocate &&
                       Current.Kind != FortranTokenKind.KeywordDo &&
                       Current.Kind != FortranTokenKind.KeywordEnd)
                {
                    Advance();
                }
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
        // Check for END SUBROUTINE (compound token) or END keyword
        while (!Check(FortranTokenKind.KeywordEnd) && 
               !Check(FortranTokenKind.KeywordEndSubroutine) && !IsAtEnd)
        {
            if (Match(FortranTokenKind.KeywordImplicit))
            {
                Consume(FortranTokenKind.KeywordNone, "Expected 'none' after 'implicit'");
                statements.Add(new FortranImplicitNoneStatement());
                continue;
            }

            if (IsTypeSpecifier(Current.Kind))
            {
                // Skip Fortran 90 declarations in subroutine
                Advance(); // consume type
                
                // Skip all declaration tokens until we reach a statement
                int depth = 0;
                while (!IsAtEnd && Current.Kind != FortranTokenKind.KeywordImplicit &&
                       Current.Kind != FortranTokenKind.KeywordPrint &&
                       Current.Kind != FortranTokenKind.KeywordCall &&
                       Current.Kind != FortranTokenKind.KeywordReturn &&
                       Current.Kind != FortranTokenKind.KeywordDo &&
                       Current.Kind != FortranTokenKind.KeywordEnd &&
                       Current.Kind != FortranTokenKind.KeywordEndSubroutine)
                {
                    if (Current.Kind == FortranTokenKind.LParen)
                        depth++;
                    else if (Current.Kind == FortranTokenKind.RParen)
                        depth--;
                    else if (depth == 0 && Current.Kind == FortranTokenKind.Identifier &&
                             Previous.Kind != FortranTokenKind.Comma)
                        break; // Hit variable name after declaration
                    Advance();
                }
                continue;
            }

            if (Match(FortranTokenKind.KeywordPrint))
            {
                statements.Add(ParsePrintStatement());
                continue;
            }

            if (Match(FortranTokenKind.KeywordCall))
            {
                statements.Add(ParseCallStatement());
                continue;
            }

            if (Match(FortranTokenKind.KeywordReturn))
            {
                statements.Add(new FortranReturnStatement());
                continue;
            }

            if (Check(FortranTokenKind.Identifier))
            {
                try
                {
                    statements.Add(ParseAssignment());
                }
                catch
                {
                    // If parsing fails, skip this line
                    while (!IsAtEnd && Current.Kind != FortranTokenKind.KeywordImplicit &&
                           Current.Kind != FortranTokenKind.KeywordPrint &&
                           Current.Kind != FortranTokenKind.KeywordCall &&
                           Current.Kind != FortranTokenKind.KeywordReturn &&
                           Current.Kind != FortranTokenKind.KeywordDo &&
                           Current.Kind != FortranTokenKind.KeywordEnd &&
                           Current.Kind != FortranTokenKind.KeywordEndSubroutine)
                    {
                        Advance();
                    }
                }
                continue;
            }

            // For any other token, try to skip it
            Advance();
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

    private FortranStatement ParseAssignment()
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
            // Return a dummy statement
            return new FortranImplicitNoneStatement(); // placeholder
        }
        
        if (!Check(FortranTokenKind.Equals))
        {
            // Not an assignment - skip this line
            while (!IsAtEnd && Current.Kind != FortranTokenKind.KeywordImplicit &&
                   Current.Kind != FortranTokenKind.KeywordPrint &&
                   Current.Kind != FortranTokenKind.KeywordCall &&
                   Current.Kind != FortranTokenKind.KeywordAllocate &&
                   Current.Kind != FortranTokenKind.KeywordDeallocate &&
                   Current.Kind != FortranTokenKind.KeywordDo &&
                   Current.Kind != FortranTokenKind.KeywordEnd)
            {
                Advance();
            }
            return new FortranImplicitNoneStatement(); // placeholder
        }
        
        Consume(FortranTokenKind.Equals, "Expected '=' in assignment");
        var expression = ParseExpression();
        return new FortranAssignmentStatement(name, expression);
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
        return ParseAdditive();
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
