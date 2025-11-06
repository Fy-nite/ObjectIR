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
            if (Match(FortranTokenKind.KeywordImplicit))
            {
                Consume(FortranTokenKind.KeywordNone, "Expected 'none' after 'implicit'");
                statements.Add(new FortranImplicitNoneStatement());
                continue;
            }

            if (IsTypeSpecifier(Current.Kind))
            {
                statements.Add(ParseDeclaration());
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

            if (Check(FortranTokenKind.Identifier))
            {
                statements.Add(ParseAssignment());
                continue;
            }

            throw Error(Current, "Unexpected token in statement");
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
        while (!Check(FortranTokenKind.KeywordEnd) && !IsAtEnd)
        {
            if (Match(FortranTokenKind.KeywordImplicit))
            {
                Consume(FortranTokenKind.KeywordNone, "Expected 'none' after 'implicit'");
                statements.Add(new FortranImplicitNoneStatement());
                continue;
            }

            if (IsTypeSpecifier(Current.Kind))
            {
                statements.Add(ParseDeclaration());
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
                statements.Add(ParseAssignment());
                continue;
            }

            throw Error(Current, "Unexpected token in subroutine body");
        }

        Consume(FortranTokenKind.KeywordEnd, "Expected 'end' to close subroutine");
        Consume(FortranTokenKind.KeywordSubroutine, "Expected 'subroutine' after 'end'");
        if (Check(FortranTokenKind.Identifier))
        {
            Advance();
        }

        return new FortranSubroutineDefinition(name, parameters, statements);
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

    private FortranStatement ParseAssignment()
    {
        string name = Consume(FortranTokenKind.Identifier, "Expected identifier").Text;
        Consume(FortranTokenKind.Equals, "Expected '=' in assignment");
        var expression = ParseExpression();
        return new FortranAssignmentStatement(name, expression);
    }

    private FortranStatement ParsePrintStatement()
    {
        Consume(FortranTokenKind.Star, "Expected '*' after PRINT");
        Consume(FortranTokenKind.Comma, "Expected ',' after PRINT *");

        var arguments = new List<FortranExpression>
        {
            ParseExpression()
        };

        while (Match(FortranTokenKind.Comma))
        {
            arguments.Add(ParseExpression());
        }

        return new FortranPrintStatement(arguments);
    }

    private FortranStatement ParseCallStatement()
    {
        string name = Consume(FortranTokenKind.Identifier, "Expected procedure name").Text;
        var arguments = new List<FortranExpression>();

        if (Match(FortranTokenKind.LParen))
        {
            arguments.AddRange(ParseParenthesizedArgumentList());
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

            if (IsTypeSpecifier(Current.Kind))
            {
                declarations.Add(ParseDeclaration());
                continue;
            }

            // Skip unknown tokens to avoid infinite loop
            Advance();
        }

        // Parse CONTAINS section (procedures inside module)
        if (Match(FortranTokenKind.KeywordContains))
        {
            // For now, skip all procedures in CONTAINS
            int nestingLevel = 0;
            while (!IsAtEnd)
            {
                if (Check(FortranTokenKind.KeywordEndModule))
                {
                    break;
                }
                
                // Track nesting with DO/END DO
                if (Check(FortranTokenKind.KeywordDo))
                {
                    nestingLevel++;
                }
                else if (Check(FortranTokenKind.KeywordEnddo))
                {
                    if (nestingLevel > 0)
                    {
                        nestingLevel--;
                    }
                }
                
                Advance();
            }
        }

        Consume(FortranTokenKind.KeywordEndModule, "Expected 'end module'");
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
