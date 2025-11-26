using System;
using System.Collections.Generic;
using ObjectIR.CSharpFrontend.AST;

namespace ObjectIR.CSharpFrontend;

/// <summary>
/// Recursive descent parser for C# 6 source code
/// Converts tokens → Abstract Syntax Tree
/// </summary>
public class CSharpParser
{
    private readonly List<Token> _tokens;
    private int _current = 0;

    public CSharpParser(List<Token> tokens)
    {
        _tokens = tokens ?? throw new ArgumentNullException(nameof(tokens));
    }

    /// <summary>
    /// Parse the entire compilation unit
    /// </summary>
    public CompilationUnit Parse()
    {
        var usings = new List<UsingDirective>();
        var namespaces = new List<NamespaceDeclaration>();
        var globalTypes = new List<TypeDeclaration>();

        while (!IsAtEnd && Check(TokenKind.Using))
        {
            usings.Add(ParseUsing());
        }

        while (!IsAtEnd)
        {
            if (Check(TokenKind.Namespace))
            {
                namespaces.Add(ParseNamespace());
            }
            else if (IsTypeDeclaration())
            {
                globalTypes.Add(ParseTypeDeclaration());
            }
            else
            {
                Advance(); // Skip unexpected token
            }
        }

        return new CompilationUnit(usings, namespaces, globalTypes, SourceLocation.Empty);
    }

    /// <summary>
    /// Parse a using directive
    /// </summary>
    private UsingDirective ParseUsing()
    {
        var usingToken = Consume(TokenKind.Using, "Expected 'using'");
        var name = ConsumeIdentifier("Expected namespace name");

        string? alias = null;
        if (Match(TokenKind.Assign))
        {
            alias = name;
            name = ConsumeIdentifier("Expected namespace after =");
        }

        while (Match(TokenKind.Dot))
        {
            name += "." + ConsumeIdentifier("Expected identifier after .");
        }

        Consume(TokenKind.Semicolon, "Expected ; after using directive");

        return new UsingDirective(name, alias, GetLocation(usingToken));
    }

    /// <summary>
    /// Parse a namespace declaration
    /// </summary>
    private NamespaceDeclaration ParseNamespace()
    {
        var nsToken = Consume(TokenKind.Namespace, "Expected 'namespace'");
        var name = ConsumeIdentifier("Expected namespace name");

        while (Match(TokenKind.Dot))
        {
            name += "." + ConsumeIdentifier("Expected identifier");
        }

        Consume(TokenKind.LeftBrace, "Expected { after namespace name");

        var usings = new List<UsingDirective>();
        while (Check(TokenKind.Using))
        {
            usings.Add(ParseUsing());
        }

        var types = new List<TypeDeclaration>();
        while (!Check(TokenKind.RightBrace) && !IsAtEnd)
        {
            if (IsTypeDeclaration())
            {
                types.Add(ParseTypeDeclaration());
            }
            else
            {
                Advance();
            }
        }

        Consume(TokenKind.RightBrace, "Expected } to close namespace");

        return new NamespaceDeclaration(name, usings, types, GetLocation(nsToken));
    }

    /// <summary>
    /// Check if current position is at a type declaration
    /// </summary>
    private bool IsTypeDeclaration()
    {
        int savedPos = _current;
        
        // Skip access modifiers and type modifiers
        while (_current < _tokens.Count && IsAccessModifier(Current.Kind) || IsTypeModifier(Current.Kind))
        {
            _current++;
        }

        bool isType = _current < _tokens.Count && (Current.Kind == TokenKind.Class ||
                                                    Current.Kind == TokenKind.Interface ||
                                                    Current.Kind == TokenKind.Struct ||
                                                    Current.Kind == TokenKind.Enum);
        
        _current = savedPos;
        return isType;
    }

    /// <summary>
    /// Parse a type declaration (class, interface, struct, enum)
    /// </summary>
    private TypeDeclaration ParseTypeDeclaration()
    {
        var access = ParseAccessModifier();
        var modifiers = ParseTypeModifiers();

        return Peek().Kind switch
        {
            TokenKind.Class => ParseClass(access, modifiers),
            TokenKind.Interface => ParseInterface(access, modifiers),
            TokenKind.Struct => ParseStruct(access, modifiers),
            TokenKind.Enum => ParseEnum(access, modifiers),
            _ => throw new ParseException("Expected type declaration")
        };
    }

    /// <summary>
    /// Parse a class declaration
    /// </summary>
    private ClassDeclaration ParseClass(AccessModifier access, List<Modifier> modifiers)
    {
        var classToken = Consume(TokenKind.Class, "Expected 'class'");
        var name = ConsumeIdentifier("Expected class name");
        var generics = ParseGenericParameters();

        TypeReference? baseClass = null;
        var interfaces = new List<TypeReference>();

        if (Match(TokenKind.Colon))
        {
            baseClass = ParseTypeReference();
            while (Match(TokenKind.Comma))
            {
                interfaces.Add(ParseTypeReference());
            }
        }

        Consume(TokenKind.LeftBrace, "Expected { after class header");

        var members = new List<Member>();
        while (!Check(TokenKind.RightBrace) && !IsAtEnd)
        {
            if (IsMemberDeclaration())
            {
                members.Add(ParseMember());
            }
            else
            {
                Advance();
            }
        }

        Consume(TokenKind.RightBrace, "Expected } to close class");

        return new ClassDeclaration(name, access, modifiers, generics, baseClass, interfaces, members, GetLocation(classToken));
    }

    /// <summary>
    /// Parse an interface declaration
    /// </summary>
    private InterfaceDeclaration ParseInterface(AccessModifier access, List<Modifier> modifiers)
    {
        var ifaceToken = Consume(TokenKind.Interface, "Expected 'interface'");
        var name = ConsumeIdentifier("Expected interface name");
        var generics = ParseGenericParameters();

        var baseInterfaces = new List<TypeReference>();
        if (Match(TokenKind.Colon))
        {
            baseInterfaces.Add(ParseTypeReference());
            while (Match(TokenKind.Comma))
            {
                baseInterfaces.Add(ParseTypeReference());
            }
        }

        Consume(TokenKind.LeftBrace, "Expected { after interface header");

        var members = new List<Member>();
        while (!Check(TokenKind.RightBrace) && !IsAtEnd)
        {
            if (IsMemberDeclaration())
            {
                members.Add(ParseMember());
            }
            else
            {
                Advance();
            }
        }

        Consume(TokenKind.RightBrace, "Expected } to close interface");

        return new InterfaceDeclaration(name, access, modifiers, generics, baseInterfaces, members, GetLocation(ifaceToken));
    }

    /// <summary>
    /// Parse a struct declaration
    /// </summary>
    private StructDeclaration ParseStruct(AccessModifier access, List<Modifier> modifiers)
    {
        var structToken = Consume(TokenKind.Struct, "Expected 'struct'");
        var name = ConsumeIdentifier("Expected struct name");
        var generics = ParseGenericParameters();

        var interfaces = new List<TypeReference>();
        if (Match(TokenKind.Colon))
        {
            interfaces.Add(ParseTypeReference());
            while (Match(TokenKind.Comma))
            {
                interfaces.Add(ParseTypeReference());
            }
        }

        Consume(TokenKind.LeftBrace, "Expected { after struct header");

        var members = new List<Member>();
        while (!Check(TokenKind.RightBrace) && !IsAtEnd)
        {
            if (IsMemberDeclaration())
            {
                members.Add(ParseMember());
            }
            else
            {
                Advance();
            }
        }

        Consume(TokenKind.RightBrace, "Expected } to close struct");

        return new StructDeclaration(name, access, modifiers, generics, interfaces, members, GetLocation(structToken));
    }

    /// <summary>
    /// Parse an enum declaration
    /// </summary>
    private EnumDeclaration ParseEnum(AccessModifier access, List<Modifier> modifiers)
    {
        var enumToken = Consume(TokenKind.Enum, "Expected 'enum'");
        var name = ConsumeIdentifier("Expected enum name");

        Consume(TokenKind.LeftBrace, "Expected { after enum name");

        var members = new List<EnumMember>();
        while (!Check(TokenKind.RightBrace) && !IsAtEnd)
        {
            var memberName = ConsumeIdentifier("Expected enum member name");
            Expression? init = null;

            if (Match(TokenKind.Assign))
            {
                init = ParseExpression();
            }

            members.Add(new EnumMember(memberName, init, GetLocation(Current)));

            if (!Match(TokenKind.Comma))
                break;
        }

        Consume(TokenKind.RightBrace, "Expected } to close enum");

        return new EnumDeclaration(name, access, modifiers, members, GetLocation(enumToken));
    }

    /// <summary>
    /// Check if current position is a member declaration
    /// </summary>
    private bool IsMemberDeclaration()
    {
        // Simplified check - in production would be more robust
        return IsAccessModifier(Current.Kind) ||
               IsTypeModifier(Current.Kind) ||
               Current.Kind == TokenKind.Identifier ||
               Current.Kind == TokenKind.Void;
    }

    /// <summary>
    /// Parse a member (field, property, method, constructor)
    /// </summary>
    private Member ParseMember()
    {
        var access = ParseAccessModifier();
        var modifiers = ParseMemberModifiers();

        // Check for constructor
        if (Current.Kind == TokenKind.Identifier && Peek(1).Kind == TokenKind.LeftParen)
        {
            var name = ConsumeIdentifier("Expected constructor name");
            if (Current.Kind == TokenKind.LeftParen)
            {
                return ParseConstructor(name, access);
            }
        }

        // Parse return type / field type
        var type = ParseTypeReference();
        var memberName = ConsumeIdentifier("Expected member name");

        // Property with accessors
        if (Match(TokenKind.LeftBrace))
        {
            return ParseProperty(access, modifiers, type, memberName);
        }

        // Method
        if (Match(TokenKind.LeftParen))
        {
            var parameters = ParseParameters();
            Consume(TokenKind.RightParen, "Expected )");

            Expression? exprBody = null;
            BlockStatement? body = null;

            if (Match(TokenKind.FatArrow))
            {
                exprBody = ParseExpression();
                Consume(TokenKind.Semicolon, "Expected ; after expression body");
            }
            else if (Check(TokenKind.LeftBrace))
            {
                body = ParseBlock();
            }
            else
            {
                Consume(TokenKind.Semicolon, "Expected ; or { after method signature");
            }

            return new MethodDeclaration(memberName, access, modifiers, type, [], parameters, body, exprBody, GetLocation(Current));
        }

        // Field with optional initializer
        Expression? fieldInit = null;
        if (Match(TokenKind.Assign))
        {
            fieldInit = ParseExpression();
        }

        Consume(TokenKind.Semicolon, "Expected ; after field");

        return new FieldDeclaration(memberName, access, modifiers, type, fieldInit, GetLocation(Current));
    }

    /// <summary>
    /// Parse a constructor
    /// </summary>
    private ConstructorDeclaration ParseConstructor(string name, AccessModifier access)
    {
        Consume(TokenKind.LeftParen, "Expected (");
        var parameters = ParseParameters();
        Consume(TokenKind.RightParen, "Expected )");

        ConstructorInitializer? initializer = null;
        if (Match(TokenKind.Colon))
        {
            var isBase = Current.Kind == TokenKind.Base;
            Consume(isBase ? TokenKind.Base : TokenKind.This, isBase ? "Expected 'base'" : "Expected 'this'");
            Consume(TokenKind.LeftParen, "Expected (");
            var args = ParseArgumentList();
            Consume(TokenKind.RightParen, "Expected )");
            initializer = new ConstructorInitializer(isBase, args, GetLocation(Current));
        }

        var body = ParseBlock();
        return new ConstructorDeclaration(name, access, parameters, initializer, body, GetLocation(Current));
    }

    /// <summary>
    /// Parse a property with get/set accessors
    /// </summary>
    private PropertyDeclaration ParseProperty(AccessModifier access, List<Modifier> modifiers, TypeReference type, string name)
    {
        PropertyAccessor? getter = null;
        PropertyAccessor? setter = null;
        Expression? initializer = null;

        while (!Check(TokenKind.RightBrace))
        {
            var getSetAccess = ParseAccessModifier();

            if (Current.Kind == TokenKind.Get)
            {
                Advance();
                BlockStatement getBody = new([], SourceLocation.Empty);
                if (Check(TokenKind.LeftBrace))
                {
                    getBody = ParseBlock();
                }
                else if (Match(TokenKind.Semicolon))
                {
                    // Auto property
                }
                getter = new PropertyAccessor(true, getSetAccess != AccessModifier.Public ? getSetAccess : null, getBody, GetLocation(Current));
            }
            else if (Current.Kind == TokenKind.Set)
            {
                Advance();
                BlockStatement setBody = new([], SourceLocation.Empty);
                if (Check(TokenKind.LeftBrace))
                {
                    setBody = ParseBlock();
                }
                else if (Match(TokenKind.Semicolon))
                {
                    // Auto property
                }
                setter = new PropertyAccessor(false, getSetAccess != AccessModifier.Public ? getSetAccess : null, setBody, GetLocation(Current));
            }
            else
            {
                break;
            }
        }

        Consume(TokenKind.RightBrace, "Expected } after property");

        // C# 6: auto-property initializer
        if (Match(TokenKind.Assign))
        {
            initializer = ParseExpression();
        }

        Consume(TokenKind.Semicolon, "Expected ; after property");

        return new PropertyDeclaration(name, access, modifiers, type, getter, setter, initializer, GetLocation(Current));
    }

    /// <summary>
    /// Parse a block statement
    /// </summary>
    private BlockStatement ParseBlock()
    {
        var blockToken = Consume(TokenKind.LeftBrace, "Expected {");
        var statements = new List<Statement>();

        while (!Check(TokenKind.RightBrace) && !IsAtEnd)
        {
            statements.Add(ParseStatement());
        }

        Consume(TokenKind.RightBrace, "Expected }");

        return new BlockStatement(statements, GetLocation(blockToken));
    }

    /// <summary>
    /// Parse a statement
    /// </summary>
    private Statement ParseStatement()
    {
        if (Check(TokenKind.LeftBrace))
            return ParseBlock();

        if (Check(TokenKind.If))
            return ParseIfStatement();

        if (Check(TokenKind.While))
            return ParseWhileStatement();

        if (Check(TokenKind.Do))
            return ParseDoWhileStatement();

        if (Check(TokenKind.For))
            return ParseForStatement();

        if (Check(TokenKind.Foreach))
            return ParseForeachStatement();

        if (Check(TokenKind.Return))
            return ParseReturnStatement();

        if (Check(TokenKind.Break))
            return new BreakStatement(GetLocation(Advance()));

        if (Check(TokenKind.Continue))
            return new ContinueStatement(GetLocation(Advance()));

        if (Check(TokenKind.Throw))
            return ParseThrowStatement();

        if (Check(TokenKind.Try))
            return ParseTryStatement();

        if (Check(TokenKind.Switch))
            return ParseSwitchStatement();

        // Variable declaration or expression statement
        if (IsTypeReference())
        {
            var type = ParseTypeReference();
            if (Check(TokenKind.Identifier))
            {
                var name = ConsumeIdentifier("Expected variable name");
                var declarators = new List<VariableDeclarator> { new(name, null, GetLocation(Current)) };

                while (Match(TokenKind.Comma))
                {
                    var decName = ConsumeIdentifier("Expected variable name");
                    declarators.Add(new VariableDeclarator(decName, null, GetLocation(Current)));
                }

                if (Match(TokenKind.Assign))
                {
                    declarators[^1] = declarators[^1] with { Initializer = ParseExpression() };
                }

                Consume(TokenKind.Semicolon, "Expected ; after variable declaration");
                return new VariableDeclarationStatement(type, declarators, GetLocation(Current));
            }
        }

        // Expression statement
        var expr = ParseExpression();
        Consume(TokenKind.Semicolon, "Expected ; after expression");
        return new ExpressionStatement(expr, GetLocation(Current));
    }

    /// <summary>
    /// Parse an if statement
    /// </summary>
    private IfStatement ParseIfStatement()
    {
        var ifToken = Consume(TokenKind.If, "Expected 'if'");
        Consume(TokenKind.LeftParen, "Expected (");
        var condition = ParseExpression();
        Consume(TokenKind.RightParen, "Expected )");

        var thenStmt = ParseStatement();
        Statement? elseStmt = null;

        if (Match(TokenKind.Else))
        {
            elseStmt = ParseStatement();
        }

        return new IfStatement(condition, thenStmt, elseStmt, GetLocation(ifToken));
    }

    /// <summary>
    /// Parse a while loop
    /// </summary>
    private WhileStatement ParseWhileStatement()
    {
        var whileToken = Consume(TokenKind.While, "Expected 'while'");
        Consume(TokenKind.LeftParen, "Expected (");
        var condition = ParseExpression();
        Consume(TokenKind.RightParen, "Expected )");
        var body = ParseStatement();

        return new WhileStatement(condition, body, GetLocation(whileToken));
    }

    /// <summary>
    /// Parse a do-while loop
    /// </summary>
    private DoWhileStatement ParseDoWhileStatement()
    {
        var doToken = Consume(TokenKind.Do, "Expected 'do'");
        var body = ParseStatement();
        Consume(TokenKind.While, "Expected 'while'");
        Consume(TokenKind.LeftParen, "Expected (");
        var condition = ParseExpression();
        Consume(TokenKind.RightParen, "Expected )");
        Consume(TokenKind.Semicolon, "Expected ; after while");

        return new DoWhileStatement(body, condition, GetLocation(doToken));
    }

    /// <summary>
    /// Parse a for loop
    /// </summary>
    private ForStatement ParseForStatement()
    {
        var forToken = Consume(TokenKind.For, "Expected 'for'");
        Consume(TokenKind.LeftParen, "Expected (");

        List<VariableDeclarator>? initializers = null;
        if (!Check(TokenKind.Semicolon))
        {
            if (IsTypeReference())
            {
                var type = ParseTypeReference();
                var name = ConsumeIdentifier("Expected variable name");
                initializers = new List<VariableDeclarator> { new(name, null, GetLocation(Current)) };

                while (Match(TokenKind.Comma))
                {
                    initializers.Add(new VariableDeclarator(ConsumeIdentifier("Expected name"), null, GetLocation(Current)));
                }
            }
            else
            {
                // TODO: Handle non-declaration initializers
            }
        }

        Consume(TokenKind.Semicolon, "Expected ;");

        Expression? condition = null;
        if (!Check(TokenKind.Semicolon))
        {
            condition = ParseExpression();
        }

        Consume(TokenKind.Semicolon, "Expected ;");

        List<Expression>? incrementors = null;
        if (!Check(TokenKind.RightParen))
        {
            incrementors = new List<Expression> { ParseExpression() };
            while (Match(TokenKind.Comma))
            {
                incrementors.Add(ParseExpression());
            }
        }

        Consume(TokenKind.RightParen, "Expected )");
        var body = ParseStatement();

        return new ForStatement(initializers, condition, incrementors, body, GetLocation(forToken));
    }

    /// <summary>
    /// Parse a foreach loop
    /// </summary>
    private ForeachStatement ParseForeachStatement()
    {
        var foreachToken = Consume(TokenKind.Foreach, "Expected 'foreach'");
        Consume(TokenKind.LeftParen, "Expected (");

        TypeReference? varType = null;
        if (!Check(TokenKind.Identifier) || Peek(1).Kind != TokenKind.Identifier)
        {
            varType = ParseTypeReference();
        }

        var varName = ConsumeIdentifier("Expected variable name");
        Consume(TokenKind.Identifier, "Expected 'in'"); // Should be 'in' keyword
        var collection = ParseExpression();
        Consume(TokenKind.RightParen, "Expected )");
        var body = ParseStatement();

        return new ForeachStatement(varName, varType, collection, body, GetLocation(foreachToken));
    }

    /// <summary>
    /// Parse a return statement
    /// </summary>
    private ReturnStatement ParseReturnStatement()
    {
        var returnToken = Consume(TokenKind.Return, "Expected 'return'");
        Expression? expr = null;

        if (!Check(TokenKind.Semicolon))
        {
            expr = ParseExpression();
        }

        Consume(TokenKind.Semicolon, "Expected ;");

        return new ReturnStatement(expr, GetLocation(returnToken));
    }

    /// <summary>
    /// Parse a throw statement
    /// </summary>
    private ThrowStatement ParseThrowStatement()
    {
        var throwToken = Consume(TokenKind.Throw, "Expected 'throw'");
        Expression? expr = null;

        if (!Check(TokenKind.Semicolon))
        {
            expr = ParseExpression();
        }

        Consume(TokenKind.Semicolon, "Expected ;");

        return new ThrowStatement(expr, GetLocation(throwToken));
    }

    /// <summary>
    /// Parse a try statement
    /// </summary>
    private TryStatement ParseTryStatement()
    {
        var tryToken = Consume(TokenKind.Try, "Expected 'try'");
        var tryBlock = ParseBlock();

        var catchClauses = new List<CatchClause>();
        while (Check(TokenKind.Catch))
        {
            Advance();
            TypeReference? exType = null;
            string? varName = null;

            if (Match(TokenKind.LeftParen))
            {
                exType = ParseTypeReference();
                if (Check(TokenKind.Identifier))
                {
                    varName = ConsumeIdentifier("Expected variable name");
                }
                Consume(TokenKind.RightParen, "Expected )");
            }

            var catchBody = ParseBlock();
            catchClauses.Add(new CatchClause(exType, varName, catchBody, GetLocation(Current)));
        }

        BlockStatement? finallyBlock = null;
        if (Match(TokenKind.Finally))
        {
            finallyBlock = ParseBlock();
        }

        return new TryStatement(tryBlock, catchClauses, finallyBlock, GetLocation(tryToken));
    }

    /// <summary>
    /// Parse a switch statement
    /// </summary>
    private SwitchStatement ParseSwitchStatement()
    {
        var switchToken = Consume(TokenKind.Switch, "Expected 'switch'");
        Consume(TokenKind.LeftParen, "Expected (");
        var expr = ParseExpression();
        Consume(TokenKind.RightParen, "Expected )");
        Consume(TokenKind.LeftBrace, "Expected {");

        var sections = new List<SwitchSection>();

        while (!Check(TokenKind.RightBrace) && !IsAtEnd)
        {
            var labels = new List<SwitchLabel>();

            while (Check(TokenKind.Case) || Check(TokenKind.Default))
            {
                if (Match(TokenKind.Case))
                {
                    var value = ParseExpression();
                    Consume(TokenKind.Colon, "Expected :");
                    labels.Add(new CaseLabel(value, GetLocation(Current)));
                }
                else if (Match(TokenKind.Default))
                {
                    Consume(TokenKind.Colon, "Expected :");
                    labels.Add(new DefaultLabel(GetLocation(Current)));
                }
            }

            var statements = new List<Statement>();
            while (!Check(TokenKind.Case) && !Check(TokenKind.Default) && !Check(TokenKind.RightBrace))
            {
                statements.Add(ParseStatement());
            }

            if (labels.Count > 0)
            {
                sections.Add(new SwitchSection(labels, statements, GetLocation(Current)));
            }
        }

        Consume(TokenKind.RightBrace, "Expected }");

        return new SwitchStatement(expr, sections, GetLocation(switchToken));
    }

    /// <summary>
    /// Parse an expression (lowest precedence)
    /// </summary>
    private Expression ParseExpression()
    {
        return ParseConditional();
    }

    /// <summary>
    /// Parse conditional (ternary) expression
    /// </summary>
    private Expression ParseConditional()
    {
        var expr = ParseLogicalOr();

        if (Match(TokenKind.Question))
        {
            var trueExpr = ParseExpression();
            Consume(TokenKind.Colon, "Expected : in ternary");
            var falseExpr = ParseExpression();
            return new ConditionalExpression(expr, trueExpr, falseExpr, GetLocation(Current));
        }

        return expr;
    }

    /// <summary>
    /// Parse logical OR
    /// </summary>
    private Expression ParseLogicalOr()
    {
        var expr = ParseLogicalAnd();

        while (Match(TokenKind.DoublePipe))
        {
            var op = Previous.Text;
            var right = ParseLogicalAnd();
            expr = new BinaryExpression(expr, op, right, GetLocation(Current));
        }

        return expr;
    }

    /// <summary>
    /// Parse logical AND
    /// </summary>
    private Expression ParseLogicalAnd()
    {
        var expr = ParseBitwiseOr();

        while (Match(TokenKind.DoubleAmpersand))
        {
            var op = Previous.Text;
            var right = ParseBitwiseOr();
            expr = new BinaryExpression(expr, op, right, GetLocation(Current));
        }

        return expr;
    }

    /// <summary>
    /// Parse bitwise OR
    /// </summary>
    private Expression ParseBitwiseOr()
    {
        var expr = ParseBitwiseXor();

        while (Match(TokenKind.Pipe))
        {
            var op = Previous.Text;
            var right = ParseBitwiseXor();
            expr = new BinaryExpression(expr, op, right, GetLocation(Current));
        }

        return expr;
    }

    /// <summary>
    /// Parse bitwise XOR
    /// </summary>
    private Expression ParseBitwiseXor()
    {
        var expr = ParseBitwiseAnd();

        while (Match(TokenKind.Caret))
        {
            var op = Previous.Text;
            var right = ParseBitwiseAnd();
            expr = new BinaryExpression(expr, op, right, GetLocation(Current));
        }

        return expr;
    }

    /// <summary>
    /// Parse bitwise AND
    /// </summary>
    private Expression ParseBitwiseAnd()
    {
        var expr = ParseEquality();

        while (Match(TokenKind.Ampersand))
        {
            var op = Previous.Text;
            var right = ParseEquality();
            expr = new BinaryExpression(expr, op, right, GetLocation(Current));
        }

        return expr;
    }

    /// <summary>
    /// Parse equality operators
    /// </summary>
    private Expression ParseEquality()
    {
        var expr = ParseRelational();

        while (Match(TokenKind.EqualEqual, TokenKind.BangEqual))
        {
            var op = Previous.Text;
            var right = ParseRelational();
            expr = new BinaryExpression(expr, op, right, GetLocation(Current));
        }

        return expr;
    }

    /// <summary>
    /// Parse relational operators
    /// </summary>
    private Expression ParseRelational()
    {
        var expr = ParseShift();

        while (Match(TokenKind.Less, TokenKind.Greater, TokenKind.LessEqual, TokenKind.GreaterEqual))
        {
            var op = Previous.Text;
            var right = ParseShift();
            expr = new BinaryExpression(expr, op, right, GetLocation(Current));
        }

        // Handle 'is' and 'as' operators
        if (Match(TokenKind.Identifier) && Previous.Text == "is")
        {
            var type = ParseTypeReference();
            expr = new IsExpression(expr, type, GetLocation(Current));
        }
        else if (Match(TokenKind.Identifier) && Previous.Text == "as")
        {
            var type = ParseTypeReference();
            expr = new AsExpression(expr, type, GetLocation(Current));
        }

        return expr;
    }

    /// <summary>
    /// Parse shift operators
    /// </summary>
    private Expression ParseShift()
    {
        var expr = ParseAdditive();

        while (Match(TokenKind.LeftShift, TokenKind.RightShift))
        {
            var op = Previous.Text;
            var right = ParseAdditive();
            expr = new BinaryExpression(expr, op, right, GetLocation(Current));
        }

        return expr;
    }

    /// <summary>
    /// Parse addition and subtraction
    /// </summary>
    private Expression ParseAdditive()
    {
        var expr = ParseMultiplicative();

        while (Match(TokenKind.Plus, TokenKind.Minus))
        {
            var op = Previous.Text;
            var right = ParseMultiplicative();
            expr = new BinaryExpression(expr, op, right, GetLocation(Current));
        }

        return expr;
    }

    /// <summary>
    /// Parse multiplication, division, modulo
    /// </summary>
    private Expression ParseMultiplicative()
    {
        var expr = ParseUnary();

        while (Match(TokenKind.Star, TokenKind.Slash, TokenKind.Percent))
        {
            var op = Previous.Text;
            var right = ParseUnary();
            expr = new BinaryExpression(expr, op, right, GetLocation(Current));
        }

        return expr;
    }

    /// <summary>
    /// Parse unary expressions
    /// </summary>
    private Expression ParseUnary()
    {
        if (Match(TokenKind.Bang, TokenKind.Tilde, TokenKind.Minus, TokenKind.Plus))
        {
            var op = Previous.Text;
            var expr = ParseUnary();
            return new UnaryExpression(op, expr, false, GetLocation(Current));
        }

        if (Match(TokenKind.New))
        {
            return ParseNewExpression();
        }

        if (Current.Kind == TokenKind.Identifier && Current.Text == "typeof")
        {
            Advance();
            Consume(TokenKind.LeftParen, "Expected (");
            var type = ParseTypeReference();
            Consume(TokenKind.RightParen, "Expected )");
            return new TypeOfExpression(type, GetLocation(Current));
        }

        if (Current.Kind == TokenKind.Identifier && Current.Text == "nameof")
        {
            Advance();
            Consume(TokenKind.LeftParen, "Expected (");
            var expr = ParseExpression();
            Consume(TokenKind.RightParen, "Expected )");
            return new NameOfExpression(expr, GetLocation(Current));
        }

        return ParsePostfix();
    }

    /// <summary>
    /// Parse postfix expressions (method calls, member access, indexing)
    /// </summary>
    private Expression ParsePostfix()
    {
        var expr = ParsePrimary();

        while (true)
        {
            if (Match(TokenKind.LeftParen))
            {
                _current--; // Back up to re-consume
                Consume(TokenKind.LeftParen, "");
                var args = ParseArgumentList();
                Consume(TokenKind.RightParen, "Expected )");
                expr = new InvocationExpression(expr, args, GetLocation(Current));
            }
            else if (Match(TokenKind.QuestionDot))
            {
                var member = ConsumeIdentifier("Expected member name");
                expr = new MemberAccessExpression(expr, member, true, GetLocation(Current));
            }
            else if (Match(TokenKind.Dot))
            {
                var member = ConsumeIdentifier("Expected member name");
                expr = new MemberAccessExpression(expr, member, false, GetLocation(Current));
            }
            else if (Match(TokenKind.LeftBracket))
            {
                var indices = new List<Expression> { ParseExpression() };
                while (Match(TokenKind.Comma))
                {
                    indices.Add(ParseExpression());
                }
                Consume(TokenKind.RightBracket, "Expected ]");
                expr = new IndexerExpression(expr, indices, GetLocation(Current));
            }
            else
            {
                break;
            }
        }

        return expr;
    }

    /// <summary>
    /// Parse primary expressions (literals, identifiers, parenthesized)
    /// </summary>
    private Expression ParsePrimary()
    {
        if (Match(TokenKind.IntLiteral))
            return new IntegerLiteral((int?)Previous.Value ?? 0, GetLocation(Previous));

        if (Match(TokenKind.FloatLiteral))
            return new FloatLiteral((float?)Previous.Value ?? 0f, GetLocation(Previous));

        if (Match(TokenKind.StringLiteral))
        {
            var text = (string?)Previous.Value ?? "";
            // Check for string interpolation (C# 6)
            if (text.Contains("{"))
            {
                return ParseStringInterpolation(text);
            }
            return new StringLiteral(text, GetLocation(Previous));
        }

        if (Match(TokenKind.CharLiteral))
            return new CharLiteral((char?)Previous.Value ?? '\0', GetLocation(Previous));

        if (Match(TokenKind.True))
            return new BooleanLiteral(true, GetLocation(Previous));

        if (Match(TokenKind.False))
            return new BooleanLiteral(false, GetLocation(Previous));

        if (Match(TokenKind.Null))
            return new NullLiteral(GetLocation(Previous));

        if (Match(TokenKind.This))
            return new ThisExpression(GetLocation(Previous));

        if (Match(TokenKind.Base))
            return new BaseExpression(GetLocation(Previous));

        if (Match(TokenKind.Identifier))
        {
            var name = Previous.Text;
            return new IdentifierExpression(name, GetLocation(Previous));
        }

        if (Match(TokenKind.LeftParen))
        {
            var expr = ParseExpression();
            Consume(TokenKind.RightParen, "Expected )");
            return expr;
        }

        throw new ParseException($"Unexpected token: {Current.Kind} at {Current.Line}:{Current.Column}");
    }

    /// <summary>
    /// Parse string interpolation (C# 6 feature)
    /// </summary>
    private InterpolatedStringExpression ParseStringInterpolation(string text)
    {
        var parts = new List<InterpolationPart>();
        var current = 0;

        while (current < text.Length)
        {
            var braceIdx = text.IndexOf('{', current);
            if (braceIdx == -1)
            {
                if (current < text.Length)
                {
                    parts.Add(new InterpolationText(text[current..], SourceLocation.Empty));
                }
                break;
            }

            if (braceIdx > current)
            {
                parts.Add(new InterpolationText(text[current..braceIdx], SourceLocation.Empty));
            }

            var closeBrace = text.IndexOf('}', braceIdx);
            if (closeBrace == -1) break;

            var exprText = text[(braceIdx + 1)..closeBrace];
            var colonIdx = exprText.IndexOf(':');
            string? format = null;

            if (colonIdx != -1)
            {
                format = exprText[(colonIdx + 1)..].Trim();
                exprText = exprText[..colonIdx];
            }

            var lexer = new CSharpLexer(exprText);
            var tokens = lexer.Tokenize();
            var parser = new CSharpParser(tokens);
            var expr = parser.ParseExpression();

            parts.Add(new InterpolationExpression(expr, format, SourceLocation.Empty));
            current = closeBrace + 1;
        }

        return new InterpolatedStringExpression(parts, SourceLocation.Empty);
    }

    /// <summary>
    /// Parse a new expression
    /// </summary>
    private Expression ParseNewExpression()
    {
        var type = ParseTypeReference();

        if (Match(TokenKind.LeftBracket))
        {
            var dims = new List<Expression> { ParseExpression() };
            while (Match(TokenKind.Comma))
            {
                dims.Add(ParseExpression());
            }
            Consume(TokenKind.RightBracket, "Expected ]");
            return new ArrayCreationExpression(type, dims, null, GetLocation(Current));
        }

        Consume(TokenKind.LeftParen, "Expected (");
        var args = ParseArgumentList();
        Consume(TokenKind.RightParen, "Expected )");

        return new ObjectCreationExpression(type, args, null, GetLocation(Current));
    }

    /// <summary>
    /// Parse argument list
    /// </summary>
    private List<Expression> ParseArgumentList()
    {
        var args = new List<Expression>();

        if (!Check(TokenKind.RightParen))
        {
            args.Add(ParseExpression());
            while (Match(TokenKind.Comma))
            {
                args.Add(ParseExpression());
            }
        }

        return args;
    }

    /// <summary>
    /// Parse parameters list
    /// </summary>
    private List<Parameter> ParseParameters()
    {
        var parameters = new List<Parameter>();

        if (!Check(TokenKind.RightParen))
        {
            do
            {
                var paramModifier = ParameterModifier.None;
                if (Current.Kind == TokenKind.RefKeyword)
                {
                    Advance();
                    paramModifier = ParameterModifier.Ref;
                }
                else if (Current.Kind == TokenKind.Out)
                {
                    Advance();
                    paramModifier = ParameterModifier.Out;
                }
                else if (Current.Kind == TokenKind.Params)
                {
                    Advance();
                    paramModifier = ParameterModifier.Params;
                }

                var type = ParseTypeReference();
                var name = ConsumeIdentifier("Expected parameter name");
                parameters.Add(new Parameter(name, type, null, paramModifier, GetLocation(Current)));
            } while (Match(TokenKind.Comma));
        }

        return parameters;
    }

    /// <summary>
    /// Parse a type reference
    /// </summary>
    private TypeReference ParseTypeReference()
    {
        var token = Current;

        // Predefined types
        if (IsPredefinedType(Current.Text))
        {
            var name = ConsumeIdentifier("Expected type");
            return new PredefinedType(name, GetLocation(token));
        }

        var typeName = ConsumeIdentifier("Expected type name");
        var location = GetLocation(token);

        // Generic type
        if (Match(TokenKind.Less))
        {
            var args = new List<TypeReference> { ParseTypeReference() };
            while (Match(TokenKind.Comma))
            {
                args.Add(ParseTypeReference());
            }
            Consume(TokenKind.Greater, "Expected >");
            return new GenericType(new NamedType(typeName, null, location), args, location);
        }

        // Array type
        if (Check(TokenKind.LeftBracket))
        {
            Advance();
            var rank = 1;
            while (Match(TokenKind.Comma))
                rank++;
            Consume(TokenKind.RightBracket, "Expected ]");
            return new ArrayType(new NamedType(typeName, null, location), rank, location);
        }

        // Nullable type (C# 8+, but parsing for compatibility)
        if (Match(TokenKind.Question))
        {
            return new NullableType(new NamedType(typeName, null, location), location);
        }

        return new NamedType(typeName, null, location);
    }

    /// <summary>
    /// Check if string is a predefined type
    /// </summary>
    private static bool IsPredefinedType(string name) => name switch
    {
        "void" or "int" or "uint" or "long" or "ulong" or "short" or "ushort" or
        "byte" or "sbyte" or "float" or "double" or "decimal" or "bool" or "char" or
        "string" or "object" => true,
        _ => false
    };

    /// <summary>
    /// Check if current is a type reference
    /// </summary>
    private bool IsTypeReference()
    {
        return IsPredefinedType(Current.Text) || Current.Kind == TokenKind.Identifier;
    }

    /// <summary>
    /// Parse generic type parameters
    /// </summary>
    private List<GenericParameter> ParseGenericParameters()
    {
        var parameters = new List<GenericParameter>();

        if (Match(TokenKind.Less))
        {
            do
            {
                var name = ConsumeIdentifier("Expected type parameter name");
                var constraints = new List<TypeReference>();

                if (Current.Kind == TokenKind.Where)
                {
                    Advance();
                    constraints.Add(ParseTypeReference());
                    while (Match(TokenKind.Comma))
                    {
                        constraints.Add(ParseTypeReference());
                    }
                }

                parameters.Add(new GenericParameter(name, constraints, GetLocation(Current)));
            } while (Match(TokenKind.Comma));

            Consume(TokenKind.Greater, "Expected >");
        }

        return parameters;
    }

    /// <summary>
    /// Parse access modifiers
    /// </summary>
    private AccessModifier ParseAccessModifier()
    {
        if (Match(TokenKind.Public)) return AccessModifier.Public;
        if (Match(TokenKind.Private)) return AccessModifier.Private;
        if (Match(TokenKind.Protected)) return AccessModifier.Protected;
        if (Match(TokenKind.Internal)) return AccessModifier.Internal;
        return AccessModifier.Private;
    }

    /// <summary>
    /// Check if token is an access modifier
    /// </summary>
    private static bool IsAccessModifier(TokenKind kind) => kind switch
    {
        TokenKind.Public or TokenKind.Private or TokenKind.Protected or TokenKind.Internal => true,
        _ => false
    };

    /// <summary>
    /// Parse type modifiers
    /// </summary>
    private List<Modifier> ParseTypeModifiers()
    {
        var modifiers = new List<Modifier>();

        while (IsTypeModifier(Current.Kind))
        {
            if (Current.Kind == TokenKind.Static)
            {
                Advance();
                modifiers.Add(Modifier.Static);
            }
            else if (Current.Kind == TokenKind.Abstract)
            {
                Advance();
                modifiers.Add(Modifier.Abstract);
            }
            else if (Current.Kind == TokenKind.Sealed)
            {
                Advance();
                modifiers.Add(Modifier.Sealed);
            }
            else if (Current.Kind == TokenKind.Partial)
            {
                Advance();
                modifiers.Add(Modifier.Partial);
            }
            else if (Current.Kind == TokenKind.New)
            {
                Advance();
                modifiers.Add(Modifier.New);
            }
            else
            {
                break;
            }
        }

        return modifiers;
    }

    /// <summary>
    /// Check if token is a type modifier
    /// </summary>
    private static bool IsTypeModifier(TokenKind kind) => kind switch
    {
        TokenKind.Static or TokenKind.Abstract or TokenKind.Sealed or
        TokenKind.Partial or TokenKind.New => true,
        _ => false
    };

    /// <summary>
    /// Parse member modifiers
    /// </summary>
    private List<Modifier> ParseMemberModifiers()
    {
        var modifiers = new List<Modifier>();

        while (IsMemberModifier(Current.Kind))
        {
            if (Current.Kind == TokenKind.Static)
            {
                Advance();
                modifiers.Add(Modifier.Static);
            }
            else if (Current.Kind == TokenKind.Abstract)
            {
                Advance();
                modifiers.Add(Modifier.Abstract);
            }
            else if (Current.Kind == TokenKind.Virtual)
            {
                Advance();
                modifiers.Add(Modifier.Virtual);
            }
            else if (Current.Kind == TokenKind.Override)
            {
                Advance();
                modifiers.Add(Modifier.Override);
            }
            else if (Current.Kind == TokenKind.Sealed)
            {
                Advance();
                modifiers.Add(Modifier.Sealed);
            }
            else if (Current.Kind == TokenKind.Readonly)
            {
                Advance();
                modifiers.Add(Modifier.Readonly);
            }
            else if (Current.Kind == TokenKind.Const)
            {
                Advance();
                modifiers.Add(Modifier.Const);
            }
            else if (Current.Kind == TokenKind.New)
            {
                Advance();
                modifiers.Add(Modifier.New);
            }
            else
            {
                break;
            }
        }

        return modifiers;
    }

    /// <summary>
    /// Check if token is a member modifier
    /// </summary>
    private static bool IsMemberModifier(TokenKind kind) => kind switch
    {
        TokenKind.Static or TokenKind.Abstract or TokenKind.Virtual or
        TokenKind.Override or TokenKind.Sealed or TokenKind.Readonly or
        TokenKind.Const or TokenKind.New => true,
        _ => false
    };

    // Helper methods for parsing

    private Token Current => _current < _tokens.Count ? _tokens[_current] : _tokens[^1];
    private Token Previous => _tokens[_current - 1];
    private bool IsAtEnd => Current.Kind == TokenKind.EOF;

    private bool Check(TokenKind kind) => Current.Kind == kind;

    private bool Match(params TokenKind[] kinds)
    {
        foreach (var kind in kinds)
        {
            if (Check(kind))
            {
                Advance();
                return true;
            }
        }
        return false;
    }

    private Token Advance()
    {
        if (!IsAtEnd) _current++;
        return Previous;
    }

    private Token Consume(TokenKind kind, string message)
    {
        if (Check(kind)) return Advance();
        throw new ParseException($"{message} at {Current.Line}:{Current.Column}, got {Current.Kind}");
    }

    private string ConsumeIdentifier(string message)
    {
        if (Current.Kind != TokenKind.Identifier)
            throw new ParseException($"{message} at {Current.Line}:{Current.Column}");
        return Advance().Text;
    }

    private Token Peek(int offset = 0)
    {
        var idx = _current + offset;
        return idx < _tokens.Count ? _tokens[idx] : _tokens[^1];
    }

    private SourceLocation GetLocation(Token token) =>
        new(token.Line, token.Column, token.Position);
}

/// <summary>
/// Exception thrown by parser
/// </summary>
public class ParseException : Exception
{
    public ParseException(string message) : base(message) { }
}
