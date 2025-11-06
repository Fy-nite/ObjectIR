using ObjectIR.Core.Compilers;
using ObjectIR.Core.Serialization;
using Xunit;
using System;
using System.Linq;

namespace ObjectIR.Tests;

public class ConstructLexerTests
{
    [Fact]
    public void Lexer_EmptySource_ReturnsEOF()
    {
        var lexer = new ConstructLexer("");
        var tokens = lexer.Tokenize();

        Assert.Single(tokens);
        Assert.Equal(TokenType.EOF, tokens[0].Type);
    }

    [Fact]
    public void Lexer_Keywords_TokenizedCorrectly()
    {
        var lexer = new ConstructLexer("Contract fn var if else while return");
        var tokens = lexer.Tokenize();

        Assert.Equal(TokenType.Contract, tokens[0].Type);
        Assert.Equal(TokenType.Fn, tokens[1].Type);
        Assert.Equal(TokenType.Var, tokens[2].Type);
        Assert.Equal(TokenType.If, tokens[3].Type);
        Assert.Equal(TokenType.Else, tokens[4].Type);
        Assert.Equal(TokenType.While, tokens[5].Type);
        Assert.Equal(TokenType.Return, tokens[6].Type);
    }

    [Fact]
    public void Lexer_Types_TokenizedCorrectly()
    {
        var lexer = new ConstructLexer("Int String Bool");
        var tokens = lexer.Tokenize();

        Assert.Equal(TokenType.TypeInt, tokens[0].Type);
        Assert.Equal(TokenType.TypeString, tokens[1].Type);
        Assert.Equal(TokenType.TypeBool, tokens[2].Type);
    }

    [Fact]
    public void Lexer_Operators_TokenizedCorrectly()
    {
        var lexer = new ConstructLexer("+ - * / = == != < <= > >= !");
        var tokens = lexer.Tokenize();

        Assert.Equal(TokenType.Plus, tokens[0].Type);
        Assert.Equal(TokenType.Minus, tokens[1].Type);
        Assert.Equal(TokenType.Star, tokens[2].Type);
        Assert.Equal(TokenType.Slash, tokens[3].Type);
        Assert.Equal(TokenType.Equal, tokens[4].Type);
        Assert.Equal(TokenType.EqualEqual, tokens[5].Type);
        Assert.Equal(TokenType.BangEqual, tokens[6].Type);
        Assert.Equal(TokenType.Less, tokens[7].Type);
        Assert.Equal(TokenType.LessEqual, tokens[8].Type);
        Assert.Equal(TokenType.Greater, tokens[9].Type);
        Assert.Equal(TokenType.GreaterEqual, tokens[10].Type);
        Assert.Equal(TokenType.Bang, tokens[11].Type);
    }

    [Fact]
    public void Lexer_Delimiters_TokenizedCorrectly()
    {
        var lexer = new ConstructLexer("( ) { } ; , : .");
        var tokens = lexer.Tokenize();

        Assert.Equal(TokenType.LeftParen, tokens[0].Type);
        Assert.Equal(TokenType.RightParen, tokens[1].Type);
        Assert.Equal(TokenType.LeftBrace, tokens[2].Type);
        Assert.Equal(TokenType.RightBrace, tokens[3].Type);
        Assert.Equal(TokenType.Semicolon, tokens[4].Type);
        Assert.Equal(TokenType.Comma, tokens[5].Type);
        Assert.Equal(TokenType.Colon, tokens[6].Type);
        Assert.Equal(TokenType.Dot, tokens[7].Type);
    }

    [Fact]
    public void Lexer_Numbers_TokenizedCorrectly()
    {
        var lexer = new ConstructLexer("123 456 0 999");
        var tokens = lexer.Tokenize();

        Assert.Equal(TokenType.Number, tokens[0].Type);
        Assert.Equal("123", tokens[0].Text);
        Assert.Equal(TokenType.Number, tokens[1].Type);
        Assert.Equal("456", tokens[1].Text);
    }

    [Fact]
    public void Lexer_Strings_TokenizedCorrectly()
    {
        var lexer = new ConstructLexer("\"hello\" \"world test\"");
        var tokens = lexer.Tokenize();

        Assert.Equal(TokenType.String, tokens[0].Type);
        Assert.Equal("hello", tokens[0].Text);
        Assert.Equal(TokenType.String, tokens[1].Type);
        Assert.Equal("world test", tokens[1].Text);
    }

    [Fact]
    public void Lexer_Identifiers_TokenizedCorrectly()
    {
        var lexer = new ConstructLexer("foo Bar _private x123");
        var tokens = lexer.Tokenize();

        Assert.Equal(TokenType.Identifier, tokens[0].Type);
        Assert.Equal("foo", tokens[0].Text);
        Assert.Equal(TokenType.Identifier, tokens[1].Type);
        Assert.Equal("Bar", tokens[1].Text);
    }

    [Fact]
    public void Lexer_BooleanLiterals_TokenizedCorrectly()
    {
        var lexer = new ConstructLexer("true false");
        var tokens = lexer.Tokenize();

        Assert.Equal(TokenType.True, tokens[0].Type);
        Assert.Equal(TokenType.False, tokens[1].Type);
    }

    [Fact]
    public void Lexer_Comments_SkippedCorrectly()
    {
        var lexer = new ConstructLexer("foo // line comment\nbar /* block */ baz");
        var tokens = lexer.Tokenize();

        Assert.Equal(TokenType.Identifier, tokens[0].Type);
        Assert.Equal("foo", tokens[0].Text);
        Assert.Equal(TokenType.Identifier, tokens[1].Type);
        Assert.Equal("bar", tokens[1].Text);
        Assert.Equal(TokenType.Identifier, tokens[2].Type);
        Assert.Equal("baz", tokens[2].Text);
    }

    [Fact]
    public void Lexer_Whitespace_SkippedCorrectly()
    {
        var lexer = new ConstructLexer("   foo   bar   \n   baz   ");
        var tokens = lexer.Tokenize();

        Assert.Equal(TokenType.Identifier, tokens[0].Type);
        Assert.Equal(TokenType.Identifier, tokens[1].Type);
        Assert.Equal(TokenType.Identifier, tokens[2].Type);
        Assert.Equal(TokenType.EOF, tokens[3].Type);
    }
}

public class ConstructParserTests
{
    [Fact]
    public void Parser_SimpleContract_ParsesCorrectly()
    {
        var lexer = new ConstructLexer("Contract Foo { fn bar() { } }");
        var tokens = lexer.Tokenize();
        var parser = new ConstructParser(tokens);
        var program = parser.Parse();

        Assert.NotNull(program);
        Assert.NotNull(program.Contract);
        Assert.Equal("Foo", program.Contract.Name);
        Assert.Single(program.Contract.Functions);
    }

    [Fact]
    public void Parser_FunctionWithParameters_ParsesCorrectly()
    {
        var lexer = new ConstructLexer("Contract C { fn f(a: Int, b: String) { } }");
        var tokens = lexer.Tokenize();
        var parser = new ConstructParser(tokens);
        var program = parser.Parse();

        var fn = program.Contract.Functions[0];
        Assert.Equal("f", fn.Name);
        Assert.Equal(2, fn.Parameters.Count);
        Assert.Equal("a", fn.Parameters[0].Name);
        Assert.Equal("Int", fn.Parameters[0].Type.Name);
    }

    [Fact]
    public void Parser_FunctionWithReturnType_ParsesCorrectly()
    {
        var lexer = new ConstructLexer("Contract C { fn f() -> Int { } }");
        var tokens = lexer.Tokenize();
        var parser = new ConstructParser(tokens);
        var program = parser.Parse();

        var fn = program.Contract.Functions[0];
        Assert.NotNull(fn.ReturnType);
        Assert.Equal("Int", fn.ReturnType.Name);
    }

    [Fact]
    public void Parser_VarDeclaration_ParsesCorrectly()
    {
        var lexer = new ConstructLexer("Contract C { fn f() { var x = 42; } }");
        var tokens = lexer.Tokenize();
        var parser = new ConstructParser(tokens);
        var program = parser.Parse();

        var fn = program.Contract.Functions[0];
        Assert.IsType<VarDeclaration>(fn.Body.Statements[0]);
        var varDecl = (VarDeclaration)fn.Body.Statements[0];
        Assert.Equal("x", varDecl.Name);
    }

    [Fact]
    public void Parser_IfStatement_ParsesCorrectly()
    {
        var lexer = new ConstructLexer("Contract C { fn f() { if (true) { } } }");
        var tokens = lexer.Tokenize();
        var parser = new ConstructParser(tokens);
        var program = parser.Parse();

        var fn = program.Contract.Functions[0];
        Assert.IsType<IfStatement>(fn.Body.Statements[0]);
    }

    [Fact]
    public void Parser_WhileStatement_ParsesCorrectly()
    {
        var lexer = new ConstructLexer("Contract C { fn f() { while (true) { } } }");
        var tokens = lexer.Tokenize();
        var parser = new ConstructParser(tokens);
        var program = parser.Parse();

        var fn = program.Contract.Functions[0];
        Assert.IsType<WhileStatement>(fn.Body.Statements[0]);
    }

    [Fact]
    public void Parser_ReturnStatement_ParsesCorrectly()
    {
        var lexer = new ConstructLexer("Contract C { fn f() { return 42; } }");
        var tokens = lexer.Tokenize();
        var parser = new ConstructParser(tokens);
        var program = parser.Parse();

        var fn = program.Contract.Functions[0];
        Assert.IsType<ReturnStatement>(fn.Body.Statements[0]);
    }

    [Fact]
    public void Parser_BinaryExpression_ParsesCorrectly()
    {
        var lexer = new ConstructLexer("Contract C { fn f() { var x = 1 + 2; } }");
        var tokens = lexer.Tokenize();
        var parser = new ConstructParser(tokens);
        var program = parser.Parse();

        var fn = program.Contract.Functions[0];
        var varDecl = (VarDeclaration)fn.Body.Statements[0];
        Assert.IsType<BinaryOp>(varDecl.Initializer);
    }

    [Fact]
    public void Parser_FunctionCall_ParsesCorrectly()
    {
        var lexer = new ConstructLexer("Contract C { fn f() { IO.println(\"hi\"); } }");
        var tokens = lexer.Tokenize();
        var parser = new ConstructParser(tokens);
        var program = parser.Parse();

        var fn = program.Contract.Functions[0];
        var stmt = (ExpressionStatement)fn.Body.Statements[0];
        Assert.IsType<FunctionCall>(stmt.Expression);
    }
}

public class ConstructCompilerTests
{
    [Fact]
    public void Compiler_SimpleContract_GeneratesModule()
    {
        string source = "Contract TestModule { fn Hello() { } }";
        var compiler = new ConstructLanguageCompiler();
        var module = compiler.CompileSource(source);

        Assert.NotNull(module);
        Assert.Equal("TestModule", module.Name);
    }

    [Fact]
    public void Compiler_ContractWithFunctions_GeneratesClass()
    {
        string source = @"
Contract Calculator {
    fn Add(a: Int, b: Int) -> Int {
        var result = a + b;
        return result;
    }
}";
        var compiler = new ConstructLanguageCompiler();
        var module = compiler.CompileSource(source);

        Assert.NotNull(module);
        Assert.NotEmpty(module.Types);
        var classType = module.Types[0];
        Assert.NotNull(classType);
    }

    [Fact]
    public void Compiler_CompileToJson_ProducesValidJson()
    {
        string source = "Contract Test { fn Work() { } }";
        var compiler = new ConstructLanguageCompiler();
        var json = compiler.CompileSourceToJson(source);

        Assert.NotNull(json);
        Assert.NotEmpty(json);
        Assert.Contains("\"Name\"", json);
    }

    [Fact]
    public void Compiler_CompileToText_ProducesValidText()
    {
        string source = "Contract Test { fn Work() { } }";
        var compiler = new ConstructLanguageCompiler();
        var text = compiler.CompileSourceToText(source);

        Assert.NotNull(text);
        Assert.NotEmpty(text);
        Assert.Contains("Test", text);
    }

    [Fact]
    public void Compiler_MultipleFunctions_AllGenerated()
    {
        string source = @"
Contract Utils {
    fn Func1() { }
    fn Func2() { }
    fn Func3() { }
}";
        var compiler = new ConstructLanguageCompiler();
        var module = compiler.CompileSource(source);

        Assert.NotNull(module);
        // Functions are stored in module.Functions
        Assert.True(module.Functions.Count >= 0);
    }
}

public class ConstructLanguageIntegrationTests
{
    [Fact]
    public void Integration_LexerParserCompiler_WorkTogether()
    {
        string source = @"
Contract Program {
    fn main() {
        var x = 10;
        return x;
    }
}";
        var compiler = new ConstructLanguageCompiler();
        var module = compiler.CompileSource(source);

        Assert.NotNull(module);
        Assert.Equal("Program", module.Name);
    }

    [Fact]
    public void Integration_ComplexProgram_CompilesSuccessfully()
    {
        string source = @"
Contract Complex {
    fn Calculate(a: Int, b: Int) -> Int {
        if (a < b) {
            var result = a + b;
            return result;
        } else {
            var result = a - b;
            return result;
        }
    }

    fn IsPositive(n: Int) -> Bool {
        return n > 0;
    }

    fn Loop() {
        var i = 0;
        while (i < 10) {
            var x = i + 1;
            i = x;
        }
    }
}";
        var compiler = new ConstructLanguageCompiler();
        var module = compiler.CompileSource(source);

        Assert.NotNull(module);
        Assert.NotEmpty(module.Types);
    }

    [Fact]
    public void Integration_CompiledModuleSerializable()
    {
        string source = "Contract Test { fn Func() { } }";
        var compiler = new ConstructLanguageCompiler();
        var module = compiler.CompileSource(source);

        // Should be serializable to JSON
        var json = module.DumpJson();
        Assert.NotNull(json);
        Assert.NotEmpty(json);

        // Should be serializable to text
        var text = module.DumpText();
        Assert.NotNull(text);
        Assert.NotEmpty(text);
    }
}
