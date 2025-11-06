using ObjectIR.Core.IR;
using ObjectIR.Core.Serialization;

namespace ObjectIR.Fortran.Compiler;

public sealed class FortranLanguageCompiler
{
    private readonly FortranCompilationOptions _options;

    public FortranLanguageCompiler(FortranCompilationOptions? options = null)
    {
        _options = options ?? FortranCompilationOptions.Default;
    }

    public Module CompileSource(string source)
    {
        var lexer = new FortranLexer(source);
        var tokens = lexer.Tokenize();
        var parser = new FortranParser(tokens);
        var program = parser.ParseProgram();
        var compiler = new FortranCompiler(_options);
        return compiler.Compile(program);
    }

    public string CompileSourceToJson(string source)
    {
        var module = CompileSource(source);
        return module.DumpJson();
    }

    public string CompileSourceToText(string source)
    {
        var module = CompileSource(source);
        return module.DumpText();
    }
}
