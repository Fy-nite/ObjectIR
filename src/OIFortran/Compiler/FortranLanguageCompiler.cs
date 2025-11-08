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
    var parser = new FortranParserV2(tokens, _options.Debug);
        var program = parser.ParseProgram();
        var compiler = new FortranCompiler(_options);
        
        try
        {
            return compiler.Compile(program);
        }
        catch (Exception)
        {
            // If full compilation fails, try partial compilation (just subroutines)
            try
            {
                return compiler.CompilePartial(program);
            }
            catch
            {
                // If even partial compilation fails, rethrow original error
                throw;
            }
        }
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

    internal string CompileSourceToOirText(string source)
    {
        var module = CompileSource(source);
        var serializer = new ModuleSerializer(module);
        return serializer.DumpToIRCode();
    }
    public string CompileSourceToMarkdown(string source)
    {
        var module = CompileSource(source);
        return AdvancedModuleFormats.DumpToMarkdown(module.Dump());
    }
    public string CompileSourceToYaml(string source)
    {
        var module = CompileSource(source);
        return module.DumpYaml();
    }
}
