using System;
using System.Collections.Generic;
using System.Linq;
using ObjectIR.Core.IR;
using ObjectIR.Fortran.Compiler;
using Xunit;

namespace ObjectIR.CSharpTests;

public class FortranCompilerTests
{
    [Fact]
    public void CompilesSimpleProgramWithPrint()
    {
        const string source = """
program hello
  implicit none
  integer :: i
  i = 42
  print *, i
end program hello
""";

        var compiler = new FortranLanguageCompiler();
        var module = compiler.CompileSource(source);

        Assert.Equal("hello", module.Name);
        var classDef = Assert.IsType<ClassDefinition>(Assert.Single(module.Types));
        Assert.Equal("hello", classDef.Name);

        var main = Assert.Single(classDef.Methods, m => m.Name == "Main");
        Assert.True(main.IsStatic);
        Assert.True(TypeEquals(TypeReference.Void, main.ReturnType));
        Assert.Single(main.Locals);
        Assert.Equal("i", main.Locals[0].Name);
        Assert.True(TypeEquals(TypeReference.Int32, main.Locals[0].Type));

        var opcodes = main.Instructions.Select(i => i.OpCode).ToArray();
        Assert.Equal(new[] { OpCode.LdcI4, OpCode.Stloc, OpCode.Ldloc, OpCode.Call, OpCode.Ret }, opcodes);
    }

        [Fact]
        public void CallStatement_EmitsRegisteredIntrinsic()
        {
                const string source = """
program sample
    call log_message("hello")
end program sample
""";

                var registry = FortranIntrinsicRegistry.CreateDefault();
                var consoleType = TypeReference.FromName("System.Console");
                var writeLine = new MethodReference(consoleType, "WriteLine", TypeReference.Void, new List<TypeReference> { TypeReference.String });
                registry.RegisterProcedure("log_message", writeLine);

                var options = new FortranCompilationOptions(registry);
                var compiler = new FortranLanguageCompiler(options);
                var module = compiler.CompileSource(source);

                var main = Assert.Single(Assert.IsType<ClassDefinition>(Assert.Single(module.Types)).Methods, m => m.Name == "Main");
                var opcodes = main.Instructions.Select(i => i.OpCode).ToArray();
                Assert.Equal(new[] { OpCode.Ldstr, OpCode.Call, OpCode.Ret }, opcodes);

            var callInstruction = Assert.IsType<CallInstruction>(main.Instructions[1]);
            Assert.Equal(writeLine.GetSignature(), callInstruction.Method.GetSignature());
        }

        [Fact]
        public void FunctionCallExpression_UsesIntrinsicReturnValue()
        {
                const string source = """
program compute
    implicit none
    integer :: result
    result = abs(-5)
end program compute
""";

                var registry = FortranIntrinsicRegistry.CreateDefault();
                var mathType = TypeReference.FromName("System.Math");
                var absMethod = new MethodReference(mathType, "Abs", TypeReference.Int32, new List<TypeReference> { TypeReference.Int32 });
                registry.RegisterFunction("abs", absMethod);

                var options = new FortranCompilationOptions(registry);
                var compiler = new FortranLanguageCompiler(options);
                var module = compiler.CompileSource(source);

                var main = Assert.Single(Assert.IsType<ClassDefinition>(Assert.Single(module.Types)).Methods, m => m.Name == "Main");
                var opcodes = main.Instructions.Select(i => i.OpCode).ToArray();

                Assert.Contains(OpCode.Call, opcodes);
                Assert.Equal(TypeReference.Int32, main.Locals.Single(l => l.Name == "result").Type);

                var callInstruction = main.Instructions.OfType<CallInstruction>().Single();
                Assert.Equal(absMethod.GetSignature(), callInstruction.Method.GetSignature());
        }

    private static bool TypeEquals(TypeReference expected, TypeReference actual)
    {
        return string.Equals(expected.Name, actual.Name, StringComparison.OrdinalIgnoreCase)
            && string.Equals(expected.Namespace ?? string.Empty, actual.Namespace ?? string.Empty, StringComparison.Ordinal);
    }
}
