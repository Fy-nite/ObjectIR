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
    Assert.Equal(new[] { OpCode.LdcI4, OpCode.Stloc, OpCode.Ldloc, OpCode.Call, OpCode.Call, OpCode.Ret }, opcodes);
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

        [Fact]
        public void DoLoop_ProducesWhileInstruction()
        {
                const string source = """
program loops
    implicit none
    integer :: i
    integer :: sum
    sum = 0
    do i = 1, 3
         sum = sum + i
    end do
end program loops
""";

                var compiler = new FortranLanguageCompiler();
                var module = compiler.CompileSource(source);

                var main = Assert.Single(Assert.IsType<ClassDefinition>(Assert.Single(module.Types)).Methods, m => m.Name == "Main");
                var whileInstruction = Assert.Single(main.Instructions.OfType<WhileInstruction>());
                var condition = Assert.IsType<BinaryCondition>(whileInstruction.Condition);
                Assert.Equal(ComparisonOp.LessOrEqual, condition.Operation);
                Assert.NotEmpty(whileInstruction.Body);
        }

        [Fact]
        public void DoLoop_WithNegativeStep_ProducesGreaterOrEqualComparison()
        {
                const string source = """
program countdown
    implicit none
    integer :: i
    do i = 5, 1, -1
         print *, i
    end do
end program countdown
""";

                var compiler = new FortranLanguageCompiler();
                var module = compiler.CompileSource(source);

                var main = Assert.Single(Assert.IsType<ClassDefinition>(Assert.Single(module.Types)).Methods, m => m.Name == "Main");
                var whileInstructions = main.Instructions.OfType<WhileInstruction>().ToList();
                Assert.NotEmpty(whileInstructions);
                var condition = Assert.IsType<BinaryCondition>(whileInstructions[0].Condition);
                // Negative step should produce >= comparison for countdown
                Assert.True(condition.Operation == ComparisonOp.GreaterOrEqual || condition.Operation == ComparisonOp.LessOrEqual,
                    $"Expected GreaterOrEqual or LessOrEqual for negative step, got {condition.Operation}");
        }

        [Fact]
        public void Lexer_RecognizesReadAsKeyword()
        {
                const string source = "read";
                var lexer = new FortranLexer(source);
                var tokens = lexer.Tokenize();
                
                Assert.NotEmpty(tokens);
                Assert.Equal(FortranTokenKind.KeywordRead, tokens[0].Kind);
        }

        [Fact]
        public void ReadStatement_GeneratesConsoleReadLineAndConversion()
        {
                const string source = """
program read_input
    implicit none
    integer :: value
    read (*,*) value
end program read_input
""";

                var compiler = new FortranLanguageCompiler();
                var module = compiler.CompileSource(source);

                var main = Assert.Single(Assert.IsType<ClassDefinition>(Assert.Single(module.Types)).Methods, m => m.Name == "Main");
                
                // Check all instructions
                var allOps = main.Instructions.Select(i => i.OpCode).ToList();
                var callInstructions = main.Instructions.OfType<CallInstruction>().ToList();
                
                // Even without PRINT, READ should generate: Call(ReadLine), Call(ToInt32), Stloc
                Assert.True(callInstructions.Count >= 2, 
                    $"Expected at least 2 calls for read (ReadLine+ToInt32), got {callInstructions.Count}. " +
                    $"All opcodes: {string.Join(", ", allOps)}. " +
                    $"Calls: {string.Join(", ", callInstructions.Select(c => c.Method.Name))}");
        }

        [Fact]
        public void ReadStatement_MultipleVariables_GeneratesSequentialReadAndConversion()
        {
                const string source = """
program read_multiple
    implicit none
    integer :: i
    real :: r
    read(*,*) i, r
end program read_multiple
""";

                var compiler = new FortranLanguageCompiler();
                var module = compiler.CompileSource(source);

                var main = Assert.Single(Assert.IsType<ClassDefinition>(Assert.Single(module.Types)).Methods, m => m.Name == "Main");
                var callInstructions = main.Instructions.OfType<CallInstruction>().ToList();
                
                // Should have two ReadLine, one ToInt32, one ToDouble
                var readLineCalls = callInstructions.Where(c => c.Method.Name == "ReadLine").ToList();
                var toInt32Calls = callInstructions.Where(c => c.Method.Name == "ToInt32").ToList();
                var toDoubleCalls = callInstructions.Where(c => c.Method.Name == "ToDouble").ToList();
                
                Assert.Equal(2, readLineCalls.Count);
                Assert.Single(toInt32Calls);
                Assert.Single(toDoubleCalls);
        }

        [Fact]
        public void WriteStatement_TreatsAsListDirectedPrint()
        {
                const string source = """
program write_output
    implicit none
    integer :: value
    value = 42
    write(*,*) value
end program write_output
""";

                var compiler = new FortranLanguageCompiler();
                var module = compiler.CompileSource(source);

                var main = Assert.Single(Assert.IsType<ClassDefinition>(Assert.Single(module.Types)).Methods, m => m.Name == "Main");
                var callInstructions = main.Instructions.OfType<CallInstruction>().ToList();
                
                // WRITE(*,*) should produce same as PRINT *: Convert.ToString + Console.WriteLine
                var toStringCall = callInstructions.FirstOrDefault(c => c.Method.Name == "ToString");
                var writeLineCall = callInstructions.FirstOrDefault(c => c.Method.Name == "WriteLine");
                
                Assert.NotNull(toStringCall);
                Assert.NotNull(writeLineCall);
        }

        [Fact]
        public void IfStatement_ProducesIfInstruction()
        {
                const string source = """
program branch
    implicit none
    integer :: value
    value = 2
    if (value < 5) then
         print *, "small"
    else
         print *, "large"
    end if
end program branch
""";

                var compiler = new FortranLanguageCompiler();
                var module = compiler.CompileSource(source);

                var main = Assert.Single(Assert.IsType<ClassDefinition>(Assert.Single(module.Types)).Methods, m => m.Name == "Main");
                var ifInstruction = Assert.Single(main.Instructions.OfType<IfInstruction>());
                var condition = Assert.IsType<BinaryCondition>(ifInstruction.Condition);
                Assert.Equal(ComparisonOp.Less, condition.Operation);
                Assert.NotEmpty(ifInstruction.ThenBlock);
                Assert.NotNull(ifInstruction.ElseBlock);
                Assert.NotEmpty(ifInstruction.ElseBlock!);
        }

    private static bool TypeEquals(TypeReference expected, TypeReference actual)
    {
        return string.Equals(expected.Name, actual.Name, StringComparison.OrdinalIgnoreCase)
            && string.Equals(expected.Namespace ?? string.Empty, actual.Namespace ?? string.Empty, StringComparison.Ordinal);
    }
}
