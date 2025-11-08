// Program entry point for Contract -> ObjectIR translation
// Uses internal namespaces defined within this project and referenced ObjectIR.Core

using ObjectIR.Core.Serialization;
using ObjectIR.ContractFrontend; // lexer, parser, emitter
using ObjectIR.ContractFrontend.AST; // AST node types
using ObjectIR.Core.IR;

var source = args.Length > 0 ? await File.ReadAllTextAsync(args[0]) : "Contract Main { fn main() { IO.println(\"Hello\"); } }";

var lexer = new ObjectIR.ContractFrontend.Lexer(source);
var tokens = lexer.Lex().ToList();
var parser = new ObjectIR.ContractFrontend.Parser(tokens);
var unit = parser.Parse();
var emitter = new ObjectIR.ContractFrontend.Emitter();
var module = emitter.Emit(unit, "ContractModule");
var json = new ObjectIR.Core.Serialization.ModuleSerializer(module).DumpToJson();

if (args.Length > 1)
    await File.WriteAllTextAsync(args[1], json);
else
    Console.WriteLine(json);
