using ObjectIR.Core.Builder;
using ObjectIR.Core.IR;
using ObjectIR.Core.Serialization;
using TypeReference = ObjectIR.Core.IR.TypeReference;

// Create a module
var builder = new IRBuilder("HelloWorld");

// Define a class with a Main method
#pragma warning disable CS8602 // Dereference of a possibly null reference.
builder.Class("Program")
.Namespace("HelloWorld")
    .Method("Main", TypeReference.Void)
        .Static()
        .Body()
      // Load the string constant
            .Ldstr("Hello, World!")

            // Call Console.WriteLine
            .Call(new MethodReference(
        TypeReference.FromName("System.Console"),
          "WriteLine",
            TypeReference.Void,
         new List<TypeReference> { TypeReference.String }))

      .Ret()
        .EndBody()
        .EndMethod()
    .EndClass();
#pragma warning restore CS8602 // Dereference of a possibly null reference.

var module = builder.Build();

// ============================================================================
// NEW: Dump module contents in various formats
// ============================================================================

Console.WriteLine("=== Module Summary ===");
Console.WriteLine(module.GenerateSummaryReport());
Console.WriteLine();

Console.WriteLine("=== Dump as Text ===");
Console.WriteLine(module.DumpText());
Console.WriteLine();

Console.WriteLine("=== Dump as JSON ===");
Console.WriteLine(module.DumpJson());
Console.WriteLine();

Console.WriteLine("=== Dump Types Array ===");
var types = module.DumpTypes();
foreach (var type in types)
{
    Console.WriteLine($"Type: {type.Kind} - {type.Name}");
    foreach (var method in type.Methods)
    {
        Console.WriteLine($"  Method: {method.Name}");
        Console.WriteLine($"    Return Type: {method.ReturnType}");
  Console.WriteLine($"    Parameters: {method.Parameters.Length}");
      Console.WriteLine($"    Instructions: {method.InstructionCount}");
    }
}
