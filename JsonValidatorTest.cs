using System;
using System.Text.Json;
using ObjectIR.Core.Serialization;

class JsonValidatorTest
{
    static void Main()
    {
        Console.WriteLine("Testing ObjectIR JsonValidator...");

        // Test 1: Valid JSON
        string validJson = @"{
  ""Name"": ""TestModule"",
  ""Version"": ""1.0.0"",
  ""Metadata"": {},
  ""Types"": []
}";

        var result = JsonValidator.Validate(validJson);
        Console.WriteLine($"Valid JSON test: {(result.IsValid ? "PASSED" : "FAILED")}");
        if (!result.IsValid)
        {
            Console.WriteLine($"  Error: {result.ErrorMessage}");
        }

        // Test 2: Invalid JSON (missing required field)
        string invalidJson = @"{
  ""Name"": ""TestModule"",
  ""Metadata"": {},
  ""Types"": []
}";

        result = JsonValidator.Validate(invalidJson);
        Console.WriteLine($"Invalid JSON test (missing Version): {(result.IsValid ? "FAILED" : "PASSED")}");
        if (!result.IsValid)
        {
            Console.WriteLine($"  Error: {result.ErrorMessage}");
        }

        // Test 3: Invalid JSON (wrong opCode)
        string invalidOpCodeJson = @"{
  ""Name"": ""TestModule"",
  ""Version"": ""1.0.0"",
  ""Metadata"": {},
  ""Types"": [{
    ""Kind"": ""Class"",
    ""Name"": ""TestClass"",
    ""Namespace"": ""Test"",
    ""Access"": ""Public"",
    ""IsAbstract"": false,
    ""IsSealed"": false,
    ""Interfaces"": [],
    ""BaseInterfaces"": [],
    ""GenericParameters"": [],
    ""Fields"": [],
    ""Methods"": [{
      ""Name"": ""TestMethod"",
      ""ReturnType"": ""System.Void"",
      ""Access"": ""Public"",
      ""IsStatic"": false,
      ""IsVirtual"": false,
      ""IsOverride"": false,
      ""IsAbstract"": false,
      ""IsConstructor"": false,
      ""Parameters"": [],
      ""LocalVariables"": [],
      ""InstructionCount"": 1,
      ""Instructions"": [{
        ""opCode"": ""invalid_op"",
        ""operand"": null
      }]
    }],
    ""Properties"": []
  }]
}";

        result = JsonValidator.Validate(invalidOpCodeJson);
        Console.WriteLine($"Invalid opCode test: {(result.IsValid ? "FAILED" : "PASSED")}");
        if (!result.IsValid)
        {
            Console.WriteLine($"  Error: {result.ErrorMessage}");
        }

        Console.WriteLine("JsonValidator tests completed!");
    }
}