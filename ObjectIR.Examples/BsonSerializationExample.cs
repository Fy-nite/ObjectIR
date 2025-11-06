using ObjectIR.Core.Builder;
using ObjectIR.Core.Serialization;
using ObjectIR.Core.IR;
using System;
using System.IO;

namespace ObjectIR.Examples;

/// <summary>
/// Example demonstrating BSON serialization for smaller module files
/// </summary>
public class BsonSerializationExample
{
    public static void Main()
    {
        Console.WriteLine("=== ObjectIR BSON Serialization Example ===\n");

        // Create a sample module
        var builder = new IRBuilder("TestModule");
        builder.Class("TestClass")
            .Method("HelloWorld", TypeReference.Void)
                .Static()
                .EndMethod()
            .EndClass();

        var module = builder.Build();

        // Save as JSON
        var jsonPath = Path.Combine(Environment.CurrentDirectory, "test_module.json");
        var loader = new ModuleLoader();
        loader.SaveToJsonFile(module, jsonPath, indented: true);

        // Save as BSON
        var bsonPath = Path.Combine(Environment.CurrentDirectory, "test_module.bson");
        loader.SaveToBsonFile(module, bsonPath);

        // Compare file sizes
        var jsonInfo = new FileInfo(jsonPath);
        var bsonInfo = new FileInfo(bsonPath);

        Console.WriteLine($"JSON File Size:    {jsonInfo.Length,8} bytes");
        Console.WriteLine($"BSON File Size:    {bsonInfo.Length,8} bytes");
        
        if (jsonInfo.Length > 0)
        {
            var reduction = jsonInfo.Length - bsonInfo.Length;
            var percentage = Math.Round(100.0 * reduction / jsonInfo.Length, 2);
            Console.WriteLine($"Size Reduction:    {reduction,8} bytes ({percentage}%)\n");
        }

        // Verify round-trip: load BSON and convert back to JSON
        var loadedModule = loader.LoadFromBsonFile(bsonPath);
        Console.WriteLine($"Module loaded from BSON: {loadedModule.Name}");
        Console.WriteLine($"Classes in module: {loadedModule.Types.Count}");

        // Compare JSON outputs
        var originalJson = module.DumpJson(indented: false);
        var roundTripJson = loadedModule.DumpJson(indented: false);
        var jsonMatch = originalJson == roundTripJson;
        Console.WriteLine($"Round-trip JSON match: {(jsonMatch ? "✓ Yes" : "✗ No")}");

        if (jsonMatch)
        {
            Console.WriteLine("\n✓ BSON serialization working correctly!");
            Console.WriteLine($"BSON file saved to: {bsonPath}");
        }
        else
        {
            Console.WriteLine("\n✗ BSON round-trip mismatch detected.");
        }

        // Clean up
        File.Delete(jsonPath);
        File.Delete(bsonPath);
    }
}
