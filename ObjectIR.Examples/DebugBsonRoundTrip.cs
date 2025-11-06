using ObjectIR.Core.Builder;
using ObjectIR.Core.Serialization;
using ObjectIR.Core.IR;
using System;
using System.IO;

namespace ObjectIR.Examples;

/// <summary>
/// Debug BSON round-trip differences
/// </summary>
public class DebugBsonRoundTrip
{
    public static void Main()
    {
        Console.WriteLine("=== Debugging BSON Round-trip ===\n");

        // Create a sample module
        var builder = new IRBuilder("TestModule");
        builder.Class("TestClass")
            .Method("HelloWorld", TypeReference.Void)
                .Static()
                .EndMethod()
            .EndClass();

        var module = builder.Build();

        // Original JSON
        var originalJson = module.DumpJson(indented: false);
        Console.WriteLine("Original JSON length: " + originalJson.Length);

        // Save and load BSON
        var loader = new ModuleLoader();
        var bsonPath = "test_debug.bson";
        loader.SaveToBsonFile(module, bsonPath);
        var loadedModule = loader.LoadFromBsonFile(bsonPath);

        // Round-trip JSON
        var roundTripJson = loadedModule.DumpJson(indented: false);
        Console.WriteLine("Round-trip JSON length: " + roundTripJson.Length);
        
        // Compare line by line
        var originalLines = originalJson.Split('"');
        var roundTripLines = roundTripJson.Split('"');
        
        Console.WriteLine($"\nOriginal has {originalLines.Length} segments");
        Console.WriteLine($"Round-trip has {roundTripLines.Length} segments");
        
        // Show a sample difference
        if (originalJson != roundTripJson)
        {
            Console.WriteLine("\nFirst few characters of each:");
            Console.WriteLine("Original:  " + originalJson.Substring(0, Math.Min(100, originalJson.Length)));
            Console.WriteLine("RoundTrip: " + roundTripJson.Substring(0, Math.Min(100, roundTripJson.Length)));
            
            // Find first difference
            for (int i = 0; i < Math.Min(originalJson.Length, roundTripJson.Length); i++)
            {
                if (originalJson[i] != roundTripJson[i])
                {
                    Console.WriteLine($"\nFirst difference at position {i}:");
                    Console.WriteLine($"Original:  ...{originalJson.Substring(Math.Max(0, i-20), Math.Min(40, originalJson.Length - Math.Max(0, i-20)))}...");
                    Console.WriteLine($"RoundTrip: ...{roundTripJson.Substring(Math.Max(0, i-20), Math.Min(40, roundTripJson.Length - Math.Max(0, i-20)))}...");
                    break;
                }
            }
        }

        File.Delete(bsonPath);
    }
}
