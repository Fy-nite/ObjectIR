using ObjectIR.Core.Builder;
using ObjectIR.Core.Serialization;
using System;
using System.IO;

// Create a sample module
var builder = new IRBuilder("TestModule");
var classBuilder = builder.Class("TestClass");

classBuilder.DefineMethod("HelloWorld", returnType: "System.Void");
var method = classBuilder.GetMethod("HelloWorld");
method?.MakeStatic();
method?.DefineParameter("name", "System.String");

classBuilder.EndClass();
var module = builder.Build();

// Save as JSON
var jsonPath = "test_module.json";
var loader = new ModuleLoader();
loader.SaveToJsonFile(module, jsonPath, indented: true);

// Save as BSON
var bsonPath = "test_module.bson";
loader.SaveToBsonFile(module, bsonPath);

// Compare file sizes
var jsonInfo = new FileInfo(jsonPath);
var bsonInfo = new FileInfo(bsonPath);

Console.WriteLine("=== ObjectIR BSON Serialization Test ===\n");
Console.WriteLine($"JSON File Size: {jsonInfo.Length} bytes");
Console.WriteLine($"BSON File Size: {bsonInfo.Length} bytes");
Console.WriteLine($"Size Reduction: {jsonInfo.Length - bsonInfo.Length} bytes ({Math.Round(100.0 * (jsonInfo.Length - bsonInfo.Length) / jsonInfo.Length, 2)}%)\n");

// Verify round-trip: load BSON and convert back to JSON
var loadedModule = loader.LoadFromBsonFile(bsonPath);
Console.WriteLine($"Module loaded from BSON: {loadedModule.Name}");
Console.WriteLine($"Classes in module: {loadedModule.Types.Count}");

// Optional: Compare JSON outputs
var originalJson = module.DumpJson(indented: false);
var roundTripJson = loadedModule.DumpJson(indented: false);
var jsonMatch = originalJson == roundTripJson;
Console.WriteLine($"Round-trip JSON match: {jsonMatch}");

if (jsonMatch)
{
    Console.WriteLine("\n✓ BSON serialization successful!");
}
else
{
    Console.WriteLine("\n✗ BSON round-trip mismatch detected.");
}
