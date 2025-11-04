using ObjectIR.Core.Compilers;
using ObjectIR.Core.Serialization;
using System;
using System.IO;

namespace ObjectIR.Examples;

/// <summary>
/// Example demonstrating compilation from Construct language to ObjectIR
/// </summary>
public static class ConstructLanguageExample
{
    public static void Main()
    {
        Console.WriteLine("=== Construct Language to ObjectIR Compiler Example ===\n");

        // Example 1: Simple calculator
        CompileSimpleCalculator();

        // Example 2: Program with multiple functions
        CompileMultipleFunctions();

        // Example 3: Save to file
        SaveCompiledModule();
    }

    static void CompileSimpleCalculator()
    {
        Console.WriteLine("--- Example 1: Simple Calculator Program ---");

        string constructCode = @"
Contract Calculator {
    fn Add(a: Int, b: Int) -> Int {
        var result = a + b;
        return result;
    }

    fn Multiply(a: Int, b: Int) -> Int {
        var result = a * b;
        return result;
    }

    fn IsPositive(n: Int) -> Bool {
        return n > 0;
    }
}
";

        try
        {
            var compiler = new ConstructLanguageCompiler();
            var module = compiler.CompileSource(constructCode);

            Console.WriteLine($"✓ Compilation successful!");
            Console.WriteLine($"  Module: {module.Name}");
            Console.WriteLine($"  Types: {module.Types.Count}");
            Console.WriteLine($"  Functions: {module.Functions.Count}");
            Console.WriteLine();

            // Dump the generated IR
            Console.WriteLine("Generated ObjectIR:");
            Console.WriteLine(module.DumpText());
            Console.WriteLine();
        }
        catch (Exception ex)
        {
            Console.WriteLine($"✗ Compilation failed: {ex.Message}");
        }
    }

    static void CompileMultipleFunctions()
    {
        Console.WriteLine("--- Example 2: Multiple Functions ---");

        string constructCode = @"
Contract StringUtils {
    fn GetLength(s: String) -> Int {
        return 42;
    }

    fn IsEmpty(s: String) -> Bool {
        return false;
    }

    fn Greet(name: String) {
        IO.println(name);
    }
}
";

        try
        {
            var compiler = new ConstructLanguageCompiler();
            var module = compiler.CompileSource(constructCode);

            Console.WriteLine($"✓ Compiled StringUtils module");
            Console.WriteLine($"  Classes: {module.Types.Count}");
            
            if (module.Types.Count > 0)
            {
                var mainClass = module.Types[0];
                Console.WriteLine($"  First class: {mainClass.Name}");
            }
            Console.WriteLine();
        }
        catch (Exception ex)
        {
            Console.WriteLine($"✗ Compilation failed: {ex.Message}");
        }
    }

    static void SaveCompiledModule()
    {
        Console.WriteLine("--- Example 3: Save Compiled Module ---");

        string constructCode = @"
Contract Example {
    fn Add(x: Int, y: Int) -> Int {
        var sum = x + y;
        return sum;
    }
}
";

        try
        {
            var compiler = new ConstructLanguageCompiler();
            var module = compiler.CompileSource(constructCode);

            // Save to files
            string outputDir = "compiled_modules";
            Directory.CreateDirectory(outputDir);

            // Save as JSON
            string jsonPath = Path.Combine(outputDir, "example.ir.json");
            var loader = new ModuleLoader();
            loader.SaveToJsonFile(module, jsonPath);
            Console.WriteLine($"✓ Saved JSON: {jsonPath}");

            // Save as text dump
            string textPath = Path.Combine(outputDir, "example.ir.txt");
            loader.SaveToTextFile(module, textPath);
            Console.WriteLine($"✓ Saved text: {textPath}");

            // Show file sizes
            var jsonInfo = new FileInfo(jsonPath);
            var textInfo = new FileInfo(textPath);
            Console.WriteLine($"  JSON size: {jsonInfo.Length} bytes");
            Console.WriteLine($"  Text size: {textInfo.Length} bytes");
            Console.WriteLine();
        }
        catch (Exception ex)
        {
            Console.WriteLine($"✗ Error: {ex.Message}");
        }
    }
}
