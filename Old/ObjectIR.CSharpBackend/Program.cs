using ObjectIR.Core.IR;
using ObjectIR.Core.Serialization;
using ObjectIR.CSharpBackend;

class Program
{
    static void Main(string[] args)
    {
        if (args.Length != 1)
        {
            Console.WriteLine("Usage: ObjectIR.CSharpBackend <module-json-file>");
            Console.WriteLine("The tool will generate C# code files in the current directory.");
            return;
        }

        string jsonFilePath = args[0];

        try
        {
            // Read the JSON file
            string json = File.ReadAllText(jsonFilePath);

            // Load the module from JSON
            Module module = ModuleSerializer.LoadFromJson(json);

                // Generate C# code. Pass optional second argument `--roslyn` to enable Roslyn formatting.
                bool useRoslyn = args.Length > 1 && args[1] == "--roslyn";
                string code;
                if (useRoslyn)
                {
                    var roslynGen = new RoslynCodeGenerator();
                    code = roslynGen.Generate(module);
                }
                else
                {
                    CSharpCodeGenerator generator = new CSharpCodeGenerator();
                    code = generator.Generate(module);
                }

            // Write to output file
            string outputFileName = $"{module.Name}.cs";
            File.WriteAllText(outputFileName, code);

            Console.WriteLine($"Generated C# code written to: {outputFileName}");
        }
        catch (Exception ex)
        {
            Console.Error.WriteLine($"Error: {ex.Message}");
            Environment.Exit(1);
        }
    }
}
