using CommandLine;
using System.Reflection;
using System.IO;

namespace SharpIR
{
    class Options
    {
        [Option('h', "help", HelpText = "Show help message")]
        public bool Help { get; set; }

        [Option('v', "version", HelpText = "Show version information")]
        public bool Version { get; set; }

        [Value(0, HelpText = "File(s) to process")]
        public string? File { get; set; }
    }

    public class Program
    {
        public static void Main(string[] args)
        {
            Parser.Default.ParseArguments<Options>(args)
                .WithParsed(o =>
                {
                    if (o.File == null)
                    {
                        // somehow this prints help.
                        Parser.Default.ParseArguments<Options>(new string[] { "--help" })
                            .WithParsed(h => { });
                        return;
                    }

                    if (o.Version)
                    {
                        Console.WriteLine($"SharpIR {Assembly.GetExecutingAssembly().GetName().Version}");
                    }
                    else if (!string.IsNullOrEmpty(o.File))
                    {
                        //string could be a wildcard, expand it 
                        if (!o.File.Contains('*') && !o.File.Contains('?'))
                        {
                            string fullPath = Path.Combine(Environment.CurrentDirectory, o.File);
                            CSharpParser.ParseFile(fullPath);
                        }
                        else
                        {
                            string inputDir = Path.GetDirectoryName(o.File);
                            string directory = string.IsNullOrEmpty(inputDir) ? Environment.CurrentDirectory : Path.Combine(Environment.CurrentDirectory, inputDir);
                            string pattern = Path.GetFileName(o.File) ?? "*";
                            var files = Directory.GetFiles(directory, pattern);
                            foreach (var file in files)
                            {
                                CSharpParser.ParseFile(file);
                            }
                        }
                    }
                });
        }
    }
}