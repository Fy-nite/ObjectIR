using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.CodeAnalysis.CSharp;
using ObjectIR.Core.IR;

namespace ObjectIR.CSharpFrontend;

class Program
{
    static int Main(string[] args)
    {
        try
        {
            // Parse command-line arguments
            var parser = new CommandLineParser();
            CompilerOptions options;

            try
            {
                options = parser.Parse(args);
            }
            catch (ArgumentException ex)
            {
                Console.Error.WriteLine($"Error: {ex.Message}");
                Console.Error.WriteLine();
                CommandLineParser.PrintHelp();
                return 1;
            }

            // Handle help flag
            if (options.ShowHelp)
            {
                CommandLineParser.PrintHelp();
                return 0;
            }

            // Handle version flag
            if (options.ShowVersion)
            {
                CommandLineParser.PrintVersion();
                return 0;
            }

            // Run compilation
            var compiler = new CompilerService(options);
            bool success = compiler.Compile();

            return compiler.GetExitCode();
        }
        catch (Exception ex)
        {
            Console.Error.WriteLine($"Fatal error: {ex.Message}");
            Console.Error.WriteLine(ex.StackTrace);
            return 1;
        }
    }
}
