using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.Formatting;
using Microsoft.CodeAnalysis.MSBuild;
using Microsoft.CodeAnalysis.Options;
using Microsoft.CodeAnalysis.Text;
using System.Text;
using ObjectIR.Core.IR;

namespace ObjectIR.CSharpBackend;

/// <summary>
/// Generates C# code using Roslyn for parsing/formatting.
/// This class leverages the existing string-based generator and then
/// parses+formats the output to produce a cleaner, Roslyn-validated result.
/// </summary>
public class RoslynCodeGenerator
{
    private readonly CSharpCodeGenerator _legacyGenerator = new CSharpCodeGenerator();

    public string Generate(Module module)
    {
        // Use the existing generator to produce the raw C# source
        var raw = _legacyGenerator.Generate(module);

        // Parse with Roslyn
        var tree = CSharpSyntaxTree.ParseText(raw);
        var root = tree.GetRoot();

        // Format using an AdhocWorkspace
        using var workspace = new AdhocWorkspace();

        var formatted = Formatter.Format(root, workspace);
        return formatted.ToFullString();
    }
}
