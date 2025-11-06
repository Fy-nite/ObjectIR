using System;
using System.IO;
using System.Text.Json;
using ObjectIR.Fortran.Compiler;

if (args.Length == 0)
{
	PrintUsage();
	return;
}

string? inputPath = null;
string? outputPath = null;
string format = "text";
string? intrinsicsPath = null;

for (int i = 0; i < args.Length; i++)
{
	var current = args[i];
	switch (current)
	{
		case "--out":
		case "-o":
			if (i + 1 >= args.Length)
			{
				Console.Error.WriteLine("Missing value for --out");
				return;
			}
			outputPath = args[++i];
			break;

		case "--format":
			if (i + 1 >= args.Length)
			{
				Console.Error.WriteLine("Missing value for --format");
				return;
			}
			format = args[++i].ToLowerInvariant();
			break;

		case "--help":
		case "-h":
			PrintUsage();
			return;

		case "--intrinsics":
			if (i + 1 >= args.Length)
			{
				Console.Error.WriteLine("Missing value for --intrinsics");
				return;
			}
			intrinsicsPath = args[++i];
			break;

		default:
			if (current.StartsWith("-"))
			{
				Console.Error.WriteLine($"Unknown option: {current}");
				return;
			}

			if (inputPath == null)
			{
				inputPath = current;
			}
			else
			{
				Console.Error.WriteLine("Multiple input files specified");
				return;
			}
			break;
	}
}

if (inputPath == null)
{
	Console.Error.WriteLine("No input file provided");
	return;
}

if (!File.Exists(inputPath))
{
	Console.Error.WriteLine($"Input file not found: {inputPath}");
	return;
}

string source = File.ReadAllText(inputPath);

FortranCompilationOptions options;
if (intrinsicsPath != null)
{
	if (!File.Exists(intrinsicsPath))
	{
		Console.Error.WriteLine($"Intrinsic configuration not found: {intrinsicsPath}");
		return;
	}

	try
	{
		var json = File.ReadAllText(intrinsicsPath);
		var config = JsonSerializer.Deserialize<FortranIntrinsicConfig>(json, new JsonSerializerOptions
		{
			PropertyNameCaseInsensitive = true,
			ReadCommentHandling = JsonCommentHandling.Skip,
			AllowTrailingCommas = true
		}) ?? new FortranIntrinsicConfig();
		var registry = FortranIntrinsicRegistry.CreateDefault();
		registry.LoadFromConfig(config);
		options = new FortranCompilationOptions(registry);
	}
	catch (Exception ex)
	{
		Console.Error.WriteLine($"Failed to read intrinsic configuration: {ex.Message}");
		return;
	}
}
else
{
	options = FortranCompilationOptions.Default;
}

var compiler = new FortranLanguageCompiler(options);

try
{
	string output = format switch
	{
		"text" => compiler.CompileSourceToText(source),
		"json" => compiler.CompileSourceToJson(source),
		_ => throw new InvalidOperationException($"Unsupported format: {format}")
	};

	if (outputPath == null)
	{
		Console.WriteLine(output);
	}
	else
	{
		File.WriteAllText(outputPath, output);
	}
}
catch (Exception ex)
{
	Console.Error.WriteLine(ex.Message);
}

static void PrintUsage()
{
	Console.WriteLine("Usage: objectir-fortran <input-file> [--out <path>] [--format text|json] [--intrinsics <config.json>]");
}
