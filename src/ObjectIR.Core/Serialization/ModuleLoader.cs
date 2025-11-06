namespace ObjectIR.Core.Serialization;

using ObjectIR.Core.IR;
using ObjectIR.Core.Builder;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;

/// <summary>
/// Loads ObjectIR modules from text format and provides storage/caching capabilities
/// </summary>
public sealed class ModuleLoader
{
    private readonly Dictionary<string, Module> _moduleCache = new();

    /// <summary>
    /// Loads a module from text format (as shown in documentation)
    /// </summary>
    /// <example>
    /// var textFormat = @"
    /// module CalculatorApp
    /// class Calculator {
    ///     field history: List&lt;int32&gt;
    ///     method Add(a: int32, b: int32) -&gt; int32 {
    ///         ldarg a
    ///         ldarg b
    ///         add
    ///         ret
    ///     }
    /// }
    /// ";
    /// var loader = new ModuleLoader();
    /// var module = loader.LoadFromText(textFormat);
    /// </example>
    public Module LoadFromText(string text)
    {
        var lines = text.Split(new[] { '\r', '\n' }, StringSplitOptions.RemoveEmptyEntries)
            .Select(l => l.Trim())
            .Where(l => !string.IsNullOrWhiteSpace(l) && !l.StartsWith("//"))
            .ToList();

        if (lines.Count == 0)
            throw new ArgumentException("Empty module text");

        int lineIndex = 0;

        // Parse module declaration
        if (!lines[lineIndex].StartsWith("module "))
            throw new FormatException($"Expected 'module' keyword at line {lineIndex}");

        string moduleName = lines[lineIndex].Substring("module ".Length).Trim();
        lineIndex++;

        var builder = new IRBuilder(moduleName);

        // Parse types and functions
        while (lineIndex < lines.Count)
        {
            string line = lines[lineIndex];

            if (line.StartsWith("class "))
            {
                lineIndex = ParseClass(builder, lines, lineIndex);
            }
            else if (line.StartsWith("interface "))
            {
                lineIndex = ParseInterface(builder, lines, lineIndex);
            }
            else if (line.StartsWith("struct "))
            {
                lineIndex = ParseStruct(builder, lines, lineIndex);
            }
            else if (line.StartsWith("enum "))
            {
                lineIndex = ParseEnum(builder, lines, lineIndex);
            }
            else
            {
                lineIndex++;
            }
        }

        var module = builder.Build();
        CacheModule(module);
        return module;
    }

    /// <summary>
    /// Loads a module from JSON format
    /// </summary>
    public Module LoadFromJson(string json)
    {
        var module = ModuleSerializer.LoadFromJson(json);
        CacheModule(module);
        return module;
    }

    /// <summary>
    /// Loads a module from a JSON file
    /// </summary>
    public Module LoadFromJsonFile(string filePath)
    {
        var json = File.ReadAllText(filePath);
        return LoadFromJson(json);
    }

    /// <summary>
    /// Loads a module from text file format
    /// </summary>
    public Module LoadFromTextFile(string filePath)
    {
        var text = File.ReadAllText(filePath);
        return LoadFromText(text);
    }

    /// <summary>
    /// Saves a module to JSON file
    /// </summary>
    public void SaveToJsonFile(Module module, string filePath, bool indented = true)
    {
        var json = module.DumpJson(indented);
        File.WriteAllText(filePath, json);
        CacheModule(module);
    }

    /// <summary>
    /// Saves a module to text file format
    /// </summary>
    public void SaveToTextFile(Module module, string filePath)
    {
        var text = module.DumpText();
        File.WriteAllText(filePath, text);
        CacheModule(module);
    }

    /// <summary>
    /// Loads a module from BSON (binary JSON) format
    /// </summary>
    public Module LoadFromBson(byte[] bsonData)
    {
        var module = ModuleSerializer.LoadFromBson(bsonData);
        CacheModule(module);
        return module;
    }

    /// <summary>
    /// Loads a module from a BSON file
    /// </summary>
    public Module LoadFromBsonFile(string filePath)
    {
        var bsonData = File.ReadAllBytes(filePath);
        return LoadFromBson(bsonData);
    }

    /// <summary>
    /// Saves a module to BSON file for smaller file size
    /// </summary>
    public void SaveToBsonFile(Module module, string filePath)
    {
        var bsonData = module.DumpBson();
        File.WriteAllBytes(filePath, bsonData);
        CacheModule(module);
    }

    /// <summary>
    /// Gets a cached module by name
    /// </summary>
    public Module? GetCachedModule(string moduleName)
    {
        return _moduleCache.TryGetValue(moduleName, out var module) ? module : null;
    }

    /// <summary>
    /// Clears the module cache
    /// </summary>
    public void ClearCache()
    {
        _moduleCache.Clear();
    }

    /// <summary>
    /// Gets all cached modules
    /// </summary>
    public IReadOnlyDictionary<string, Module> GetAllCachedModules()
    {
        return _moduleCache.AsReadOnly();
    }

    /// <summary>
    /// Loads multiple module files from a directory
    /// </summary>
    public Dictionary<string, Module> LoadModulesFromDirectory(string directoryPath, string pattern = "*.ir.txt")
    {
        var modules = new Dictionary<string, Module>();
        var files = Directory.GetFiles(directoryPath, pattern);

        foreach (var file in files)
        {
            try
            {
                // Detect format based on file extension
                Module module;
                if (file.EndsWith(".json", StringComparison.OrdinalIgnoreCase))
                {
                    module = LoadFromJsonFile(file);
                }
                else
                {
                    module = LoadFromTextFile(file);
                }
                modules[module.Name] = module;
            }
            catch (Exception ex)
            {
                throw new IOException($"Failed to load module from {file}: {ex.Message}", ex);
            }
        }

        return modules;
    }

    /// <summary>
    /// Saves multiple modules to a directory
    /// </summary>
    public void SaveModulesToDirectory(IEnumerable<Module> modules, string directoryPath)
    {
        Directory.CreateDirectory(directoryPath);

        foreach (var module in modules)
        {
            var filename = Path.Combine(directoryPath, $"{module.Name}.ir.txt");
            SaveToTextFile(module, filename);
        }
    }

    // ============================================================================
    // Private Parsing Methods
    // ============================================================================

    private void CacheModule(Module module)
    {
        _moduleCache[module.Name] = module;
    }

    private int ParseClass(IRBuilder builder, List<string> lines, int startIndex)
    {
        // Extract class name and base type
        var classLine = lines[startIndex];
        var classMatch = Regex.Match(classLine, @"class\s+(\w+)\s*(?:\:\s*(\w+))?\s*\{?");

        if (!classMatch.Success)
            throw new FormatException($"Invalid class declaration at line {startIndex}");

        string className = classMatch.Groups[1].Value;
        string? baseType = classMatch.Groups[2].Success ? classMatch.Groups[2].Value : null;

        var classBuilder = builder.Class(className);
        int lineIndex = startIndex + 1;

        // Parse class members until closing brace
        while (lineIndex < lines.Count && !lines[lineIndex].Contains("}"))
        {
            var line = lines[lineIndex];

            if (line.StartsWith("field "))
            {
                lineIndex = ParseField(classBuilder, lines, lineIndex);
            }
            else if (line.StartsWith("method "))
            {
                lineIndex = ParseMethod(classBuilder, lines, lineIndex);
            }
            else if (line.StartsWith("property "))
            {
                lineIndex = ParseProperty(classBuilder, lines, lineIndex);
            }
            else
            {
                lineIndex++;
            }
        }

        classBuilder.EndClass();
        return lineIndex + 1;
    }

    private int ParseInterface(IRBuilder builder, List<string> lines, int startIndex)
    {
        var interfaceLine = lines[startIndex];
        var interfaceMatch = Regex.Match(interfaceLine, @"interface\s+(\w+)\s*\{?");

        if (!interfaceMatch.Success)
            throw new FormatException($"Invalid interface declaration at line {startIndex}");

        string interfaceName = interfaceMatch.Groups[1].Value;
        var interfaceBuilder = builder.Interface(interfaceName);
        int lineIndex = startIndex + 1;

        while (lineIndex < lines.Count && !lines[lineIndex].Contains("}"))
        {
            lineIndex++;
        }

        interfaceBuilder.EndInterface();
        return lineIndex + 1;
    }

    private int ParseStruct(IRBuilder builder, List<string> lines, int startIndex)
    {
        var structLine = lines[startIndex];
        var structMatch = Regex.Match(structLine, @"struct\s+(\w+)\s*\{?");

        if (!structMatch.Success)
            throw new FormatException($"Invalid struct declaration at line {startIndex}");

        string structName = structMatch.Groups[1].Value;
        var structBuilder = builder.Struct(structName);
        int lineIndex = startIndex + 1;

        while (lineIndex < lines.Count && !lines[lineIndex].Contains("}"))
        {
            var line = lines[lineIndex];

            if (line.StartsWith("field "))
            {
                lineIndex = ParseField(structBuilder, lines, lineIndex);
            }
            else
            {
                lineIndex++;
            }
        }

        structBuilder.EndStruct();
        return lineIndex + 1;
    }

    private int ParseEnum(IRBuilder builder, List<string> lines, int startIndex)
    {
        var enumLine = lines[startIndex];
        var enumMatch = Regex.Match(enumLine, @"enum\s+(\w+)\s*(?::\s*(\w+))?\s*\{?");

        if (!enumMatch.Success)
            throw new FormatException($"Invalid enum declaration at line {startIndex}");

        string enumName = enumMatch.Groups[1].Value;
        int lineIndex = startIndex + 1;

        while (lineIndex < lines.Count && !lines[lineIndex].Contains("}"))
        {
            lineIndex++;
        }

        return lineIndex + 1;
    }

    private int ParseField(dynamic classBuilder, List<string> lines, int startIndex)
    {
        // field name: type
        var fieldLine = lines[startIndex];
        var fieldMatch = Regex.Match(fieldLine, @"field\s+(\w+)\s*:\s*(.+)$");

        if (!fieldMatch.Success)
            throw new FormatException($"Invalid field declaration at line {startIndex}");

        string fieldName = fieldMatch.Groups[1].Value;
        string fieldType = fieldMatch.Groups[2].Value.Trim();

        var typeRef = ParseTypeReference(fieldType);
        classBuilder.Field(fieldName, typeRef);

        return startIndex + 1;
    }

    private int ParseProperty(dynamic classBuilder, List<string> lines, int startIndex)
    {
        var propLine = lines[startIndex];
        var propMatch = Regex.Match(propLine, @"property\s+(\w+)\s*:\s*(.+)$");

        if (!propMatch.Success)
            throw new FormatException($"Invalid property declaration at line {startIndex}");

        string propName = propMatch.Groups[1].Value;
        string propType = propMatch.Groups[2].Value.Trim();

        var typeRef = ParseTypeReference(propType);
        classBuilder.Property(propName, typeRef);

        return startIndex + 1;
    }

    private int ParseMethod(dynamic classBuilder, List<string> lines, int startIndex)
    {
        // method Name(param: type, ...) -> ReturnType { ... }
        var methodLine = lines[startIndex];
        var methodMatch = Regex.Match(methodLine, 
            @"method\s+(\w+)\s*\((.*?)\)\s*(?:->\s*(\w+))?\s*\{?");

        if (!methodMatch.Success)
            throw new FormatException($"Invalid method declaration at line {startIndex}");

        string methodName = methodMatch.Groups[1].Value;
        string paramsStr = methodMatch.Groups[2].Value;
        string returnTypeStr = methodMatch.Groups[3].Success ? methodMatch.Groups[3].Value : "void";

        var methodBuilder = classBuilder.Method(methodName, ParseTypeReference(returnTypeStr));

        // Parse parameters
        if (!string.IsNullOrWhiteSpace(paramsStr))
        {
            var paramParts = paramsStr.Split(',');
            foreach (var paramPart in paramParts)
            {
                var paramMatch = Regex.Match(paramPart.Trim(), @"(\w+)\s*:\s*(.+)");
                if (paramMatch.Success)
                {
                    string paramName = paramMatch.Groups[1].Value;
                    string paramType = paramMatch.Groups[2].Value.Trim();
                    methodBuilder.Parameter(paramName, ParseTypeReference(paramType));
                }
            }
        }

        int lineIndex = startIndex + 1;

        // Parse locals and body
        while (lineIndex < lines.Count && !lines[lineIndex].Contains("}"))
        {
            var line = lines[lineIndex];

            if (line.StartsWith("local "))
            {
                var localMatch = Regex.Match(line, @"local\s+(\w+)\s*:\s*(.+)$");
                if (localMatch.Success)
                {
                    string localName = localMatch.Groups[1].Value;
                    string localType = localMatch.Groups[2].Value.Trim();
                    methodBuilder.Local(localName, ParseTypeReference(localType));
                }
                lineIndex++;
            }
            else if (IsInstruction(line))
            {
                // Parse instruction - for now, just add as a comment/marker
                // In a full implementation, this would parse the IL instructions
                lineIndex++;
            }
            else
            {
                lineIndex++;
            }
        }

        methodBuilder.EndMethod();
        classBuilder.EndClass();
        return lineIndex + 1;
    }

    private TypeReference ParseTypeReference(string typeStr)
    {
        typeStr = typeStr.Trim();

        // Handle generic types like List<int32>
        if (typeStr.Contains("<") && typeStr.Contains(">"))
        {
            var match = Regex.Match(typeStr, @"(\w+)<(.+)>");
            if (match.Success)
            {
                string genericType = match.Groups[1].Value;
                string innerType = match.Groups[2].Value;

                if (genericType == "List")
                    return TypeReference.List(ParseTypeReference(innerType));
                else if (genericType == "Dict")
                {
                    var parts = innerType.Split(',');
                    if (parts.Length == 2)
                        return TypeReference.Dict(ParseTypeReference(parts[0].Trim()), 
                            ParseTypeReference(parts[1].Trim()));
                    throw new FormatException($"Invalid Dict type: {typeStr}");
                }
                else if (genericType == "Set")
                    return TypeReference.Set(ParseTypeReference(innerType));
                else if (genericType == "Optional")
                    return TypeReference.Optional(ParseTypeReference(innerType));
                else
                    return TypeReference.FromName(genericType);
            }
        }

        // Handle primitive types
        if (typeStr == "void") return TypeReference.Void;
        if (typeStr == "bool") return TypeReference.Bool;
        if (typeStr == "int8") return TypeReference.Int8;
        if (typeStr == "int16") return TypeReference.Int16;
        if (typeStr == "int32") return TypeReference.Int32;
        if (typeStr == "int64") return TypeReference.Int64;
        if (typeStr == "uint8") return TypeReference.UInt8;
        if (typeStr == "uint16") return TypeReference.UInt16;
        if (typeStr == "uint32") return TypeReference.UInt32;
        if (typeStr == "uint64") return TypeReference.UInt64;
        if (typeStr == "float32") return TypeReference.Float32;
        if (typeStr == "float64") return TypeReference.Float64;
        if (typeStr == "char") return TypeReference.Char;
        if (typeStr == "string") return TypeReference.String;
        
        return TypeReference.FromName(typeStr);
    }

    private bool IsInstruction(string line)
    {
        var instructionKeywords = new[] 
        { 
            "ldarg", "ldloc", "ldfld", "stloc", "stfld", "ldc", "ldstr", "ldnull",
            "add", "sub", "mul", "div", "rem", "neg", "ceq", "cgt", "clt",
            "call", "callvirt", "newobj", "dup", "pop", "ret", "br", "beq",
            "if", "while", "for", "switch"
        };

        return instructionKeywords.Any(keyword => line.StartsWith(keyword));
    }
}
