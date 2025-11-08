namespace ObjectIR.Core.Serialization;

using ObjectIR.Core.IR;

/// <summary>
/// Extension methods for serializing IR modules
/// </summary>
public static class ModuleSerializationExtensions
{
    /// <summary>
    /// Creates a serializer for this module
    /// </summary>
    public static ModuleSerializer Serialize(this Module module) 
        => new ModuleSerializer(module);

    /// <summary>
    /// Dumps the module as a structured object representation
    /// </summary>
    public static ModuleData Dump(this Module module)
        => module.Serialize().Dump();

    /// <summary>
    /// Dumps the module as JSON string
    /// </summary>
    public static string DumpJson(this Module module, bool indented = true)
        => module.Serialize().DumpToJson(indented);

    /// <summary>
    /// Dumps the module as human-readable text
    /// </summary>
    public static string DumpText(this Module module)
        => module.Serialize().DumpToText();

    /// <summary>
    /// Dumps the module as BSON (binary JSON) for smaller file sizes
    /// </summary>
    public static byte[] DumpBson(this Module module)
        => module.Serialize().DumpToBson();

    /// <summary>
    /// Dumps the module as FOB (Finite Open Bytecode) binary format
    /// </summary>
    public static byte[] DumpFob(this Module module)
        => module.Serialize().DumpToFOB();

    /// <summary>
 /// Dumps the module to an array of type descriptions
    /// </summary>
    public static TypeData[] DumpTypes(this Module module)
        => module.Serialize().DumpTypes();

 /// <summary>
    /// Dumps the module to an array of function descriptions
    /// </summary>
    public static FunctionData[] DumpFunctions(this Module module)
      => module.Serialize().DumpFunctions();

    // ============================================================================
    // Advanced Format Extensions
    // ============================================================================

    /// <summary>
    /// Dumps the module as CSV format for spreadsheet analysis
    /// </summary>
    public static string DumpCsv(this Module module)
        => AdvancedModuleFormats.DumpToCSV(module.Dump());

    /// <summary>
    /// Dumps the module as Markdown for documentation
  /// </summary>
    public static string DumpMarkdown(this Module module)
        => AdvancedModuleFormats.DumpToMarkdown(module.Dump());

    /// <summary>
    /// Dumps the module as YAML format
    /// </summary>
    public static string DumpYaml(this Module module)
        => AdvancedModuleFormats.DumpToYAML(module.Dump());

    /// <summary>
    /// Generates a summary report of the module
    /// </summary>
    public static string GenerateSummaryReport(this Module module)
        => AdvancedModuleFormats.GenerateSummaryReport(module.Dump());

    /// <summary>
    /// Loads a module from JSON string
    /// </summary>
    public static Module LoadFromJson(this Module module, string json)
        => ModuleSerializer.LoadFromJson(json);

    /// <summary>
    /// Loads a module from BSON (binary JSON) data
    /// </summary>
    public static Module LoadFromBson(this Module module, byte[] bsonData)
        => ModuleSerializer.LoadFromBson(bsonData);
}
