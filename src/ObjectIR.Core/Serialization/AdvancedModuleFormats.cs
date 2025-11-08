namespace ObjectIR.Core.Serialization;

using System.Text;

/// <summary>
/// Advanced serialization formats for module analysis and reporting
/// </summary>
public static class AdvancedModuleFormats
{
  /// <summary>
  /// Dumps module as CSV format for spreadsheet analysis
  /// </summary>
  public static string DumpToCSV(ModuleData module)
  {
    var sb = new StringBuilder();

    // Header
    sb.AppendLine("Type,Kind,Namespace,Access,IsAbstract,IsSealed,FieldCount,MethodCount,PropertyCount");

    // Types
    foreach (var type in module.Types)
    {
      sb.AppendLine($"\"{type.Name}\",\"{type.Kind}\",\"{type.Namespace ?? ""}\",\"{type.Access}\"," +
  $"{type.IsAbstract},{type.IsSealed},{type.Fields.Length},{type.Methods.Length},{type.Properties.Length}");
    }

    return sb.ToString();
  }

  /// <summary>
  /// Dumps module as markdown for documentation
  /// </summary>
  public static string DumpToMarkdown(ModuleData module)
  {
    var sb = new StringBuilder();

    sb.AppendLine($"# {module.Name}");
    sb.AppendLine($"Version: {module.Version}");
    sb.AppendLine();

    if (module.Metadata.Count > 0)
    {
      sb.AppendLine("## Metadata");
      foreach (var (key, value) in module.Metadata)
      {
        sb.AppendLine($"- **{key}**: {value}");
      }
      sb.AppendLine();
    }

    if (module.Types.Length > 0)
    {
      sb.AppendLine("## Types");
      foreach (var type in module.Types)
      {
        var modifiers = new List<string>();
        if (type.IsAbstract) modifiers.Add("abstract");
        if (type.IsSealed) modifiers.Add("sealed");
        var modStr = modifiers.Count > 0 ? string.Join(" ", modifiers) + " " : "";

        sb.AppendLine($"### {modStr}{type.Kind}: {type.Name}");

        if (!string.IsNullOrEmpty(type.Namespace))
          sb.AppendLine($"**Namespace**: {type.Namespace}");

        sb.AppendLine($"**Access**: {type.Access}");

        if (!string.IsNullOrEmpty(type.BaseType))
          sb.AppendLine($"**Base Type**: {type.BaseType}");

        if (type.Interfaces.Length > 0)
          sb.AppendLine($"**Implements**: {string.Join(", ", type.Interfaces)}");

        if (type.GenericParameters.Length > 0)
          sb.AppendLine($"**Generic Parameters**: {string.Join(", ", type.GenericParameters)}");

        sb.AppendLine();

        if (type.Fields.Length > 0)
        {
          sb.AppendLine("#### Fields");
          sb.AppendLine("| Name | Type | Access | Static | ReadOnly |");
          sb.AppendLine("|------|------|--------|--------|----------|");
          foreach (var field in type.Fields)
          {
            sb.AppendLine($"| {field.Name} | {field.Type} | {field.Access} | {field.IsStatic} | {field.IsReadOnly} |");
          }
          sb.AppendLine();
        }

        if (type.Methods.Length > 0)
        {
          sb.AppendLine("#### Methods");
          sb.AppendLine("| Name | Return Type | Access | Static | Virtual | Abstract |");
          sb.AppendLine("|------|-------------|--------|--------|---------|----------|");
          foreach (var method in type.Methods)
          {
            var name = method.IsConstructor ? ".ctor" : method.Name;
            sb.AppendLine($"| {name} | {method.ReturnType} | {method.Access} | {method.IsStatic} | {method.IsVirtual} | {method.IsAbstract} |");
          }
          sb.AppendLine();
        }

        if (type.Properties.Length > 0)
        {
          sb.AppendLine("#### Properties");
          sb.AppendLine("| Name | Type | Access | Getter | Setter |");
          sb.AppendLine("|------|------|--------|--------|--------|");
          foreach (var prop in type.Properties)
          {
            sb.AppendLine($"| {prop.Name} | {prop.Type} | {prop.Access} | {prop.HasGetter} | {prop.HasSetter} |");
          }
          sb.AppendLine();
        }
      }
    }

    if (module.Functions.Length > 0)
    {
      sb.AppendLine("## Functions");
      sb.AppendLine("| Name | Return Type | Parameter Count | Instruction Count |");
      sb.AppendLine("|------|-------------|-----------------|-------------------|");
      foreach (var func in module.Functions)
      {
        sb.AppendLine($"| {func.Name} | {func.ReturnType} | {func.Parameters.Length} | {func.InstructionCount} |");
      }
      sb.AppendLine();
    }

    return sb.ToString();
  }

  /// <summary>
  /// Dumps module as YAML format
  /// </summary>
  public static string DumpToYAML(ModuleData module)
  {
    var sb = new StringBuilder();

    sb.AppendLine($"module: {module.Name}");
    sb.AppendLine($"version: {module.Version}");

    if (module.Metadata.Count > 0)
    {
      sb.AppendLine("metadata:");
      foreach (var (key, value) in module.Metadata)
      {
        sb.AppendLine($"  {key}: {value}");
      }
    }

    if (module.Types.Length > 0)
    {
      sb.AppendLine("types:");
      foreach (var type in module.Types)
      {
        sb.AppendLine($"  - name: {type.Name}");
        sb.AppendLine($"    kind: {type.Kind}");
        sb.AppendLine($"    namespace: {type.Namespace ?? "null"}");
        sb.AppendLine($"    access: {type.Access}");
        sb.AppendLine($"    abstract: {type.IsAbstract}");
        sb.AppendLine($"sealed: {type.IsSealed}");

        if (!string.IsNullOrEmpty(type.BaseType))
          sb.AppendLine($"    baseType: {type.BaseType}");

        if (type.Interfaces.Length > 0)
        {
          sb.AppendLine($"    interfaces:");
          foreach (var iface in type.Interfaces)
          {
            sb.AppendLine($"      - {iface}");
          }
        }

        if (type.Fields.Length > 0)
        {
          sb.AppendLine($"    fields:");
          foreach (var field in type.Fields)
          {
            sb.AppendLine($"      - name: {field.Name}");
            sb.AppendLine($"   type: {field.Type}");
          }
        }

        if (type.Methods.Length > 0)
        {
          sb.AppendLine($"    methods:");
          foreach (var method in type.Methods)
          {
            var name = method.IsConstructor ? ".ctor" : method.Name;
            sb.AppendLine($"      - name: {name}");
            sb.AppendLine($"        returnType: {method.ReturnType}");
            sb.AppendLine($"        parameters: {method.Parameters.Length}");
          }
        }
      }
    }

    return sb.ToString();
  }

  /// <summary>
  /// Generates a summary report of the module
  /// </summary>
  public static string GenerateSummaryReport(ModuleData module)
  {
    var sb = new StringBuilder();

    sb.AppendLine("??????????????????????????????????????????????????????????????????");
    sb.AppendLine("?  MODULE SUMMARY REPORT             ?");
    sb.AppendLine("??????????????????????????????????????????????????????????????????");
    sb.AppendLine();

    sb.AppendLine($"Module Name:        {module.Name}");
    sb.AppendLine($"Version:       {module.Version}");
    sb.AppendLine();

    sb.AppendLine("? Statistics ?????????????????????????????????????????????????????");
    sb.AppendLine($"Total Types:        {module.Types.Length}");
    sb.AppendLine($"Total Functions:    {module.Functions.Length}");
    sb.AppendLine();

    var classes = module.Types.Count(t => t.Kind == "Class");
    var interfaces = module.Types.Count(t => t.Kind == "Interface");
    var structs = module.Types.Count(t => t.Kind == "Struct");

    sb.AppendLine("Type Breakdown:");
    sb.AppendLine($"  � Classes:        {classes}");
    sb.AppendLine($"  � Interfaces:     {interfaces}");
    sb.AppendLine($"  � Structs:        {structs}");
    sb.AppendLine();

    var totalFields = module.Types.Sum(t => t.Fields.Length);
    var totalMethods = module.Types.Sum(t => t.Methods.Length);
    var totalProperties = module.Types.Sum(t => t.Properties.Length);
    var totalInstructions = module.Types.Sum(t => t.Methods.Sum(m => m.InstructionCount)) +
        module.Functions.Sum(f => f.InstructionCount);

    sb.AppendLine("Members:");
    sb.AppendLine($"  � Total Fields:   {totalFields}");
    sb.AppendLine($"  � Total Methods:  {totalMethods}");
    sb.AppendLine($"  � Total Properties: {totalProperties}");
    sb.AppendLine();

    sb.AppendLine("Code Metrics:");
    sb.AppendLine($"  � Total Instructions: {totalInstructions}");

    if (totalMethods > 0)
    {
      var avgInstructionsPerMethod = (double)module.Types.Sum(t => t.Methods.Sum(m => m.InstructionCount)) / totalMethods;
      sb.AppendLine($"  � Avg Instructions/Method: {avgInstructionsPerMethod:F2}");
    }

    sb.AppendLine();
    sb.AppendLine("? Top Methods by Instruction Count ???????????????????????????????");

    var topMethods = module.Types
      .SelectMany(t => t.Methods.Select(m => (Type: t, Method: m)))
            .OrderByDescending(x => x.Method.InstructionCount)
              .Take(5)
              .ToList();

    if (topMethods.Count == 0)
    {
      sb.AppendLine("(No methods found)");
    }
    else
    {
      foreach (var (type, method) in topMethods)
      {
        sb.AppendLine($"  {type.Name}.{method.Name}: {method.InstructionCount} instructions");
      }
    }

    sb.AppendLine();
    sb.AppendLine("??????????????????????????????????????????????????????????????????");

    return sb.ToString();
  }
}
