using System.Text;
using ObjectIR.Core.IR;

namespace ObjectIR.CSharpBackend;

/// <summary>
/// Generates C# code from ObjectIR modules
/// </summary>
public class CSharpCodeGenerator
{
    private readonly StringBuilder _output = new();
  private int _indentLevel = 0;
    private const string IndentString = "    ";

    /// <summary>
    /// Generate C# code from an ObjectIR module
    /// </summary>
    public string Generate(Module module)
    {
    _output.Clear();
        _indentLevel = 0;

        WriteUsings();
     WriteBlankLine();
        WriteNamespaceStart(module.Name);
        
        foreach (var type in module.Types)
        {
            GenerateType(type);
        WriteBlankLine();
        }

        foreach (var function in module.Functions)
        {
            GenerateFunction(function);
            WriteBlankLine();
        }

        WriteNamespaceEnd();
        return _output.ToString();
    }

    private void GenerateType(TypeDefinition typeDef)
    {
      if (typeDef is ClassDefinition classDef)
        {
       GenerateClass(classDef);
     }
        else if (typeDef is InterfaceDefinition interfaceDef)
   {
    GenerateInterface(interfaceDef);
   }
        else if (typeDef is StructDefinition structDef)
    {
            GenerateStruct(structDef);
        }
        else if (typeDef is EnumDefinition enumDef)
        {
     GenerateEnum(enumDef);
   }
    }

    private void GenerateClass(ClassDefinition classDef)
    {
 var modifiers = new List<string>();
        modifiers.Add(GetAccessModifier(classDef.Access));
        if (classDef.IsAbstract) modifiers.Add("abstract");
 if (classDef.IsSealed) modifiers.Add("sealed");

  var genericPart = GenerateGenericParameters(classDef.GenericParameters);
        var baseTypes = new List<string>();

     if (classDef.BaseType != null)
{
      baseTypes.Add(FormatType(classDef.BaseType));
        }

        foreach (var iface in classDef.Interfaces)
  {
         baseTypes.Add(FormatType(iface));
        }

    var baseClause = baseTypes.Count > 0 ? $" : {string.Join(", ", baseTypes)}" : "";

 WriteLine($"{string.Join(" ", modifiers)} class {classDef.Name}{genericPart}{baseClause}");
       WriteLine("{");
        _indentLevel++;

 // Fields
  foreach (var field in classDef.Fields)
        {
      GenerateField(field);
        }

    if (classDef.Fields.Count > 0 && (classDef.Properties.Count > 0 || classDef.Methods.Count > 0))
        {
     WriteBlankLine();
  }

   // Properties
        foreach (var property in classDef.Properties)
        {
            GenerateProperty(property);
        }

    if (classDef.Properties.Count > 0 && classDef.Methods.Count > 0)
       {
         WriteBlankLine();
        }

       // Methods
   for (int i = 0; i < classDef.Methods.Count; i++)
      {
        GenerateMethod(classDef.Methods[i], classDef.Name);
            if (i < classDef.Methods.Count - 1)
        {
          WriteBlankLine();
          }
      }

  _indentLevel--;
        WriteLine("}");
    }

    private void GenerateInterface(InterfaceDefinition interfaceDef)
    {
        var modifiers = new List<string> { GetAccessModifier(interfaceDef.Access) };
        var genericPart = GenerateGenericParameters(interfaceDef.GenericParameters);

  var baseInterfaces = interfaceDef.BaseInterfaces.Count > 0
            ? $" : {string.Join(", ", interfaceDef.BaseInterfaces.Select(FormatType))}"
          : "";

        WriteLine($"{string.Join(" ", modifiers)} interface {interfaceDef.Name}{genericPart}{baseInterfaces}");
        WriteLine("{");
      _indentLevel++;

        // Properties
    foreach (var property in interfaceDef.Properties)
        {
          WriteLine($"{FormatType(property.Type)} {property.Name} {{ get; set; }}");
      }

  if (interfaceDef.Properties.Count > 0 && interfaceDef.Methods.Count > 0)
   {
          WriteBlankLine();
      }

    // Methods
        for (int i = 0; i < interfaceDef.Methods.Count; i++)
     {
            var method = interfaceDef.Methods[i];
       var genericPart2 = GenerateGenericParameters(method.GenericParameters);
     var paramsPart = string.Join(", ", method.Parameters.Select(p => $"{FormatType(p.Type)} {p.Name}"));
        WriteLine($"{FormatType(method.ReturnType)} {method.Name}{genericPart2}({paramsPart});");
     
            if (i < interfaceDef.Methods.Count - 1)
   {
       WriteBlankLine();
          }
        }

   _indentLevel--;
        WriteLine("}");
    }

    private void GenerateStruct(StructDefinition structDef)
    {
        var modifiers = new List<string> { GetAccessModifier(structDef.Access) };
        var genericPart = GenerateGenericParameters(structDef.GenericParameters);

       var interfaces = structDef.Interfaces.Count > 0
    ? $" : {string.Join(", ", structDef.Interfaces.Select(FormatType))}"
       : "";

  WriteLine($"{string.Join(" ", modifiers)} struct {structDef.Name}{genericPart}{interfaces}");
      WriteLine("{");
        _indentLevel++;

     // Fields
        foreach (var field in structDef.Fields)
     {
         GenerateField(field);
        }

    if (structDef.Fields.Count > 0 && structDef.Methods.Count > 0)
      {
         WriteBlankLine();
        }

  // Methods
      for (int i = 0; i < structDef.Methods.Count; i++)
   {
     GenerateMethod(structDef.Methods[i], structDef.Name);
if (i < structDef.Methods.Count - 1)
      {
  WriteBlankLine();
         }
    }

  _indentLevel--;
WriteLine("}");
    }

    private void GenerateEnum(EnumDefinition enumDef)
{
        var modifiers = new List<string> { GetAccessModifier(enumDef.Access) };
        var underlyingType = enumDef.UnderlyingType.Name != "int32" 
            ? $" : {FormatType(enumDef.UnderlyingType)}"
            : "";

        WriteLine($"{string.Join(" ", modifiers)} enum {enumDef.Name}{underlyingType}");
        WriteLine("{");
     _indentLevel++;

        for (int i = 0; i < enumDef.Members.Count; i++)
  {
            var member = enumDef.Members[i];
 var line = $"{member.Name}";
         
   // Only include the value if it's not the default sequential value
         if (i > 0)
            {
    var expectedValue = enumDef.Members[i - 1].Value + 1;
   if (member.Value != expectedValue)
         {
  line += $" = {member.Value}";
    }
  }
          else if (member.Value != 0)
{
        line += $" = {member.Value}";
    }

            if (i < enumDef.Members.Count - 1)
          {
        line += ",";
            }

      WriteLine(line);
        }

      _indentLevel--;
    WriteLine("}");
    }

    private void GenerateField(FieldDefinition field)
    {
        var modifiers = new List<string>();
        modifiers.Add(GetAccessModifier(field.Access));
        if (field.IsStatic) modifiers.Add("static");
        if (field.IsReadOnly) modifiers.Add("readonly");

        var initializer = field.InitialValue != null ? $" = {FormatValue(field.InitialValue)}" : "";
        WriteLine($"{string.Join(" ", modifiers)} {FormatType(field.Type)} {field.Name}{initializer};");
    }

    private void GenerateProperty(PropertyDefinition property)
    {
        var modifiers = GetAccessModifier(property.Access);
        var accessorBody = " { get; set; }";

 // If there are custom getters/setters, we'd need to generate them
 // For now, using auto-properties
  WriteLine($"{modifiers} {FormatType(property.Type)} {property.Name}{accessorBody}");
    }

    private void GenerateMethod(MethodDefinition method, string? declaringTypeName = null)
    {
        var modifiers = new List<string>();
        modifiers.Add(GetAccessModifier(method.Access));
      if (method.IsStatic) modifiers.Add("static");
        if (method.IsAbstract) modifiers.Add("abstract");
  if (method.IsVirtual) modifiers.Add("virtual");
        if (method.IsOverride) modifiers.Add("override");

     var genericPart = GenerateGenericParameters(method.GenericParameters);
      var returnType = method.IsConstructor ? "" : FormatType(method.ReturnType);
       
        // For constructors, use the declaring type name
        var methodName = method.IsConstructor && declaringTypeName != null ? declaringTypeName : method.Name;
        var paramsPart = string.Join(", ", method.Parameters.Select(p => FormatParameter(p)));

   if (method.IsAbstract || method.Instructions.Count == 0)
        {
        WriteLine($"{string.Join(" ", modifiers)} {returnType} {methodName}{genericPart}({paramsPart});".TrimStart());
       }
 else
      {
    WriteLine($"{string.Join(" ", modifiers)} {returnType} {methodName}{genericPart}({paramsPart})".TrimStart());
     WriteLine("{");
     _indentLevel++;

    // Declare locals
     foreach (var local in method.Locals)
          {
         WriteLine($"{FormatType(local.Type)} {local.Name};");
          }

      if (method.Locals.Count > 0)
     {
    WriteBlankLine();
           }

    // Generate instructions
           GenerateInstructions(method.Instructions);

    _indentLevel--;
     WriteLine("}");
        }
 }
    private void GenerateFunction(FunctionDefinition function)
    {
        var genericPart = GenerateGenericParameters(function.GenericParameters);
   var paramsPart = string.Join(", ", function.Parameters.Select(p => FormatParameter(p)));

    WriteLine($"public static {FormatType(function.ReturnType)} {function.Name}{genericPart}({paramsPart})");
        WriteLine("{");
        _indentLevel++;

        // Declare locals
  foreach (var local in function.Locals)
        {
 WriteLine($"{FormatType(local.Type)} {local.Name};");
   }

        if (function.Locals.Count > 0)
        {
    WriteBlankLine();
        }

        // Generate instructions
        GenerateInstructions(function.Instructions);

      _indentLevel--;
        WriteLine("}");
  }

    private void GenerateInstructions(InstructionList instructions)
    {
        var visitor = new CSharpInstructionVisitor(this);
        
foreach (var instruction in instructions)
     {
  instruction.Accept(visitor);
        }
}

    private string GenerateGenericParameters(List<GenericParameter> parameters)
    {
        if (parameters.Count == 0) return "";
        
     var genericArgs = parameters.Select(p => {
    if (p.Constraints.Count == 0) return p.Name;
       
       var constraints = p.Constraints.Select(c => {
       if (c is ClassConstraint) return "class";
          if (c is StructConstraint) return "struct";
         if (c is BaseTypeConstraint btc) return FormatType(btc.BaseType);
          return "";
            }).Where(s => !string.IsNullOrEmpty(s));
      
       return $"{p.Name} where {p.Name} : {string.Join(", ", constraints)}";
        });
        
        return $"<{string.Join(", ", genericArgs)}>";
 }

    private string FormatType(TypeReference type)
    {
        if (type.Name == "void") return "void";
        if (type.Name == "bool") return "bool";
        if (type.Name == "int8") return "sbyte";
        if (type.Name == "uint8") return "byte";
        if (type.Name == "int16") return "short";
        if (type.Name == "uint16") return "ushort";
        if (type.Name == "int32") return "int";
        if (type.Name == "uint32") return "uint";
      if (type.Name == "int64") return "long";
  if (type.Name == "uint64") return "ulong";
        if (type.Name == "float32") return "float";
        if (type.Name == "float64") return "double";
        if (type.Name == "char") return "char";
        if (type.Name == "string") return "string";

        var qualified = string.IsNullOrEmpty(type.Namespace) ? type.Name : $"{type.Namespace}.{type.Name}";
  
        if (type.GenericArguments.Count > 0)
        {
       qualified += $"<{string.Join(", ", type.GenericArguments.Select(FormatType))}>";
        }

        if (type.IsArray) qualified += "[]";
        if (type.IsPointer) qualified += "*";
        if (type.IsReference) qualified += "&";

        return qualified;
    }

    private string FormatParameter(Parameter parameter)
    {
     var prefix = "";
     if (parameter.IsOut) prefix = "out ";
        if (parameter.IsRef) prefix = "ref ";

        var defaultValue = parameter.DefaultValue != null ? $" = {FormatValue(parameter.DefaultValue)}" : "";
        return $"{prefix}{FormatType(parameter.Type)} {parameter.Name}{defaultValue}";
    }

    private string FormatValue(object? value)
    {
   if (value == null) return "null";
  if (value is bool b) return b ? "true" : "false";
        if (value is string s) return $"\"{EscapeString(s)}\"";
        if (value is char c) return $"'{c}'";
     if (value is float f) return $"{f}f";
        return value.ToString() ?? "null";
  }

    private string EscapeString(String str)
    {
   return str.Replace("\\", "\\\\")
       .Replace("\"", "\\\"")
         .Replace("\n", "\\n")
          .Replace("\r", "\\r")
 .Replace("\t", "\\t");
    }

    private string GetAccessModifier(AccessModifier modifier)
    {
        return modifier switch
        {
   AccessModifier.Public => "public",
            AccessModifier.Private => "private",
     AccessModifier.Protected => "protected",
            AccessModifier.Internal => "internal",
         _ => "private"
        };
    }

    private void WriteUsings()
    {
      WriteLine("using System;");
        WriteLine("using System.Collections.Generic;");
        WriteLine("using System.Linq;");
    }

    private void WriteNamespaceStart(string namespaceName)
    {
        WriteLine($"namespace {namespaceName};");
    }

    private void WriteNamespaceEnd()
    {
        // C# 10+ file-scoped namespaces don't need explicit ending
    }

    public void WriteLine(string text)
    {
        _output.Append(new string(' ', _indentLevel * IndentString.Length));
        _output.AppendLine(text);
    }

    private void WriteBlankLine()
    {
     _output.AppendLine();
    }

    public int CurrentIndentLevel => _indentLevel;
    public void IncreaseIndent() => _indentLevel++;
    public void DecreaseIndent() => _indentLevel--;
    public StringBuilder Output => _output;
}
