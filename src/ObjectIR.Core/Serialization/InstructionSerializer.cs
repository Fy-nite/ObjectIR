namespace ObjectIR.Core.Serialization;

using ObjectIR.Core.IR;
using System.Collections.Generic;
using System.Linq;
using System.Text.Json;
using System.Text.Json.Serialization;

/// <summary>
/// Serializes and deserializes IR instructions to/from JSON
/// </summary>
public sealed class InstructionSerializer
{
    /// <summary>
    /// Serializes a list of instructions to JSON array
    /// </summary>
    public static JsonElement SerializeInstructions(InstructionList instructions)
    {
        var jsonDocs = new List<JsonDocument>();
        foreach (var instruction in instructions)
        {
            var doc = SerializeInstruction(instruction);
            jsonDocs.Add(doc);
        }

        var options = new JsonSerializerOptions { WriteIndented = false };
        var array = JsonSerializer.Serialize(
            jsonDocs.Select(d => d.RootElement).ToList(),
            options
        );

        return JsonDocument.Parse(array).RootElement;
    }

    /// <summary>
    /// Serializes a single instruction to JSON
    /// </summary>
    public static JsonDocument SerializeInstruction(Instruction instruction)
    {
        var data = instruction switch
        {
            LoadArgInstruction lai => new InstructionData
            {
                OpCode = "ldarg",
                Operand = new { argumentName = lai.ArgumentName }
            },
            LoadLocalInstruction lli => new InstructionData
            {
                OpCode = "ldloc",
                Operand = new { localName = lli.LocalName }
            },
            LoadFieldInstruction lfi => new InstructionData
            {
                OpCode = "ldfld",
                Operand = new
                {
                    field = new
                    {
                        declaringType = lfi.Field.DeclaringType.GetQualifiedName(),
                        name = lfi.Field.Name
                    }
                }
            },
            LoadStaticFieldInstruction lsfi => new InstructionData
            {
                OpCode = "ldsfld",
                Operand = new
                {
                    field = new
                    {
                        declaringType = lsfi.Field.DeclaringType.GetQualifiedName(),
                        name = lsfi.Field.Name
                    }
                }
            },
            LoadConstantInstruction lci => new InstructionData
            {
                OpCode = "ldc",
                Operand = new
                {
                    value = lci.Value?.ToString() ?? "null",
                    type = lci.Type.GetQualifiedName()
                }
            },
            LoadNullInstruction => new InstructionData
            {
                OpCode = "ldnull",
                Operand = null
            },
            StoreArgInstruction sai => new InstructionData
            {
                OpCode = "starg",
                Operand = new { argumentName = sai.ArgumentName }
            },
            StoreLocalInstruction sli => new InstructionData
            {
                OpCode = "stloc",
                Operand = new { localName = sli.LocalName }
            },
            StoreFieldInstruction sfi => new InstructionData
            {
                OpCode = "stfld",
                Operand = new
                {
                    field = new
                    {
                        declaringType = sfi.Field.DeclaringType.GetQualifiedName(),
                        name = sfi.Field.Name
                    }
                }
            },
            StoreStaticFieldInstruction ssfi => new InstructionData
            {
                OpCode = "stsfld",
                Operand = new
                {
                    field = new
                    {
                        declaringType = ssfi.Field.DeclaringType.GetQualifiedName(),
                        name = ssfi.Field.Name
                    }
                }
            },
            ArithmeticInstruction ai => new InstructionData
            {
                OpCode = MapArithmeticOp(ai.Operation),
                Operand = null
            },
            ComparisonInstruction ci => new InstructionData
            {
                OpCode = MapComparisonOp(ci.Operation),
                Operand = null
            },
            CallInstruction cai => new InstructionData
            {
                OpCode = "call",
                Operand = new
                {
                    method = new
                    {
                        declaringType = cai.Method.DeclaringType.GetQualifiedName(),
                        name = cai.Method.Name,
                        returnType = cai.Method.ReturnType.GetQualifiedName(),
                        parameterTypes = cai.Method.ParameterTypes.Select(p => p.GetQualifiedName()).ToArray()
                    }
                }
            },
            CallVirtualInstruction cvi => new InstructionData
            {
                OpCode = "callvirt",
                Operand = new
                {
                    method = new
                    {
                        declaringType = cvi.Method.DeclaringType.GetQualifiedName(),
                        name = cvi.Method.Name,
                        returnType = cvi.Method.ReturnType.GetQualifiedName(),
                        parameterTypes = cvi.Method.ParameterTypes.Select(p => p.GetQualifiedName()).ToArray()
                    }
                }
            },
            NewObjectInstruction noi => new InstructionData
            {
                OpCode = "newobj",
                Operand = new { type = noi.Type.GetQualifiedName() }
            },
            NewArrayInstruction nai => new InstructionData
            {
                OpCode = "newarr",
                Operand = new { elementType = nai.ElementType.GetQualifiedName() }
            },
            CastInstruction csti => new InstructionData
            {
                OpCode = "castclass",
                Operand = new { targetType = csti.TargetType.GetQualifiedName() }
            },
            IsInstanceInstruction isii => new InstructionData
            {
                OpCode = "isinst",
                Operand = new { targetType = isii.TargetType.GetQualifiedName() }
            },
            ConversionInstruction convi => new InstructionData
            {
                OpCode = "conv",
                Operand = new { targetType = convi.TargetType.GetQualifiedName() }
            },
            ReturnInstruction ri => new InstructionData
            {
                OpCode = "ret",
                Operand = null
            },
            DupInstruction => new InstructionData
            {
                OpCode = "dup",
                Operand = null
            },
            PopInstruction => new InstructionData
            {
                OpCode = "pop",
                Operand = null
            },
            BreakInstruction => new InstructionData
            {
                OpCode = "break",
                Operand = null
            },
            ContinueInstruction => new InstructionData
            {
                OpCode = "continue",
                Operand = null
            },
            _ => throw new NotSupportedException($"Instruction type {instruction.GetType().Name} not supported for serialization")
        };

        var options = new JsonSerializerOptions
        {
            WriteIndented = false,
            DefaultIgnoreCondition = JsonIgnoreCondition.WhenWritingNull
        };

        var json = JsonSerializer.Serialize(data, options);
        return JsonDocument.Parse(json);
    }

    /// <summary>
    /// Deserializes a JSON array to instruction list
    /// </summary>
    public static InstructionList DeserializeInstructions(JsonElement jsonArray)
    {
        var instructions = new InstructionList();

        if (jsonArray.ValueKind != JsonValueKind.Array)
        {
            throw new ArgumentException("Expected JSON array for instructions", nameof(jsonArray));
        }

        foreach (var element in jsonArray.EnumerateArray())
        {
            var instruction = DeserializeInstruction(element);
            instructions.Add(instruction);
        }

        return instructions;
    }

    /// <summary>
    /// Deserializes a single instruction from JSON
    /// </summary>
    public static Instruction DeserializeInstruction(JsonElement element)
    {
        var opCode = element.GetProperty("opCode").GetString() ?? throw new InvalidOperationException("opCode missing");
        var operand = element.TryGetProperty("operand", out var op) ? op : default(JsonElement);

        return opCode switch
        {
            "ldarg" => new LoadArgInstruction(GetString(operand, "argumentName")),
            "ldloc" => new LoadLocalInstruction(GetString(operand, "localName")),
            "ldfld" => new LoadFieldInstruction(DeserializeFieldReference(operand)),
            "ldsfld" => new LoadStaticFieldInstruction(DeserializeFieldReference(operand)),
            "ldc" => new LoadConstantInstruction(
                ParseConstantValue(GetString(operand, "value"), GetString(operand, "type")),
                TypeReference.FromName(GetString(operand, "type"))
            ),
            "ldnull" => new LoadNullInstruction(),
            "starg" => new StoreArgInstruction(GetString(operand, "argumentName")),
            "stloc" => new StoreLocalInstruction(GetString(operand, "localName")),
            "stfld" => new StoreFieldInstruction(DeserializeFieldReference(operand)),
            "stsfld" => new StoreStaticFieldInstruction(DeserializeFieldReference(operand)),
            "add" => new ArithmeticInstruction(ArithmeticOp.Add),
            "sub" => new ArithmeticInstruction(ArithmeticOp.Sub),
            "mul" => new ArithmeticInstruction(ArithmeticOp.Mul),
            "div" => new ArithmeticInstruction(ArithmeticOp.Div),
            "rem" => new ArithmeticInstruction(ArithmeticOp.Rem),
            "ceq" => new ComparisonInstruction(ComparisonOp.Equal),
            "cgt" => new ComparisonInstruction(ComparisonOp.Greater),
            "clt" => new ComparisonInstruction(ComparisonOp.Less),
            "call" => new CallInstruction(DeserializeMethodReference(operand)),
            "callvirt" => new CallVirtualInstruction(DeserializeMethodReference(operand)),
            "newobj" => new NewObjectInstruction(TypeReference.FromName(GetString(operand, "type"))),
            "newarr" => new NewArrayInstruction(TypeReference.FromName(GetString(operand, "elementType"))),
            "castclass" => new CastInstruction(TypeReference.FromName(GetString(operand, "targetType"))),
            "isinst" => new IsInstanceInstruction(TypeReference.FromName(GetString(operand, "targetType"))),
            "conv" => new ConversionInstruction(TypeReference.FromName(GetString(operand, "targetType"))),
            "ret" => new ReturnInstruction(null),
            "dup" => new DupInstruction(),
            "pop" => new PopInstruction(),
            "break" => new BreakInstruction(),
            "continue" => new ContinueInstruction(),
            _ => throw new NotSupportedException($"OpCode '{opCode}' not supported for deserialization")
        };
    }

    // ============================================================================
    // Private Methods
    // ============================================================================

    private static string MapArithmeticOp(ArithmeticOp op) => op switch
    {
        ArithmeticOp.Add => "add",
        ArithmeticOp.Sub => "sub",
        ArithmeticOp.Mul => "mul",
        ArithmeticOp.Div => "div",
        ArithmeticOp.Rem => "rem",
        _ => throw new ArgumentException($"Unknown arithmetic op: {op}")
    };

    private static string MapComparisonOp(ComparisonOp op) => op switch
    {
        ComparisonOp.Equal => "ceq",
        ComparisonOp.Greater => "cgt",
        ComparisonOp.Less => "clt",
        _ => throw new ArgumentException($"Unknown comparison op: {op}")
    };

    private static FieldReference DeserializeFieldReference(JsonElement operand)
    {
        var fieldElement = operand.GetProperty("field");
        var declaringType = TypeReference.FromName(GetString(fieldElement, "declaringType"));
        var name = GetString(fieldElement, "name");
        var fieldType = TypeReference.FromName(GetString(fieldElement, "type") ?? "object");
        
        return new FieldReference(declaringType, name, fieldType);
    }

    private static MethodReference DeserializeMethodReference(JsonElement operand)
    {
        var methodElement = operand.GetProperty("method");
        var declaringType = TypeReference.FromName(GetString(methodElement, "declaringType"));
        var name = GetString(methodElement, "name");
        var returnType = TypeReference.FromName(GetString(methodElement, "returnType"));
        
        var paramTypesElement = methodElement.GetProperty("parameterTypes");
        var paramTypes = new List<TypeReference>();
        if (paramTypesElement.ValueKind == JsonValueKind.Array)
        {
            foreach (var paramType in paramTypesElement.EnumerateArray())
            {
                paramTypes.Add(TypeReference.FromName(paramType.GetString() ?? "object"));
            }
        }

        return new MethodReference(declaringType, name, returnType, paramTypes);
    }

    private static object ParseConstantValue(string value, string type)
    {
        if (value == "null") return null!;

        return type switch
        {
            "System.Int32" => int.Parse(value),
            "System.Int64" => long.Parse(value),
            "System.Single" => float.Parse(value),
            "System.Double" => double.Parse(value),
            "System.String" => value,
            "System.Boolean" => bool.Parse(value),
            _ => value
        };
    }

    private static string GetString(JsonElement element, string propertyName)
    {
        if (!element.TryGetProperty(propertyName, out var prop))
            throw new InvalidOperationException($"Property '{propertyName}' not found");
        return prop.GetString() ?? throw new InvalidOperationException($"Property '{propertyName}' is not a string");
    }

    // ============================================================================
    // Data Class
    // ============================================================================

    private sealed class InstructionData
    {
        [JsonPropertyName("opCode")]
        public string OpCode { get; set; } = string.Empty;

        [JsonPropertyName("operand")]
        [JsonIgnore(Condition = JsonIgnoreCondition.WhenWritingNull)]
        public object? Operand { get; set; }
    }
}
