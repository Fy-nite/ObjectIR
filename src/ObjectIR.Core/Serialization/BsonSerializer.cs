namespace ObjectIR.Core.Serialization;

using MongoDB.Bson;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.Json.Nodes;

#pragma warning disable CS1591

/// <summary>
/// Provides BSON serialization and deserialization for IR modules
/// BSON (Binary JSON) offers smaller file sizes compared to JSON
/// </summary>
public sealed class BsonSerializer
{
    /// <summary>
    /// Converts ModuleData to a BSON document
    /// </summary>
    public static BsonDocument ModuleDataToBson(ModuleData moduleData)
    {
        var doc = new BsonDocument
        {
            { "name", moduleData.Name },
            { "version", moduleData.Version ?? "" },
            { "metadata", DictionaryToBson(moduleData.Metadata ?? new Dictionary<string, string>()) },
            { "types", new BsonArray(moduleData.Types.Select(TypeDataToBson)) },
            { "functions", new BsonArray(moduleData.Functions.Select(FunctionDataToBson)) }
        };
        return doc;
    }

    /// <summary>
    /// Converts a BSON document back to ModuleData
    /// </summary>
    public static ModuleData BsonToModuleData(BsonDocument doc)
    {
        return new ModuleData
        {
            Name = doc["name"].AsString,
            Version = (doc.Contains("version") ? doc["version"].AsString : null) ?? "",
            Metadata = BsonToDictionary(doc["metadata"].AsBsonDocument),
            Types = doc["types"].AsBsonArray.Select(BsonToTypeData).ToArray(),
            Functions = doc["functions"].AsBsonArray.Select(BsonToFunctionData).ToArray()
        };
    }

    private static BsonDocument TypeDataToBson(TypeData typeData)
    {
        return new BsonDocument
        {
            { "kind", typeData.Kind },
            { "name", typeData.Name },
            { "namespace", typeData.Namespace ?? "" },
            { "access", typeData.Access },
            { "isAbstract", typeData.IsAbstract },
            { "isSealed", typeData.IsSealed },
            { "baseType", typeData.BaseType ?? "" },
            { "interfaces", new BsonArray(typeData.Interfaces ?? Array.Empty<string>()) },
            { "baseInterfaces", new BsonArray(typeData.BaseInterfaces ?? Array.Empty<string>()) },
            { "underlyingType", typeData.UnderlyingType ?? "" },
            { "genericParameters", new BsonArray(typeData.GenericParameters ?? Array.Empty<string>()) },
            { "fields", new BsonArray(typeData.Fields?.Select(FieldDataToBson) ?? Array.Empty<BsonDocument>()) },
            { "properties", new BsonArray(typeData.Properties?.Select(PropertyDataToBson) ?? Array.Empty<BsonDocument>()) },
            { "methods", new BsonArray(typeData.Methods?.Select(MethodDataToBson) ?? Array.Empty<BsonDocument>()) }
        };
    }

    private static TypeData BsonToTypeData(BsonValue bsonValue)
    {
        var doc = bsonValue.AsBsonDocument;
        return new TypeData
        {
            Kind = doc["kind"].AsString,
            Name = doc["name"].AsString,
            Namespace = GetStringOrNull(doc, "namespace"),
            Access = doc["access"].AsString,
            IsAbstract = doc["isAbstract"].AsBoolean,
            IsSealed = doc["isSealed"].AsBoolean,
            BaseType = GetStringOrNull(doc, "baseType"),
            Interfaces = doc["interfaces"].AsBsonArray.Select(v => v.AsString).ToArray(),
            BaseInterfaces = doc["baseInterfaces"].AsBsonArray.Select(v => v.AsString).ToArray(),
            UnderlyingType = GetStringOrNull(doc, "underlyingType"),
            GenericParameters = doc["genericParameters"].AsBsonArray.Select(v => v.AsString).ToArray(),
            Fields = doc["fields"].AsBsonArray.Select(BsonToFieldData).ToArray(),
            Properties = doc["properties"].AsBsonArray.Select(BsonToPropertyData).ToArray(),
            Methods = doc["methods"].AsBsonArray.Select(BsonToMethodData).ToArray()
        };
    }

    private static string? GetStringOrNull(BsonDocument doc, string key)
    {
        if (!doc.Contains(key) || doc[key].IsBsonNull)
            return null;
        var str = doc[key].AsString;
        return string.IsNullOrEmpty(str) ? null : str;
    }

    private static BsonDocument FieldDataToBson(FieldData fieldData)
    {
        return new BsonDocument
        {
            { "name", fieldData.Name },
            { "type", fieldData.Type },
            { "access", fieldData.Access },
            { "isStatic", fieldData.IsStatic },
            { "isReadOnly", fieldData.IsReadOnly },
            { "initialValue", fieldData.InitialValue == null ? (BsonValue)BsonNull.Value : fieldData.InitialValue }
        };
    }

    private static FieldData BsonToFieldData(BsonValue bsonValue)
    {
        var doc = bsonValue.AsBsonDocument;
        return new FieldData
        {
            Name = doc["name"].AsString,
            Type = doc["type"].AsString,
            Access = doc["access"].AsString,
            IsStatic = doc["isStatic"].AsBoolean,
            IsReadOnly = doc["isReadOnly"].AsBoolean,
            InitialValue = doc["initialValue"].IsBsonNull ? null : doc["initialValue"].AsString
        };
    }

    private static BsonDocument PropertyDataToBson(PropertyData propertyData)
    {
        return new BsonDocument
        {
            { "name", propertyData.Name },
            { "type", propertyData.Type },
            { "access", propertyData.Access },
            { "hasGetter", propertyData.HasGetter },
            { "hasSetter", propertyData.HasSetter },
            { "getterAccess", propertyData.GetterAccess ?? "" },
            { "setterAccess", propertyData.SetterAccess ?? "" }
        };
    }

    private static PropertyData BsonToPropertyData(BsonValue bsonValue)
    {
        var doc = bsonValue.AsBsonDocument;
        return new PropertyData
        {
            Name = doc["name"].AsString,
            Type = doc["type"].AsString,
            Access = doc["access"].AsString,
            HasGetter = doc["hasGetter"].AsBoolean,
            HasSetter = doc["hasSetter"].AsBoolean,
            GetterAccess = GetStringOrNull(doc, "getterAccess"),
            SetterAccess = GetStringOrNull(doc, "setterAccess")
        };
    }

    private static BsonDocument MethodDataToBson(MethodData methodData)
    {
        return new BsonDocument
        {
            { "name", methodData.Name },
            { "returnType", methodData.ReturnType },
            { "access", methodData.Access },
            { "isStatic", methodData.IsStatic },
            { "isVirtual", methodData.IsVirtual },
            { "isOverride", methodData.IsOverride },
            { "isAbstract", methodData.IsAbstract },
            { "isConstructor", methodData.IsConstructor },
            { "parameters", new BsonArray(methodData.Parameters?.Select(ParameterDataToBson) ?? Array.Empty<BsonDocument>()) },
            { "localVariables", new BsonArray(methodData.LocalVariables?.Select(LocalVariableDataToBson) ?? Array.Empty<BsonDocument>()) },
            { "instructionCount", methodData.InstructionCount },
            { "instructions", JsonNodeToBson(methodData.Instructions) }
        };
    }

    private static MethodData BsonToMethodData(BsonValue bsonValue)
    {
        var doc = bsonValue.AsBsonDocument;
        return new MethodData
        {
            Name = doc["name"].AsString,
            ReturnType = doc["returnType"].AsString,
            Access = doc["access"].AsString,
            IsStatic = doc["isStatic"].AsBoolean,
            IsVirtual = doc["isVirtual"].AsBoolean,
            IsOverride = doc["isOverride"].AsBoolean,
            IsAbstract = doc["isAbstract"].AsBoolean,
            IsConstructor = doc["isConstructor"].AsBoolean,
            Parameters = doc["parameters"].AsBsonArray.Select(BsonToParameterData).ToArray(),
            LocalVariables = doc["localVariables"].AsBsonArray.Select(BsonToLocalVariableData).ToArray(),
            InstructionCount = doc["instructionCount"].AsInt32,
            Instructions = BsonToJsonNode(doc["instructions"])
        };
    }

    private static BsonDocument ParameterDataToBson(ParameterData paramData)
    {
        return new BsonDocument
        {
            { "name", paramData.Name },
            { "type", paramData.Type }
        };
    }

    private static ParameterData BsonToParameterData(BsonValue bsonValue)
    {
        var doc = bsonValue.AsBsonDocument;
        return new ParameterData
        {
            Name = doc["name"].AsString,
            Type = doc["type"].AsString
        };
    }

    private static BsonDocument LocalVariableDataToBson(LocalVariableData localVarData)
    {
        return new BsonDocument
        {
            { "name", localVarData.Name },
            { "type", localVarData.Type }
        };
    }

    private static LocalVariableData BsonToLocalVariableData(BsonValue bsonValue)
    {
        var doc = bsonValue.AsBsonDocument;
        return new LocalVariableData
        {
            Name = doc["name"].AsString,
            Type = doc["type"].AsString
        };
    }

    private static BsonDocument FunctionDataToBson(FunctionData funcData)
    {
        return new BsonDocument
        {
            { "name", funcData.Name },
            { "returnType", funcData.ReturnType },
            { "parameters", new BsonArray(funcData.Parameters?.Select(ParameterDataToBson) ?? Array.Empty<BsonDocument>()) },
            { "localVariables", new BsonArray(funcData.LocalVariables?.Select(LocalVariableDataToBson) ?? Array.Empty<BsonDocument>()) },
            { "instructionCount", funcData.InstructionCount },
            { "instructions", JsonNodeToBson(funcData.Instructions) }
        };
    }

    private static FunctionData BsonToFunctionData(BsonValue bsonValue)
    {
        var doc = bsonValue.AsBsonDocument;
        return new FunctionData
        {
            Name = doc["name"].AsString,
            ReturnType = doc["returnType"].AsString,
            Parameters = doc["parameters"].AsBsonArray.Select(BsonToParameterData).ToArray(),
            LocalVariables = doc["localVariables"].AsBsonArray.Select(BsonToLocalVariableData).ToArray(),
            InstructionCount = doc["instructionCount"].AsInt32,
            Instructions = BsonToJsonNode(doc["instructions"])
        };
    }

    /// <summary>
    /// Converts a JsonNode to BSON - used for complex instruction data
    /// </summary>
    private static BsonValue JsonNodeToBson(JsonNode? node)
    {
        if (node == null)
            return BsonNull.Value;

        if (node is JsonValue value)
        {
            if (value.TryGetValue<string>(out var strVal))
                return new BsonString(strVal);
            if (value.TryGetValue<int>(out var intVal))
                return new BsonInt32(intVal);
            if (value.TryGetValue<long>(out var longVal))
                return new BsonInt64(longVal);
            if (value.TryGetValue<double>(out var dblVal))
                return new BsonDouble(dblVal);
            if (value.TryGetValue<bool>(out var boolVal))
                return new BsonBoolean(boolVal);
            return BsonNull.Value;
        }

        if (node is JsonArray array)
        {
            var bsonArray = new BsonArray();
            foreach (var item in array)
            {
                bsonArray.Add(JsonNodeToBson(item));
            }
            return bsonArray;
        }

        if (node is JsonObject obj)
        {
            var bsonDoc = new BsonDocument();
            foreach (var property in obj)
            {
                bsonDoc[property.Key] = JsonNodeToBson(property.Value);
            }
            return bsonDoc;
        }

        return BsonNull.Value;
    }

    /// <summary>
    /// Converts BSON value back to JsonNode
    /// </summary>
    private static JsonNode? BsonToJsonNode(BsonValue bsonValue)
    {
        if (bsonValue.IsBsonNull)
            return null;

        return bsonValue.BsonType switch
        {
            BsonType.String => JsonValue.Create(bsonValue.AsString),
            BsonType.Int32 => JsonValue.Create(bsonValue.AsInt32),
            BsonType.Int64 => JsonValue.Create(bsonValue.AsInt64),
            BsonType.Double => JsonValue.Create(bsonValue.AsDouble),
            BsonType.Boolean => JsonValue.Create(bsonValue.AsBoolean),
            BsonType.Array => ConvertBsonArrayToJsonArray(bsonValue.AsBsonArray),
            BsonType.Document => ConvertBsonDocumentToJsonObject(bsonValue.AsBsonDocument),
            _ => null
        };
    }

    private static JsonArray ConvertBsonArrayToJsonArray(BsonArray bsonArray)
    {
        var array = new JsonArray();
        foreach (var item in bsonArray)
        {
            var jsonNode = BsonToJsonNode(item);
            if (jsonNode != null)
                array.Add(jsonNode);
        }
        return array;
    }

    private static JsonObject ConvertBsonDocumentToJsonObject(BsonDocument bsonDoc)
    {
        var obj = new JsonObject();
        foreach (var element in bsonDoc.Elements)
        {
            var jsonNode = BsonToJsonNode(element.Value);
            if (jsonNode != null)
                obj[element.Name] = jsonNode;
        }
        return obj;
    }

    private static BsonDocument DictionaryToBson(Dictionary<string, string> dict)
    {
        var doc = new BsonDocument();
        foreach (var kvp in dict)
        {
            doc[kvp.Key] = kvp.Value;
        }
        return doc;
    }

    private static Dictionary<string, string> BsonToDictionary(BsonDocument doc)
    {
        var dict = new Dictionary<string, string>();
        foreach (var element in doc.Elements)
        {
            if (element.Value.IsString)
                dict[element.Name] = element.Value.AsString;
        }
        return dict;
    }
}
