namespace ObjectIR.Core.Serialization;

using System.Text.Json;
using System.Text.Json.Nodes;
using Json.Schema;

/// <summary>
/// Validates ObjectIR JSON modules against the official schema
/// </summary>
public static class JsonValidator
{
    private static readonly JsonSchema _schema;

    static JsonValidator()
    {
        // Load the embedded schema
        var schemaJson = GetEmbeddedSchema();
        _schema = JsonSchema.FromText(schemaJson);
    }

    /// <summary>
    /// Validates a JSON string against the ObjectIR module schema
    /// </summary>
    /// <param name="json">The JSON string to validate</param>
    /// <returns>Validation result with success status and error details</returns>
    public static ValidationResult Validate(string json)
    {
        try
        {
            var jsonDocument = JsonDocument.Parse(json);
            return Validate(jsonDocument);
        }
        catch (JsonException ex)
        {
            return new ValidationResult(false, $"Invalid JSON: {ex.Message}");
        }
    }

    /// <summary>
    /// Validates a JsonDocument against the ObjectIR module schema
    /// </summary>
    /// <param name="jsonDocument">The JSON document to validate</param>
    /// <returns>Validation result with success status and error details</returns>
    public static ValidationResult Validate(JsonDocument jsonDocument)
    {
        var options = new EvaluationOptions
        {
            OutputFormat = OutputFormat.List
        };

        var jsonNode = JsonNode.Parse(jsonDocument.RootElement.GetRawText());
        var result = _schema.Evaluate(jsonNode, options);

        if (result.IsValid)
        {
            return new ValidationResult(true, null);
        }

        var errors = new List<string>();
        CollectValidationErrors(result, errors);
        return new ValidationResult(false, string.Join("; ", errors));
    }

    /// <summary>
    /// Validates a JSON string and throws an exception if invalid
    /// </summary>
    /// <param name="json">The JSON string to validate</param>
    /// <exception cref="JsonValidationException">Thrown when validation fails</exception>
    public static void ValidateOrThrow(string json)
    {
        var result = Validate(json);
        if (!result.IsValid)
        {
            throw new JsonValidationException(result.ErrorMessage!);
        }
    }

    /// <summary>
    /// Validates a JsonDocument and throws an exception if invalid
    /// </summary>
    /// <param name="jsonDocument">The JSON document to validate</param>
    /// <exception cref="JsonValidationException">Thrown when validation fails</exception>
    public static void ValidateOrThrow(JsonDocument jsonDocument)
    {
        var result = Validate(jsonDocument);
        if (!result.IsValid)
        {
            throw new JsonValidationException(result.ErrorMessage!);
        }
    }

    private static void CollectValidationErrors(EvaluationResults results, List<string> errors)
    {
        // Add top-level errors
        if (results.Errors != null)
        {
            foreach (var error in results.Errors)
            {
                errors.Add($"{error.Key}: {error.Value}");
            }
        }

        // Recursively collect errors from details
        if (results.Details != null)
        {
            foreach (var detail in results.Details)
            {
                if (!detail.IsValid)
                {
                    CollectValidationErrors(detail, errors);
                }
            }
        }
    }

    private static string GetEmbeddedSchema()
    {
        // Embedded schema content - matches the actual JSON format produced by compilers with camelCase naming
        return @"{
  ""$schema"": ""https://json-schema.org/draft/2020-12/schema"",
  ""$id"": ""https://objectir.org/schema/module.schema.json"",
  ""title"": ""ObjectIR Module JSON Schema"",
  ""description"": ""Schema for ObjectIR module JSON format used by compilers and runtime"",
  ""type"": ""object"",
  ""properties"": {
    ""name"": {
      ""type"": ""string"",
      ""description"": ""Module name identifier""
    },
    ""version"": {
      ""type"": ""string"",
      ""pattern"": ""^\\d+\\.\\d+\\.\\d+$"",
      ""description"": ""Module version in semantic versioning format""
    },
    ""typeCount"": {
      ""type"": ""integer"",
      ""description"": ""Number of types in the module""
    },
    ""types"": {
      ""type"": ""array"",
      ""description"": ""Array of type definitions"",
      ""items"": {
        ""$ref"": ""#/$defs/TypeDefinition""
      }
    }
  },
  ""required"": [""name"", ""version"", ""typeCount"", ""types""],
  ""additionalProperties"": false,

  ""$defs"": {
    ""TypeDefinition"": {
      ""type"": ""object"",
      ""properties"": {
        ""name"": {
          ""type"": ""string"",
          ""description"": ""Type name""
        },
        ""namespace"": {
          ""type"": ""string"",
          ""description"": ""Type namespace""
        },
        ""kind"": {
          ""enum"": [""Class"", ""Interface"", ""Struct"", ""Enum""],
          ""description"": ""Type kind""
        },
        ""access"": {
          ""type"": ""string"",
          ""enum"": [""Public"", ""Private"", ""Protected"", ""Internal"", ""ProtectedInternal""],
          ""description"": ""Type access modifier""
        },
        ""fieldCount"": {
          ""type"": ""integer"",
          ""description"": ""Number of fields in the type""
        },
        ""methodCount"": {
          ""type"": ""integer"",
          ""description"": ""Number of methods in the type""
        },
        ""fields"": {
          ""type"": ""array"",
          ""description"": ""Type fields"",
          ""items"": {
            ""$ref"": ""#/$defs/FieldDefinition""
          }
        },
        ""methods"": {
          ""type"": ""array"",
          ""description"": ""Type methods"",
          ""items"": {
            ""$ref"": ""#/$defs/MethodDefinition""
          }
        }
      },
      ""required"": [""name"", ""namespace"", ""kind"", ""access""],
      ""additionalProperties"": false
    },

    ""FieldDefinition"": {
      ""type"": ""object"",
      ""properties"": {
        ""name"": {
          ""type"": ""string"",
          ""description"": ""Field name""
        },
        ""type"": {
          ""type"": ""string"",
          ""description"": ""Field type name""
        },
        ""access"": {
          ""type"": ""string"",
          ""enum"": [""Public"", ""Private"", ""Protected"", ""Internal"", ""ProtectedInternal""],
          ""description"": ""Field access modifier""
        },
        ""isStatic"": {
          ""type"": ""boolean"",
          ""description"": ""Whether the field is static""
        },
        ""isReadOnly"": {
          ""type"": ""boolean"",
          ""description"": ""Whether the field is read-only""
        }
      },
      ""required"": [""name"", ""type"", ""access"", ""isStatic"", ""isReadOnly""],
      ""additionalProperties"": false
    },

    ""MethodDefinition"": {
      ""type"": ""object"",
      ""properties"": {
        ""name"": {
          ""type"": ""string"",
          ""description"": ""Method name""
        },
        ""returnType"": {
          ""type"": ""string"",
          ""description"": ""Return type name""
        },
        ""access"": {
          ""type"": ""string"",
          ""enum"": [""Public"", ""Private"", ""Protected"", ""Internal"", ""ProtectedInternal""],
          ""description"": ""Method access modifier""
        },
        ""parameterCount"": {
          ""type"": ""integer"",
          ""description"": ""Number of parameters""
        },
        ""parameters"": {
          ""type"": ""array"",
          ""description"": ""Method parameters"",
          ""items"": {
            ""$ref"": ""#/$defs/ParameterDefinition""
          }
        },
        ""localVariables"": {
          ""type"": ""array"",
          ""description"": ""Local variables"",
          ""items"": {
            ""$ref"": ""#/$defs/LocalVariableDefinition""
          }
        },
        ""isStatic"": {
          ""type"": ""boolean"",
          ""description"": ""Whether the method is static""
        },
        ""isVirtual"": {
          ""type"": ""boolean"",
          ""description"": ""Whether the method is virtual""
        },
        ""isAbstract"": {
          ""type"": ""boolean"",
          ""description"": ""Whether the method is abstract""
        },
        ""instructionCount"": {
          ""type"": ""integer"",
          ""description"": ""Number of instructions""
        },
        ""instructions"": {
          ""type"": ""array"",
          ""description"": ""Method instructions"",
          ""items"": {
            ""$ref"": ""#/$defs/InstructionDefinition""
          }
        }
      },
      ""required"": [""name"", ""returnType"", ""access"", ""parameterCount"", ""parameters"", ""localVariables"", ""isStatic"", ""isVirtual"", ""isAbstract"", ""instructionCount"", ""instructions""],
      ""additionalProperties"": false
    },

    ""ParameterDefinition"": {
      ""type"": ""object"",
      ""properties"": {
        ""name"": {
          ""type"": ""string"",
          ""description"": ""Parameter name""
        },
        ""type"": {
          ""type"": ""string"",
          ""description"": ""Parameter type name""
        }
      },
      ""required"": [""name"", ""type""],
      ""additionalProperties"": false
    },

    ""LocalVariableDefinition"": {
      ""type"": ""object"",
      ""properties"": {
        ""name"": {
          ""type"": ""string"",
          ""description"": ""Local variable name""
        },
        ""type"": {
          ""type"": ""string"",
          ""description"": ""Local variable type name""
        }
      },
      ""required"": [""name"", ""type""],
      ""additionalProperties"": false
    },

    ""InstructionDefinition"": {
      ""type"": ""object"",
      ""properties"": {
        ""opCode"": {
          ""type"": ""string"",
          ""description"": ""Operation code""
        },
        ""operand"": {
          ""description"": ""Optional instruction operand""
        }
      },
      ""required"": [""opCode""],
      ""additionalProperties"": false
    },

    ""MethodReference"": {
      ""type"": ""object"",
      ""properties"": {
        ""declaringType"": { ""type"": ""string"" },
        ""name"": { ""type"": ""string"" },
        ""returnType"": { ""type"": ""string"" },
        ""parameterTypes"": {
          ""type"": ""array"",
          ""items"": { ""type"": ""string"" }
        }
      },
      ""required"": [""declaringType"", ""name"", ""returnType"", ""parameterTypes""],
      ""additionalProperties"": false
    },

    ""FieldReference"": {
      ""type"": ""object"",
      ""properties"": {
        ""declaringType"": { ""type"": ""string"" },
        ""name"": { ""type"": ""string"" },
        ""fieldType"": { ""type"": ""string"" }
      },
      ""required"": [""declaringType"", ""name"", ""fieldType""],
      ""additionalProperties"": false
    }
  }
}";
    }
}

/// <summary>
/// Result of JSON validation
/// </summary>
public class ValidationResult
{
    /// <summary>
    /// Whether the JSON is valid
    /// </summary>
    public bool IsValid { get; }

    /// <summary>
    /// Error message if validation failed, null if valid
    /// </summary>
    public string? ErrorMessage { get; }

    internal ValidationResult(bool isValid, string? errorMessage)
    {
        IsValid = isValid;
        ErrorMessage = errorMessage;
    }
}

/// <summary>
/// Exception thrown when JSON validation fails
/// </summary>
public class JsonValidationException : Exception
{
    /// <summary>
    /// Initializes a new instance of the JsonValidationException class
    /// </summary>
    /// <param name="message">The error message</param>
    public JsonValidationException(string message) : base(message) { }
}