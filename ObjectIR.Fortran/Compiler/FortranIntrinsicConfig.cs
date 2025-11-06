using System.Collections.Generic;
using System.Text.Json.Serialization;

namespace ObjectIR.Fortran.Compiler;

public sealed class FortranIntrinsicConfig
{
    [JsonPropertyName("procedures")]
    public List<IntrinsicBinding>? Procedures { get; set; }

    [JsonPropertyName("functions")]
    public List<IntrinsicBinding>? Functions { get; set; }
}

public sealed class IntrinsicBinding
{
    [JsonPropertyName("name")]
    public string Name { get; set; } = string.Empty;

    [JsonPropertyName("declaringType")]
    public string DeclaringType { get; set; } = string.Empty;

    [JsonPropertyName("method")]
    public string Method { get; set; } = string.Empty;

    [JsonPropertyName("returnType")]
    public string? ReturnType { get; set; }

    [JsonPropertyName("parameters")]
    public List<string>? Parameters { get; set; }

    [JsonPropertyName("callKind")]
    public string? CallKind { get; set; }
}
