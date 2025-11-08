using System.Collections.Generic;
using ObjectIR.Core.IR;

namespace ObjectIR.Fortran.Compiler;

/// <summary>
/// Central mapping between Fortran I/O concepts and VM methods/types.
/// Keeps MethodReferences for Console and Convert calls used by the compiler.
/// </summary>
public sealed class FortranIOMapping
{
    public MethodReference Console_ReadLine { get; }
    public MethodReference Console_WriteLine_Void { get; }
    public MethodReference Console_WriteLine_String { get; }

    public MethodReference Convert_ToInt32 { get; }
    public MethodReference Convert_ToDouble { get; }
    public MethodReference Convert_ToString_Int32 { get; }
    public MethodReference Convert_ToString_Float32 { get; }
    public MethodReference Convert_ToString_Bool { get; }

    private FortranIOMapping()
    {
        var consoleType = TypeReference.FromName("System.Console");
        var convertType = TypeReference.FromName("System.Convert");

        Console_ReadLine = new MethodReference(consoleType, "ReadLine", TypeReference.String, new List<TypeReference>());
        Console_WriteLine_Void = new MethodReference(consoleType, "WriteLine", TypeReference.Void, new List<TypeReference>());
        Console_WriteLine_String = new MethodReference(consoleType, "WriteLine", TypeReference.Void, new List<TypeReference> { TypeReference.String });

        Convert_ToInt32 = new MethodReference(convertType, "ToInt32", TypeReference.Int32, new List<TypeReference> { TypeReference.String });
        // Use ToDouble for parsing reals; we'll emit a conversion to Float32 when storing if needed
        Convert_ToDouble = new MethodReference(convertType, "ToDouble", TypeReference.Float64, new List<TypeReference> { TypeReference.String });
        Convert_ToString_Int32 = new MethodReference(convertType, "ToString", TypeReference.String, new List<TypeReference> { TypeReference.Int32 });
        Convert_ToString_Float32 = new MethodReference(convertType, "ToString", TypeReference.String, new List<TypeReference> { TypeReference.Float32 });
        Convert_ToString_Bool = new MethodReference(convertType, "ToString", TypeReference.String, new List<TypeReference> { TypeReference.Bool });
    }

    public static FortranIOMapping CreateDefault() => new();
}
