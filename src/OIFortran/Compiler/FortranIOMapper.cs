using System;
using System.Collections.Generic;
using ObjectIR.Core.IR;

namespace ObjectIR.Fortran.Compiler;

/// <summary>
/// Maps Fortran I/O operations (READ/WRITE) to corresponding System.Console and System.Convert methods.
/// Centralizes the logic for input/output type conversions.
/// </summary>
internal sealed class FortranIOMapper
{
    /// <summary>
    /// Represents the native methods needed to read a Fortran type from console.
    /// </summary>
    public sealed class ReadMapping
    {
        /// <summary>Method to call to read from console (e.g., System.Console.ReadLine).</summary>
        public MethodReference ReadMethod { get; }

        /// <summary>Type that ReadMethod returns (usually String).</summary>
        public TypeReference ReadReturnType { get; }

        /// <summary>Optional conversion method if ReadMethod doesn't return the target type directly.</summary>
        public MethodReference? ConversionMethod { get; }

        /// <summary>Final type after read + conversion.</summary>
        public TypeReference FinalType { get; }

        public ReadMapping(
            MethodReference readMethod,
            TypeReference readReturnType,
            TypeReference finalType,
            MethodReference? conversionMethod = null)
        {
            ReadMethod = readMethod;
            ReadReturnType = readReturnType;
            FinalType = finalType;
            ConversionMethod = conversionMethod;
        }
    }

    /// <summary>
    /// Represents the native methods needed to write a Fortran type to console.
    /// </summary>
    public sealed class WriteMapping
    {
        /// <summary>Method to call to write to console (e.g., System.Console.WriteLine).</summary>
        public MethodReference WriteMethod { get; }

        /// <summary>Type that WriteMethod expects (usually String for WriteLine).</summary>
        public TypeReference WriteParameterType { get; }

        /// <summary>Optional conversion method if source type doesn't match WriteMethod parameter.</summary>
        public MethodReference? ConversionMethod { get; }

        public WriteMapping(
            MethodReference writeMethod,
            TypeReference writeParameterType,
            MethodReference? conversionMethod = null)
        {
            WriteMethod = writeMethod;
            WriteParameterType = writeParameterType;
            ConversionMethod = conversionMethod;
        }
    }

    private readonly Dictionary<TypeReference, ReadMapping> _readMappings = new();
    private readonly Dictionary<TypeReference, WriteMapping> _writeMappings = new();

    public FortranIOMapper()
    {
        InitializeMappings();
    }

    private void InitializeMappings()
    {
        var consoleType = TypeReference.FromName("System.Console");
        var convertType = TypeReference.FromName("System.Convert");

        // READ mappings: how to read each Fortran type from console
        
        // INTEGER: ReadLine() → string, then Convert.ToInt32(string) → int32
        var readLineMethod = new MethodReference(consoleType, "ReadLine", TypeReference.String, new List<TypeReference>());
        var toInt32Method = new MethodReference(convertType, "ToInt32", TypeReference.Int32, new List<TypeReference> { TypeReference.String });
        _readMappings[TypeReference.Int32] = new ReadMapping(
            readLineMethod,
            TypeReference.String,
            TypeReference.Int32,
            toInt32Method);

        // REAL (Float32): ReadLine() → string, then Convert.ToDouble(string) → double (then Conv.R4 if needed)
        var toDoubleMethod = new MethodReference(convertType, "ToDouble", TypeReference.Float64, new List<TypeReference> { TypeReference.String });
        _readMappings[TypeReference.Float32] = new ReadMapping(
            readLineMethod,
            TypeReference.String,
            TypeReference.Float64,  // Convert.ToDouble returns double; caller can handle conversion if Float32 needed
            toDoubleMethod);

        // CHARACTER (String): ReadLine() → string directly
        _readMappings[TypeReference.String] = new ReadMapping(
            readLineMethod,
            TypeReference.String,
            TypeReference.String);

        // WRITE mappings: how to write each Fortran type to console
        
        // INTEGER: Convert.ToString(int32) → string, then Console.WriteLine(string)
        var toStringInt32 = new MethodReference(convertType, "ToString", TypeReference.String, new List<TypeReference> { TypeReference.Int32 });
        var writeLineString = new MethodReference(consoleType, "WriteLine", TypeReference.Void, new List<TypeReference> { TypeReference.String });
        _writeMappings[TypeReference.Int32] = new WriteMapping(
            writeLineString,
            TypeReference.String,
            toStringInt32);

        // REAL (Float32/Float64): Convert.ToString(double) → string, then Console.WriteLine(string)
        var toStringDouble = new MethodReference(convertType, "ToString", TypeReference.String, new List<TypeReference> { TypeReference.Float64 });
        _writeMappings[TypeReference.Float32] = new WriteMapping(
            writeLineString,
            TypeReference.String,
            toStringDouble);
        
        _writeMappings[TypeReference.Float64] = new WriteMapping(
            writeLineString,
            TypeReference.String,
            toStringDouble);

        // CHARACTER (String): Console.WriteLine(string) directly
        _writeMappings[TypeReference.String] = new WriteMapping(
            writeLineString,
            TypeReference.String);

        // BOOLEAN: Convert.ToString(bool) → string, then Console.WriteLine(string)
        var toStringBool = new MethodReference(convertType, "ToString", TypeReference.String, new List<TypeReference> { TypeReference.Bool });
        _writeMappings[TypeReference.Bool] = new WriteMapping(
            writeLineString,
            TypeReference.String,
            toStringBool);
    }

    /// <summary>
    /// Get the read mapping for a Fortran type.
    /// </summary>
    public ReadMapping? GetReadMapping(TypeReference fortranType)
    {
        // Normalize type comparison (case-insensitive name matching)
        foreach (var kvp in _readMappings)
        {
            if (TypeEquals(kvp.Key, fortranType))
            {
                return kvp.Value;
            }
        }
        return null;
    }

    /// <summary>
    /// Get the write mapping for a Fortran type.
    /// </summary>
    public WriteMapping? GetWriteMapping(TypeReference fortranType)
    {
        // Normalize type comparison (case-insensitive name matching)
        foreach (var kvp in _writeMappings)
        {
            if (TypeEquals(kvp.Key, fortranType))
            {
                return kvp.Value;
            }
        }
        return null;
    }

    private static bool TypeEquals(TypeReference left, TypeReference right)
    {
        return string.Equals(left.Name, right.Name, StringComparison.OrdinalIgnoreCase)
            && string.Equals(left.Namespace ?? string.Empty, right.Namespace ?? string.Empty, StringComparison.Ordinal);
    }
}
