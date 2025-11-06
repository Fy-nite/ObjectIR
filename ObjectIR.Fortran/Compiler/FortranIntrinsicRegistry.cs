using System;
using System.Collections.Generic;
using ObjectIR.Core.IR;

namespace ObjectIR.Fortran.Compiler;

public enum IntrinsicCallKind
{
    Static,
    Virtual
}

public sealed class FortranIntrinsicRegistry
{
    private readonly Dictionary<string, FortranIntrinsic> _entries = new(StringComparer.OrdinalIgnoreCase);

    public static FortranIntrinsicRegistry CreateDefault()
    {
        return new FortranIntrinsicRegistry();
    }

    public void RegisterProcedure(string name, MethodReference method, IntrinsicCallKind callKind = IntrinsicCallKind.Static)
    {
        if (!TypeEquals(method.ReturnType, TypeReference.Void))
        {
            throw new ArgumentException("Procedure intrinsics must have void return type", nameof(method));
        }

        Register(name, method, callKind);
    }

    public void RegisterFunction(string name, MethodReference method, IntrinsicCallKind callKind = IntrinsicCallKind.Static)
    {
        if (TypeEquals(method.ReturnType, TypeReference.Void))
        {
            throw new ArgumentException("Function intrinsics must return a value", nameof(method));
        }

        Register(name, method, callKind);
    }

    public bool TryGet(string name, out FortranIntrinsic intrinsic) => _entries.TryGetValue(name, out intrinsic!);

    public void LoadFromConfig(FortranIntrinsicConfig config)
    {
        if (config.Procedures != null)
        {
            foreach (var entry in config.Procedures)
            {
                RegisterProcedure(entry.Name, BuildMethod(entry), ParseCallKind(entry.CallKind));
            }
        }

        if (config.Functions != null)
        {
            foreach (var entry in config.Functions)
            {
                RegisterFunction(entry.Name, BuildMethod(entry), ParseCallKind(entry.CallKind));
            }
        }
    }

    private void Register(string name, MethodReference method, IntrinsicCallKind callKind)
    {
        if (string.IsNullOrWhiteSpace(name))
        {
            throw new ArgumentException("Intrinsic name cannot be empty", nameof(name));
        }

        _entries[name] = new FortranIntrinsic(method, callKind);
    }

    private static MethodReference BuildMethod(IntrinsicBinding binding)
    {
        if (string.IsNullOrWhiteSpace(binding.DeclaringType))
        {
            throw new ArgumentException($"Intrinsic '{binding.Name}' is missing a declaringType");
        }

        if (string.IsNullOrWhiteSpace(binding.Method))
        {
            throw new ArgumentException($"Intrinsic '{binding.Name}' is missing a method name");
        }

        var declaringType = ParseType(binding.DeclaringType);
        var parameters = new List<TypeReference>();
        if (binding.Parameters != null)
        {
            foreach (var parameter in binding.Parameters)
            {
                parameters.Add(ParseType(parameter));
            }
        }

        var returnType = string.IsNullOrWhiteSpace(binding.ReturnType)
            ? TypeReference.Void
            : ParseType(binding.ReturnType);

        return new MethodReference(declaringType, binding.Method, returnType, parameters);
    }

    private static IntrinsicCallKind ParseCallKind(string? callKind)
    {
        if (string.IsNullOrWhiteSpace(callKind))
        {
            return IntrinsicCallKind.Static;
        }

        return callKind.ToLowerInvariant() switch
        {
            "static" => IntrinsicCallKind.Static,
            "call" => IntrinsicCallKind.Static,
            "virtual" => IntrinsicCallKind.Virtual,
            "callvirt" => IntrinsicCallKind.Virtual,
            _ => throw new ArgumentException($"Unknown call kind '{callKind}'")
        };
    }

    private static bool TypeEquals(TypeReference left, TypeReference right)
    {
        return string.Equals(left.Name, right.Name, StringComparison.OrdinalIgnoreCase)
            && string.Equals(left.Namespace ?? string.Empty, right.Namespace ?? string.Empty, StringComparison.Ordinal);
    }

    private static TypeReference ParseType(string name)
    {
        return name.ToLowerInvariant() switch
        {
            "void" => TypeReference.Void,
            "integer" or "int" or "int32" => TypeReference.Int32,
            "real" or "float" or "single" or "float32" => TypeReference.Float32,
            "logical" or "bool" or "boolean" => TypeReference.Bool,
            "string" => TypeReference.String,
            _ => TypeReference.FromName(name)
        };
    }
}

public sealed class FortranIntrinsic
{
    public MethodReference Method { get; }
    public IntrinsicCallKind CallKind { get; }

    public FortranIntrinsic(MethodReference method, IntrinsicCallKind callKind)
    {
        Method = method;
        CallKind = callKind;
    }

    public bool ReturnsValue => !string.Equals(Method.ReturnType.Name, TypeReference.Void.Name, StringComparison.OrdinalIgnoreCase);
}