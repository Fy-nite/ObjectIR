using System;
using System.Runtime.InteropServices;
using System.Text;

namespace ObjectIR.Runtime.Wrapper;

/// <summary>
/// Native interop bindings for the ObjectIR C++ Runtime library
/// </summary>
public static class NativeRuntime
{
    private const string LibraryName = "objectir_runtime";

    // Determine platform-specific library name
    static NativeRuntime()
    {
        if (RuntimeInformation.IsOSPlatform(OSPlatform.Windows))
        {
            // Windows uses .dll
        }
        else if (RuntimeInformation.IsOSPlatform(OSPlatform.Linux))
        {
            // Linux uses .so
        }
        else if (RuntimeInformation.IsOSPlatform(OSPlatform.OSX))
        {
            // macOS uses .dylib
        }
    }

    // ============================================================================
    // Runtime Functions
    // ============================================================================

    /// <summary>
    /// Create a new virtual machine instance
    /// </summary>
    [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
    public static extern IntPtr CreateVirtualMachine();

    /// <summary>
    /// Delete a virtual machine instance
    /// </summary>
    [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
    public static extern void DeleteVirtualMachine(IntPtr vm);

    /// <summary>
    /// Load an ObjectIR module from JSON file
    /// </summary>
    [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
    public static extern IntPtr LoadModuleFromFile(IntPtr vm, [MarshalAs(UnmanagedType.LPStr)] string filePath);

    /// <summary>
    /// Load an ObjectIR module from JSON string
    /// </summary>
    [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
    public static extern IntPtr LoadModuleFromString(IntPtr vm, [MarshalAs(UnmanagedType.LPStr)] string jsonString);

    /// <summary>
    /// Execute a method on an object
    /// </summary>
    [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
    public static extern IntPtr InvokeMethod(
        IntPtr vm,
        [MarshalAs(UnmanagedType.LPStr)] string className,
        [MarshalAs(UnmanagedType.LPStr)] string methodName,
        IntPtr instance);

    /// <summary>
    /// Create a new instance of a class
    /// </summary>
    [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
    public static extern IntPtr CreateInstance(
        IntPtr vm,
        [MarshalAs(UnmanagedType.LPStr)] string className);

    /// <summary>
    /// Get the string representation of a runtime value
    /// </summary>
    [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
    public static extern IntPtr ValueToString(IntPtr value);

    /// <summary>
    /// Free a string returned by the runtime
    /// </summary>
    [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
    public static extern void FreeString(IntPtr str);

    /// <summary>
    /// Free a runtime value
    /// </summary>
    [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
    public static extern void FreeValue(IntPtr value);

    /// <summary>
    /// Free a runtime object
    /// </summary>
    [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
    public static extern void FreeObject(IntPtr obj);

    /// <summary>
    /// Get last error message
    /// </summary>
    [DllImport(LibraryName, CallingConvention = CallingConvention.Cdecl)]
    public static extern IntPtr GetLastError();
}

/// <summary>
/// Safe wrapper for ObjectIR C++ Runtime
/// </summary>
public class RuntimeWrapper : IDisposable
{
    private IntPtr _vmHandle;
    private bool _disposed = false;

    public RuntimeWrapper()
    {
        _vmHandle = NativeRuntime.CreateVirtualMachine();
        if (_vmHandle == IntPtr.Zero)
        {
            throw new RuntimeException("Failed to create virtual machine");
        }
    }

    /// <summary>
    /// Load a module from a JSON file
    /// </summary>
    public void LoadModuleFromFile(string filePath)
    {
        if (_disposed)
            throw new ObjectDisposedException(nameof(RuntimeWrapper));

        if (!File.Exists(filePath))
            throw new FileNotFoundException($"Module file not found: {filePath}");

        IntPtr result = NativeRuntime.LoadModuleFromFile(_vmHandle, filePath);
        CheckError(result);
    }

    /// <summary>
    /// Load a module from a JSON string
    /// </summary>
    public void LoadModuleFromString(string jsonContent)
    {
        if (_disposed)
            throw new ObjectDisposedException(nameof(RuntimeWrapper));

        IntPtr result = NativeRuntime.LoadModuleFromString(_vmHandle, jsonContent);
        CheckError(result);
    }

    /// <summary>
    /// Create a new instance of a class
    /// </summary>
    public RuntimeObject CreateInstance(string className)
    {
        if (_disposed)
            throw new ObjectDisposedException(nameof(RuntimeWrapper));

        IntPtr objHandle = NativeRuntime.CreateInstance(_vmHandle, className);
        if (objHandle == IntPtr.Zero)
        {
            throw new RuntimeException($"Failed to create instance of class '{className}'");
        }

        return new RuntimeObject(objHandle);
    }

    /// <summary>
    /// Invoke a method on an object
    /// </summary>
    public RuntimeValue InvokeMethod(string className, string methodName, RuntimeObject instance)
    {
        if (_disposed)
            throw new ObjectDisposedException(nameof(RuntimeWrapper));

        IntPtr valueHandle = NativeRuntime.InvokeMethod(
            _vmHandle,
            className,
            methodName,
            instance?.Handle ?? IntPtr.Zero);

        if (valueHandle == IntPtr.Zero)
        {
            throw new RuntimeException($"Failed to invoke method '{methodName}' on class '{className}'");
        }

        return new RuntimeValue(valueHandle);
    }

    public void Dispose()
    {
        if (_disposed)
            return;

        if (_vmHandle != IntPtr.Zero)
        {
            NativeRuntime.DeleteVirtualMachine(_vmHandle);
            _vmHandle = IntPtr.Zero;
        }

        _disposed = true;
        GC.SuppressFinalize(this);
    }

    ~RuntimeWrapper()
    {
        Dispose();
    }

    private static void CheckError(IntPtr result)
    {
        if (result == IntPtr.Zero)
        {
            IntPtr errorPtr = NativeRuntime.GetLastError();
            string? errorMsg = null;
            if (errorPtr != IntPtr.Zero)
            {
                errorMsg = Marshal.PtrToStringAnsi(errorPtr);
                NativeRuntime.FreeString(errorPtr);
            }
            throw new RuntimeException(errorMsg ?? "Unknown error in native runtime");
        }
    }
}

/// <summary>
/// Represents a runtime object instance
/// </summary>
public class RuntimeObject : IDisposable
{
    public IntPtr Handle { get; private set; }
    private bool _disposed = false;

    public RuntimeObject(IntPtr handle)
    {
        Handle = handle;
    }

    public void Dispose()
    {
        if (_disposed)
            return;

        if (Handle != IntPtr.Zero)
        {
            NativeRuntime.FreeObject(Handle);
            Handle = IntPtr.Zero;
        }

        _disposed = true;
        GC.SuppressFinalize(this);
    }

    ~RuntimeObject()
    {
        Dispose();
    }
}

/// <summary>
/// Represents a runtime value (return value from method)
/// </summary>
public class RuntimeValue : IDisposable
{
    public IntPtr Handle { get; private set; }
    private bool _disposed = false;

    public RuntimeValue(IntPtr handle)
    {
        Handle = handle;
    }

    /// <summary>
    /// Get the string representation of the value
    /// </summary>
    public override string ToString()
    {
        if (_disposed)
            throw new ObjectDisposedException(nameof(RuntimeValue));

        IntPtr strPtr = NativeRuntime.ValueToString(Handle);
        string? result = Marshal.PtrToStringAnsi(strPtr);
        NativeRuntime.FreeString(strPtr);
        return result ?? "<null>";
    }

    public void Dispose()
    {
        if (_disposed)
            return;

        if (Handle != IntPtr.Zero)
        {
            NativeRuntime.FreeValue(Handle);
            Handle = IntPtr.Zero;
        }

        _disposed = true;
        GC.SuppressFinalize(this);
    }

    ~RuntimeValue()
    {
        Dispose();
    }
}

/// <summary>
/// Exception thrown by the ObjectIR runtime
/// </summary>
public class RuntimeException : Exception
{
    public RuntimeException(string? message) : base(message) { }
    public RuntimeException(string? message, Exception? innerException) : base(message, innerException) { }
}
