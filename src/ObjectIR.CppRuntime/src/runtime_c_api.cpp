#include "objectir_runtime.hpp"
#include "ir_loader.hpp"
#include "fob_loader.hpp"

#include <cstdint>
#include <cstring>
#include <iostream>
#include <memory>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

namespace
{
    struct RuntimeHandle
    {
        std::shared_ptr<ObjectIR::VirtualMachine> vm;
    };

    struct ObjectHandle
    {
        ObjectIR::ObjectRef object;
    };

    struct ValueHandle
    {
        std::shared_ptr<ObjectIR::Value> value;
    };

    thread_local std::string g_lastError;

    void ClearLastError()
    {
        g_lastError.clear();
    }

    void SetLastError(const std::string &message)
    {
        g_lastError = message;
    }

    char *CopyToCString(const std::string &source)
    {
        auto *buffer = new char[source.size() + 1];
        std::memcpy(buffer, source.c_str(), source.size() + 1);
        return buffer;
    }

    RuntimeHandle *AsRuntimeHandle(void *ptr)
    {
        return static_cast<RuntimeHandle *>(ptr);
    }

    ObjectHandle *AsObjectHandle(void *ptr)
    {
        return static_cast<ObjectHandle *>(ptr);
    }

    ValueHandle *AsValueHandle(void *ptr)
    {
        return static_cast<ValueHandle *>(ptr);
    }

    ObjectIR::VirtualMachine *GetVm(RuntimeHandle *handle)
    {
        if (!handle || !handle->vm)
        {
            throw std::runtime_error("Virtual machine is not initialized");
        }
        return handle->vm.get();
    }

    ValueHandle *CreateValueHandle(const ObjectIR::Value &value)
    {
        auto *handle = new ValueHandle();
        handle->value = std::make_shared<ObjectIR::Value>(value);
        return handle;
    }

    ValueHandle *CreateValueHandle(ObjectIR::Value &&value)
    {
        auto *handle = new ValueHandle();
        handle->value = std::make_shared<ObjectIR::Value>(std::move(value));
        return handle;
    }

    std::vector<ObjectIR::Value> BuildArguments(void **args, int32_t argCount)
    {
        std::vector<ObjectIR::Value> result;
        if (argCount <= 0)
        {
            return result;
        }

        if (!args)
        {
            throw std::runtime_error("Argument pointer array is null");
        }

        result.reserve(static_cast<size_t>(argCount));
        for (int32_t i = 0; i < argCount; ++i)
        {
            auto *valueHandle = AsValueHandle(args[i]);
            if (!valueHandle || !valueHandle->value)
            {
                throw std::runtime_error("Argument value handle is invalid");
            }
            result.push_back(*valueHandle->value);
        }

        return result;
    }

    std::string ValueToStringInternal(const ObjectIR::Value &value)
    {
        if (value.IsInt32())
        {
            return std::to_string(value.AsInt32());
        }
        if (value.IsInt64())
        {
            return std::to_string(value.AsInt64());
        }
        if (value.IsFloat32())
        {
            std::ostringstream oss;
            oss << value.AsFloat32();
            return oss.str();
        }
        if (value.IsFloat64())
        {
            std::ostringstream oss;
            oss << value.AsFloat64();
            return oss.str();
        }
        if (value.IsBool())
        {
            return value.AsBool() ? "true" : "false";
        }
        if (value.IsString())
        {
            return value.AsString();
        }
        if (value.IsObject())
        {
            auto obj = value.AsObject();
            if (!obj)
            {
                return "<null object>";
            }
            auto classRef = obj->GetClass();
            if (classRef)
            {
                return "<object " + classRef->GetName() + ">";
            }
            return "<object>";
        }
        if (value.IsNull())
        {
            return "<null>";
        }
        return "<unknown>";
    }
}

#if defined(_WIN32)
#define RUNTIME_API extern "C" __declspec(dllexport)
#elif defined(__GNUC__)
#define RUNTIME_API extern "C" __attribute__((visibility("default")))
#else
#define RUNTIME_API extern "C"
#endif

RUNTIME_API void *CreateVirtualMachine()
{
    try
    {
        auto *handle = new RuntimeHandle();
        handle->vm = std::make_shared<ObjectIR::VirtualMachine>();
        ClearLastError();
        return handle;
    }
    catch (const std::exception &ex)
    {
        SetLastError(ex.what());
    }
    catch (...)
    {
        SetLastError("Unknown error in CreateVirtualMachine");
    }
    return nullptr;
}

RUNTIME_API void DeleteVirtualMachine(void *vmPtr)
{
    try
    {
        auto *handle = AsRuntimeHandle(vmPtr);
        delete handle;
        ClearLastError();
    }
    catch (...)
    {
        // Swallow exceptions; deletion should not throw across the boundary.
    }
}

RUNTIME_API void *LoadModuleFromFile(void *vmPtr, const char *filePath)
{
    if (!vmPtr || !filePath)
    {
        SetLastError("Invalid arguments to LoadModuleFromFile");
        return nullptr;
    }

    try
    {
        auto *handle = AsRuntimeHandle(vmPtr);
        handle->vm = ObjectIR::IRLoader::LoadFromFile(filePath);
        ClearLastError();
        return handle;
    }
    catch (const std::exception &ex)
    {
        SetLastError(ex.what());
    }
    catch (...)
    {
        SetLastError("Unknown error in LoadModuleFromFile");
    }
    return nullptr;
}

RUNTIME_API void *LoadModuleFromString(void *vmPtr, const char *json)
{
    if (!vmPtr || !json)
    {
        SetLastError("Invalid arguments to LoadModuleFromString");
        return nullptr;
    }

    try
    {
        auto *handle = AsRuntimeHandle(vmPtr);
        handle->vm = ObjectIR::IRLoader::LoadFromString(json);
        ClearLastError();
        return handle;
    }
    catch (const std::exception &ex)
    {
        SetLastError(ex.what());
    }
    catch (...)
    {
        SetLastError("Unknown error in LoadModuleFromString");
    }
    return nullptr;
}

RUNTIME_API void *LoadFOBModuleFromFile(void *vmPtr, const char *filePath, char **entryClassName, char **entryMethodName)
{
    if (!vmPtr || !filePath || !entryClassName || !entryMethodName)
    {
        SetLastError("Invalid arguments to LoadFOBModuleFromFile");
        return nullptr;
    }

    try
    {
        auto *handle = AsRuntimeHandle(vmPtr);
        auto result = ObjectIR::FOBLoader::LoadFromFile(filePath);
        handle->vm = result.vm;

        // Get entry point class and method names
        std::cerr << "[C++ Runtime] Entry point indices: type=" << result.entryTypeIndex 
                  << ", method=" << result.entryMethodIndex 
                  << ", classNames.size()=" << result.classNames.size() << std::endl;
        
        if (result.entryTypeIndex < result.classNames.size() &&
            result.entryMethodIndex < result.methodNames[result.entryTypeIndex].size())
        {
            std::cerr << "[C++ Runtime] Found entry point: " << result.classNames[result.entryTypeIndex] 
                      << "." << result.methodNames[result.entryTypeIndex][result.entryMethodIndex] << std::endl;
            *entryClassName = CopyToCString(result.classNames[result.entryTypeIndex]);
            *entryMethodName = CopyToCString(result.methodNames[result.entryTypeIndex][result.entryMethodIndex]);
        }
        else
        {
            std::cerr << "[C++ Runtime] Entry point validation failed!" << std::endl;
            *entryClassName = nullptr;
            *entryMethodName = nullptr;
        }

        ClearLastError();
        return handle;
    }
    catch (const std::exception &ex)
    {
        SetLastError(ex.what());
    }
    catch (...)
    {
        SetLastError("Unknown error in LoadFOBModuleFromFile");
    }
    return nullptr;
}

RUNTIME_API void *CreateInstance(void *vmPtr, const char *className)
{
    if (!vmPtr || !className)
    {
        SetLastError("Invalid arguments to CreateInstance");
        return nullptr;
    }

    try
    {
        auto *handle = AsRuntimeHandle(vmPtr);
        auto *vm = GetVm(handle);
        auto objRef = vm->CreateObject(className);
        auto *objectHandle = new ObjectHandle();
        objectHandle->object = objRef;
        ClearLastError();
        return objectHandle;
    }
    catch (const std::exception &ex)
    {
        SetLastError(ex.what());
    }
    catch (...)
    {
        SetLastError("Unknown error in CreateInstance");
    }
    return nullptr;
}

RUNTIME_API void *InvokeMethod(void *vmPtr, const char *className, const char *methodName, void *instancePtr, void **args, int32_t argCount)
{
    if (!vmPtr || !methodName)
    {
        SetLastError("Invalid arguments to InvokeMethod");
        return nullptr;
    }

    try
    {
        auto *handle = AsRuntimeHandle(vmPtr);
        auto *vm = GetVm(handle);
        auto arguments = BuildArguments(args, argCount);
        ObjectIR::Value result;

        if (instancePtr)
        {
            auto *objectHandle = AsObjectHandle(instancePtr);
            if (!objectHandle || !objectHandle->object)
            {
                throw std::runtime_error("Invalid object handle");
            }
            result = vm->InvokeMethod(objectHandle->object, methodName, arguments);
        }
        else
        {
            if (!className)
            {
                throw std::runtime_error("Class name is required for static method invocation");
            }
            auto classRef = vm->GetClass(className);
            result = vm->InvokeStaticMethod(classRef, methodName, arguments);
        }

        auto *valueHandle = CreateValueHandle(std::move(result));
        ClearLastError();
        return valueHandle;
    }
    catch (const std::exception &ex)
    {
        SetLastError(ex.what());
    }
    catch (...)
    {
        SetLastError("Unknown error in InvokeMethod");
    }
    return nullptr;
}

RUNTIME_API char *ValueToString(void *valuePtr)
{
    if (!valuePtr)
    {
        SetLastError("Value handle is null");
        return nullptr;
    }

    try
    {
        auto *handle = AsValueHandle(valuePtr);
        if (!handle || !handle->value)
        {
            SetLastError("Value handle is invalid");
            return nullptr;
        }

        auto text = ValueToStringInternal(*handle->value);
        ClearLastError();
        return CopyToCString(text);
    }
    catch (const std::exception &ex)
    {
        SetLastError(ex.what());
    }
    catch (...)
    {
        SetLastError("Unknown error in ValueToString");
    }
    return nullptr;
}

RUNTIME_API void FreeString(char *str)
{
    delete[] str;
}

RUNTIME_API void FreeValue(void *valuePtr)
{
    auto *handle = AsValueHandle(valuePtr);
    delete handle;
}

RUNTIME_API void FreeObject(void *objectPtr)
{
    auto *handle = AsObjectHandle(objectPtr);
    delete handle;
}

RUNTIME_API char *GetRuntimeLastError()
{
    if (g_lastError.empty())
    {
        return nullptr;
    }
    return CopyToCString(g_lastError);
}

RUNTIME_API void *CreateNullValue()
{
    try
    {
        auto *valueHandle = CreateValueHandle(ObjectIR::Value());
        ClearLastError();
        return valueHandle;
    }
    catch (const std::exception &ex)
    {
        SetLastError(ex.what());
    }
    catch (...)
    {
        SetLastError("Unknown error in CreateNullValue");
    }
    return nullptr;
}

RUNTIME_API void *CreateInt32Value(int32_t value)
{
    try
    {
        auto *valueHandle = CreateValueHandle(ObjectIR::Value(value));
        ClearLastError();
        return valueHandle;
    }
    catch (const std::exception &ex)
    {
        SetLastError(ex.what());
    }
    catch (...)
    {
        SetLastError("Unknown error in CreateInt32Value");
    }
    return nullptr;
}

RUNTIME_API void *CreateInt64Value(int64_t value)
{
    try
    {
        auto *valueHandle = CreateValueHandle(ObjectIR::Value(value));
        ClearLastError();
        return valueHandle;
    }
    catch (const std::exception &ex)
    {
        SetLastError(ex.what());
    }
    catch (...)
    {
        SetLastError("Unknown error in CreateInt64Value");
    }
    return nullptr;
}

RUNTIME_API void *CreateFloat32Value(float value)
{
    try
    {
        auto *valueHandle = CreateValueHandle(ObjectIR::Value(value));
        ClearLastError();
        return valueHandle;
    }
    catch (const std::exception &ex)
    {
        SetLastError(ex.what());
    }
    catch (...)
    {
        SetLastError("Unknown error in CreateFloat32Value");
    }
    return nullptr;
}

RUNTIME_API void *CreateFloat64Value(double value)
{
    try
    {
        auto *valueHandle = CreateValueHandle(ObjectIR::Value(value));
        ClearLastError();
        return valueHandle;
    }
    catch (const std::exception &ex)
    {
        SetLastError(ex.what());
    }
    catch (...)
    {
        SetLastError("Unknown error in CreateFloat64Value");
    }
    return nullptr;
}

RUNTIME_API void *CreateBoolValue(int32_t value)
{
    try
    {
        auto *valueHandle = CreateValueHandle(ObjectIR::Value(value != 0));
        ClearLastError();
        return valueHandle;
    }
    catch (const std::exception &ex)
    {
        SetLastError(ex.what());
    }
    catch (...)
    {
        SetLastError("Unknown error in CreateBoolValue");
    }
    return nullptr;
}

RUNTIME_API void *CreateStringValue(const char *value)
{
    if (!value)
    {
        return CreateNullValue();
    }

    try
    {
        auto *valueHandle = CreateValueHandle(ObjectIR::Value(std::string(value)));
        ClearLastError();
        return valueHandle;
    }
    catch (const std::exception &ex)
    {
        SetLastError(ex.what());
    }
    catch (...)
    {
        SetLastError("Unknown error in CreateStringValue");
    }
    return nullptr;
}

RUNTIME_API void *CreateObjectValue(void *objectPtr)
{
    try
    {
        if (!objectPtr)
        {
            auto *valueHandle = CreateValueHandle(ObjectIR::Value());
            ClearLastError();
            return valueHandle;
        }

        auto *objectHandle = AsObjectHandle(objectPtr);
        if (!objectHandle || !objectHandle->object)
        {
            throw std::runtime_error("Invalid object handle");
        }

        auto *valueHandle = CreateValueHandle(ObjectIR::Value(objectHandle->object));
        ClearLastError();
        return valueHandle;
    }
    catch (const std::exception &ex)
    {
        SetLastError(ex.what());
    }
    catch (...)
    {
        SetLastError("Unknown error in CreateObjectValue");
    }
    return nullptr;
}
