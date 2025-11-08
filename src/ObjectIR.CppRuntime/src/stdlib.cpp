#include "stdlib.hpp"
#include <iostream>
#include <string>
#include <sstream>

namespace ObjectIR {

namespace {

// Converts a Value to a string representation for output
std::string ValueToDisplayString(const Value& value) {
    if (value.IsNull()) {
        return "null";
    }
    if (value.IsString()) {
        return value.AsString();
    }
    if (value.IsInt32()) {
        return std::to_string(value.AsInt32());
    }
    if (value.IsInt64()) {
        return std::to_string(value.AsInt64());
    }
    if (value.IsFloat32()) {
        return std::to_string(value.AsFloat32());
    }
    if (value.IsFloat64()) {
        return std::to_string(value.AsFloat64());
    }
    if (value.IsBool()) {
        return value.AsBool() ? "true" : "false";
    }
    if (value.IsObject()) {
        return "<object>";
    }
    return "";
}

} // namespace

// ============================================================================
// System.Console Implementation
// ============================================================================

Value Console_WriteLine_String(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsString()) {
        std::cout << args[0].AsString() << std::endl;
    }
    return Value();
}

Value Console_WriteLine_Int32(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsInt32()) {
        std::cout << args[0].AsInt32() << std::endl;
    }
    return Value();
}

Value Console_WriteLine_Int64(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsInt64()) {
        std::cout << args[0].AsInt64() << std::endl;
    }
    return Value();
}

Value Console_WriteLine_Double(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsFloat64()) {
        std::cout << args[0].AsFloat64() << std::endl;
    }
    return Value();
}

// Overload for float32
Value Console_WriteLine_Float(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsFloat32()) {
        std::cout << args[0].AsFloat32() << std::endl;
    }
    return Value();
}

Value Console_WriteLine_Bool(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsBool()) {
        std::cout << (args[0].AsBool() ? "true" : "false") << std::endl;
    }
    return Value();
}

Value Console_WriteLine_Void(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    std::cout << std::endl;
    return Value();
}

Value Console_Write_String(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsString()) {
        std::cout << args[0].AsString();
    }
    return Value();
}

Value Console_Write_Int32(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsInt32()) {
        std::cout << args[0].AsInt32();
    }
    return Value();
}

Value Console_Write_Double(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsFloat64()) {
        std::cout << args[0].AsFloat64();
    }
    return Value();
}

// Overload for float32
Value Console_Write_Float(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsFloat32()) {
        std::cout << args[0].AsFloat32();
    }
    return Value();
}

Value Console_ReadLine(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    std::string line;
    if (std::getline(std::cin, line)) {
        return Value(line);
    }
    return Value("");
}

// ============================================================================
// System.String Implementation
// ============================================================================

Value String_Concat_TwoStrings(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 2 && args[0].IsString() && args[1].IsString()) {
        return Value(args[0].AsString() + args[1].AsString());
    }
    if (args.size() >= 1 && args[0].IsString()) {
        return args[0];
    }
    return Value("");
}

Value String_IsNullOrEmpty(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1) {
        if (args[0].IsNull()) {
            return Value(true);
        }
        if (args[0].IsString()) {
            return Value(args[0].AsString().empty());
        }
    }
    return Value(true);
}

Value String_Length(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsString()) {
        return Value(static_cast<int32_t>(args[0].AsString().length()));
    }
    return Value(0);
}

Value String_Substring(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 3 && args[0].IsString() && args[1].IsInt32() && args[2].IsInt32()) {
        const auto& str = args[0].AsString();
        int32_t start = args[1].AsInt32();
        int32_t length = args[2].AsInt32();
        
        if (start >= 0 && start < static_cast<int32_t>(str.length()) && length > 0) {
            return Value(str.substr(start, length));
        }
    }
    return Value("");
}

// ============================================================================
// System.Convert Implementation
// ============================================================================

Value Convert_ToString_Int32(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsInt32()) {
        return Value(std::to_string(args[0].AsInt32()));
    }
    return Value("");
}

Value Convert_ToString_Int64(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsInt64()) {
        return Value(std::to_string(args[0].AsInt64()));
    }
    return Value("");
}

Value Convert_ToString_Double(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsFloat64()) {
        return Value(std::to_string(args[0].AsFloat64()));
    }
    return Value("");
}

Value Convert_ToString_Float(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsFloat32()) {
        return Value(std::to_string(args[0].AsFloat32()));
    }
    return Value("");
}

Value Convert_ToString_Bool(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsBool()) {
        return Value(args[0].AsBool() ? std::string("true") : std::string("false"));
    }
    return Value("");
}

Value Convert_ToInt32(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1) {
        if (args[0].IsInt32()) {
            return args[0];
        }
        if (args[0].IsString()) {
            try {
                return Value(std::stoi(args[0].AsString()));
            } catch (...) {
                return Value(0);
            }
        }
        if (args[0].IsFloat64()) {
            return Value(static_cast<int32_t>(args[0].AsFloat64()));
        }
    }
    return Value(0);
}

Value Convert_ToDouble(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1) {
        if (args[0].IsFloat64()) {
            return args[0];
        }
        if (args[0].IsInt32()) {
            return Value(static_cast<double>(args[0].AsInt32()));
        }
        if (args[0].IsInt64()) {
            return Value(static_cast<double>(args[0].AsInt64()));
        }
        if (args[0].IsString()) {
            try {
                return Value(std::stod(args[0].AsString()));
            } catch (...) {
                return Value(0.0);
            }
        }
    }
    return Value(0.0);
}

Value Convert_ToSingle(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1) {
        if (args[0].IsFloat32()) {
            return args[0];
        }
        if (args[0].IsFloat64()) {
            return Value(static_cast<float>(args[0].AsFloat64()));
        }
        if (args[0].IsInt32()) {
            return Value(static_cast<float>(args[0].AsInt32()));
        }
        if (args[0].IsInt64()) {
            return Value(static_cast<float>(args[0].AsInt64()));
        }
        if (args[0].IsString()) {
            try {
                return Value(std::stof(args[0].AsString()));
            } catch (...) {
                return Value(0.0f);
            }
        }
    }
    return Value(0.0f);
}

// ============================================================================
// Standard Library Registration
// ============================================================================

void RegisterStandardLibrary(std::shared_ptr<VirtualMachine> vm) {
    // Create System.Console class
    auto consoleClass = std::make_shared<Class>("System.Console");
    consoleClass->SetNamespace("System");
    consoleClass->SetAbstract(true);
    
    // Console.WriteLine overloads
    auto writeLineString = std::make_shared<Method>("WriteLine", TypeReference::Void(), true, false);
    writeLineString->AddParameter("value", TypeReference::String());
    writeLineString->SetNativeImpl(Console_WriteLine_String);
    consoleClass->AddMethod(writeLineString);
    
    auto writeLineInt32 = std::make_shared<Method>("WriteLine", TypeReference::Void(), true, false);
    writeLineInt32->AddParameter("value", TypeReference::Int32());
    writeLineInt32->SetNativeImpl(Console_WriteLine_Int32);
    consoleClass->AddMethod(writeLineInt32);
    
    auto writeLineInt64 = std::make_shared<Method>("WriteLine", TypeReference::Void(), true, false);
    writeLineInt64->AddParameter("value", TypeReference::Int64());
    writeLineInt64->SetNativeImpl(Console_WriteLine_Int64);
    consoleClass->AddMethod(writeLineInt64);
    
    auto writeLineDouble = std::make_shared<Method>("WriteLine", TypeReference::Void(), true, false);
    writeLineDouble->AddParameter("value", TypeReference::Float64());
    writeLineDouble->SetNativeImpl(Console_WriteLine_Double);
    consoleClass->AddMethod(writeLineDouble);

    auto writeLineFloat = std::make_shared<Method>("WriteLine", TypeReference::Void(), true, false);
    writeLineFloat->AddParameter("value", TypeReference::Float32());
    writeLineFloat->SetNativeImpl(Console_WriteLine_Float);
    consoleClass->AddMethod(writeLineFloat);
    
    auto writeLineBool = std::make_shared<Method>("WriteLine", TypeReference::Void(), true, false);
    writeLineBool->AddParameter("value", TypeReference::Bool());
    writeLineBool->SetNativeImpl(Console_WriteLine_Bool);
    consoleClass->AddMethod(writeLineBool);
    
    auto writeLineVoid = std::make_shared<Method>("WriteLine", TypeReference::Void(), true, false);
    writeLineVoid->SetNativeImpl(Console_WriteLine_Void);
    consoleClass->AddMethod(writeLineVoid);
    
    // Console.Write overloads
    auto writeString = std::make_shared<Method>("Write", TypeReference::Void(), true, false);
    writeString->AddParameter("value", TypeReference::String());
    writeString->SetNativeImpl(Console_Write_String);
    consoleClass->AddMethod(writeString);
    
    auto writeInt32 = std::make_shared<Method>("Write", TypeReference::Void(), true, false);
    writeInt32->AddParameter("value", TypeReference::Int32());
    writeInt32->SetNativeImpl(Console_Write_Int32);
    consoleClass->AddMethod(writeInt32);
    
    auto writeDouble = std::make_shared<Method>("Write", TypeReference::Void(), true, false);
    writeDouble->AddParameter("value", TypeReference::Float64());
    writeDouble->SetNativeImpl(Console_Write_Double);
    consoleClass->AddMethod(writeDouble);

    auto writeFloat = std::make_shared<Method>("Write", TypeReference::Void(), true, false);
    writeFloat->AddParameter("value", TypeReference::Float32());
    writeFloat->SetNativeImpl(Console_Write_Float);
    consoleClass->AddMethod(writeFloat);
    
    // Console.ReadLine
    auto readLine = std::make_shared<Method>("ReadLine", TypeReference::String(), true, false);
    readLine->SetNativeImpl(Console_ReadLine);
    consoleClass->AddMethod(readLine);
    
    vm->RegisterClass(consoleClass);
    
    // Create System.String class with both uppercase and lowercase names
    auto stringClass = std::make_shared<Class>("System.String");
    stringClass->SetNamespace("System");
    
    auto concat = std::make_shared<Method>("Concat", TypeReference::String(), true, false);
    concat->AddParameter("str0", TypeReference::String());
    concat->AddParameter("str1", TypeReference::String());
    concat->SetNativeImpl(String_Concat_TwoStrings);
    stringClass->AddMethod(concat);
    
    auto isNullOrEmpty = std::make_shared<Method>("IsNullOrEmpty", TypeReference::Bool(), true, false);
    isNullOrEmpty->AddParameter("value", TypeReference::String());
    isNullOrEmpty->SetNativeImpl(String_IsNullOrEmpty);
    stringClass->AddMethod(isNullOrEmpty);
    
    // Register with uppercase name
    vm->RegisterClass(stringClass);
    
    // Also register with lowercase name for compatibility
    auto stringClassLower = std::make_shared<Class>("System.string");
    stringClassLower->SetNamespace("System");
    stringClassLower->AddMethod(concat);
    stringClassLower->AddMethod(isNullOrEmpty);
    vm->RegisterClass(stringClassLower);
    
    // Create System.Convert class
    auto convertClass = std::make_shared<Class>("System.Convert");
    convertClass->SetNamespace("System");
    convertClass->SetAbstract(true);
    
    auto toStringInt32 = std::make_shared<Method>("ToString", TypeReference::String(), true, false);
    toStringInt32->AddParameter("value", TypeReference::Int32());
    toStringInt32->SetNativeImpl(Convert_ToString_Int32);
    convertClass->AddMethod(toStringInt32);
    
    auto toStringInt64 = std::make_shared<Method>("ToString", TypeReference::String(), true, false);
    toStringInt64->AddParameter("value", TypeReference::Int64());
    toStringInt64->SetNativeImpl(Convert_ToString_Int64);
    convertClass->AddMethod(toStringInt64);
    
    auto toStringDouble = std::make_shared<Method>("ToString", TypeReference::String(), true, false);
    toStringDouble->AddParameter("value", TypeReference::Float64());
    toStringDouble->SetNativeImpl(Convert_ToString_Double);
    convertClass->AddMethod(toStringDouble);

    auto toStringFloat = std::make_shared<Method>("ToString", TypeReference::String(), true, false);
    toStringFloat->AddParameter("value", TypeReference::Float32());
    toStringFloat->SetNativeImpl(Convert_ToString_Float);
    convertClass->AddMethod(toStringFloat);
    
    auto toStringBool = std::make_shared<Method>("ToString", TypeReference::String(), true, false);
    toStringBool->AddParameter("value", TypeReference::Bool());
    toStringBool->SetNativeImpl(Convert_ToString_Bool);
    convertClass->AddMethod(toStringBool);
    
    auto toInt32 = std::make_shared<Method>("ToInt32", TypeReference::Int32(), true, false);
    toInt32->AddParameter("value", TypeReference::String());
    toInt32->SetNativeImpl(Convert_ToInt32);
    convertClass->AddMethod(toInt32);
    
    auto toDouble = std::make_shared<Method>("ToDouble", TypeReference::Float64(), true, false);
    toDouble->AddParameter("value", TypeReference::String());
    toDouble->SetNativeImpl(Convert_ToDouble);
    convertClass->AddMethod(toDouble);

    auto toSingle = std::make_shared<Method>("ToSingle", TypeReference::Float32(), true, false);
    toSingle->AddParameter("value", TypeReference::String());
    toSingle->SetNativeImpl(Convert_ToSingle);
    convertClass->AddMethod(toSingle);
    
    vm->RegisterClass(convertClass);
    
    // Also register System.Convert with lowercase for compatibility
    auto convertClassLower = std::make_shared<Class>("System.convert");
    convertClassLower->SetNamespace("System");
    convertClassLower->SetAbstract(true);
    convertClassLower->AddMethod(toStringInt32);
    convertClassLower->AddMethod(toStringInt64);
    convertClassLower->AddMethod(toStringDouble);
    convertClassLower->AddMethod(toStringFloat);
    convertClassLower->AddMethod(toStringBool);
    convertClassLower->AddMethod(toInt32);
    convertClassLower->AddMethod(toDouble);
    convertClassLower->AddMethod(toSingle);
    vm->RegisterClass(convertClassLower);
}

} // namespace ObjectIR
