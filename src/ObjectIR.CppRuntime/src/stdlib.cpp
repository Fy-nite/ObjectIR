#include "stdlib.hpp"
#include "math_stubs.hpp"
#include "io_stubs.hpp"
#include "collections_stubs.hpp"
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
        vm->WriteOutput(args[0].AsString() + "\n");
    }
    return Value();
}

Value Console_WriteLine_Int32(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsInt32()) {
        vm->WriteOutput(std::to_string(args[0].AsInt32()) + "\n");
    }
    return Value();
}

Value Console_WriteLine_Int64(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsInt64()) {
        vm->WriteOutput(std::to_string(args[0].AsInt64()) + "\n");
    }
    return Value();
}

Value Console_WriteLine_Double(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsFloat64()) {
        vm->WriteOutput(std::to_string(args[0].AsFloat64()) + "\n");
    }
    return Value();
}

// Overload for float32
Value Console_WriteLine_Float(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsFloat32()) {
        vm->WriteOutput(std::to_string(args[0].AsFloat32()) + "\n");
    }
    return Value();
}

Value Console_WriteLine_Bool(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsBool()) {
        vm->WriteOutput((args[0].AsBool() ? "true" : "false") + std::string("\n"));
    }
    return Value();
}

Value Console_WriteLine_Void(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    vm->WriteOutput("\n");
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
// System.Math Implementation
// ============================================================================

Value Math_PI(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    return Value(3.141592653589793);
}

Value Math_E(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    return Value(2.718281828459045);
}

Value Math_Tau(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    return Value(6.283185307179586);
}

Value Math_Sin(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsFloat64()) {
        return Value(std::sin(args[0].AsFloat64()));
    }
    return Value(0.0);
}

Value Math_Cos(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsFloat64()) {
        return Value(std::cos(args[0].AsFloat64()));
    }
    return Value(0.0);
}

Value Math_Tan(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsFloat64()) {
        return Value(std::tan(args[0].AsFloat64()));
    }
    return Value(0.0);
}

Value Math_Asin(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsFloat64()) {
        return Value(std::asin(args[0].AsFloat64()));
    }
    return Value(0.0);
}

Value Math_Acos(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsFloat64()) {
        return Value(std::acos(args[0].AsFloat64()));
    }
    return Value(0.0);
}

Value Math_Atan(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsFloat64()) {
        return Value(std::atan(args[0].AsFloat64()));
    }
    return Value(0.0);
}

Value Math_Atan2(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 2 && args[0].IsFloat64() && args[1].IsFloat64()) {
        return Value(std::atan2(args[0].AsFloat64(), args[1].AsFloat64()));
    }
    return Value(0.0);
}

Value Math_Sinh(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsFloat64()) {
        return Value(std::sinh(args[0].AsFloat64()));
    }
    return Value(0.0);
}

Value Math_Cosh(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsFloat64()) {
        return Value(std::cosh(args[0].AsFloat64()));
    }
    return Value(0.0);
}

Value Math_Tanh(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsFloat64()) {
        return Value(std::tanh(args[0].AsFloat64()));
    }
    return Value(0.0);
}

Value Math_Exp(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsFloat64()) {
        return Value(std::exp(args[0].AsFloat64()));
    }
    return Value(0.0);
}

Value Math_Log(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsFloat64()) {
        if (args.size() >= 2 && args[1].IsFloat64()) {
            // Log with base
            return Value(std::log(args[0].AsFloat64()) / std::log(args[1].AsFloat64()));
        } else {
            // Natural log
            return Value(std::log(args[0].AsFloat64()));
        }
    }
    return Value(0.0);
}

Value Math_Log10(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsFloat64()) {
        return Value(std::log10(args[0].AsFloat64()));
    }
    return Value(0.0);
}

Value Math_Pow(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 2 && args[0].IsFloat64() && args[1].IsFloat64()) {
        return Value(std::pow(args[0].AsFloat64(), args[1].AsFloat64()));
    }
    return Value(0.0);
}

Value Math_Sqrt(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsFloat64()) {
        return Value(std::sqrt(args[0].AsFloat64()));
    }
    return Value(0.0);
}

Value Math_Ceiling(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsFloat64()) {
        return Value(std::ceil(args[0].AsFloat64()));
    }
    return Value(0.0);
}

Value Math_Floor(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsFloat64()) {
        return Value(std::floor(args[0].AsFloat64()));
    }
    return Value(0.0);
}

Value Math_Round(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsFloat64()) {
        if (args.size() >= 2 && args[1].IsInt32()) {
            // Round with digits
            double factor = std::pow(10.0, args[1].AsInt32());
            return Value(std::round(args[0].AsFloat64() * factor) / factor);
        } else {
            return Value(std::round(args[0].AsFloat64()));
        }
    }
    return Value(0.0);
}

Value Math_Truncate(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsFloat64()) {
        return Value(std::trunc(args[0].AsFloat64()));
    }
    return Value(0.0);
}

Value Math_Abs(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsFloat64()) {
        return Value(std::abs(args[0].AsFloat64()));
    }
    return Value(0.0);
}

Value Math_Sign(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsFloat64()) {
        double val = args[0].AsFloat64();
        if (val > 0) return Value(1);
        if (val < 0) return Value(-1);
        return Value(0);
    }
    return Value(0);
}

Value Math_Min(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 2 && args[0].IsFloat64() && args[1].IsFloat64()) {
        return Value(std::min(args[0].AsFloat64(), args[1].AsFloat64()));
    }
    return Value(0.0);
}

Value Math_Max(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 2 && args[0].IsFloat64() && args[1].IsFloat64()) {
        return Value(std::max(args[0].AsFloat64(), args[1].AsFloat64()));
    }
    return Value(0.0);
}

void RegisterMathLibrary(std::shared_ptr<VirtualMachine> vm) {
    // Create System.Math class
    auto mathClass = std::make_shared<Class>("System.Math");
    mathClass->SetNamespace("System");
    mathClass->SetAbstract(true);

    // Constants
    auto pi = std::make_shared<Method>("PI", TypeReference::Float64(), true, false);
    pi->SetNativeImpl(Math_PI);
    mathClass->AddMethod(pi);

    auto e = std::make_shared<Method>("E", TypeReference::Float64(), true, false);
    e->SetNativeImpl(Math_E);
    mathClass->AddMethod(e);

    auto tau = std::make_shared<Method>("Tau", TypeReference::Float64(), true, false);
    tau->SetNativeImpl(Math_Tau);
    mathClass->AddMethod(tau);

    // Trigonometric functions
    auto sin = std::make_shared<Method>("Sin", TypeReference::Float64(), true, false);
    sin->AddParameter("value", TypeReference::Float64());
    sin->SetNativeImpl(Math_Sin);
    mathClass->AddMethod(sin);

    auto cos = std::make_shared<Method>("Cos", TypeReference::Float64(), true, false);
    cos->AddParameter("value", TypeReference::Float64());
    cos->SetNativeImpl(Math_Cos);
    mathClass->AddMethod(cos);

    auto tan = std::make_shared<Method>("Tan", TypeReference::Float64(), true, false);
    tan->AddParameter("value", TypeReference::Float64());
    tan->SetNativeImpl(Math_Tan);
    mathClass->AddMethod(tan);

    // Inverse trigonometric functions
    auto asin = std::make_shared<Method>("Asin", TypeReference::Float64(), true, false);
    asin->AddParameter("value", TypeReference::Float64());
    asin->SetNativeImpl(Math_Asin);
    mathClass->AddMethod(asin);

    auto acos = std::make_shared<Method>("Acos", TypeReference::Float64(), true, false);
    acos->AddParameter("value", TypeReference::Float64());
    acos->SetNativeImpl(Math_Acos);
    mathClass->AddMethod(acos);

    auto atan = std::make_shared<Method>("Atan", TypeReference::Float64(), true, false);
    atan->AddParameter("value", TypeReference::Float64());
    atan->SetNativeImpl(Math_Atan);
    mathClass->AddMethod(atan);

    auto atan2 = std::make_shared<Method>("Atan2", TypeReference::Float64(), true, false);
    atan2->AddParameter("y", TypeReference::Float64());
    atan2->AddParameter("x", TypeReference::Float64());
    atan2->SetNativeImpl(Math_Atan2);
    mathClass->AddMethod(atan2);

    // Hyperbolic functions
    auto sinh = std::make_shared<Method>("Sinh", TypeReference::Float64(), true, false);
    sinh->AddParameter("value", TypeReference::Float64());
    sinh->SetNativeImpl(Math_Sinh);
    mathClass->AddMethod(sinh);

    auto cosh = std::make_shared<Method>("Cosh", TypeReference::Float64(), true, false);
    cosh->AddParameter("value", TypeReference::Float64());
    cosh->SetNativeImpl(Math_Cosh);
    mathClass->AddMethod(cosh);

    auto tanh = std::make_shared<Method>("Tanh", TypeReference::Float64(), true, false);
    tanh->AddParameter("value", TypeReference::Float64());
    tanh->SetNativeImpl(Math_Tanh);
    mathClass->AddMethod(tanh);

    // Exponential and logarithmic functions
    auto exp = std::make_shared<Method>("Exp", TypeReference::Float64(), true, false);
    exp->AddParameter("value", TypeReference::Float64());
    exp->SetNativeImpl(Math_Exp);
    mathClass->AddMethod(exp);

    auto log = std::make_shared<Method>("Log", TypeReference::Float64(), true, false);
    log->AddParameter("value", TypeReference::Float64());
    log->SetNativeImpl(Math_Log);
    mathClass->AddMethod(log);

    auto logWithBase = std::make_shared<Method>("Log", TypeReference::Float64(), true, false);
    logWithBase->AddParameter("value", TypeReference::Float64());
    logWithBase->AddParameter("newBase", TypeReference::Float64());
    logWithBase->SetNativeImpl(Math_Log);
    mathClass->AddMethod(logWithBase);

    auto log10 = std::make_shared<Method>("Log10", TypeReference::Float64(), true, false);
    log10->AddParameter("value", TypeReference::Float64());
    log10->SetNativeImpl(Math_Log10);
    mathClass->AddMethod(log10);

    auto pow = std::make_shared<Method>("Pow", TypeReference::Float64(), true, false);
    pow->AddParameter("x", TypeReference::Float64());
    pow->AddParameter("y", TypeReference::Float64());
    pow->SetNativeImpl(Math_Pow);
    mathClass->AddMethod(pow);

    auto sqrt = std::make_shared<Method>("Sqrt", TypeReference::Float64(), true, false);
    sqrt->AddParameter("value", TypeReference::Float64());
    sqrt->SetNativeImpl(Math_Sqrt);
    mathClass->AddMethod(sqrt);

    // Rounding functions
    auto ceiling = std::make_shared<Method>("Ceiling", TypeReference::Float64(), true, false);
    ceiling->AddParameter("value", TypeReference::Float64());
    ceiling->SetNativeImpl(Math_Ceiling);
    mathClass->AddMethod(ceiling);

    auto floor = std::make_shared<Method>("Floor", TypeReference::Float64(), true, false);
    floor->AddParameter("value", TypeReference::Float64());
    floor->SetNativeImpl(Math_Floor);
    mathClass->AddMethod(floor);

    auto round = std::make_shared<Method>("Round", TypeReference::Float64(), true, false);
    round->AddParameter("value", TypeReference::Float64());
    round->SetNativeImpl(Math_Round);
    mathClass->AddMethod(round);

    auto roundWithDigits = std::make_shared<Method>("Round", TypeReference::Float64(), true, false);
    roundWithDigits->AddParameter("value", TypeReference::Float64());
    roundWithDigits->AddParameter("digits", TypeReference::Int32());
    roundWithDigits->SetNativeImpl(Math_Round);
    mathClass->AddMethod(roundWithDigits);

    auto truncate = std::make_shared<Method>("Truncate", TypeReference::Float64(), true, false);
    truncate->AddParameter("value", TypeReference::Float64());
    truncate->SetNativeImpl(Math_Truncate);
    mathClass->AddMethod(truncate);

    // Sign and absolute value
    auto abs = std::make_shared<Method>("Abs", TypeReference::Float64(), true, false);
    abs->AddParameter("value", TypeReference::Float64());
    abs->SetNativeImpl(Math_Abs);
    mathClass->AddMethod(abs);

    auto sign = std::make_shared<Method>("Sign", TypeReference::Int32(), true, false);
    sign->AddParameter("value", TypeReference::Float64());
    sign->SetNativeImpl(Math_Sign);
    mathClass->AddMethod(sign);

    // Min/Max functions
    auto min = std::make_shared<Method>("Min", TypeReference::Float64(), true, false);
    min->AddParameter("val1", TypeReference::Float64());
    min->AddParameter("val2", TypeReference::Float64());
    min->SetNativeImpl(Math_Min);
    mathClass->AddMethod(min);

    auto max = std::make_shared<Method>("Max", TypeReference::Float64(), true, false);
    max->AddParameter("val1", TypeReference::Float64());
    max->AddParameter("val2", TypeReference::Float64());
    max->SetNativeImpl(Math_Max);
    mathClass->AddMethod(max);

    vm->RegisterClass(mathClass);

    // Also register with lowercase name for compatibility
    auto mathClassLower = std::make_shared<Class>("System.math");
    mathClassLower->SetNamespace("System");
    mathClassLower->SetAbstract(true);
    // Copy all methods to lowercase version
    mathClassLower->AddMethod(pi);
    mathClassLower->AddMethod(e);
    mathClassLower->AddMethod(tau);
    mathClassLower->AddMethod(sin);
    mathClassLower->AddMethod(cos);
    mathClassLower->AddMethod(tan);
    mathClassLower->AddMethod(asin);
    mathClassLower->AddMethod(acos);
    mathClassLower->AddMethod(atan);
    mathClassLower->AddMethod(atan2);
    mathClassLower->AddMethod(sinh);
    mathClassLower->AddMethod(cosh);
    mathClassLower->AddMethod(tanh);
    mathClassLower->AddMethod(exp);
    mathClassLower->AddMethod(log);
    mathClassLower->AddMethod(logWithBase);
    mathClassLower->AddMethod(log10);
    mathClassLower->AddMethod(pow);
    mathClassLower->AddMethod(sqrt);
    mathClassLower->AddMethod(ceiling);
    mathClassLower->AddMethod(floor);
    mathClassLower->AddMethod(round);
    mathClassLower->AddMethod(roundWithDigits);
    mathClassLower->AddMethod(truncate);
    mathClassLower->AddMethod(abs);
    mathClassLower->AddMethod(sign);
    mathClassLower->AddMethod(min);
    mathClassLower->AddMethod(max);
    vm->RegisterClass(mathClassLower);
}

// ============================================================================
// System.IO Implementation
// ============================================================================

Value Stream_Dispose(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    // Base stream dispose - subclasses should override
    return Value();
}

Value Stream_CanRead(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    // Default implementation - subclasses should override
    return Value(false);
}

Value Stream_CanWrite(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    // Default implementation - subclasses should override
    return Value(false);
}

Value Stream_CanSeek(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    // Default implementation - subclasses should override
    return Value(false);
}

Value Stream_Length(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    // Default implementation - subclasses should override
    return Value(static_cast<int64_t>(0));
}

Value Stream_Position(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    // Default implementation - subclasses should override
    return Value(static_cast<int64_t>(0));
}

Value Stream_SetPosition(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    // Default implementation - subclasses should override
    return Value();
}

Value Stream_Read(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    // Default implementation - subclasses should override
    return Value(static_cast<int32_t>(0));
}

Value Stream_Write(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    // Default implementation - subclasses should override
    return Value();
}

Value Stream_Flush(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    // Default implementation - subclasses should override
    return Value();
}

Value Stream_Close(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    // Default implementation - subclasses should override
    return Value();
}

Value FileStream_ctor(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 2 && args[0].IsString() && args[1].IsInt32()) {
        std::string path = args[0].AsString();
        int32_t mode = args[1].AsInt32();

        // Store file stream in object data
        auto fileStream = std::make_shared<std::fstream>();
        std::ios_base::openmode openMode = std::ios_base::binary;

        if (mode & 1) openMode |= std::ios_base::in;  // Read
        if (mode & 2) openMode |= std::ios_base::out; // Write
        if (mode & 4) openMode |= std::ios_base::app; // Append
        if (mode & 8) openMode |= std::ios_base::trunc; // Truncate

        fileStream->open(path, openMode);
        thisPtr->SetData(fileStream);
    }
    return Value();
}

Value FileStream_Dispose(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    auto fileStream = std::static_pointer_cast<std::fstream>(thisPtr->GetData<std::fstream>());
    if (fileStream && fileStream->is_open()) {
        fileStream->close();
    }
    return Value();
}

Value FileStream_CanRead(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    auto fileStream = std::static_pointer_cast<std::fstream>(thisPtr->GetData<std::fstream>());
    return Value(fileStream && fileStream->is_open() && (fileStream->flags() & std::ios_base::in));
}

Value FileStream_CanWrite(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    auto fileStream = std::static_pointer_cast<std::fstream>(thisPtr->GetData<std::fstream>());
    return Value(fileStream && fileStream->is_open() && (fileStream->flags() & std::ios_base::out));
}

Value FileStream_CanSeek(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    auto fileStream = std::static_pointer_cast<std::fstream>(thisPtr->GetData<std::fstream>());
    return Value(fileStream && fileStream->is_open());
}

Value FileStream_Length(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    auto fileStream = std::static_pointer_cast<std::fstream>(thisPtr->GetData<std::fstream>());
    if (fileStream && fileStream->is_open()) {
        auto currentPos = fileStream->tellg();
        fileStream->seekg(0, std::ios::end);
        auto length = fileStream->tellg();
        fileStream->seekg(currentPos);
        return Value(static_cast<int64_t>(length));
    }
    return Value(static_cast<int64_t>(0));
}

Value FileStream_Position(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    auto fileStream = std::static_pointer_cast<std::fstream>(thisPtr->GetData<std::fstream>());
    if (fileStream && fileStream->is_open()) {
        return Value(static_cast<int64_t>(fileStream->tellg()));
    }
    return Value(static_cast<int64_t>(0));
}

Value FileStream_SetPosition(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsInt64()) {
        auto fileStream = std::static_pointer_cast<std::fstream>(thisPtr->GetData<std::fstream>());
        if (fileStream && fileStream->is_open()) {
            fileStream->seekg(args[0].AsInt64());
        }
    }
    return Value();
}

Value FileStream_Read(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 3 && args[0].IsObject() && args[1].IsInt32() && args[2].IsInt32()) {
        auto fileStream = std::static_pointer_cast<std::fstream>(thisPtr->GetData<std::fstream>());
        if (fileStream && fileStream->is_open()) {
            // args[0] is buffer array, args[1] is offset, args[2] is count
            // For simplicity, we'll read directly into a byte array
            int32_t count = args[2].AsInt32();
            std::vector<uint8_t> buffer(count);
            fileStream->read(reinterpret_cast<char*>(buffer.data()), count);
            auto bytesRead = fileStream->gcount();

            // Create a byte array object to return
            auto byteArray = vm->CreateArray(TypeReference::UInt8(), bytesRead);
            for (int32_t i = 0; i < bytesRead; ++i) {
                byteArray->SetElement(i, Value(static_cast<int32_t>(buffer[i])));
            }
            return Value(byteArray);
        }
    }
    return Value(static_cast<int32_t>(0));
}

Value FileStream_Write(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 3 && args[0].IsObject() && args[1].IsInt32() && args[2].IsInt32()) {
        auto fileStream = std::static_pointer_cast<std::fstream>(thisPtr->GetData<std::fstream>());
        if (fileStream && fileStream->is_open()) {
            // args[0] is buffer array, args[1] is offset, args[2] is count
            auto buffer = std::static_pointer_cast<Array>(args[0].AsObject());
            int32_t offset = args[1].AsInt32();
            int32_t count = args[2].AsInt32();

            for (int32_t i = 0; i < count; ++i) {
                auto byteVal = buffer->GetElement(offset + i);
                if (byteVal.IsInt32()) {
                    char byte = static_cast<char>(byteVal.AsInt32());
                    fileStream->write(&byte, 1);
                }
            }
        }
    }
    return Value();
}

Value FileStream_Flush(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    auto fileStream = std::static_pointer_cast<std::fstream>(thisPtr->GetData<std::fstream>());
    if (fileStream && fileStream->is_open()) {
        fileStream->flush();
    }
    return Value();
}

Value FileStream_Close(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    auto fileStream = std::static_pointer_cast<std::fstream>(thisPtr->GetData<std::fstream>());
    if (fileStream && fileStream->is_open()) {
        fileStream->close();
    }
    return Value();
}

Value StreamReader_ctor(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsObject()) {
        // args[0] is the stream to read from
        thisPtr->SetData(args[0].AsObject());
    }
    return Value();
}

Value StreamReader_ReadLine(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    auto stream = std::static_pointer_cast<Object>(thisPtr->GetData<Object>());
    if (stream) {
        // For simplicity, assume it's a FileStream and read a line
        // In a real implementation, this would use the stream's Read method
        std::string line;
        std::getline(std::cin, line); // Placeholder - should read from stream
        return Value(line);
    }
    return Value(std::string(""));
}

Value StreamReader_ReadToEnd(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    auto stream = std::static_pointer_cast<Object>(thisPtr->GetData<Object>());
    if (stream) {
        // Placeholder implementation
        return Value(std::string(""));
    }
    return Value(std::string(""));
}

Value StreamReader_Close(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    auto stream = std::static_pointer_cast<Object>(thisPtr->GetData<Object>());
    if (stream) {
        // Call stream's Close method
        // stream->Close();
    }
    return Value();
}

Value StreamWriter_ctor(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsObject()) {
        // args[0] is the stream to write to
        thisPtr->SetData(args[0].AsObject());
    }
    return Value();
}

Value StreamWriter_Write(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsString()) {
        auto stream = std::static_pointer_cast<Object>(thisPtr->GetData<Object>());
        if (stream) {
            // For simplicity, write to stdout
            std::cout << args[0].AsString();
        }
    }
    return Value();
}

Value StreamWriter_WriteLine(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsString()) {
        auto stream = std::static_pointer_cast<Object>(thisPtr->GetData<Object>());
        if (stream) {
            // For simplicity, write to stdout
            std::cout << args[0].AsString() << std::endl;
        }
    }
    return Value();
}

Value StreamWriter_Flush(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    auto stream = std::static_pointer_cast<Object>(thisPtr->GetData<Object>());
    if (stream) {
        // Call stream's Flush method
        // stream->Flush();
        std::cout.flush();
    }
    return Value();
}

Value StreamWriter_Close(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    auto stream = std::static_pointer_cast<Object>(thisPtr->GetData<Object>());
    if (stream) {
        // Call stream's Close method
        // stream->Close();
    }
    return Value();
}

Value File_Exists(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsString()) {
        std::string path = args[0].AsString();
        std::ifstream file(path);
        return Value(file.good());
    }
    return Value(false);
}

Value File_ReadAllText(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsString()) {
        std::string path = args[0].AsString();
        std::ifstream file(path);
        if (file.good()) {
            std::string content((std::istreambuf_iterator<char>(file)),
                               std::istreambuf_iterator<char>());
            return Value(content);
        }
    }
    return Value(std::string(""));
}

Value File_WriteAllText(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 2 && args[0].IsString() && args[1].IsString()) {
        std::string path = args[0].AsString();
        std::string content = args[1].AsString();
        std::ofstream file(path);
        if (file.good()) {
            file << content;
        }
    }
    return Value();
}

Value File_ReadAllLines(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsString()) {
        std::string path = args[0].AsString();
        std::ifstream file(path);
        if (file.good()) {
            std::vector<std::string> lines;
            std::string line;
            while (std::getline(file, line)) {
                lines.push_back(line);
            }
            // Create string array
            auto stringArray = vm->CreateArray(TypeReference::String(), lines.size());
            for (size_t i = 0; i < lines.size(); ++i) {
                stringArray->SetElement(static_cast<int32_t>(i), Value(lines[i]));
            }
            return Value(stringArray);
        }
    }
    return Value(); // null
}

Value File_WriteAllLines(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 2 && args[0].IsString() && args[1].IsObject()) {
        std::string path = args[0].AsString();
        auto linesArray = std::static_pointer_cast<Array>(args[1].AsObject());
        if (linesArray) {
            std::ofstream file(path);
            if (file.good()) {
                int32_t length = linesArray->GetArrayLength();
                for (int32_t i = 0; i < length; ++i) {
                    auto lineVal = linesArray->GetElement(i);
                    if (lineVal.IsString()) {
                        file << lineVal.AsString() << std::endl;
                    }
                }
            }
        }
    }
    return Value();
}

Value File_Delete(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsString()) {
        std::string path = args[0].AsString();
        return Value(std::remove(path.c_str()) == 0);
    }
    return Value(false);
}

void RegisterIOLibrary(std::shared_ptr<VirtualMachine> vm) {
    // Create System.IO.Stream base class
    auto streamClass = std::make_shared<Class>("System.IO.Stream");
    streamClass->SetNamespace("System.IO");

    auto dispose = std::make_shared<Method>("Dispose", TypeReference::Void(), false, false);
    dispose->SetNativeImpl(Stream_Dispose);
    streamClass->AddMethod(dispose);

    auto canRead = std::make_shared<Method>("get_CanRead", TypeReference::Bool(), false, false);
    canRead->SetNativeImpl(Stream_CanRead);
    streamClass->AddMethod(canRead);

    auto canWrite = std::make_shared<Method>("get_CanWrite", TypeReference::Bool(), false, false);
    canWrite->SetNativeImpl(Stream_CanWrite);
    streamClass->AddMethod(canWrite);

    auto canSeek = std::make_shared<Method>("get_CanSeek", TypeReference::Bool(), false, false);
    canSeek->SetNativeImpl(Stream_CanSeek);
    streamClass->AddMethod(canSeek);

    auto length = std::make_shared<Method>("get_Length", TypeReference::Int64(), false, false);
    length->SetNativeImpl(Stream_Length);
    streamClass->AddMethod(length);

    auto position = std::make_shared<Method>("get_Position", TypeReference::Int64(), false, false);
    position->SetNativeImpl(Stream_Position);
    streamClass->AddMethod(position);

    auto setPosition = std::make_shared<Method>("set_Position", TypeReference::Void(), false, false);
    setPosition->AddParameter("value", TypeReference::Int64());
    setPosition->SetNativeImpl(Stream_SetPosition);
    streamClass->AddMethod(setPosition);

    auto read = std::make_shared<Method>("Read", TypeReference::Int32(), false, false);
    read->AddParameter("buffer", TypeReference::Object());
    read->AddParameter("offset", TypeReference::Int32());
    read->AddParameter("count", TypeReference::Int32());
    read->SetNativeImpl(Stream_Read);
    streamClass->AddMethod(read);

    auto write = std::make_shared<Method>("Write", TypeReference::Void(), false, false);
    write->AddParameter("buffer", TypeReference::Object());
    write->AddParameter("offset", TypeReference::Int32());
    write->AddParameter("count", TypeReference::Int32());
    write->SetNativeImpl(Stream_Write);
    streamClass->AddMethod(write);

    auto flush = std::make_shared<Method>("Flush", TypeReference::Void(), false, false);
    flush->SetNativeImpl(Stream_Flush);
    streamClass->AddMethod(flush);

    auto close = std::make_shared<Method>("Close", TypeReference::Void(), false, false);
    close->SetNativeImpl(Stream_Close);
    streamClass->AddMethod(close);

    vm->RegisterClass(streamClass);

    // Create System.IO.FileStream class
    auto fileStreamClass = std::make_shared<Class>("System.IO.FileStream");
    fileStreamClass->SetNamespace("System.IO");
    fileStreamClass->SetBaseClass(streamClass);

    auto fsCtor = std::make_shared<Method>(".ctor", TypeReference::Void(), false, false);
    fsCtor->AddParameter("path", TypeReference::String());
    fsCtor->AddParameter("mode", TypeReference::Int32());
    fsCtor->SetNativeImpl(FileStream_ctor);
    fileStreamClass->AddMethod(fsCtor);

    auto fsDispose = std::make_shared<Method>("Dispose", TypeReference::Void(), false, false);
    fsDispose->SetNativeImpl(FileStream_Dispose);
    fileStreamClass->AddMethod(fsDispose);

    auto fsCanRead = std::make_shared<Method>("get_CanRead", TypeReference::Bool(), false, false);
    fsCanRead->SetNativeImpl(FileStream_CanRead);
    fileStreamClass->AddMethod(fsCanRead);

    auto fsCanWrite = std::make_shared<Method>("get_CanWrite", TypeReference::Bool(), false, false);
    fsCanWrite->SetNativeImpl(FileStream_CanWrite);
    fileStreamClass->AddMethod(fsCanWrite);

    auto fsCanSeek = std::make_shared<Method>("get_CanSeek", TypeReference::Bool(), false, false);
    fsCanSeek->SetNativeImpl(FileStream_CanSeek);
    fileStreamClass->AddMethod(fsCanSeek);

    auto fsLength = std::make_shared<Method>("get_Length", TypeReference::Int64(), false, false);
    fsLength->SetNativeImpl(FileStream_Length);
    fileStreamClass->AddMethod(fsLength);

    auto fsPosition = std::make_shared<Method>("get_Position", TypeReference::Int64(), false, false);
    fsPosition->SetNativeImpl(FileStream_Position);
    fileStreamClass->AddMethod(fsPosition);

    auto fsSetPosition = std::make_shared<Method>("set_Position", TypeReference::Void(), false, false);
    fsSetPosition->AddParameter("value", TypeReference::Int64());
    fsSetPosition->SetNativeImpl(FileStream_SetPosition);
    fileStreamClass->AddMethod(fsSetPosition);

    auto fsRead = std::make_shared<Method>("Read", TypeReference::Int32(), false, false);
    fsRead->AddParameter("buffer", TypeReference::Object());
    fsRead->AddParameter("offset", TypeReference::Int32());
    fsRead->AddParameter("count", TypeReference::Int32());
    fsRead->SetNativeImpl(FileStream_Read);
    fileStreamClass->AddMethod(fsRead);

    auto fsWrite = std::make_shared<Method>("Write", TypeReference::Void(), false, false);
    fsWrite->AddParameter("buffer", TypeReference::Object());
    fsWrite->AddParameter("offset", TypeReference::Int32());
    fsWrite->AddParameter("count", TypeReference::Int32());
    fsWrite->SetNativeImpl(FileStream_Write);
    fileStreamClass->AddMethod(fsWrite);

    auto fsFlush = std::make_shared<Method>("Flush", TypeReference::Void(), false, false);
    fsFlush->SetNativeImpl(FileStream_Flush);
    fileStreamClass->AddMethod(fsFlush);

    auto fsClose = std::make_shared<Method>("Close", TypeReference::Void(), false, false);
    fsClose->SetNativeImpl(FileStream_Close);
    fileStreamClass->AddMethod(fsClose);

    vm->RegisterClass(fileStreamClass);

    // Create System.IO.StreamReader class
    auto streamReaderClass = std::make_shared<Class>("System.IO.StreamReader");
    streamReaderClass->SetNamespace("System.IO");

    auto srCtor = std::make_shared<Method>(".ctor", TypeReference::Void(), false, false);
    srCtor->AddParameter("stream", TypeReference::Object());
    srCtor->SetNativeImpl(StreamReader_ctor);
    streamReaderClass->AddMethod(srCtor);

    auto readLine = std::make_shared<Method>("ReadLine", TypeReference::String(), false, false);
    readLine->SetNativeImpl(StreamReader_ReadLine);
    streamReaderClass->AddMethod(readLine);

    auto readToEnd = std::make_shared<Method>("ReadToEnd", TypeReference::String(), false, false);
    readToEnd->SetNativeImpl(StreamReader_ReadToEnd);
    streamReaderClass->AddMethod(readToEnd);

    auto srClose = std::make_shared<Method>("Close", TypeReference::Void(), false, false);
    srClose->SetNativeImpl(StreamReader_Close);
    streamReaderClass->AddMethod(srClose);

    vm->RegisterClass(streamReaderClass);

    // Create System.IO.StreamWriter class
    auto streamWriterClass = std::make_shared<Class>("System.IO.StreamWriter");
    streamWriterClass->SetNamespace("System.IO");

    auto swCtor = std::make_shared<Method>(".ctor", TypeReference::Void(), false, false);
    swCtor->AddParameter("stream", TypeReference::Object());
    swCtor->SetNativeImpl(StreamWriter_ctor);
    streamWriterClass->AddMethod(swCtor);

    auto swWrite = std::make_shared<Method>("Write", TypeReference::Void(), false, false);
    swWrite->AddParameter("value", TypeReference::String());
    swWrite->SetNativeImpl(StreamWriter_Write);
    streamWriterClass->AddMethod(swWrite);

    auto swWriteLine = std::make_shared<Method>("WriteLine", TypeReference::Void(), false, false);
    swWriteLine->AddParameter("value", TypeReference::String());
    swWriteLine->SetNativeImpl(StreamWriter_WriteLine);
    streamWriterClass->AddMethod(swWriteLine);

    auto swFlush = std::make_shared<Method>("Flush", TypeReference::Void(), false, false);
    swFlush->SetNativeImpl(StreamWriter_Flush);
    streamWriterClass->AddMethod(swFlush);

    auto swClose = std::make_shared<Method>("Close", TypeReference::Void(), false, false);
    swClose->SetNativeImpl(StreamWriter_Close);
    streamWriterClass->AddMethod(swClose);

    vm->RegisterClass(streamWriterClass);

    // Create System.IO.File static class
    auto fileClass = std::make_shared<Class>("System.IO.File");
    fileClass->SetNamespace("System.IO");
    fileClass->SetAbstract(true);

    auto exists = std::make_shared<Method>("Exists", TypeReference::Bool(), true, false);
    exists->AddParameter("path", TypeReference::String());
    exists->SetNativeImpl(File_Exists);
    fileClass->AddMethod(exists);

    auto readAllText = std::make_shared<Method>("ReadAllText", TypeReference::String(), true, false);
    readAllText->AddParameter("path", TypeReference::String());
    readAllText->SetNativeImpl(File_ReadAllText);
    fileClass->AddMethod(readAllText);

    auto writeAllText = std::make_shared<Method>("WriteAllText", TypeReference::Void(), true, false);
    writeAllText->AddParameter("path", TypeReference::String());
    writeAllText->AddParameter("contents", TypeReference::String());
    writeAllText->SetNativeImpl(File_WriteAllText);
    fileClass->AddMethod(writeAllText);

    auto readAllLines = std::make_shared<Method>("ReadAllLines", TypeReference::Object(), true, false);
    readAllLines->AddParameter("path", TypeReference::String());
    readAllLines->SetNativeImpl(File_ReadAllLines);
    fileClass->AddMethod(readAllLines);

    auto writeAllLines = std::make_shared<Method>("WriteAllLines", TypeReference::Void(), true, false);
    writeAllLines->AddParameter("path", TypeReference::String());
    writeAllLines->AddParameter("contents", TypeReference::Object());
    writeAllLines->SetNativeImpl(File_WriteAllLines);
    fileClass->AddMethod(writeAllLines);

    auto deleteFile = std::make_shared<Method>("Delete", TypeReference::Bool(), true, false);
    deleteFile->AddParameter("path", TypeReference::String());
    deleteFile->SetNativeImpl(File_Delete);
    fileClass->AddMethod(deleteFile);

    vm->RegisterClass(fileClass);
}

// ============================================================================
// System.Collections.Generic Implementation
// ============================================================================

Value List_ctor(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    // Initialize empty list
    auto list = std::make_shared<std::vector<Value>>();
    thisPtr->SetData(list);
    return Value();
}

Value List_ctor_Capacity(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsInt32()) {
        int32_t capacity = args[0].AsInt32();
        auto list = std::make_shared<std::vector<Value>>();
        list->reserve(capacity);
        thisPtr->SetData(list);
    }
    return Value();
}

Value List_get_Count(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    auto list = std::static_pointer_cast<std::vector<Value>>(thisPtr->GetData<std::vector<Value>>());
    if (list) {
        return Value(static_cast<int32_t>(list->size()));
    }
    return Value(static_cast<int32_t>(0));
}

Value List_get_Capacity(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    auto list = std::static_pointer_cast<std::vector<Value>>(thisPtr->GetData<std::vector<Value>>());
    if (list) {
        return Value(static_cast<int32_t>(list->capacity()));
    }
    return Value(static_cast<int32_t>(0));
}

Value List_set_Capacity(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsInt32()) {
        auto list = std::static_pointer_cast<std::vector<Value>>(thisPtr->GetData<std::vector<Value>>());
        if (list) {
            int32_t capacity = args[0].AsInt32();
            list->reserve(capacity);
        }
    }
    return Value();
}

Value List_get_Item(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsInt32()) {
        auto list = std::static_pointer_cast<std::vector<Value>>(thisPtr->GetData<std::vector<Value>>());
        if (list) {
            int32_t index = args[0].AsInt32();
            if (index >= 0 && index < static_cast<int32_t>(list->size())) {
                return (*list)[index];
            }
        }
    }
    return Value(); // null or default
}

Value List_set_Item(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 2 && args[0].IsInt32()) {
        auto list = std::static_pointer_cast<std::vector<Value>>(thisPtr->GetData<std::vector<Value>>());
        if (list) {
            int32_t index = args[0].AsInt32();
            if (index >= 0 && index < static_cast<int32_t>(list->size())) {
                (*list)[index] = args[1];
            }
        }
    }
    return Value();
}

Value List_Add(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1) {
        auto list = std::static_pointer_cast<std::vector<Value>>(thisPtr->GetData<std::vector<Value>>());
        if (list) {
            list->push_back(args[0]);
        }
    }
    return Value();
}

Value List_AddRange(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsObject()) {
        auto list = std::static_pointer_cast<std::vector<Value>>(thisPtr->GetData<std::vector<Value>>());
        auto collection = std::static_pointer_cast<Array>(args[0].AsObject());
        if (list && collection) {
            int32_t length = collection->GetArrayLength();
            for (int32_t i = 0; i < length; ++i) {
                list->push_back(collection->GetElement(i));
            }
        }
    }
    return Value();
}

Value List_Insert(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 2 && args[0].IsInt32()) {
        auto list = std::static_pointer_cast<std::vector<Value>>(thisPtr->GetData<std::vector<Value>>());
        if (list) {
            int32_t index = args[0].AsInt32();
            if (index >= 0 && index <= static_cast<int32_t>(list->size())) {
                list->insert(list->begin() + index, args[1]);
            }
        }
    }
    return Value();
}

Value List_RemoveAt(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1 && args[0].IsInt32()) {
        auto list = std::static_pointer_cast<std::vector<Value>>(thisPtr->GetData<std::vector<Value>>());
        if (list) {
            int32_t index = args[0].AsInt32();
            if (index >= 0 && index < static_cast<int32_t>(list->size())) {
                list->erase(list->begin() + index);
            }
        }
    }
    return Value();
}

Value List_Remove(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1) {
        auto list = std::static_pointer_cast<std::vector<Value>>(thisPtr->GetData<std::vector<Value>>());
        if (list) {
            auto it = std::find(list->begin(), list->end(), args[0]);
            if (it != list->end()) {
                list->erase(it);
                return Value(true);
            }
        }
    }
    return Value(false);
}

Value List_Clear(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    auto list = std::static_pointer_cast<std::vector<Value>>(thisPtr->GetData<std::vector<Value>>());
    if (list) {
        list->clear();
    }
    return Value();
}

Value List_Contains(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1) {
        auto list = std::static_pointer_cast<std::vector<Value>>(thisPtr->GetData<std::vector<Value>>());
        if (list) {
            return Value(std::find(list->begin(), list->end(), args[0]) != list->end());
        }
    }
    return Value(false);
}

Value List_IndexOf(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1) {
        auto list = std::static_pointer_cast<std::vector<Value>>(thisPtr->GetData<std::vector<Value>>());
        if (list) {
            auto it = std::find(list->begin(), list->end(), args[0]);
            if (it != list->end()) {
                return Value(static_cast<int32_t>(std::distance(list->begin(), it)));
            }
        }
    }
    return Value(static_cast<int32_t>(-1));
}

Value List_ToArray(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    auto list = std::static_pointer_cast<std::vector<Value>>(thisPtr->GetData<std::vector<Value>>());
    if (list) {
        // Create array of the same type as the list elements
        // For simplicity, assume all elements are the same type
        auto array = vm->CreateArray(TypeReference::Object(), list->size());
        for (size_t i = 0; i < list->size(); ++i) {
            array->SetElement(static_cast<int32_t>(i), (*list)[i]);
        }
        return Value(array);
    }
    return Value(); // null
}

Value Dictionary_ctor(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    // Initialize empty dictionary
    auto dict = std::make_shared<std::unordered_map<Value, Value>>();
    thisPtr->SetData(dict);
    return Value();
}

Value Dictionary_get_Count(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    auto dict = std::static_pointer_cast<std::unordered_map<Value, Value>>(thisPtr->GetData<std::unordered_map<Value, Value>>());
    if (dict) {
        return Value(static_cast<int32_t>(dict->size()));
    }
    return Value(static_cast<int32_t>(0));
}

Value Dictionary_get_Item(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1) {
        auto dict = std::static_pointer_cast<std::unordered_map<Value, Value>>(thisPtr->GetData<std::unordered_map<Value, Value>>());
        if (dict) {
            auto it = dict->find(args[0]);
            if (it != dict->end()) {
                return it->second;
            }
        }
    }
    return Value(); // null or default
}

Value Dictionary_set_Item(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 2) {
        auto dict = std::static_pointer_cast<std::unordered_map<Value, Value>>(thisPtr->GetData<std::unordered_map<Value, Value>>());
        if (dict) {
            (*dict)[args[0]] = args[1];
        }
    }
    return Value();
}

Value Dictionary_Add(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 2) {
        auto dict = std::static_pointer_cast<std::unordered_map<Value, Value>>(thisPtr->GetData<std::unordered_map<Value, Value>>());
        if (dict) {
            (*dict)[args[0]] = args[1];
        }
    }
    return Value();
}

Value Dictionary_Remove(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1) {
        auto dict = std::static_pointer_cast<std::unordered_map<Value, Value>>(thisPtr->GetData<std::unordered_map<Value, Value>>());
        if (dict) {
            return Value(dict->erase(args[0]) > 0);
        }
    }
    return Value(false);
}

Value Dictionary_Clear(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    auto dict = std::static_pointer_cast<std::unordered_map<Value, Value>>(thisPtr->GetData<std::unordered_map<Value, Value>>());
    if (dict) {
        dict->clear();
    }
    return Value();
}

Value Dictionary_ContainsKey(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1) {
        auto dict = std::static_pointer_cast<std::unordered_map<Value, Value>>(thisPtr->GetData<std::unordered_map<Value, Value>>());
        if (dict) {
            return Value(dict->find(args[0]) != dict->end());
        }
    }
    return Value(false);
}

Value Dictionary_ContainsValue(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1) {
        auto dict = std::static_pointer_cast<std::unordered_map<Value, Value>>(thisPtr->GetData<std::unordered_map<Value, Value>>());
        if (dict) {
            for (const auto& pair : *dict) {
                if (pair.second == args[0]) {
                    return Value(true);
                }
            }
        }
    }
    return Value(false);
}

Value Dictionary_get_Keys(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    auto dict = std::static_pointer_cast<std::unordered_map<Value, Value>>(thisPtr->GetData<std::unordered_map<Value, Value>>());
    if (dict) {
        auto keyCollection = vm->CreateArray(TypeReference::Object(), dict->size());
        int32_t index = 0;
        for (const auto& pair : *dict) {
            keyCollection->SetElement(index++, pair.first);
        }
        return Value(keyCollection);
    }
    return Value(); // null
}

Value Dictionary_get_Values(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    auto dict = std::static_pointer_cast<std::unordered_map<Value, Value>>(thisPtr->GetData<std::unordered_map<Value, Value>>());
    if (dict) {
        auto valueCollection = vm->CreateArray(TypeReference::Object(), dict->size());
        int32_t index = 0;
        for (const auto& pair : *dict) {
            valueCollection->SetElement(index++, pair.second);
        }
        return Value(valueCollection);
    }
    return Value(); // null
}

Value Queue_ctor(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    // Initialize empty queue
    auto queue = std::make_shared<std::deque<Value>>();
    thisPtr->SetData(queue);
    return Value();
}

Value Queue_get_Count(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    auto queue = std::static_pointer_cast<std::deque<Value>>(thisPtr->GetData<std::deque<Value>>());
    if (queue) {
        return Value(static_cast<int32_t>(queue->size()));
    }
    return Value(static_cast<int32_t>(0));
}

Value Queue_Enqueue(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1) {
        auto queue = std::static_pointer_cast<std::deque<Value>>(thisPtr->GetData<std::deque<Value>>());
        if (queue) {
            queue->push_back(args[0]);
        }
    }
    return Value();
}

Value Queue_Dequeue(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    auto queue = std::static_pointer_cast<std::deque<Value>>(thisPtr->GetData<std::deque<Value>>());
    if (queue && !queue->empty()) {
        Value front = queue->front();
        queue->pop_front();
        return front;
    }
    return Value(); // null or default
}

Value Queue_Peek(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    auto queue = std::static_pointer_cast<std::deque<Value>>(thisPtr->GetData<std::deque<Value>>());
    if (queue && !queue->empty()) {
        return queue->front();
    }
    return Value(); // null or default
}

Value Queue_Clear(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    auto queue = std::static_pointer_cast<std::deque<Value>>(thisPtr->GetData<std::deque<Value>>());
    if (queue) {
        queue->clear();
    }
    return Value();
}

Value Queue_Contains(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1) {
        auto queue = std::static_pointer_cast<std::deque<Value>>(thisPtr->GetData<std::deque<Value>>());
        if (queue) {
            return Value(std::find(queue->begin(), queue->end(), args[0]) != queue->end());
        }
    }
    return Value(false);
}

Value Stack_ctor(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    // Initialize empty stack
    auto stack = std::make_shared<std::vector<Value>>();
    thisPtr->SetData(stack);
    return Value();
}

Value Stack_get_Count(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    auto stack = std::static_pointer_cast<std::vector<Value>>(thisPtr->GetData<std::vector<Value>>());
    if (stack) {
        return Value(static_cast<int32_t>(stack->size()));
    }
    return Value(static_cast<int32_t>(0));
}

Value Stack_Push(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1) {
        auto stack = std::static_pointer_cast<std::vector<Value>>(thisPtr->GetData<std::vector<Value>>());
        if (stack) {
            stack->push_back(args[0]);
        }
    }
    return Value();
}

Value Stack_Pop(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    auto stack = std::static_pointer_cast<std::vector<Value>>(thisPtr->GetData<std::vector<Value>>());
    if (stack && !stack->empty()) {
        Value top = stack->back();
        stack->pop_back();
        return top;
    }
    return Value(); // null or default
}

Value Stack_Peek(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    auto stack = std::static_pointer_cast<std::vector<Value>>(thisPtr->GetData<std::vector<Value>>());
    if (stack && !stack->empty()) {
        return stack->back();
    }
    return Value(); // null or default
}

Value Stack_Clear(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    auto stack = std::static_pointer_cast<std::vector<Value>>(thisPtr->GetData<std::vector<Value>>());
    if (stack) {
        stack->clear();
    }
    return Value();
}

Value Stack_Contains(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1) {
        auto stack = std::static_pointer_cast<std::vector<Value>>(thisPtr->GetData<std::vector<Value>>());
        if (stack) {
            return Value(std::find(stack->begin(), stack->end(), args[0]) != stack->end());
        }
    }
    return Value(false);
}

Value HashSet_ctor(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    // Initialize empty hash set
    auto hashSet = std::make_shared<std::unordered_set<Value>>();
    thisPtr->SetData(hashSet);
    return Value();
}

Value HashSet_get_Count(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    auto hashSet = std::static_pointer_cast<std::unordered_set<Value>>(thisPtr->GetData<std::unordered_set<Value>>());
    if (hashSet) {
        return Value(static_cast<int32_t>(hashSet->size()));
    }
    return Value(static_cast<int32_t>(0));
}

Value HashSet_Add(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1) {
        auto hashSet = std::static_pointer_cast<std::unordered_set<Value>>(thisPtr->GetData<std::unordered_set<Value>>());
        if (hashSet) {
            auto result = hashSet->insert(args[0]);
            return Value(result.second); // true if inserted, false if already exists
        }
    }
    return Value(false);
}

Value HashSet_Remove(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1) {
        auto hashSet = std::static_pointer_cast<std::unordered_set<Value>>(thisPtr->GetData<std::unordered_set<Value>>());
        if (hashSet) {
            return Value(hashSet->erase(args[0]) > 0);
        }
    }
    return Value(false);
}

Value HashSet_Clear(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    auto hashSet = std::static_pointer_cast<std::unordered_set<Value>>(thisPtr->GetData<std::unordered_set<Value>>());
    if (hashSet) {
        hashSet->clear();
    }
    return Value();
}

Value HashSet_Contains(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm) {
    if (args.size() >= 1) {
        auto hashSet = std::static_pointer_cast<std::unordered_set<Value>>(thisPtr->GetData<std::unordered_set<Value>>());
        if (hashSet) {
            return Value(hashSet->find(args[0]) != hashSet->end());
        }
    }
    return Value(false);
}

void RegisterCollectionsLibrary(std::shared_ptr<VirtualMachine> vm) {
    // Create System.Collections.Generic.List<T> class
    auto listClass = std::make_shared<Class>("System.Collections.Generic.List`1");
    listClass->SetNamespace("System.Collections.Generic");

    auto listCtor = std::make_shared<Method>(".ctor", TypeReference::Void(), false, false);
    listCtor->SetNativeImpl(List_ctor);
    listClass->AddMethod(listCtor);

    auto listCtorCapacity = std::make_shared<Method>(".ctor", TypeReference::Void(), false, false);
    listCtorCapacity->AddParameter("capacity", TypeReference::Int32());
    listCtorCapacity->SetNativeImpl(List_ctor_Capacity);
    listClass->AddMethod(listCtorCapacity);

    auto listCount = std::make_shared<Method>("get_Count", TypeReference::Int32(), false, false);
    listCount->SetNativeImpl(List_get_Count);
    listClass->AddMethod(listCount);

    auto listCapacity = std::make_shared<Method>("get_Capacity", TypeReference::Int32(), false, false);
    listCapacity->SetNativeImpl(List_get_Capacity);
    listClass->AddMethod(listCapacity);

    auto listSetCapacity = std::make_shared<Method>("set_Capacity", TypeReference::Void(), false, false);
    listSetCapacity->AddParameter("value", TypeReference::Int32());
    listSetCapacity->SetNativeImpl(List_set_Capacity);
    listClass->AddMethod(listSetCapacity);

    auto listItem = std::make_shared<Method>("get_Item", TypeReference::Object(), false, false);
    listItem->AddParameter("index", TypeReference::Int32());
    listItem->SetNativeImpl(List_get_Item);
    listClass->AddMethod(listItem);

    auto listSetItem = std::make_shared<Method>("set_Item", TypeReference::Void(), false, false);
    listSetItem->AddParameter("index", TypeReference::Int32());
    listSetItem->AddParameter("value", TypeReference::Object());
    listSetItem->SetNativeImpl(List_set_Item);
    listClass->AddMethod(listSetItem);

    auto listAdd = std::make_shared<Method>("Add", TypeReference::Void(), false, false);
    listAdd->AddParameter("item", TypeReference::Object());
    listAdd->SetNativeImpl(List_Add);
    listClass->AddMethod(listAdd);

    auto listAddRange = std::make_shared<Method>("AddRange", TypeReference::Void(), false, false);
    listAddRange->AddParameter("collection", TypeReference::Object());
    listAddRange->SetNativeImpl(List_AddRange);
    listClass->AddMethod(listAddRange);

    auto listInsert = std::make_shared<Method>("Insert", TypeReference::Void(), false, false);
    listInsert->AddParameter("index", TypeReference::Int32());
    listInsert->AddParameter("item", TypeReference::Object());
    listInsert->SetNativeImpl(List_Insert);
    listClass->AddMethod(listInsert);

    auto listRemoveAt = std::make_shared<Method>("RemoveAt", TypeReference::Void(), false, false);
    listRemoveAt->AddParameter("index", TypeReference::Int32());
    listRemoveAt->SetNativeImpl(List_RemoveAt);
    listClass->AddMethod(listRemoveAt);

    auto listRemove = std::make_shared<Method>("Remove", TypeReference::Bool(), false, false);
    listRemove->AddParameter("item", TypeReference::Object());
    listRemove->SetNativeImpl(List_Remove);
    listClass->AddMethod(listRemove);

    auto listClear = std::make_shared<Method>("Clear", TypeReference::Void(), false, false);
    listClear->SetNativeImpl(List_Clear);
    listClass->AddMethod(listClear);

    auto listContains = std::make_shared<Method>("Contains", TypeReference::Bool(), false, false);
    listContains->AddParameter("item", TypeReference::Object());
    listContains->SetNativeImpl(List_Contains);
    listClass->AddMethod(listContains);

    auto listIndexOf = std::make_shared<Method>("IndexOf", TypeReference::Int32(), false, false);
    listIndexOf->AddParameter("item", TypeReference::Object());
    listIndexOf->SetNativeImpl(List_IndexOf);
    listClass->AddMethod(listIndexOf);

    auto listToArray = std::make_shared<Method>("ToArray", TypeReference::Object(), false, false);
    listToArray->SetNativeImpl(List_ToArray);
    listClass->AddMethod(listToArray);

    vm->RegisterClass(listClass);

    // Create System.Collections.Generic.Dictionary<TKey,TValue> class
    auto dictClass = std::make_shared<Class>("System.Collections.Generic.Dictionary`2");
    dictClass->SetNamespace("System.Collections.Generic");

    auto dictCtor = std::make_shared<Method>(".ctor", TypeReference::Void(), false, false);
    dictCtor->SetNativeImpl(Dictionary_ctor);
    dictClass->AddMethod(dictCtor);

    auto dictCount = std::make_shared<Method>("get_Count", TypeReference::Int32(), false, false);
    dictCount->SetNativeImpl(Dictionary_get_Count);
    dictClass->AddMethod(dictCount);

    auto dictItem = std::make_shared<Method>("get_Item", TypeReference::Object(), false, false);
    dictItem->AddParameter("key", TypeReference::Object());
    dictItem->SetNativeImpl(Dictionary_get_Item);
    dictClass->AddMethod(dictItem);

    auto dictSetItem = std::make_shared<Method>("set_Item", TypeReference::Void(), false, false);
    dictSetItem->AddParameter("key", TypeReference::Object());
    dictSetItem->AddParameter("value", TypeReference::Object());
    dictSetItem->SetNativeImpl(Dictionary_set_Item);
    dictClass->AddMethod(dictSetItem);

    auto dictAdd = std::make_shared<Method>("Add", TypeReference::Void(), false, false);
    dictAdd->AddParameter("key", TypeReference::Object());
    dictAdd->AddParameter("value", TypeReference::Object());
    dictAdd->SetNativeImpl(Dictionary_Add);
    dictClass->AddMethod(dictAdd);

    auto dictRemove = std::make_shared<Method>("Remove", TypeReference::Bool(), false, false);
    dictRemove->AddParameter("key", TypeReference::Object());
    dictRemove->SetNativeImpl(Dictionary_Remove);
    dictClass->AddMethod(dictRemove);

    auto dictClear = std::make_shared<Method>("Clear", TypeReference::Void(), false, false);
    dictClear->SetNativeImpl(Dictionary_Clear);
    dictClass->AddMethod(dictClear);

    auto dictContainsKey = std::make_shared<Method>("ContainsKey", TypeReference::Bool(), false, false);
    dictContainsKey->AddParameter("key", TypeReference::Object());
    dictContainsKey->SetNativeImpl(Dictionary_ContainsKey);
    dictClass->AddMethod(dictContainsKey);

    auto dictContainsValue = std::make_shared<Method>("ContainsValue", TypeReference::Bool(), false, false);
    dictContainsValue->AddParameter("value", TypeReference::Object());
    dictContainsValue->SetNativeImpl(Dictionary_ContainsValue);
    dictClass->AddMethod(dictContainsValue);

    auto dictKeys = std::make_shared<Method>("get_Keys", TypeReference::Object(), false, false);
    dictKeys->SetNativeImpl(Dictionary_get_Keys);
    dictClass->AddMethod(dictKeys);

    auto dictValues = std::make_shared<Method>("get_Values", TypeReference::Object(), false, false);
    dictValues->SetNativeImpl(Dictionary_get_Values);
    dictClass->AddMethod(dictValues);

    vm->RegisterClass(dictClass);

    // Create System.Collections.Generic.Queue<T> class
    auto queueClass = std::make_shared<Class>("System.Collections.Generic.Queue`1");
    queueClass->SetNamespace("System.Collections.Generic");

    auto queueCtor = std::make_shared<Method>(".ctor", TypeReference::Void(), false, false);
    queueCtor->SetNativeImpl(Queue_ctor);
    queueClass->AddMethod(queueCtor);

    auto queueCount = std::make_shared<Method>("get_Count", TypeReference::Int32(), false, false);
    queueCount->SetNativeImpl(Queue_get_Count);
    queueClass->AddMethod(queueCount);

    auto queueEnqueue = std::make_shared<Method>("Enqueue", TypeReference::Void(), false, false);
    queueEnqueue->AddParameter("item", TypeReference::Object());
    queueEnqueue->SetNativeImpl(Queue_Enqueue);
    queueClass->AddMethod(queueEnqueue);

    auto queueDequeue = std::make_shared<Method>("Dequeue", TypeReference::Object(), false, false);
    queueDequeue->SetNativeImpl(Queue_Dequeue);
    queueClass->AddMethod(queueDequeue);

    auto queuePeek = std::make_shared<Method>("Peek", TypeReference::Object(), false, false);
    queuePeek->SetNativeImpl(Queue_Peek);
    queueClass->AddMethod(queuePeek);

    auto queueClear = std::make_shared<Method>("Clear", TypeReference::Void(), false, false);
    queueClear->SetNativeImpl(Queue_Clear);
    queueClass->AddMethod(queueClear);

    auto queueContains = std::make_shared<Method>("Contains", TypeReference::Bool(), false, false);
    queueContains->AddParameter("item", TypeReference::Object());
    queueContains->SetNativeImpl(Queue_Contains);
    queueClass->AddMethod(queueContains);

    vm->RegisterClass(queueClass);

    // Create System.Collections.Generic.Stack<T> class
    auto stackClass = std::make_shared<Class>("System.Collections.Generic.Stack`1");
    stackClass->SetNamespace("System.Collections.Generic");

    auto stackCtor = std::make_shared<Method>(".ctor", TypeReference::Void(), false, false);
    stackCtor->SetNativeImpl(Stack_ctor);
    stackClass->AddMethod(stackCtor);

    auto stackCount = std::make_shared<Method>("get_Count", TypeReference::Int32(), false, false);
    stackCount->SetNativeImpl(Stack_get_Count);
    stackClass->AddMethod(stackCount);

    auto stackPush = std::make_shared<Method>("Push", TypeReference::Void(), false, false);
    stackPush->AddParameter("item", TypeReference::Object());
    stackPush->SetNativeImpl(Stack_Push);
    stackClass->AddMethod(stackPush);

    auto stackPop = std::make_shared<Method>("Pop", TypeReference::Object(), false, false);
    stackPop->SetNativeImpl(Stack_Pop);
    stackClass->AddMethod(stackPop);

    auto stackPeek = std::make_shared<Method>("Peek", TypeReference::Object(), false, false);
    stackPeek->SetNativeImpl(Stack_Peek);
    stackClass->AddMethod(stackPeek);

    auto stackClear = std::make_shared<Method>("Clear", TypeReference::Void(), false, false);
    stackClear->SetNativeImpl(Stack_Clear);
    stackClass->AddMethod(stackClear);

    auto stackContains = std::make_shared<Method>("Contains", TypeReference::Bool(), false, false);
    stackContains->AddParameter("item", TypeReference::Object());
    stackContains->SetNativeImpl(Stack_Contains);
    stackClass->AddMethod(stackContains);

    vm->RegisterClass(stackClass);

    // Create System.Collections.Generic.HashSet<T> class
    auto hashSetClass = std::make_shared<Class>("System.Collections.Generic.HashSet`1");
    hashSetClass->SetNamespace("System.Collections.Generic");

    auto hashSetCtor = std::make_shared<Method>(".ctor", TypeReference::Void(), false, false);
    hashSetCtor->SetNativeImpl(HashSet_ctor);
    hashSetClass->AddMethod(hashSetCtor);

    auto hashSetCount = std::make_shared<Method>("get_Count", TypeReference::Int32(), false, false);
    hashSetCount->SetNativeImpl(HashSet_get_Count);
    hashSetClass->AddMethod(hashSetCount);

    auto hashSetAdd = std::make_shared<Method>("Add", TypeReference::Bool(), false, false);
    hashSetAdd->AddParameter("item", TypeReference::Object());
    hashSetAdd->SetNativeImpl(HashSet_Add);
    hashSetClass->AddMethod(hashSetAdd);

    auto hashSetRemove = std::make_shared<Method>("Remove", TypeReference::Bool(), false, false);
    hashSetRemove->AddParameter("item", TypeReference::Object());
    hashSetRemove->SetNativeImpl(HashSet_Remove);
    hashSetClass->AddMethod(hashSetRemove);

    auto hashSetClear = std::make_shared<Method>("Clear", TypeReference::Void(), false, false);
    hashSetClear->SetNativeImpl(HashSet_Clear);
    hashSetClass->AddMethod(hashSetClear);

    auto hashSetContains = std::make_shared<Method>("Contains", TypeReference::Bool(), false, false);
    hashSetContains->AddParameter("item", TypeReference::Object());
    hashSetContains->SetNativeImpl(HashSet_Contains);
    hashSetClass->AddMethod(hashSetContains);

    vm->RegisterClass(hashSetClass);
}

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
    
    // Register System.Math library
    RegisterMathLibrary(vm);
    
    // Register System.IO library
    RegisterIOLibrary(vm);
    
    // Register System.Collections.Generic library
    RegisterCollectionsLibrary(vm);
}

} // namespace ObjectIR
