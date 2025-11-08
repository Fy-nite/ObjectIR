#pragma once

#include "objectir_runtime.hpp"
#include <memory>
#include <cmath>

namespace ObjectIR {

// ============================================================================
// System.Math Implementation
// ============================================================================

// Math constants
Value Math_PI(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Math_E(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Math_Tau(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);

// Trigonometric functions
Value Math_Sin(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Math_Cos(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Math_Tan(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);

// Inverse trigonometric functions
Value Math_Asin(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Math_Acos(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Math_Atan(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Math_Atan2(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);

// Hyperbolic functions
Value Math_Sinh(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Math_Cosh(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Math_Tanh(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);

// Exponential and logarithmic functions
Value Math_Exp(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Math_Log(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Math_Log10(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Math_Pow(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Math_Sqrt(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);

// Rounding functions
Value Math_Ceiling(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Math_Floor(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Math_Round(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Math_Truncate(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);

// Sign and absolute value
Value Math_Abs(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Math_Sign(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);

// Min/Max functions
Value Math_Min(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Math_Max(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);

} // namespace ObjectIR