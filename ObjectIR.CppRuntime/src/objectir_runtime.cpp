#include "objectir_runtime.hpp"
#include "instruction_executor.hpp"
#include <algorithm>
#include <stdexcept>

namespace ObjectIR {

// ============================================================================
// TypeReference Implementation
// ============================================================================

TypeReference::TypeReference(PrimitiveType primitive)
    : _isPrimitive(true), _primitiveType(primitive) {}

TypeReference::TypeReference(ClassRef classType)
    : _isPrimitive(false), _classType(classType) {}

TypeReference TypeReference::Int32() {
    return TypeReference(PrimitiveType::Int32);
}

TypeReference TypeReference::Int64() {
    return TypeReference(PrimitiveType::Int64);
}

TypeReference TypeReference::Float32() {
    return TypeReference(PrimitiveType::Float32);
}

TypeReference TypeReference::Float64() {
    return TypeReference(PrimitiveType::Float64);
}

TypeReference TypeReference::Bool() {
    return TypeReference(PrimitiveType::Bool);
}

TypeReference TypeReference::Void() {
    return TypeReference(PrimitiveType::Void);
}

TypeReference TypeReference::String() {
    return TypeReference(PrimitiveType::String);
}

TypeReference TypeReference::Object(ClassRef classType) {
    return TypeReference(classType);
}

// ============================================================================
// Value Implementation
// ============================================================================

Value::Value() : _value(nullptr) {}
Value::Value(int32_t i32) : _value(i32) {}
Value::Value(int64_t i64) : _value(i64) {}
Value::Value(float f) : _value(f) {}
Value::Value(double d) : _value(d) {}
Value::Value(bool b) : _value(b) {}
Value::Value(const std::string& str) : _value(str) {}
Value::Value(ObjectRef obj) : _value(obj) {}

bool Value::IsInt32() const { return _value.index() == 1; }
bool Value::IsInt64() const { return _value.index() == 2; }
bool Value::IsFloat32() const { return _value.index() == 3; }
bool Value::IsFloat64() const { return _value.index() == 4; }
bool Value::IsBool() const { return _value.index() == 5; }
bool Value::IsString() const { return _value.index() == 6; }
bool Value::IsObject() const { return _value.index() == 7; }

int32_t Value::AsInt32() const {
    if (!IsInt32()) throw std::runtime_error("Value is not int32");
    return std::get<int32_t>(_value);
}

int64_t Value::AsInt64() const {
    if (!IsInt64()) throw std::runtime_error("Value is not int64");
    return std::get<int64_t>(_value);
}

float Value::AsFloat32() const {
    if (!IsFloat32()) throw std::runtime_error("Value is not float32");
    return std::get<float>(_value);
}

double Value::AsFloat64() const {
    if (!IsFloat64()) throw std::runtime_error("Value is not float64");
    return std::get<double>(_value);
}

bool Value::AsBool() const {
    if (!IsBool()) throw std::runtime_error("Value is not bool");
    return std::get<bool>(_value);
}

std::string Value::AsString() const {
    if (!IsString()) throw std::runtime_error("Value is not string");
    return std::get<std::string>(_value);
}

ObjectRef Value::AsObject() const {
    if (!IsObject()) throw std::runtime_error("Value is not object");
    return std::get<ObjectRef>(_value);
}

// ============================================================================
// Object Implementation
// ============================================================================

void Object::SetField(const std::string& fieldName, const Value& value) {
    _fieldValues[fieldName] = value;
}

Value Object::GetField(const std::string& fieldName) const {
    auto it = _fieldValues.find(fieldName);
    if (it != _fieldValues.end()) {
        return it->second;
    }
    
    // Check base class instance
    if (_baseInstance) {
        return _baseInstance->GetField(fieldName);
    }
    
    throw std::runtime_error("Field not found: " + fieldName);
}

bool Object::IsInstanceOf(ClassRef classType) const {
    if (!_class) return false;
    
    auto current = _class;
    while (current) {
        if (current == classType) return true;
        current = current->GetBaseClass();
    }
    
    return _class->ImplementsInterface(classType);
}

// ============================================================================
// Field Implementation
// ============================================================================

// Implementation is inline in header

// ============================================================================
// Method Implementation
// ============================================================================

void Method::AddParameter(const std::string& name, const TypeReference& type) {
    _parameters.emplace_back(name, type);
}

void Method::AddLocal(const std::string& name, const TypeReference& type) {
    _locals.emplace_back(name, type);
}

void Method::SetInstructions(std::vector<Instruction> instructions) {
    _instructions = std::move(instructions);
}

// ============================================================================
// Class Implementation
// ============================================================================

Class::Class(std::string name) : _name(std::move(name)) {}

void Class::AddField(FieldRef field) {
    _fields.push_back(field);
}

FieldRef Class::GetField(const std::string& name) const {
    for (const auto& field : _fields) {
        if (field->GetName() == name) {
            return field;
        }
    }
    
    if (_baseClass) {
        return _baseClass->GetField(name);
    }
    
    return nullptr;
}

void Class::AddMethod(MethodRef method) {
    _methods.push_back(method);
}

MethodRef Class::GetMethod(const std::string& name) const {
    for (const auto& method : _methods) {
        if (method->GetName() == name) {
            return method;
        }
    }
    return nullptr;
}

MethodRef Class::LookupMethod(const std::string& name) const {
    auto method = GetMethod(name);
    if (method) return method;
    
    if (_baseClass) {
        return _baseClass->LookupMethod(name);
    }
    
    return nullptr;
}

ObjectRef Class::CreateInstance() const {
    auto obj = std::make_shared<Object>();
    obj->SetClass(std::const_pointer_cast<Class>(shared_from_this()));
    return obj;
}

void Class::AddInterface(ClassRef interface) {
    _interfaces.push_back(interface);
}

bool Class::ImplementsInterface(ClassRef interface) const {
    for (const auto& iface : _interfaces) {
        if (iface == interface) return true;
    }
    return false;
}

// ============================================================================
// ExecutionContext Implementation
// ============================================================================

ExecutionContext::ExecutionContext(MethodRef method)
    : _method(std::move(method)) {
    if (!_method) {
        throw std::runtime_error("ExecutionContext requires a valid method reference");
    }

    const auto& locals = _method->GetLocals();
    _locals.resize(locals.size());
    for (size_t i = 0; i < locals.size(); ++i) {
        _localIndices[locals[i].first] = i;
    }

    const auto& parameters = _method->GetParameters();
    _arguments.resize(parameters.size());
    for (size_t i = 0; i < parameters.size(); ++i) {
        _parameterIndices[parameters[i].first] = i;
    }
}

void ExecutionContext::PushStack(const Value& value) {
    _stack.push_back(value);
}

Value ExecutionContext::PopStack() {
    if (_stack.empty()) {
        throw std::runtime_error("Stack underflow");
    }
    Value value = _stack.back();
    _stack.pop_back();
    return value;
}

Value ExecutionContext::PeekStack() const {
    if (_stack.empty()) {
        throw std::runtime_error("Stack underflow");
    }
    return _stack.back();
}

void ExecutionContext::SetLocal(size_t index, const Value& value) {
    if (index >= _locals.size()) {
        _locals.resize(index + 1);
    }
    _locals[index] = value;
}

Value ExecutionContext::GetLocal(size_t index) const {
    if (index >= _locals.size()) {
        throw std::out_of_range("Local variable index out of range");
    }
    return _locals[index];
}

void ExecutionContext::SetLocal(const std::string& name, const Value& value) {
    auto it = _localIndices.find(name);
    if (it == _localIndices.end()) {
        throw std::runtime_error("Local variable not found: " + name);
    }
    SetLocal(it->second, value);
}

Value ExecutionContext::GetLocal(const std::string& name) const {
    auto it = _localIndices.find(name);
    if (it == _localIndices.end()) {
        throw std::runtime_error("Local variable not found: " + name);
    }
    return GetLocal(it->second);
}

void ExecutionContext::SetArguments(const std::vector<Value>& args) {
    if (args.size() != _arguments.size()) {
        _arguments.resize(args.size());
    }
    std::copy(args.begin(), args.end(), _arguments.begin());
}

Value ExecutionContext::GetArgument(size_t index) const {
    if (index >= _arguments.size()) {
        throw std::out_of_range("Argument index out of range");
    }
    return _arguments[index];
}

Value ExecutionContext::GetArgument(const std::string& name) const {
    auto it = _parameterIndices.find(name);
    if (it == _parameterIndices.end()) {
        throw std::runtime_error("Argument not found: " + name);
    }
    return GetArgument(it->second);
}

void ExecutionContext::SetArgument(const std::string& name, const Value& value) {
    auto it = _parameterIndices.find(name);
    if (it == _parameterIndices.end()) {
        throw std::runtime_error("Argument not found: " + name);
    }
    if (it->second >= _arguments.size()) {
        _arguments.resize(it->second + 1);
    }
    _arguments[it->second] = value;
}

// ============================================================================
// VirtualMachine Implementation
// ============================================================================

VirtualMachine::VirtualMachine() = default;

void VirtualMachine::RegisterClass(ClassRef classType) {
    _classes[classType->GetName()] = classType;
}

ClassRef VirtualMachine::GetClass(const std::string& name) const {
    auto it = _classes.find(name);
    if (it != _classes.end()) {
        return it->second;
    }
    throw std::runtime_error("Class not found: " + name);
}

bool VirtualMachine::HasClass(const std::string& name) const {
    return _classes.find(name) != _classes.end();
}

ObjectRef VirtualMachine::CreateObject(ClassRef classType) {
    return classType->CreateInstance();
}

ObjectRef VirtualMachine::CreateObject(const std::string& className) {
    return GetClass(className)->CreateInstance();
}

Value VirtualMachine::InvokeMethod(ObjectRef object, const std::string& methodName, const std::vector<Value>& args) {
    if (!object || !object->GetClass()) {
        throw std::runtime_error("Cannot invoke method on null object");
    }
    
    auto method = object->GetClass()->LookupMethod(methodName);
    if (!method) {
        throw std::runtime_error("Method not found: " + methodName);
    }
    
    auto impl = method->GetNativeImpl();
    if (impl) {
        return impl(object, args, this);
    }

    if (method->HasInstructions()) {
        auto context = std::make_unique<ExecutionContext>(method);
        context->SetThis(object);
        context->SetArguments(args);
        auto* rawContext = context.get();
        PushContext(std::move(context));
        auto result = InstructionExecutor::ExecuteInstructions(method->GetInstructions(), object, args, rawContext, this);
        PopContext();
        return result;
    }

    throw std::runtime_error("Method has no implementation: " + methodName);
}

Value VirtualMachine::InvokeStaticMethod(ClassRef classType, const std::string& methodName, const std::vector<Value>& args) {
    auto method = classType->LookupMethod(methodName);
    if (!method) {
        throw std::runtime_error("Static method not found: " + methodName);
    }
    
    auto impl = method->GetNativeImpl();
    if (impl) {
        return impl(nullptr, args, this);
    }

    if (method->HasInstructions()) {
        auto context = std::make_unique<ExecutionContext>(method);
        context->SetArguments(args);
        auto* rawContext = context.get();
        PushContext(std::move(context));
        auto result = InstructionExecutor::ExecuteInstructions(method->GetInstructions(), nullptr, args, rawContext, this);
        PopContext();
        return result;
    }

    throw std::runtime_error("Method has no implementation: " + methodName);
}

void VirtualMachine::PushContext(std::unique_ptr<ExecutionContext> context) {
    _contextStack.push_back(std::move(_currentContext));
    _currentContext = std::move(context);
}

void VirtualMachine::PopContext() {
    if (!_contextStack.empty()) {
        _currentContext = std::move(_contextStack.back());
        _contextStack.pop_back();
    } else {
        _currentContext = nullptr;
    }
}

// ============================================================================
// RuntimeBuilder Implementation
// ============================================================================

RuntimeBuilder& RuntimeBuilder::Class(const std::string& name) {
    _currentClass = std::make_shared<::ObjectIR::Class>(name);
    _vm->RegisterClass(_currentClass);
    return *this;
}

RuntimeBuilder& RuntimeBuilder::Field(const std::string& name, const TypeReference& type) {
    if (!_currentClass) {
        throw std::runtime_error("No current class");
    }
    auto field = std::make_shared<::ObjectIR::Field>(name, type);
    _currentClass->AddField(field);
    return *this;
}

RuntimeBuilder& RuntimeBuilder::Method(const std::string& name, const TypeReference& returnType, bool isStatic) {
    if (!_currentClass) {
        throw std::runtime_error("No current class");
    }
    _currentMethod = std::make_shared<::ObjectIR::Method>(name, returnType, isStatic);
    return *this;
}

RuntimeBuilder& RuntimeBuilder::Parameter(const std::string& name, const TypeReference& type) {
    if (!_currentMethod) {
        throw std::runtime_error("No current method");
    }
    _currentMethod->AddParameter(name, type);
    return *this;
}

RuntimeBuilder& RuntimeBuilder::NativeImpl(NativeMethodImpl impl) {
    if (!_currentMethod) {
        throw std::runtime_error("No current method");
    }
    _currentMethod->SetNativeImpl(impl);
    return *this;
}

RuntimeBuilder& RuntimeBuilder::EndMethod() {
    if (_currentClass && _currentMethod) {
        _currentClass->AddMethod(_currentMethod);
        _currentMethod = nullptr;
    }
    return *this;
}

RuntimeBuilder& RuntimeBuilder::EndClass() {
    _currentClass = nullptr;
    return *this;
}
} // namespace ObjectIR
