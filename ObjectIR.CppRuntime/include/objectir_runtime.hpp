#pragma once

#include <memory>
#include <string>
#include <vector>
#include <map>
#include <unordered_map>
#include <functional>
#include <stdexcept>
#include <variant>
#include <type_traits>
#include <cstdint>
#include <any>

namespace ObjectIR
{

    // ============================================================================
    // Forward Declarations
    // ============================================================================
    class Object;
    class Class;
    class Method;
    class Field;
    class VirtualMachine;
    class ExecutionContext;

    using ObjectRef = std::shared_ptr<Object>;
    using ClassRef = std::shared_ptr<Class>;
    using MethodRef = std::shared_ptr<Method>;
    using FieldRef = std::shared_ptr<Field>;

    // ============================================================================
    // Type System
    // ============================================================================

    /// Represents different primitive types
    enum class PrimitiveType
    {
        Int32,
        Int64,
        Float32,
        Float64,
        Bool,
        Void,
        String,
        Object
    };

    /// Represents a type reference with support for generics
    class TypeReference
    {
    public:
        TypeReference() = default;
        explicit TypeReference(PrimitiveType primitive);
        explicit TypeReference(ClassRef classType);

        [[nodiscard]] bool IsPrimitive() const { return _isPrimitive; }
        [[nodiscard]] PrimitiveType GetPrimitiveType() const { return _primitiveType; }
        [[nodiscard]] ClassRef GetClassType() const { return _classType; }

        static TypeReference Int32();
        static TypeReference Int64();
        static TypeReference Float32();
        static TypeReference Float64();
        static TypeReference Bool();
        static TypeReference Void();
        static TypeReference String();
        static TypeReference Object(ClassRef classType);

    private:
        bool _isPrimitive = true;
        PrimitiveType _primitiveType = PrimitiveType::Void;
        ClassRef _classType = nullptr;
    };

    // ============================================================================
    // Value Type - Stack-based value representation
    // ============================================================================

    /// Represents a runtime value that can be stored on the stack
    class Value
    {
    public:
        Value();
        explicit Value(int32_t i32);
        explicit Value(int64_t i64);
        explicit Value(float f);
        explicit Value(double d);
        explicit Value(bool b);
        explicit Value(const std::string &str);
        explicit Value(ObjectRef obj);

        [[nodiscard]] bool IsInt32() const;
        [[nodiscard]] bool IsInt64() const;
        [[nodiscard]] bool IsFloat32() const;
        [[nodiscard]] bool IsFloat64() const;
        [[nodiscard]] bool IsBool() const;
        [[nodiscard]] bool IsString() const;
        [[nodiscard]] bool IsObject() const;
        [[nodiscard]] bool IsNull() const { return _value.index() == 0; }

        [[nodiscard]] int32_t AsInt32() const;
        [[nodiscard]] int64_t AsInt64() const;
        [[nodiscard]] float AsFloat32() const;
        [[nodiscard]] double AsFloat64() const;
        [[nodiscard]] bool AsBool() const;
        [[nodiscard]] std::string AsString() const;
        [[nodiscard]] ObjectRef AsObject() const;

    private:
        std::variant<std::nullptr_t, int32_t, int64_t, float, double, bool, std::string, ObjectRef> _value;
    };

    // ============================================================================
    // Object Model - Core OOP support
    // ============================================================================

    /// Base class for all runtime objects
    class Object : public std::enable_shared_from_this<Object>
    {
    public:
        Object() = default;
        virtual ~Object() = default;

        void SetField(const std::string &fieldName, const Value &value);
        [[nodiscard]] Value GetField(const std::string &fieldName) const;

        [[nodiscard]] ClassRef GetClass() const { return _class; }
        void SetClass(ClassRef classType) { _class = classType; }

        [[nodiscard]] bool IsInstanceOf(ClassRef classType) const;
        [[nodiscard]] ObjectRef GetBaseInstance() const { return _baseInstance; }
        void SetBaseInstance(ObjectRef base) { _baseInstance = base; }

    protected:
        std::unordered_map<std::string, Value> _fieldValues;
        ClassRef _class;
        ObjectRef _baseInstance;
    };

    /// Represents a field definition within a class
    class Field
    {
    public:
        Field(std::string name, TypeReference type)
            : _name(std::move(name)), _type(type) {}

        [[nodiscard]] const std::string &GetName() const { return _name; }
        [[nodiscard]] const TypeReference &GetType() const { return _type; }

    private:
        std::string _name;
        TypeReference _type;
    };

    // ============================================================================
    // Methods and Function Pointers
    // ============================================================================

    /// Signature for native method implementations
    using NativeMethodImpl = std::function<Value(ObjectRef thisPtr, const std::vector<Value> &, VirtualMachine *)>;

    /// Represents a method definition
    class Method
    {
    public:
        Method(std::string name, TypeReference returnType, bool isStatic = false, bool isVirtual = false)
            : _name(std::move(name)), _returnType(returnType), _isStatic(isStatic), _isVirtual(isVirtual) {}

        [[nodiscard]] const std::string &GetName() const { return _name; }
        [[nodiscard]] const TypeReference &GetReturnType() const { return _returnType; }
        [[nodiscard]] bool IsStatic() const { return _isStatic; }
        [[nodiscard]] bool IsVirtual() const { return _isVirtual; }
        [[nodiscard]] const std::vector<std::pair<std::string, TypeReference>> &GetParameters() const { return _parameters; }

        void AddParameter(const std::string &name, const TypeReference &type);
        void SetNativeImpl(NativeMethodImpl impl) { _nativeImpl = impl; }
        [[nodiscard]] NativeMethodImpl GetNativeImpl() const { return _nativeImpl; }

    private:
        std::string _name;
        TypeReference _returnType;
        bool _isStatic;
        bool _isVirtual;
        std::vector<std::pair<std::string, TypeReference>> _parameters;
        NativeMethodImpl _nativeImpl;
    };

    // ============================================================================
    // Class Definition - Represents a class at runtime
    // ============================================================================

    class Class : public std::enable_shared_from_this<Class>
    {
    public:
        explicit Class(std::string name);

        [[nodiscard]] const std::string &GetName() const { return _name; }
        [[nodiscard]] ClassRef GetBaseClass() const { return _baseClass; }
        void SetBaseClass(ClassRef base) { _baseClass = base; }

        [[nodiscard]] const std::string &GetNamespace() const { return _namespace; }
        void SetNamespace(const std::string &ns) { _namespace = ns; }

        [[nodiscard]] bool IsAbstract() const { return _isAbstract; }
        void SetAbstract(bool abstract) { _isAbstract = abstract; }

        [[nodiscard]] bool IsSealed() const { return _isSealed; }
        void SetSealed(bool sealed) { _isSealed = sealed; }

        // Field management
        void AddField(FieldRef field);
        [[nodiscard]] FieldRef GetField(const std::string &name) const;
        [[nodiscard]] const std::vector<FieldRef> &GetAllFields() const { return _fields; }

        // Method management
        void AddMethod(MethodRef method);
        [[nodiscard]] MethodRef GetMethod(const std::string &name) const;
        [[nodiscard]] MethodRef LookupMethod(const std::string &name) const;
        [[nodiscard]] const std::vector<MethodRef> &GetAllMethods() const { return _methods; }

        // Object construction
        [[nodiscard]] ObjectRef CreateInstance() const;

        // Interface/contract support
        void AddInterface(ClassRef interface);
        [[nodiscard]] bool ImplementsInterface(ClassRef interface) const;

    private:
        std::string _name;
        std::string _namespace;
        ClassRef _baseClass;
        std::vector<FieldRef> _fields;
        std::vector<MethodRef> _methods;
        std::vector<ClassRef> _interfaces;
        bool _isAbstract = false;
        bool _isSealed = false;
    };

    // ============================================================================
    // Generic Collections Support
    // ============================================================================

    /// Base class for generic list
    class ListBase : public Object
    {
    public:
        virtual ~ListBase() = default;
        [[nodiscard]] virtual size_t GetSize() const = 0;
        [[nodiscard]] virtual Value GetAt(size_t index) const = 0;
        virtual void SetAt(size_t index, const Value &value) = 0;
        virtual void Add(const Value &value) = 0;
        virtual void Remove(size_t index) = 0;
        virtual void Clear() = 0;
    };

    /// Typed list implementation
    template <typename T>
    class List : public ListBase
    {
    public:
        [[nodiscard]] size_t GetSize() const override { return _items.size(); }

        [[nodiscard]] Value GetAt(size_t index) const override
        {
            if (index >= _items.size())
                throw std::out_of_range("List index out of range");
            return Value(_items[index]);
        }

        void SetAt(size_t index, const Value &value) override
        {
            if (index >= _items.size())
                throw std::out_of_range("List index out of range");
            _items[index] = ExtractValue<T>(value);
        }

        void Add(const Value &value) override
        {
            _items.push_back(ExtractValue<T>(value));
        }

        void Remove(size_t index) override
        {
            if (index >= _items.size())
                throw std::out_of_range("List index out of range");
            _items.erase(_items.begin() + index);
        }

        void Clear() override { _items.clear(); }

    private:
        std::vector<T> _items;

        template <typename U>
        static U ExtractValue(const Value &v);
    };

    // Template specializations for ExtractValue - implementations
    template <>
    template <>
    inline int32_t List<int32_t>::ExtractValue<int32_t>(const Value &v)
    {
        return v.AsInt32();
    }

    template <>
    template <>
    inline std::string List<std::string>::ExtractValue<std::string>(const Value &v)
    {
        return v.AsString();
    }

    template <>
    template <>
    inline ObjectRef List<ObjectRef>::ExtractValue<ObjectRef>(const Value &v)
    {
        return v.AsObject();
    }

    // ============================================================================
    // Execution Context - Runtime state for method execution
    // ============================================================================

    class ExecutionContext
    {
    public:
        explicit ExecutionContext(MethodRef method);

        void PushStack(const Value &value);
        [[nodiscard]] Value PopStack();
        [[nodiscard]] Value PeekStack() const;

        void SetLocal(size_t index, const Value &value);
        [[nodiscard]] Value GetLocal(size_t index) const;

        [[nodiscard]] ObjectRef GetThis() const { return _this; }
        void SetThis(ObjectRef obj) { _this = obj; }

        [[nodiscard]] MethodRef GetMethod() const { return _method; }

    private:
        MethodRef _method;
        std::vector<Value> _stack;
        std::vector<Value> _locals;
        ObjectRef _this;
    };

    // ============================================================================
    // Virtual Machine - Runtime engine
    // ============================================================================

    class VirtualMachine
    {
    public:
        VirtualMachine();

        // Class registry
        void RegisterClass(ClassRef classType);
        [[nodiscard]] ClassRef GetClass(const std::string &name) const;
        [[nodiscard]] bool HasClass(const std::string &name) const;

        // Object creation
        [[nodiscard]] ObjectRef CreateObject(ClassRef classType);
        [[nodiscard]] ObjectRef CreateObject(const std::string &className);

        // Method invocation
        Value InvokeMethod(ObjectRef object, const std::string &methodName, const std::vector<Value> &args);
        Value InvokeStaticMethod(ClassRef classType, const std::string &methodName, const std::vector<Value> &args);

        // Global state
        [[nodiscard]] ExecutionContext *GetCurrentContext() const { return _currentContext.get(); }
        void PushContext(std::unique_ptr<ExecutionContext> context);
        void PopContext();

    private:
        std::unordered_map<std::string, ClassRef> _classes;
        std::vector<std::unique_ptr<ExecutionContext>> _contextStack;
        std::unique_ptr<ExecutionContext> _currentContext;
    };

    // ============================================================================
    // Builder API - Fluent interface for constructing runtime objects
    // ============================================================================

    class RuntimeBuilder
    {
    public:
        RuntimeBuilder() : _vm(std::make_unique<VirtualMachine>()) {}

        RuntimeBuilder &Class(const std::string &name);
        RuntimeBuilder &Field(const std::string &name, const TypeReference &type);
        RuntimeBuilder &Method(const std::string &name, const TypeReference &returnType, bool isStatic = false);
        RuntimeBuilder &Parameter(const std::string &name, const TypeReference &type);
        RuntimeBuilder &EndMethod();
        RuntimeBuilder &EndClass();
        RuntimeBuilder &NativeImpl(NativeMethodImpl impl);

        [[nodiscard]] VirtualMachine *Build() { return _vm.get(); }
        [[nodiscard]] std::unique_ptr<VirtualMachine> Release() { return std::move(_vm); }

    private:
        std::unique_ptr<VirtualMachine> _vm;
        ClassRef _currentClass;
        MethodRef _currentMethod;
    };

} // namespace ObjectIR
