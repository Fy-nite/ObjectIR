---
-- lua_types.lua
-- ObjectIR type system for Lua 5.2
-- Implements TypeReference, Class, Method, Field abstractions
---

local Value = require("lua_value")

-- ============================================================================
-- TypeReference: Represents a type at runtime
-- ============================================================================

local TypeReference = {}
TypeReference.__index = TypeReference

--- Primitive type constants
TypeReference.PRIMITIVE_INT32 = "int32"
TypeReference.PRIMITIVE_INT64 = "int64"
TypeReference.PRIMITIVE_FLOAT32 = "float32"
TypeReference.PRIMITIVE_FLOAT64 = "float64"
TypeReference.PRIMITIVE_BOOL = "bool"
TypeReference.PRIMITIVE_VOID = "void"
TypeReference.PRIMITIVE_STRING = "string"
TypeReference.PRIMITIVE_UINT8 = "uint8"

--- Create a primitive type reference
function TypeReference.Primitive(primitiveType)
    local self = setmetatable({}, TypeReference)
    self.isPrimitive = true
    self.primitiveType = primitiveType
    self.classType = nil
    return self
end

--- Create an object type reference
function TypeReference.Object(classType)
    local self = setmetatable({}, TypeReference)
    self.isPrimitive = false
    self.classType = classType
    return self
end

--- Create common primitive types
function TypeReference.Int32() return TypeReference.Primitive(TypeReference.PRIMITIVE_INT32) end
function TypeReference.Int64() return TypeReference.Primitive(TypeReference.PRIMITIVE_INT64) end
function TypeReference.Float32() return TypeReference.Primitive(TypeReference.PRIMITIVE_FLOAT32) end
function TypeReference.Float64() return TypeReference.Primitive(TypeReference.PRIMITIVE_FLOAT64) end
function TypeReference.Bool() return TypeReference.Primitive(TypeReference.PRIMITIVE_BOOL) end
function TypeReference.Void() return TypeReference.Primitive(TypeReference.PRIMITIVE_VOID) end
function TypeReference.String() return TypeReference.Primitive(TypeReference.PRIMITIVE_STRING) end

-- ============================================================================
-- Field: Represents a class field/property
-- ============================================================================

local Field = {}
Field.__index = Field

function Field.new(name, typeRef, accessLevel)
    local self = setmetatable({}, Field)
    self.name = name
    self.typeRef = typeRef or TypeReference.Void()
    self.accessLevel = accessLevel or "public"
    self.value = nil
    return self
end

function Field:GetValue(instance)
    if instance._fields then
        return instance._fields[self.name]
    end
    return nil
end

function Field:SetValue(instance, value)
    if not instance._fields then
        instance._fields = {}
    end
    instance._fields[self.name] = value
end

-- ============================================================================
-- Method: Represents a class method
-- ============================================================================

local Method = {}
Method.__index = Method

function Method.new(name, returnType, isVirtual)
    local self = setmetatable({}, Method)
    self.name = name
    self.returnType = returnType or TypeReference.Void()
    self.isVirtual = isVirtual or false
    self.parameters = {}
    self.locals = {}
    self.instructions = {}
    self.body = nil  -- For native implementations
    return self
end

function Method:AddParameter(name, typeRef)
    table.insert(self.parameters, {name = name, typeRef = typeRef})
    return self
end

function Method:SetBody(func)
    self.body = func
    return self
end

function Method:SetInstructions(instructions)
    self.instructions = instructions
    return self
end

-- ============================================================================
-- Class: Represents an object class
-- ============================================================================

local Class = {}
Class.__index = Class

function Class.new(name, baseClass)
    local self = setmetatable({}, Class)
    self.name = name
    self.baseClass = baseClass
    self.fields = {}
    self.methods = {}
    self.interfaces = {}
    self._fieldsByName = {}
    self._methodsByName = {}
    self.accessLevel = "public"
    return self
end

function Class:AddField(field)
    table.insert(self.fields, field)
    self._fieldsByName[field.name] = field
    return self
end

function Class:AddMethod(method)
    table.insert(self.methods, method)
    self._methodsByName[method.name] = method
    return self
end

function Class:GetField(name)
    if self._fieldsByName[name] then
        return self._fieldsByName[name]
    elseif self.baseClass then
        return self.baseClass:GetField(name)
    end
    return nil
end

function Class:GetMethod(name)
    if self._methodsByName[name] then
        return self._methodsByName[name]
    elseif self.baseClass then
        return self.baseClass:GetMethod(name)
    end
    return nil
end

function Class:GetAllFields()
    local allFields = {}
    if self.baseClass then
        for _, f in ipairs(self.baseClass:GetAllFields()) do
            table.insert(allFields, f)
        end
    end
    for _, f in ipairs(self.fields) do
        table.insert(allFields, f)
    end
    return allFields
end

function Class:GetAllMethods()
    local allMethods = {}
    if self.baseClass then
        for _, m in ipairs(self.baseClass:GetAllMethods()) do
            table.insert(allMethods, m)
        end
    end
    for _, m in ipairs(self.methods) do
        table.insert(allMethods, m)
    end
    return allMethods
end

function Class:IsSubclassOf(other)
    if self == other then return true end
    if self.baseClass then
        return self.baseClass:IsSubclassOf(other)
    end
    return false
end

-- ============================================================================
-- Object: Runtime instance of a class
-- ============================================================================

local Object = {}
Object.__index = Object

function Object.new(class)
    local self = setmetatable({}, Object)
    self._class = class
    self._fields = {}
    
    -- Initialize field values
    for _, field in ipairs(class:GetAllFields()) do
        self._fields[field.name] = Value.null()
    end
    
    return self
end

function Object:GetClass()
    return self._class
end

function Object:GetField(fieldName)
    local field = self._class:GetField(fieldName)
    if field then
        return self._fields[fieldName] or Value.null()
    end
    return nil
end

function Object:SetField(fieldName, value)
    local field = self._class:GetField(fieldName)
    if field then
        self._fields[fieldName] = value
        return true
    end
    return false
end

function Object:InvokeMethod(methodName, args)
    local method = self._class:GetMethod(methodName)
    if method then
        if method.body then
            -- Native method
            return method.body(self, args)
        else
            -- IR method - will be executed by the runtime
            return {method = method, instance = self, args = args}
        end
    end
    return nil
end

-- ============================================================================
-- Export
-- ============================================================================

return {
    TypeReference = TypeReference,
    Field = Field,
    Method = Method,
    Class = Class,
    Object = Object,
}
