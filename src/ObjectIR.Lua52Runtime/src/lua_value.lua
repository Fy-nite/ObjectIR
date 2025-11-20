---
-- lua_value.lua
-- ObjectIR runtime value representation for Lua 5.2
-- Represents runtime values that can be stored on the execution stack
---

local Value = {}
Value.__index = Value

--- Value type constants
Value.TYPE_NULL = "null"
Value.TYPE_INT32 = "int32"
Value.TYPE_INT64 = "int64"
Value.TYPE_FLOAT32 = "float32"
Value.TYPE_FLOAT64 = "float64"
Value.TYPE_BOOL = "bool"
Value.TYPE_STRING = "string"
Value.TYPE_OBJECT = "object"

--- Create a new Value
-- @param type The type of the value (one of Value.TYPE_*)
-- @param data The actual value
-- @return A new Value instance
function Value.new(type, data)
    local self = setmetatable({}, Value)
    self.type = type
    self.data = data
    return self
end

--- Create a null value
function Value.null()
    return Value.new(Value.TYPE_NULL, nil)
end

--- Create an int32 value
function Value.int32(n)
    -- Clamp to int32 range
    n = tonumber(n)
    if n > 2147483647 then n = 2147483647 end
    if n < -2147483648 then n = -2147483648 end
    return Value.new(Value.TYPE_INT32, math.floor(n))
end

--- Create an int64 value
function Value.int64(n)
    return Value.new(Value.TYPE_INT64, tonumber(n))
end

--- Create a float32 value
function Value.float32(n)
    return Value.new(Value.TYPE_FLOAT32, tonumber(n))
end

--- Create a float64 value
function Value.float64(n)
    return Value.new(Value.TYPE_FLOAT64, tonumber(n))
end

--- Create a boolean value
function Value.bool(b)
    return Value.new(Value.TYPE_BOOL, b and true or false)
end

--- Create a string value
function Value.string(s)
    return Value.new(Value.TYPE_STRING, tostring(s))
end

--- Create an object reference value
function Value.object(obj)
    return Value.new(Value.TYPE_OBJECT, obj)
end

--- Check if value is null
function Value:IsNull()
    return self.type == Value.TYPE_NULL
end

--- Check if value is a specific type
function Value:IsType(t)
    return self.type == t
end

--- Get the value's data
function Value:GetData()
    return self.data
end

--- Convert value to Lua native type (for interop)
function Value:ToNative()
    if self.type == Value.TYPE_NULL then
        return nil
    elseif self.type == Value.TYPE_BOOL then
        return self.data
    elseif self.type == Value.TYPE_STRING then
        return self.data
    elseif self.type == Value.TYPE_OBJECT then
        return self.data
    else
        -- Numeric types
        return self.data
    end
end

--- Convert from Lua native type
-- @param value The Lua value
-- @param hint Optional type hint (one of Value.TYPE_*)
-- @return A Value instance
function Value.FromNative(value, hint)
    if value == nil then
        return Value.null()
    elseif hint == Value.TYPE_BOOL or type(value) == "boolean" then
        return Value.bool(value)
    elseif hint == Value.TYPE_STRING or type(value) == "string" then
        return Value.string(value)
    elseif hint == Value.TYPE_INT32 then
        return Value.int32(value)
    elseif hint == Value.TYPE_INT64 then
        return Value.int64(value)
    elseif hint == Value.TYPE_FLOAT32 then
        return Value.float32(value)
    elseif hint == Value.TYPE_FLOAT64 then
        return Value.float64(value)
    elseif type(value) == "number" then
        -- Default to float64 for plain numbers
        return Value.float64(value)
    elseif type(value) == "table" then
        -- Assume it's an object
        return Value.object(value)
    else
        return Value.null()
    end
end

--- Perform arithmetic operation
-- @param op The operation ("+", "-", "*", "/", "%", "-" for negation)
-- @param right The right operand (for binary ops)
-- @return A new Value with the result
function Value:Arithmetic(op, right)
    local a = self:ToNative()
    local b = right and right:ToNative() or nil
    
    if op == "+" then return Value.float64(a + b)
    elseif op == "-" then return Value.float64(a - b)
    elseif op == "*" then return Value.float64(a * b)
    elseif op == "/" then return Value.float64(a / b)
    elseif op == "%" then return Value.float64(a % b)
    elseif op == "neg" then return Value.float64(-a)
    end
    
    return Value.null()
end

--- Perform comparison operation
-- @param op The operation ("==", "~=", "<", "<=", ">", ">=")
-- @param right The right operand
-- @return A boolean Value
function Value:Compare(op, right)
    local a = self:ToNative()
    local b = right:ToNative()
    
    local result
    if op == "==" then result = a == b
    elseif op == "~=" then result = a ~= b
    elseif op == "<" then result = a < b
    elseif op == "<=" then result = a <= b
    elseif op == ">" then result = a > b
    elseif op == ">=" then result = a >= b
    else result = false
    end
    
    return Value.bool(result)
end

--- Convert to string for debugging
function Value:__tostring()
    if self.type == Value.TYPE_NULL then
        return "null"
    elseif self.type == Value.TYPE_OBJECT then
        return string.format("Object@%p", self.data)
    else
        return string.format("%s(%s)", self.type, tostring(self.data))
    end
end

return Value
