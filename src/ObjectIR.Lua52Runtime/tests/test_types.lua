#!/usr/bin/env lua
---
-- test_types.lua
-- Unit tests for the type system and class model
---

package.path = package.path .. ";../src/?.lua"

local types = require("lua_types")
local Value = require("lua_value")

-- Test counter
local testsRun = 0
local testsPassed = 0

local function assert_equals(actual, expected, message)
    testsRun = testsRun + 1
    if actual == expected then
        testsPassed = testsPassed + 1
        print("✓ " .. message)
    else
        print("✗ " .. message)
        print("  Expected: " .. tostring(expected) .. ", Got: " .. tostring(actual))
    end
end

local function assert_true(actual, message)
    assert_equals(actual, true, message)
end

local function assert_false(actual, message)
    assert_equals(actual, false, message)
end

print("=== ObjectIR Lua 5.2 Runtime - Type System Tests ===\n")

-- Test TypeReference
print("Testing TypeReference...")
local intType = types.TypeReference.Int32()
assert_equals(intType.isPrimitive, true, "Int32 is primitive")
assert_equals(intType.primitiveType, "int32", "Int32 has correct type")

local float64Type = types.TypeReference.Float64()
assert_equals(float64Type.isPrimitive, true, "Float64 is primitive")

-- Test Field
print("\nTesting Field...")
local field = types.Field.new("myField", types.TypeReference.Int32())
assert_equals(field.name, "myField", "Field has correct name")
assert_equals(field.typeRef.primitiveType, "int32", "Field has correct type")

-- Test Method
print("\nTesting Method...")
local method = types.Method.new("Calculate", types.TypeReference.Int32())
assert_equals(method.name, "Calculate", "Method has correct name")
assert_equals(method.returnType.primitiveType, "int32", "Method has correct return type")

method:AddParameter("x", types.TypeReference.Int32())
method:AddParameter("y", types.TypeReference.Int32())
assert_equals(#method.parameters, 2, "Method has correct parameter count")

-- Test Class
print("\nTesting Class...")
local baseClass = types.Class.new("Animal")
baseClass:AddField(types.Field.new("name", types.TypeReference.String()))
baseClass:AddMethod(types.Method.new("Speak", types.TypeReference.String()))

assert_equals(baseClass.name, "Animal", "Class has correct name")
assert_equals(#baseClass.fields, 1, "Class has correct field count")
assert_equals(#baseClass.methods, 1, "Class has correct method count")

-- Test inheritance
local dogClass = types.Class.new("Dog", baseClass)
assert_equals(dogClass.baseClass, baseClass, "Dog has Animal as base class")

local field = dogClass:GetField("name")
assert_equals(field ~= nil, true, "Inherited field accessible")
assert_equals(field.name, "name", "Inherited field has correct name")

local method = dogClass:GetMethod("Speak")
assert_equals(method ~= nil, true, "Inherited method accessible")

-- Test subclass checking
assert_true(dogClass:IsSubclassOf(baseClass), "Dog is subclass of Animal")
assert_true(dogClass:IsSubclassOf(dogClass), "Dog is subclass of itself")
assert_false(baseClass:IsSubclassOf(dogClass), "Animal is not subclass of Dog")

-- Test Object
print("\nTesting Object...")
local obj = types.Object.new(dogClass)
assert_equals(obj:GetClass(), dogClass, "Object has correct class")

-- Test field access
obj:SetField("name", Value.string("Buddy"))
local name = obj:GetField("name")
assert_equals(name:ToNative(), "Buddy", "Object field get/set works")

-- Test all fields retrieval
local allFields = dogClass:GetAllFields()
assert_equals(#allFields, 1, "All fields includes inherited fields")

-- Test all methods retrieval
local allMethods = dogClass:GetAllMethods()
assert_equals(#allMethods, 1, "All methods includes inherited methods")

-- Test method body
print("\nTesting Method with native body...")
local nativeMethod = types.Method.new("Add", types.TypeReference.Int32())
nativeMethod:AddParameter("a", types.TypeReference.Int32())
nativeMethod:AddParameter("b", types.TypeReference.Int32())

-- Set a native implementation
function nativeMethod:SetBody(func)
    self.body = func
    return self
end

nativeMethod:SetBody(function(instance, args)
    return Value.int32(args[1]:ToNative() + args[2]:ToNative())
end)

assert_equals(nativeMethod.body ~= nil, true, "Method has native body")

-- Test method invocation
local calcClass = types.Class.new("Calculator")
calcClass:AddMethod(nativeMethod)

local calculator = types.Object.new(calcClass)
local result = calculator:InvokeMethod("Add", {Value.int32(5), Value.int32(3)})
assert_equals(result.method ~= nil or result:ToNative() ~= nil, true, "Method invocation returns result")

-- Summary
print("\n=== Test Summary ===")
print(string.format("Tests run: %d", testsRun))
print(string.format("Tests passed: %d", testsPassed))
print(string.format("Tests failed: %d", testsRun - testsPassed))

if testsPassed == testsRun then
    print("\n✓ All tests passed!")
else
    print("\n✗ Some tests failed")
end
