#!/usr/bin/env lua
---
-- test_runtime.lua
-- Unit tests for the VirtualMachine and ClassBuilder
---

package.path = package.path .. ";../src/?.lua"

local runtime = require("lua_runtime")
local Value = require("lua_value")
local types = require("lua_types")

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

print("=== ObjectIR Lua 5.2 Runtime - Runtime Tests ===\n")

-- Test VirtualMachine creation
print("Testing VirtualMachine...")
local vm = runtime.VirtualMachine.new()
assert_equals(vm ~= nil, true, "VirtualMachine created")
assert_equals(#vm.classes, 0, "Initial classes list is empty")

-- Test string registration
print("\nTesting string registration...")
local idx1 = vm:RegisterString("hello")
local idx2 = vm:RegisterString("world")
assert_equals(idx1, 0, "First string at index 0")
assert_equals(idx2, 1, "Second string at index 1")
assert_equals(vm:GetString(0), "hello", "String retrieval works")
assert_equals(vm:GetString(1), "world", "String retrieval works")

-- Test constant registration
print("\nTesting constant registration...")
local const1 = vm:RegisterConstant(Value.int32(42))
local const2 = vm:RegisterConstant(Value.string("test"))
assert_equals(const1, 0, "First constant at index 0")
assert_equals(vm:GetConstant(0):ToNative(), 42, "Constant retrieval works")

-- Test ClassBuilder
print("\nTesting ClassBuilder...")
local builder = runtime.ClassBuilder.new(vm)
assert_equals(builder ~= nil, true, "ClassBuilder created")

-- Build a simple class
builder:Class("Point")
    :Field("x", "Float64")
    :Field("y", "Float64")
    :Method("Distance", "Float64")
    :EndMethod()
:EndClass()

local pointClass = vm:GetClassByName("Point")
assert_equals(pointClass ~= nil, true, "Class registered")
assert_equals(pointClass.name, "Point", "Class has correct name")
assert_equals(#pointClass.fields, 2, "Class has 2 fields")
assert_equals(#pointClass.methods, 1, "Class has 1 method")

-- Test class registration
print("\nTesting class registration...")
local manualClass = types.Class.new("TestClass")
local classIdx = vm:RegisterClass(manualClass)
assert_equals(classIdx, 2, "Manual class registered with correct index")
assert_equals(vm:GetClass(1), manualClass, "Class retrieval by index works")

-- Test object creation
print("\nTesting object creation...")
local point = vm:NewObject("Point")
assert_equals(point ~= nil, true, "Object created")
assert_equals(point:GetClass(), pointClass, "Object has correct class")

-- Test field access on objects
print("\nTesting object field access...")
point:SetField("x", Value.float64(3.0))
point:SetField("y", Value.float64(4.0))

local x = point:GetField("x")
local y = point:GetField("y")
assert_equals(x:ToNative(), 3.0, "Field x set and retrieved")
assert_equals(y:ToNative(), 4.0, "Field y set and retrieved")

-- Test class inheritance with builder
print("\nTesting class inheritance...")
builder:Class("Shape")
    :Field("color", "String")
:EndClass()

builder:Class("Circle", "Shape")
    :Field("radius", "Float64")
:EndClass()

local circleClass = vm:GetClassByName("Circle")
assert_equals(circleClass.baseClass ~= nil, true, "Circle has base class")
assert_equals(circleClass.baseClass.name, "Shape", "Circle's base is Shape")

local circle = vm:NewObject("Circle")
circle:SetField("color", Value.string("red"))
circle:SetField("radius", Value.float64(5.0))

local color = circle:GetField("color")
assert_equals(color:ToNative(), "red", "Inherited field accessible")

-- Test method invocation
print("\nTesting method invocation...")
builder:Class("Calculator")
    :Method("Add", "Int32")
    :EndMethod()
:EndClass()

-- Get the calculator class and add a native implementation
local calcClass = vm:GetClassByName("Calculator")
local addMethod = calcClass:GetMethod("Add")

if addMethod then
    addMethod:AddParameter("a", "Int32")
    addMethod:AddParameter("b", "Int32")
    
    -- Create a simple native wrapper
    function addMethod.body(instance, args)
        if args and #args >= 2 then
            local a = args[1]:ToNative()
            local b = args[2]:ToNative()
            return Value.int32(a + b)
        end
        return Value.null()
    end
    
    -- Now test invocation
    local result = vm:InvokeMethod("Calculator", "Add", {Value.int32(10), Value.int32(20)})
    assert_equals(result:ToNative(), 30, "Method invocation works")
end

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
