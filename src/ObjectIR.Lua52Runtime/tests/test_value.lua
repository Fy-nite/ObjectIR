#!/usr/bin/env lua
---
-- test_value.lua
-- Unit tests for the Value type system
---

package.path = package.path .. ";../src/?.lua"

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

print("=== ObjectIR Lua 5.2 Runtime - Value Type Tests ===\n")

-- Test null values
print("Testing null values...")
local nullVal = Value.null()
assert_true(nullVal:IsNull(), "null() creates null value")
assert_equals(nullVal:IsType(Value.TYPE_NULL), true, "IsNull check")

-- Test int32 values
print("\nTesting int32 values...")
local i32 = Value.int32(42)
assert_equals(i32:IsType(Value.TYPE_INT32), true, "int32() creates int32 value")
assert_equals(i32:ToNative(), 42, "int32 native value")

-- Test int64 values
print("\nTesting int64 values...")
local i64 = Value.int64(999999999999)
assert_equals(i64:IsType(Value.TYPE_INT64), true, "int64() creates int64 value")
assert_equals(i64:ToNative(), 999999999999, "int64 native value")

-- Test float values
print("\nTesting float values...")
local f32 = Value.float32(3.14)
assert_equals(f32:IsType(Value.TYPE_FLOAT32), true, "float32() creates float32 value")

local f64 = Value.float64(2.71828)
assert_equals(f64:IsType(Value.TYPE_FLOAT64), true, "float64() creates float64 value")

-- Test bool values
print("\nTesting bool values...")
local t = Value.bool(true)
assert_equals(t:IsType(Value.TYPE_BOOL), true, "bool(true) creates bool value")
assert_equals(t:ToNative(), true, "bool(true) native value")

local f = Value.bool(false)
assert_equals(f:ToNative(), false, "bool(false) native value")

-- Test string values
print("\nTesting string values...")
local str = Value.string("hello")
assert_equals(str:IsType(Value.TYPE_STRING), true, "string() creates string value")
assert_equals(str:ToNative(), "hello", "string native value")

-- Test arithmetic
print("\nTesting arithmetic operations...")
local a = Value.int32(10)
local b = Value.int32(5)

local sum = a:Arithmetic("+", b)
assert_equals(sum:ToNative(), 15, "Addition: 10 + 5 = 15")

local diff = a:Arithmetic("-", b)
assert_equals(diff:ToNative(), 5, "Subtraction: 10 - 5 = 5")

local prod = a:Arithmetic("*", b)
assert_equals(prod:ToNative(), 50, "Multiplication: 10 * 5 = 50")

local quot = a:Arithmetic("/", b)
assert_equals(quot:ToNative(), 2, "Division: 10 / 5 = 2")

-- Test comparison
print("\nTesting comparison operations...")
local eq = a:Compare("==", Value.int32(10))
assert_equals(eq:ToNative(), true, "Equality: 10 == 10")

local ne = a:Compare("~=", b)
assert_equals(ne:ToNative(), true, "Not equal: 10 ~= 5")

local lt = b:Compare("<", a)
assert_equals(lt:ToNative(), true, "Less than: 5 < 10")

local gt = a:Compare(">", b)
assert_equals(gt:ToNative(), true, "Greater than: 10 > 5")

local le = Value.int32(10):Compare("<=", Value.int32(10))
assert_equals(le:ToNative(), true, "Less or equal: 10 <= 10")

local ge = Value.int32(10):Compare(">=", Value.int32(5))
assert_equals(ge:ToNative(), true, "Greater or equal: 10 >= 5")

-- Test FromNative conversion
print("\nTesting FromNative conversions...")
local native_int = Value.FromNative(42)
assert_equals(native_int:ToNative(), 42, "FromNative(42) converts to number")

local native_str = Value.FromNative("test")
assert_equals(native_str:ToNative(), "test", "FromNative('test') converts to string")

local native_bool = Value.FromNative(true)
assert_equals(native_bool:ToNative(), true, "FromNative(true) converts to bool")

local native_nil = Value.FromNative(nil)
assert_equals(native_nil:IsNull(), true, "FromNative(nil) converts to null")

-- Test object values
print("\nTesting object values...")
local obj = {value = 42}
local objVal = Value.object(obj)
assert_equals(objVal:IsType(Value.TYPE_OBJECT), true, "object() creates object value")
assert_equals(objVal:ToNative(), obj, "object native value")

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
