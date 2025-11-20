#!/usr/bin/env lua
---
-- example_simple_calculator.lua
-- Simple calculator example showing basic arithmetic operations
---

local function add_to_path()
    local scriptDir = debug.getinfo(1, "S").source:match("^@?(.*/)") or "./"
    package.path = package.path .. ";" .. scriptDir .. "../src/?.lua"
end
add_to_path()

local runtime = require("lua_runtime")
local Value = require("lua_value")

-- Create a virtual machine
local vm = runtime.VirtualMachine.new()

-- Create a Calculator class using the builder API
local builder = runtime.ClassBuilder.new(vm)

-- Build the Calculator class
builder:Class("Calculator")
    :Method("Add", "Int32")
        :Parameter("a", "Int32")
        :Parameter("b", "Int32")
    :EndMethod()
    :Method("Subtract", "Int32")
        :Parameter("a", "Int32")
        :Parameter("b", "Int32")
    :EndMethod()
    :Method("Multiply", "Int32")
        :Parameter("a", "Int32")
        :Parameter("b", "Int32")
    :EndMethod()
:EndClass()

-- Add native implementations
local calcClass = vm:GetClassByName("Calculator")
local addMethod = calcClass:GetMethod("Add")
local subtractMethod = calcClass:GetMethod("Subtract")
local multiplyMethod = calcClass:GetMethod("Multiply")

-- Simple native implementation
addMethod.body = function(instance, args)
    local a = args[1]:ToNative()
    local b = args[2]:ToNative()
    return Value.int32(a + b)
end

subtractMethod.body = function(instance, args)
    local a = args[1]:ToNative()
    local b = args[2]:ToNative()
    return Value.int32(a - b)
end

multiplyMethod.body = function(instance, args)
    local a = args[1]:ToNative()
    local b = args[2]:ToNative()
    return Value.int32(a * b)
end

-- Test the calculator
print("=== ObjectIR Lua 5.2 Runtime - Calculator Example ===\n")

local result1 = vm:InvokeMethod("Calculator", "Add", {Value.int32(10), Value.int32(20)})
print("10 + 20 = " .. result1:ToNative())

local result2 = vm:InvokeMethod("Calculator", "Subtract", {Value.int32(50), Value.int32(15)})
print("50 - 15 = " .. result2:ToNative())

local result3 = vm:InvokeMethod("Calculator", "Multiply", {Value.int32(7), Value.int32(8)})
print("7 × 8 = " .. result3:ToNative())

print("\n✓ Calculator example completed successfully!")
