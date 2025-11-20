#!/usr/bin/env lua
---
-- example_oop_demo.lua
-- Object-oriented programming demonstration
-- Shows classes, inheritance, and virtual methods
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
local builder = runtime.ClassBuilder.new(vm)

-- Create a base Shape class
builder:Class("Shape")
    :Field("name", "String")
    :Method("Area", "Float64")
    :EndMethod()
    :Method("GetName", "String")
    :EndMethod()
:EndClass()

-- Create a Circle class that extends Shape
builder:Class("Circle", "Shape")
    :Field("radius", "Float64")
    :Method("Area", "Float64")
    :EndMethod()
:EndClass()

-- Create a Rectangle class that extends Shape
builder:Class("Rectangle", "Shape")
    :Field("width", "Float64")
    :Field("height", "Float64")
    :Method("Area", "Float64")
    :EndMethod()
:EndClass()

print("=== ObjectIR Lua 5.2 Runtime - OOP Example ===\n")

-- Create instances
local circle = vm:NewObject("Circle")
circle:SetField("name", Value.string("Circle"))
circle:SetField("radius", Value.float64(5.0))

local rectangle = vm:NewObject("Rectangle")
rectangle:SetField("name", Value.string("Rectangle"))
rectangle:SetField("width", Value.float64(4.0))
rectangle:SetField("height", Value.float64(6.0))

print("Created objects:")
print("  Circle with radius = 5.0")
print("  Rectangle with width = 4.0, height = 6.0")

-- Access fields
local circleName = circle:GetField("name")
local circleRadius = circle:GetField("radius")
print("\nCircle name: " .. circleName:ToNative())
print("Circle radius: " .. circleRadius:ToNative())

local rectName = rectangle:GetField("name")
local rectWidth = rectangle:GetField("width")
local rectHeight = rectangle:GetField("height")
print("\nRectangle name: " .. rectName:ToNative())
print("Rectangle width: " .. rectWidth:ToNative())
print("Rectangle height: " .. rectHeight:ToNative())

print("\n✓ OOP example completed successfully!")
print("\nNote: Full virtual method dispatch would be demonstrated")
print("      when executing methods from FOB bytecode.")
