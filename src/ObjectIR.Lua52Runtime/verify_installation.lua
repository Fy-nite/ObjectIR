#!/usr/bin/env lua
---
-- verify_installation.lua
-- Verifies that the ObjectIR Lua 5.2 Runtime is properly installed
-- and all modules can be loaded
---

package.path = package.path .. ";src/?.lua"

print("=== ObjectIR Lua 5.2 Runtime - Installation Verification ===\n")

-- Check Lua version
print("Lua Version: " .. _VERSION)
assert(_VERSION:match("5.2") or _VERSION:match("5.3") or _VERSION:match("5.4") or _VERSION:match("LuaJIT"),
       "Error: Lua 5.2+ required")
print("✓ Lua version compatible\n")

-- Try to load each module
local modules = {
    "lua_value",
    "lua_types", 
    "lua_fob_loader",
    "lua_ir_instruction",
    "lua_runtime"
}

print("Loading modules...")
local loadedModules = {}

for _, moduleName in ipairs(modules) do
    local status, result = pcall(require, moduleName)
    if status then
        loadedModules[moduleName] = result
        print("✓ Loaded: " .. moduleName)
    else
        print("✗ Failed to load: " .. moduleName)
        print("  Error: " .. result)
        os.exit(1)
    end
end

print("\nVerifying module contents...")

-- Verify Value module
local Value = loadedModules.lua_value
assert(Value.int32, "Value.int32 not found")
assert(Value.string, "Value.string not found")
assert(Value.null, "Value.null not found")
print("✓ Value module complete")

-- Verify Types module
local types = loadedModules.lua_types
assert(types.TypeReference, "TypeReference not found")
assert(types.Class, "Class not found")
assert(types.Field, "Field not found")
assert(types.Method, "Method not found")
assert(types.Object, "Object not found")
print("✓ Types module complete")

-- Verify FOBLoader module
local FOBLoader = loadedModules.lua_fob_loader
assert(FOBLoader.LoadFromFile, "FOBLoader.LoadFromFile not found")
assert(FOBLoader.LoadFromData, "FOBLoader.LoadFromData not found")
assert(FOBLoader.OpCode, "FOBLoader.OpCode not found")
print("✓ FOBLoader module complete")

-- Verify Instruction Executor
local executor = loadedModules.lua_ir_instruction
assert(executor.Execute, "InstructionExecutor.Execute not found")
print("✓ Instruction Executor module complete")

-- Verify Runtime module
local runtime = loadedModules.lua_runtime
assert(runtime.VirtualMachine, "VirtualMachine not found")
assert(runtime.ExecutionContext, "ExecutionContext not found")
assert(runtime.ClassBuilder, "ClassBuilder not found")
print("✓ Runtime module complete")

print("\nTesting basic functionality...")

-- Test Value creation
local val1 = Value.int32(42)
assert(val1:IsType(Value.TYPE_INT32), "Value type check failed")
assert(val1:ToNative() == 42, "Value conversion failed")
print("✓ Value creation and operations")

-- Test Type creation
local typeRef = types.TypeReference.Int32()
assert(typeRef.isPrimitive, "Type reference check failed")
print("✓ Type reference creation")

-- Test Class and Object
local class = types.Class.new("TestClass")
class:AddField(types.Field.new("field", types.TypeReference.String()))
assert(#class.fields == 1, "Field addition failed")
local obj = types.Object.new(class)
assert(obj:GetClass() == class, "Object creation failed")
print("✓ Class and object creation")

-- Test VirtualMachine
local vm = runtime.VirtualMachine.new()
assert(vm ~= nil, "VirtualMachine creation failed")
vm:RegisterClass(class)
assert(vm:GetClassByName("TestClass") == class, "Class registration failed")
print("✓ VirtualMachine registration")

-- Test ClassBuilder
local builder = runtime.ClassBuilder.new(vm)
builder:Class("BuiltClass")
    :Field("name", "String")
:EndClass()
assert(vm:GetClassByName("BuiltClass") ~= nil, "ClassBuilder failed")
print("✓ ClassBuilder fluent API")

-- Test string and constant registration
local strIdx = vm:RegisterString("test")
assert(vm:GetString(strIdx) == "test", "String registration failed")
local constIdx = vm:RegisterConstant(Value.int32(100))
assert(vm:GetConstant(constIdx):ToNative() == 100, "Constant registration failed")
print("✓ String and constant tables")

print("\n=== Installation Verification Complete ===")
print("\nAll modules loaded successfully!")
print("All basic functionality tests passed!")
print("\nYou can now:")
print("1. Run example programs: lua examples/example_*.lua")
print("2. Run test suite: lua tests/test_*.lua")
print("3. Load FOB files: lua examples/example_fob_loader.lua <path>")
print("4. Use in your Lua code: require 'lua_runtime'")

print("\nFor more information, see:")
print("- README.md - Overview and architecture")
print("- GETTING_STARTED.md - Quick start guide")
print("- IMPLEMENTATION.md - Developer documentation")
print("- SUMMARY.md - Complete feature summary")
