#!/usr/bin/env lua
---
-- example_fob_loader.lua
-- Demonstrates loading and executing a FOB module
-- This example shows how to load a pre-compiled ObjectIR module
---

local function add_to_path()
    local scriptDir = debug.getinfo(1, "S").source:match("^@?(.*/)") or "./"
    package.path = package.path .. ";" .. scriptDir .. "../src/?.lua"
end
add_to_path()

local runtime = require("lua_runtime")
local FOBLoader = require("lua_fob_loader")

print("=== ObjectIR Lua 5.2 Runtime - FOB Loader Example ===\n")

-- Example usage (requires a valid FOB file)
local function LoadAndExecuteModule(fobFilePath)
    print("Loading FOB module: " .. fobFilePath)
    
    -- Load the FOB data
    local fobData = FOBLoader.LoadFromFile(fobFilePath)
    
    print("\nFOB Module Information:")
    print("  Fork: " .. fobData.header.forkName)
    print("  File size: " .. fobData.header.fileSize .. " bytes")
    print("  Entry point: " .. fobData.header.entryPoint)
    print("  Classes: " .. #fobData.types)
    print("  Strings: " .. #fobData.strings)
    print("  Constants: " .. #fobData.constants)
    
    -- Build virtual machine from FOB
    local vm = runtime.VirtualMachine.LoadFromData(
        io.open(fobFilePath, "rb"):read("*a")
    )
    
    print("\nVirtual machine created successfully!")
    print("Registered classes:")
    for i, class in ipairs(vm.classes) do
        if class then
            print("  [" .. (i-1) .. "] " .. class.name)
        end
    end
    
    -- Execute entry point
    local entryIdx = fobData.header.entryPoint
    if entryIdx > 0 and vm.classes[entryIdx] then
        local entryClass = vm.classes[entryIdx]
        print("\nEntry point class: " .. entryClass.name)
        
        -- Find and execute a main method if it exists
        local mainMethod = entryClass:GetMethod("Main")
        if mainMethod then
            print("Executing Main method...")
            local result = vm:InvokeMethod(entryClass.name, "Main", {})
            print("Result: " .. tostring(result))
        else
            print("No Main method found in entry class")
        end
    end
end

-- Usage
if arg[1] then
    LoadAndExecuteModule(arg[1])
else
    print("Usage: lua example_fob_loader.lua <path-to-fob-file>")
    print("\nExample:")
    print("  lua example_fob_loader.lua ../../OIFortran/test.fob")
    print("\nThis example demonstrates:")
    print("  1. Loading a FOB module from a file")
    print("  2. Parsing the binary format")
    print("  3. Building a runtime from the parsed data")
    print("  4. Discovering classes and methods")
    print("  5. Executing entry point methods")
end
