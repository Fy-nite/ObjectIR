---
-- lua_runtime.lua
-- ObjectIR Virtual Machine implementation for Lua 5.2
-- Core runtime engine for executing ObjectIR bytecode
---

local Value = require("lua_value")
local types = require("lua_types")
local FOBLoader = require("lua_fob_loader")
local InstructionExecutor = require("lua_ir_instruction")

-- ============================================================================
-- ExecutionContext: Stack frame for method execution
-- ============================================================================

local ExecutionContext = {}
ExecutionContext.__index = ExecutionContext

function ExecutionContext.new(method, instance, arguments)
    local self = setmetatable({}, ExecutionContext)
    self.method = method
    self.instance = instance
    self.arguments = arguments or {}
    self.stack = {}
    self.locals = {}
    self.pc = 0
    self.returnValue = Value.null()
    return self
end

-- ============================================================================
-- VirtualMachine: Runtime execution engine
-- ============================================================================

local VirtualMachine = {}
VirtualMachine.__index = VirtualMachine

function VirtualMachine.new()
    local self = setmetatable({}, VirtualMachine)
    self.classes = {}
    self.classNameMap = {}
    self.strings = {}
    self.constants = {}
    self.methods = {}
    self.callStack = {}
    return self
end

--- Register a class in the virtual machine
function VirtualMachine:RegisterClass(class)
    local index = #self.classes + 1
    self.classes[index] = class
    self.classNameMap[class.name] = index
    self.methods[index] = {}
    for i, method in ipairs(class:GetAllMethods()) do
        self.methods[index][i] = method
    end
    return index
end

--- Get a class by index
function VirtualMachine:GetClass(index)
    if type(index) == "number" then
        return self.classes[index + 1]
    end
    return self.classes[self.classNameMap[index] or 0]
end

--- Get a class by name
function VirtualMachine:GetClassByName(name)
    return self.classes[self.classNameMap[name] or 0]
end

--- Register a string in the string table
function VirtualMachine:RegisterString(str)
    table.insert(self.strings, str)
    return #self.strings - 1
end

--- Get a string by index
function VirtualMachine:GetString(index)
    return self.strings[index + 1] or ""
end

--- Register a constant
function VirtualMachine:RegisterConstant(value)
    table.insert(self.constants, value)
    return #self.constants - 1
end

--- Get a constant by index
function VirtualMachine:GetConstant(index)
    return self.constants[index + 1] or Value.null()
end

--- Get a method by class and method indices
function VirtualMachine:GetMethod(classIndex, methodIndex)
    local methods = self.methods[classIndex + 1]
    if methods then
        return methods[methodIndex + 1]
    end
    return nil
end

--- Execute a method
function VirtualMachine:ExecuteMethod(classIndex, methodIndex, arguments)
    local class = self:GetClass(classIndex)
    if not class then
        error("Class not found: " .. classIndex)
    end
    
    local method = self:GetMethod(classIndex, methodIndex)
    if not method then
        error("Method not found in class " .. class.name .. ": " .. methodIndex)
    end
    
    -- If it's a native method, call it directly
    if method.body then
        return method.body(nil, arguments)
    end
    
    -- Create execution context
    local context = ExecutionContext.new(method, nil, arguments)
    
    -- Execute instructions
    local instructions = method.instructions
    while context.pc < #instructions do
        context.pc = context.pc + 1
        local instruction = instructions[context.pc]
        if not InstructionExecutor.Execute(self, context, instruction) then
            break
        end
    end
    
    return context.returnValue
end

--- Invoke a method by class name and method name
function VirtualMachine:InvokeMethod(className, methodName, arguments)
    local class = self:GetClassByName(className)
    if not class then
        error("Class not found: " .. className)
    end
    
    local method = class:GetMethod(methodName)
    if not method then
        error("Method not found: " .. className .. "." .. methodName)
    end
    
    -- If it's a native method, call it directly
    if method.body then
        return method.body(nil, arguments)
    end
    
    -- Create execution context
    local context = ExecutionContext.new(method, nil, arguments)
    
    -- Execute instructions
    local instructions = method.instructions
    while context.pc < #instructions do
        context.pc = context.pc + 1
        local instruction = instructions[context.pc]
        if not InstructionExecutor.Execute(self, context, instruction) then
            break
        end
    end
    
    return context.returnValue
end

--- Create a new object instance
function VirtualMachine:NewObject(className)
    local class = self:GetClassByName(className)
    if not class then
        error("Class not found: " .. className)
    end
    return types.Object.new(class)
end

-- ============================================================================
-- ClassBuilder: Fluent API for building classes
-- ============================================================================

local ClassBuilder = {}
ClassBuilder.__index = ClassBuilder

function ClassBuilder.new(vm)
    local self = setmetatable({}, ClassBuilder)
    self.vm = vm
    self.currentClass = nil
    self.currentMethod = nil
    return self
end

function ClassBuilder:Class(name, baseClassName)
    local baseClass = nil
    if baseClassName then
        baseClass = self.vm:GetClassByName(baseClassName)
    end
    self.currentClass = types.Class.new(name, baseClass)
    return self
end

function ClassBuilder:Field(name, typeName)
    if not self.currentClass then
        error("No class in context")
    end
    local typeRef = self:GetType(typeName)
    local field = types.Field.new(name, typeRef)
    self.currentClass:AddField(field)
    return self
end

function ClassBuilder:Method(name, returnTypeName)
    if not self.currentClass then
        error("No class in context")
    end
    local returnType = self:GetType(returnTypeName)
    self.currentMethod = types.Method.new(name, returnType)
    return self
end

function ClassBuilder:Parameter(name, typeName)
    if not self.currentMethod then
        error("No method in context")
    end
    local typeRef = self:GetType(typeName)
    self.currentMethod:AddParameter(name, typeRef)
    return self
end

function ClassBuilder:EndMethod()
    if self.currentMethod and self.currentClass then
        self.currentClass:AddMethod(self.currentMethod)
    end
    self.currentMethod = nil
    return self
end

function ClassBuilder:EndClass()
    if self.currentClass then
        self.vm:RegisterClass(self.currentClass)
    end
    self.currentClass = nil
    return self
end

function ClassBuilder:GetType(typeName)
    if typeName == "Int32" or typeName == "int32" then
        return types.TypeReference.Int32()
    elseif typeName == "Int64" or typeName == "int64" then
        return types.TypeReference.Int64()
    elseif typeName == "Float32" or typeName == "float32" then
        return types.TypeReference.Float32()
    elseif typeName == "Float64" or typeName == "float64" then
        return types.TypeReference.Float64()
    elseif typeName == "Bool" or typeName == "bool" then
        return types.TypeReference.Bool()
    elseif typeName == "String" or typeName == "string" then
        return types.TypeReference.String()
    elseif typeName == "Void" or typeName == "void" then
        return types.TypeReference.Void()
    else
        -- Assume it's a class name
        local class = self.vm:GetClassByName(typeName)
        if class then
            return types.TypeReference.Object(class)
        end
        return types.TypeReference.Void()
    end
end

-- ============================================================================
-- Module loading from FOB format
-- ============================================================================

local function BuildVirtualMachineFromFOB(fobData)
    local vm = VirtualMachine.new()
    
    -- Register strings
    for i, str in ipairs(fobData.strings) do
        vm.strings[i] = str
    end
    
    -- Register constants
    for i, constant in ipairs(fobData.constants) do
        local value
        if constant.type == 0 then  -- Null
            value = Value.null()
        elseif constant.type == 1 then  -- Int32
            value = Value.int32(tonumber(constant.value))
        elseif constant.type == 2 then  -- Int64
            value = Value.int64(tonumber(constant.value))
        elseif constant.type == 3 then  -- Float32
            value = Value.float32(tonumber(constant.value))
        elseif constant.type == 4 then  -- Float64
            value = Value.float64(tonumber(constant.value))
        elseif constant.type == 5 then  -- Bool
            value = Value.bool(constant.value == "1" or constant.value == "true")
        elseif constant.type == 6 then  -- String
            value = Value.string(constant.value)
        else
            value = Value.null()
        end
        vm.constants[i] = value
    end
    
    -- Build type definitions
    for typeIdx, typeDef in ipairs(fobData.types) do
        local className = fobData.strings[typeDef.nameIndex + 1] or "UnknownClass"
        local baseClass = nil
        
        if typeDef.baseTypeIndex ~= 0xFFFFFFFF and typeDef.baseTypeIndex > 0 then
            baseClass = vm.classes[typeDef.baseTypeIndex]
        end
        
        local class = types.Class.new(className, baseClass)
        
        -- Add fields
        for _, fieldDef in ipairs(typeDef.fields) do
            local fieldName = fobData.strings[fieldDef.nameIndex + 1] or "field"
            local field = types.Field.new(fieldName)
            class:AddField(field)
        end
        
        -- Add methods
        for _, methodDef in ipairs(typeDef.methods) do
            local methodName = fobData.strings[methodDef.nameIndex + 1] or "method"
            local method = types.Method.new(methodName)
            
            -- Add parameters
            for _, paramDef in ipairs(methodDef.parameters) do
                local paramName = fobData.strings[paramDef.nameIndex + 1] or "param"
                method:AddParameter(paramName)
            end
            
            -- Store instructions
            method:SetInstructions(methodDef.instructions)
            
            class:AddMethod(method)
        end
        
        vm:RegisterClass(class)
    end
    
    return vm, fobData.header.entryPoint
end

--- Load a FOB module from a file
function VirtualMachine.LoadFromFile(filePath)
    local fobData = FOBLoader.LoadFromFile(filePath)
    return BuildVirtualMachineFromFOB(fobData)
end

--- Load a FOB module from binary data
function VirtualMachine.LoadFromData(data)
    local fobData = FOBLoader.LoadFromData(data)
    return BuildVirtualMachineFromFOB(fobData)
end

-- ============================================================================
-- Export
-- ============================================================================

return {
    VirtualMachine = VirtualMachine,
    ExecutionContext = ExecutionContext,
    ClassBuilder = ClassBuilder,
}
