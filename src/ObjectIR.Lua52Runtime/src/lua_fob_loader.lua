---
-- lua_fob_loader.lua
-- FOB (Finite Open Bytecode) format parser for Lua 5.2
-- Loads ObjectIR modules from FOB binary format
---

local Value = require("lua_value")
local types = require("lua_types")

local FOBLoader = {}

--- Opcode mappings
FOBLoader.OpCode = {
    Nop = 0,
    Dup = 1,
    Pop = 2,
    LdArg = 3,
    LdLoc = 4,
    LdFld = 5,
    LdCon = 6,
    LdStr = 7,
    LdI4 = 8,
    LdI8 = 9,
    LdR4 = 10,
    LdR8 = 11,
    LdTrue = 12,
    LdFalse = 13,
    LdNull = 14,
    StLoc = 15,
    StFld = 16,
    StArg = 17,
    Add = 18,
    Sub = 19,
    Mul = 20,
    Div = 21,
    Rem = 22,
    Neg = 23,
    Ceq = 24,
    Cne = 25,
    Clt = 26,
    Cle = 27,
    Cgt = 28,
    Cge = 29,
    Ret = 30,
    Br = 31,
    BrTrue = 32,
    BrFalse = 33,
    NewObj = 34,
    Call = 35,
    CallVirt = 36,
    CastClass = 37,
    IsInst = 38,
    NewArr = 39,
    LdElem = 40,
    StElem = 41,
    LdLen = 42,
    Break = 43,
    Continue = 44,
    Throw = 45,
    While = 46,
}

--- Reverse lookup for opcode names
FOBLoader.OpCodeNames = {}
for name, code in pairs(FOBLoader.OpCode) do
    FOBLoader.OpCodeNames[code] = name
end

-- ============================================================================
-- Binary Reader Utilities
-- ============================================================================

local function ReadU8(data, pos)
    return string.byte(data, pos), pos + 1
end

local function ReadU16(data, pos)
    local b1, b2 = string.byte(data, pos), string.byte(data, pos + 1)
    return b1 + b2 * 256, pos + 2
end

local function ReadU32(data, pos)
    local b1, b2, b3, b4 = string.byte(data, pos), string.byte(data, pos + 1), 
                           string.byte(data, pos + 2), string.byte(data, pos + 3)
    return b1 + b2 * 256 + b3 * 65536 + b4 * 16777216, pos + 4
end

local function ReadString(data, pos, len)
    return string.sub(data, pos, pos + len - 1), pos + len
end

local function ReadCString(data, pos)
    local start = pos
    while pos <= #data and string.byte(data, pos) ~= 0 do
        pos = pos + 1
    end
    return string.sub(data, start, pos - 1), pos + 1
end

-- ============================================================================
-- FOB Parser
-- ============================================================================

local function ParseHeader(data)
    local pos = 1
    local magic = string.sub(data, pos, pos + 2)
    pos = pos + 3
    
    if magic ~= "FOB" then
        error("Invalid FOB file: wrong magic bytes")
    end
    
    local forkNameLen
    forkNameLen, pos = ReadU8(data, pos)
    local forkName
    forkName, pos = ReadString(data, pos, forkNameLen)
    
    if forkName ~= "OBJECTIR,FOB" then
        error("Unsupported FOB fork: " .. forkName)
    end
    
    local fileSize
    fileSize, pos = ReadU32(data, pos)
    local entryPoint
    entryPoint, pos = ReadU32(data, pos)
    
    return {
        magic = magic,
        forkName = forkName,
        fileSize = fileSize,
        entryPoint = entryPoint,
    }, pos
end

local function ParseSectionHeaders(data, pos, fileSize)
    local sections = {}
    local startAddr = pos
    
    while pos <= #data and startAddr + fileSize > pos do
        local sectionName
        sectionName, pos = ReadCString(data, pos)
        
        if sectionName == "" then
            break
        end
        
        local sectionSize
        sectionSize, pos = ReadU32(data, pos)
        
        table.insert(sections, {
            name = sectionName,
            startAddr = pos,
            size = sectionSize,
        })
        
        pos = pos + sectionSize
    end
    
    return sections
end

local function ParseStringsSection(data, pos, sectionSize)
    local strings = {}
    local endPos = pos + sectionSize
    
    local count
    count, pos = ReadU32(data, pos)
    
    for i = 1, count do
        local len
        len, pos = ReadU32(data, pos)
        local str
        str, pos = ReadString(data, pos, len)
        table.insert(strings, str)
    end
    
    return strings, pos
end

local function ParseTypesSection(data, pos, sectionSize)
    local types_list = {}
    local endPos = pos + sectionSize
    
    local count
    count, pos = ReadU32(data, pos)
    
    for i = 1, count do
        local kind
        kind, pos = ReadU8(data, pos)
        
        local nameIndex
        nameIndex, pos = ReadU32(data, pos)
        
        local namespaceIndex
        namespaceIndex, pos = ReadU32(data, pos)
        
        local access
        access, pos = ReadU8(data, pos)
        
        local flags
        flags, pos = ReadU8(data, pos)
        
        local baseTypeIndex
        baseTypeIndex, pos = ReadU32(data, pos)
        
        local interfaceCount
        interfaceCount, pos = ReadU32(data, pos)
        
        local interfaceIndices = {}
        for j = 1, interfaceCount do
            local idx
            idx, pos = ReadU32(data, pos)
            table.insert(interfaceIndices, idx)
        end
        
        local fieldCount
        fieldCount, pos = ReadU32(data, pos)
        
        local fields = {}
        for j = 1, fieldCount do
            local fieldNameIndex
            fieldNameIndex, pos = ReadU32(data, pos)
            local fieldTypeIndex
            fieldTypeIndex, pos = ReadU32(data, pos)
            local fieldAccess
            fieldAccess, pos = ReadU8(data, pos)
            local fieldFlags
            fieldFlags, pos = ReadU8(data, pos)
            
            table.insert(fields, {
                nameIndex = fieldNameIndex,
                typeIndex = fieldTypeIndex,
                access = fieldAccess,
                flags = fieldFlags,
            })
        end
        
        local methodCount
        methodCount, pos = ReadU32(data, pos)
        
        local methods = {}
        for j = 1, methodCount do
            local methodNameIndex
            methodNameIndex, pos = ReadU32(data, pos)
            local returnTypeIndex
            returnTypeIndex, pos = ReadU32(data, pos)
            local methodAccess
            methodAccess, pos = ReadU8(data, pos)
            local methodFlags
            methodFlags, pos = ReadU8(data, pos)
            
            local paramCount
            paramCount, pos = ReadU32(data, pos)
            
            local parameters = {}
            for k = 1, paramCount do
                local paramNameIndex
                paramNameIndex, pos = ReadU32(data, pos)
                local paramTypeIndex
                paramTypeIndex, pos = ReadU32(data, pos)
                table.insert(parameters, {
                    nameIndex = paramNameIndex,
                    typeIndex = paramTypeIndex,
                })
            end
            
            local localCount
            localCount, pos = ReadU32(data, pos)
            
            local locals = {}
            for k = 1, localCount do
                local localNameIndex
                localNameIndex, pos = ReadU32(data, pos)
                local localTypeIndex
                localTypeIndex, pos = ReadU32(data, pos)
                table.insert(locals, {
                    nameIndex = localNameIndex,
                    typeIndex = localTypeIndex,
                })
            end
            
            local instrCount
            instrCount, pos = ReadU32(data, pos)
            
            local instructions = {}
            for k = 1, instrCount do
                local opcode
                opcode, pos = ReadU8(data, pos)
                local operandCount
                operandCount, pos = ReadU8(data, pos)
                
                local operands = {}
                for o = 1, operandCount do
                    local operand
                    operand, pos = ReadU32(data, pos)
                    table.insert(operands, operand)
                end
                
                table.insert(instructions, {
                    opcode = opcode,
                    operandCount = operandCount,
                    operands = operands,
                })
            end
            
            table.insert(methods, {
                nameIndex = methodNameIndex,
                returnTypeIndex = returnTypeIndex,
                access = methodAccess,
                flags = methodFlags,
                parameters = parameters,
                locals = locals,
                instructions = instructions,
            })
        end
        
        table.insert(types_list, {
            kind = kind,
            nameIndex = nameIndex,
            namespaceIndex = namespaceIndex,
            access = access,
            flags = flags,
            baseTypeIndex = baseTypeIndex,
            interfaceIndices = interfaceIndices,
            fields = fields,
            methods = methods,
        })
    end
    
    return types_list, pos
end

local function ParseCodeSection(data, pos, sectionSize)
    -- Code section structure varies; for now, skip
    return {}, pos + sectionSize
end

local function ParseConstantsSection(data, pos, sectionSize)
    local constants = {}
    local endPos = pos + sectionSize
    
    local count
    count, pos = ReadU32(data, pos)
    
    for i = 1, count do
        local constantType
        constantType, pos = ReadU8(data, pos)
        
        local valueLen
        valueLen, pos = ReadU32(data, pos)
        
        local value
        value, pos = ReadString(data, pos, valueLen)
        
        table.insert(constants, {
            type = constantType,
            value = value,
        })
    end
    
    return constants, pos
end

-- ============================================================================
-- Public API
-- ============================================================================

--- Load a FOB module from a file path
function FOBLoader.LoadFromFile(filePath)
    local file = io.open(filePath, "rb")
    if not file then
        error("Cannot open FOB file: " .. filePath)
    end
    
    local data = file:read("*a")
    file:close()
    
    return FOBLoader.LoadFromData(data)
end

--- Load a FOB module from binary data
function FOBLoader.LoadFromData(data)
    local pos = 1
    
    -- Parse header
    local header
    header, pos = ParseHeader(data)
    
    -- Parse sections
    local sections = ParseSectionHeaders(data, pos, header.fileSize)
    
    -- Initialize section data
    local strings = {}
    local typeDefs = {}
    local instructions = {}
    local constants = {}
    
    -- Parse each section
    for _, section in ipairs(sections) do
        if section.name == ".strings" then
            strings, _ = ParseStringsSection(data, section.startAddr, section.size)
        elseif section.name == ".types" then
            typeDefs, _ = ParseTypesSection(data, section.startAddr, section.size)
        elseif section.name == ".code" then
            instructions, _ = ParseCodeSection(data, section.startAddr, section.size)
        elseif section.name == ".constants" then
            constants, _ = ParseConstantsSection(data, section.startAddr, section.size)
        end
    end
    
    return {
        header = header,
        strings = strings,
        types = typeDefs,
        instructions = instructions,
        constants = constants,
    }
end

return FOBLoader
