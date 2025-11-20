---
-- lua_ir_instruction.lua
-- ObjectIR instruction executor for Lua 5.2
-- Executes IR opcodes on the stack-based virtual machine
---

local Value = require("lua_value")
local types = require("lua_types")
local FOBLoader = require("lua_fob_loader")

local InstructionExecutor = {}

--- Execute a single instruction
-- @param vm The virtual machine
-- @param context The execution context (stack, locals, etc.)
-- @param instruction The instruction to execute
-- @return true if execution should continue, false to return
function InstructionExecutor.Execute(vm, context, instruction)
    local opcode = instruction.opcode
    local operands = instruction.operands or {}
    
    -- Stack operations
    if opcode == FOBLoader.OpCode.Nop then
        return true
        
    elseif opcode == FOBLoader.OpCode.Dup then
        if #context.stack > 0 then
            table.insert(context.stack, context.stack[#context.stack])
        end
        return true
        
    elseif opcode == FOBLoader.OpCode.Pop then
        if #context.stack > 0 then
            table.remove(context.stack)
        end
        return true
    
    -- Load operations
    elseif opcode == FOBLoader.OpCode.LdArg then
        local argIndex = operands[1] or 0
        local value = context.arguments[argIndex + 1] or Value.null()
        table.insert(context.stack, value)
        return true
        
    elseif opcode == FOBLoader.OpCode.LdLoc then
        local locIndex = operands[1] or 0
        local value = context.locals[locIndex + 1] or Value.null()
        table.insert(context.stack, value)
        return true
        
    elseif opcode == FOBLoader.OpCode.LdFld then
        local fieldNameIndex = operands[1] or 0
        local fieldName = vm:GetString(fieldNameIndex)
        if #context.stack > 0 then
            local obj = context.stack[#context.stack]:ToNative()
            if obj and type(obj) == "table" and obj.GetField then
                local value = obj:GetField(fieldName)
                table.remove(context.stack)
                table.insert(context.stack, value or Value.null())
            else
                table.remove(context.stack)
            end
        end
        return true
        
    elseif opcode == FOBLoader.OpCode.LdCon then
        local constantIndex = operands[1] or 0
        local constant = vm:GetConstant(constantIndex)
        table.insert(context.stack, constant or Value.null())
        return true
        
    elseif opcode == FOBLoader.OpCode.LdStr then
        local stringIndex = operands[1] or 0
        local str = vm:GetString(stringIndex)
        table.insert(context.stack, Value.string(str))
        return true
        
    elseif opcode == FOBLoader.OpCode.LdI4 then
        local val = operands[1] or 0
        table.insert(context.stack, Value.int32(val))
        return true
        
    elseif opcode == FOBLoader.OpCode.LdI8 then
        local val = operands[1] or 0
        table.insert(context.stack, Value.int64(val))
        return true
        
    elseif opcode == FOBLoader.OpCode.LdR4 then
        local val = operands[1] or 0
        table.insert(context.stack, Value.float32(val))
        return true
        
    elseif opcode == FOBLoader.OpCode.LdR8 then
        local val = operands[1] or 0
        table.insert(context.stack, Value.float64(val))
        return true
        
    elseif opcode == FOBLoader.OpCode.LdTrue then
        table.insert(context.stack, Value.bool(true))
        return true
        
    elseif opcode == FOBLoader.OpCode.LdFalse then
        table.insert(context.stack, Value.bool(false))
        return true
        
    elseif opcode == FOBLoader.OpCode.LdNull then
        table.insert(context.stack, Value.null())
        return true
    
    -- Store operations
    elseif opcode == FOBLoader.OpCode.StLoc then
        local locIndex = operands[1] or 0
        if #context.stack > 0 then
            context.locals[locIndex + 1] = table.remove(context.stack)
        end
        return true
        
    elseif opcode == FOBLoader.OpCode.StFld then
        local fieldNameIndex = operands[1] or 0
        local fieldName = vm:GetString(fieldNameIndex)
        if #context.stack >= 2 then
            local value = table.remove(context.stack)
            local obj = context.stack[#context.stack]:ToNative()
            if obj and type(obj) == "table" and obj.SetField then
                obj:SetField(fieldName, value)
                table.remove(context.stack)
                table.insert(context.stack, value)
            else
                table.remove(context.stack)
            end
        end
        return true
        
    elseif opcode == FOBLoader.OpCode.StArg then
        local argIndex = operands[1] or 0
        if #context.stack > 0 then
            context.arguments[argIndex + 1] = table.remove(context.stack)
        end
        return true
    
    -- Arithmetic operations
    elseif opcode == FOBLoader.OpCode.Add then
        if #context.stack >= 2 then
            local b = table.remove(context.stack)
            local a = table.remove(context.stack)
            local result = a:Arithmetic("+", b)
            table.insert(context.stack, result)
        end
        return true
        
    elseif opcode == FOBLoader.OpCode.Sub then
        if #context.stack >= 2 then
            local b = table.remove(context.stack)
            local a = table.remove(context.stack)
            local result = a:Arithmetic("-", b)
            table.insert(context.stack, result)
        end
        return true
        
    elseif opcode == FOBLoader.OpCode.Mul then
        if #context.stack >= 2 then
            local b = table.remove(context.stack)
            local a = table.remove(context.stack)
            local result = a:Arithmetic("*", b)
            table.insert(context.stack, result)
        end
        return true
        
    elseif opcode == FOBLoader.OpCode.Div then
        if #context.stack >= 2 then
            local b = table.remove(context.stack)
            local a = table.remove(context.stack)
            local result = a:Arithmetic("/", b)
            table.insert(context.stack, result)
        end
        return true
        
    elseif opcode == FOBLoader.OpCode.Rem then
        if #context.stack >= 2 then
            local b = table.remove(context.stack)
            local a = table.remove(context.stack)
            local result = a:Arithmetic("%", b)
            table.insert(context.stack, result)
        end
        return true
        
    elseif opcode == FOBLoader.OpCode.Neg then
        if #context.stack > 0 then
            local a = table.remove(context.stack)
            local result = a:Arithmetic("neg", nil)
            table.insert(context.stack, result)
        end
        return true
    
    -- Comparison operations
    elseif opcode == FOBLoader.OpCode.Ceq then
        if #context.stack >= 2 then
            local b = table.remove(context.stack)
            local a = table.remove(context.stack)
            local result = a:Compare("==", b)
            table.insert(context.stack, result)
        end
        return true
        
    elseif opcode == FOBLoader.OpCode.Cne then
        if #context.stack >= 2 then
            local b = table.remove(context.stack)
            local a = table.remove(context.stack)
            local result = a:Compare("~=", b)
            table.insert(context.stack, result)
        end
        return true
        
    elseif opcode == FOBLoader.OpCode.Clt then
        if #context.stack >= 2 then
            local b = table.remove(context.stack)
            local a = table.remove(context.stack)
            local result = a:Compare("<", b)
            table.insert(context.stack, result)
        end
        return true
        
    elseif opcode == FOBLoader.OpCode.Cle then
        if #context.stack >= 2 then
            local b = table.remove(context.stack)
            local a = table.remove(context.stack)
            local result = a:Compare("<=", b)
            table.insert(context.stack, result)
        end
        return true
        
    elseif opcode == FOBLoader.OpCode.Cgt then
        if #context.stack >= 2 then
            local b = table.remove(context.stack)
            local a = table.remove(context.stack)
            local result = a:Compare(">", b)
            table.insert(context.stack, result)
        end
        return true
        
    elseif opcode == FOBLoader.OpCode.Cge then
        if #context.stack >= 2 then
            local b = table.remove(context.stack)
            local a = table.remove(context.stack)
            local result = a:Compare(">=", b)
            table.insert(context.stack, result)
        end
        return true
    
    -- Control flow
    elseif opcode == FOBLoader.OpCode.Ret then
        if #context.stack > 0 then
            context.returnValue = table.remove(context.stack)
        else
            context.returnValue = Value.null()
        end
        return false  -- Stop execution
        
    elseif opcode == FOBLoader.OpCode.Br then
        context.pc = (operands[1] or 0) - 1  -- Will be incremented
        return true
        
    elseif opcode == FOBLoader.OpCode.BrTrue then
        if #context.stack > 0 then
            local cond = table.remove(context.stack)
            if cond:GetData() then
                context.pc = (operands[1] or 0) - 1
            end
        end
        return true
        
    elseif opcode == FOBLoader.OpCode.BrFalse then
        if #context.stack > 0 then
            local cond = table.remove(context.stack)
            if not cond:GetData() then
                context.pc = (operands[1] or 0) - 1
            end
        end
        return true
    
    -- Object operations
    elseif opcode == FOBLoader.OpCode.NewObj then
        local typeIndex = operands[1] or 0
        local class = vm:GetClass(typeIndex)
        if class then
            local obj = types.Object.new(class)
            table.insert(context.stack, Value.object(obj))
        else
            table.insert(context.stack, Value.null())
        end
        return true
        
    elseif opcode == FOBLoader.OpCode.Call then
        local methodIndex = operands[1] or 0
        local typeIndex = operands[2] or 0
        local method = vm:GetMethod(typeIndex, methodIndex)
        if method and method.body then
            -- Native method
            local args = {}
            for i = 1, #method.parameters do
                table.insert(args, 1, table.remove(context.stack))
            end
            local result = method.body(args)
            if result then
                table.insert(context.stack, result)
            else
                table.insert(context.stack, Value.null())
            end
        end
        return true
        
    elseif opcode == FOBLoader.OpCode.CallVirt then
        if #context.stack > 0 then
            local obj = context.stack[#context.stack]:ToNative()
            if obj and type(obj) == "table" and obj.GetClass then
                local methodIndex = operands[1] or 0
                local methodName = vm:GetString(methodIndex)
                local method = obj:GetClass():GetMethod(methodName)
                if method then
                    -- For now, just call it
                    local result = obj:InvokeMethod(methodName, {})
                    if result then
                        table.insert(context.stack, result)
                    else
                        table.insert(context.stack, Value.null())
                    end
                end
            end
        end
        return true
        
    elseif opcode == FOBLoader.OpCode.CastClass then
        local typeIndex = operands[1] or 0
        -- For now, just validate the cast
        return true
        
    elseif opcode == FOBLoader.OpCode.IsInst then
        local typeIndex = operands[1] or 0
        if #context.stack > 0 then
            local obj = context.stack[#context.stack]:ToNative()
            local result = (obj ~= nil)
            table.remove(context.stack)
            table.insert(context.stack, Value.bool(result))
        end
        return true
    
    -- Array operations
    elseif opcode == FOBLoader.OpCode.NewArr then
        local typeIndex = operands[1] or 0
        if #context.stack > 0 then
            local len = table.remove(context.stack):ToNative()
            local arr = {}
            for i = 1, len do
                arr[i] = Value.null()
            end
            table.insert(context.stack, Value.object(arr))
        end
        return true
        
    elseif opcode == FOBLoader.OpCode.LdElem then
        if #context.stack >= 2 then
            local idx = table.remove(context.stack):ToNative()
            local arr = table.remove(context.stack):ToNative()
            if type(arr) == "table" then
                table.insert(context.stack, arr[idx + 1] or Value.null())
            else
                table.insert(context.stack, Value.null())
            end
        end
        return true
        
    elseif opcode == FOBLoader.OpCode.StElem then
        if #context.stack >= 3 then
            local val = table.remove(context.stack)
            local idx = table.remove(context.stack):ToNative()
            local arr = table.remove(context.stack):ToNative()
            if type(arr) == "table" then
                arr[idx + 1] = val
            end
        end
        return true
        
    elseif opcode == FOBLoader.OpCode.LdLen then
        if #context.stack > 0 then
            local arr = table.remove(context.stack):ToNative()
            if type(arr) == "table" then
                table.insert(context.stack, Value.int32(#arr))
            else
                table.insert(context.stack, Value.int32(0))
            end
        end
        return true
    
    -- Default: unknown opcode
    else
        return true
    end
end

return InstructionExecutor
