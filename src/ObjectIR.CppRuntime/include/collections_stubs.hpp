#pragma once

#include "objectir_runtime.hpp"
#include <vector>
#include <list>
#include <deque>
#include <unordered_map>
#include <unordered_set>
#include <memory>
#include <algorithm>

namespace ObjectIR {

// ============================================================================
// System.Collections.Generic.List<T> - Dynamic array implementation
// ============================================================================

Value List_ctor(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value List_ctor_Capacity(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value List_get_Count(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value List_get_Capacity(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value List_set_Capacity(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value List_get_Item(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value List_set_Item(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value List_Add(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value List_AddRange(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value List_Insert(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value List_RemoveAt(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value List_Remove(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value List_Clear(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value List_Contains(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value List_IndexOf(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value List_ToArray(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);

// ============================================================================
// System.Collections.Generic.Dictionary<TKey,TValue> - Hash map implementation
// ============================================================================

Value Dictionary_ctor(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Dictionary_get_Count(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Dictionary_get_Item(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Dictionary_set_Item(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Dictionary_Add(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Dictionary_Remove(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Dictionary_Clear(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Dictionary_ContainsKey(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Dictionary_ContainsValue(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Dictionary_get_Keys(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Dictionary_get_Values(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);

// ============================================================================
// System.Collections.Generic.Queue<T> - FIFO queue implementation
// ============================================================================

Value Queue_ctor(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Queue_get_Count(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Queue_Enqueue(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Queue_Dequeue(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Queue_Peek(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Queue_Clear(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Queue_Contains(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);

// ============================================================================
// System.Collections.Generic.Stack<T> - LIFO stack implementation
// ============================================================================

Value Stack_ctor(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Stack_get_Count(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Stack_Push(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Stack_Pop(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Stack_Peek(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Stack_Clear(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value Stack_Contains(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);

// ============================================================================
// System.Collections.Generic.HashSet<T> - Set implementation
// ============================================================================

Value HashSet_ctor(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value HashSet_get_Count(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value HashSet_Add(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value HashSet_Remove(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value HashSet_Clear(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);
Value HashSet_Contains(ObjectRef thisPtr, const std::vector<Value>& args, VirtualMachine* vm);

} // namespace ObjectIR