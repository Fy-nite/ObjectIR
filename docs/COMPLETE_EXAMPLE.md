# Complete Example: Todo List Application

This example demonstrates building a complete todo list application in ObjectIR, showcasing:
- Class hierarchies
- Interfaces
- Generic collections
- Control flow
- Exception handling

## The Application

We'll build three classes:
1. `TodoItem` - Represents a single todo item
2. `TodoList` - Manages a collection of todos
3. `Program` - Entry point with example usage

## IR Code

```
module TodoApp version 1.0.0

// Interface for items
interface IItem {
    method GetId() -> int32
    method GetDescription() -> string
    method IsComplete() -> bool
}

// TodoItem class
class TodoItem : IItem {
    private field id: int32
    private field description: string
    private field isComplete: bool
    
    constructor(id: int32, description: string) {
        ldarg this
        ldarg id
        stfld TodoItem.id
        
        ldarg this
        ldarg description
        stfld TodoItem.description
        
        ldarg this
        ldc.i4 0  // false
        stfld TodoItem.isComplete
        
        ret
    }
    
    method GetId() -> int32 implements IItem.GetId {
        ldarg this
        ldfld TodoItem.id
        ret
    }
    
    method GetDescription() -> string implements IItem.GetDescription {
        ldarg this
        ldfld TodoItem.description
        ret
    }
    
    method IsComplete() -> bool implements IItem.IsComplete {
        ldarg this
        ldfld TodoItem.isComplete
        ret
    }
    
    method MarkComplete() -> void {
        ldarg this
        ldc.i4 1  // true
        stfld TodoItem.isComplete
        ret
    }
    
    method ToString() -> string {
        local result: string
        
        // Build string: "[X] description" or "[ ] description"
        ldarg this
        ldfld TodoItem.isComplete
        
        if (stack) {
            ldstr "[X] "
            stloc result
        } else {
            ldstr "[ ] "
            stloc result
        }
        
        ldloc result
        ldarg this
        ldfld TodoItem.description
        call System.String.Concat(string, string) -> string
        ret
    }
}

// TodoList class
class TodoList {
    private field items: List<TodoItem>
    private field nextId: int32
    
    constructor() {
        newobj List<TodoItem>
        stfld TodoList.items
        
        ldc.i4 1
        stfld TodoList.nextId
        
        ret
    }
    
    method Add(description: string) -> TodoItem {
        local item: TodoItem
        local id: int32
        
        // Get and increment next ID
        ldarg this
        dup
        ldfld TodoList.nextId
        dup
        stloc id
        ldc.i4 1
        add
        stfld TodoList.nextId
        
        // Create new item
        ldloc id
        ldarg description
        newobj TodoItem.constructor(int32, string)
        dup
        stloc item
        
        // Add to list
        ldarg this
        ldfld TodoList.items
        ldloc item
        callvirt List<TodoItem>.Add(TodoItem) -> void
        
        // Return item
        ldloc item
        ret
    }
    
    method Remove(id: int32) -> bool {
        local i: int32
        local count: int32
        local item: TodoItem
        
        // Get count
        ldarg this
        ldfld TodoList.items
        callvirt List<TodoItem>.get_Count() -> int32
        stloc count
        
        // Initialize i = 0
        ldc.i4 0
        stloc i
        
        // Search for item with matching ID
        while (i < count) {
            // Get item at index i
            ldarg this
            ldfld TodoList.items
            ldloc i
            callvirt List<TodoItem>.get_Item(int32) -> TodoItem
            stloc item
            
            // Check if item.id == id
            ldloc item
            callvirt TodoItem.GetId() -> int32
            ldarg id
            ceq
            
            if (stack) {
                // Found it! Remove and return true
                ldarg this
                ldfld TodoList.items
                ldloc item
                callvirt List<TodoItem>.Remove(TodoItem) -> bool
                pop  // Discard result
                
                ldc.i4 1  // true
                ret
            }
            
            // i++
            ldloc i
            ldc.i4 1
            add
            stloc i
        }
        
        // Not found
        ldc.i4 0  // false
        ret
    }
    
    method Complete(id: int32) -> bool {
        local i: int32
        local count: int32
        local item: TodoItem
        
        ldarg this
        ldfld TodoList.items
        callvirt List<TodoItem>.get_Count() -> int32
        stloc count
        
        ldc.i4 0
        stloc i
        
        while (i < count) {
            ldarg this
            ldfld TodoList.items
            ldloc i
            callvirt List<TodoItem>.get_Item(int32) -> TodoItem
            stloc item
            
            ldloc item
            callvirt TodoItem.GetId() -> int32
            ldarg id
            ceq
            
            if (stack) {
                ldloc item
                callvirt TodoItem.MarkComplete() -> void
                ldc.i4 1
                ret
            }
            
            ldloc i
            ldc.i4 1
            add
            stloc i
        }
        
        ldc.i4 0
        ret
    }
    
    method PrintAll() -> void {
        local i: int32
        local count: int32
        local item: TodoItem
        
        ldarg this
        ldfld TodoList.items
        callvirt List<TodoItem>.get_Count() -> int32
        stloc count
        
        // Check if empty
        ldloc count
        ldc.i4 0
        ceq
        if (stack) {
            ldstr "No todos!"
            call System.Console.WriteLine(string) -> void
            ret
        }
        
        ldstr "Todo List:"
        call System.Console.WriteLine(string) -> void
        
        ldc.i4 0
        stloc i
        
        while (i < count) {
            ldarg this
            ldfld TodoList.items
            ldloc i
            callvirt List<TodoItem>.get_Item(int32) -> TodoItem
            stloc item
            
            ldloc item
            callvirt TodoItem.ToString() -> string
            call System.Console.WriteLine(string) -> void
            
            ldloc i
            ldc.i4 1
            add
            stloc i
        }
        
        ret
    }
    
    method GetCompleteCount() -> int32 {
        local i: int32
        local count: int32
        local completeCount: int32
        local item: TodoItem
        
        ldarg this
        ldfld TodoList.items
        callvirt List<TodoItem>.get_Count() -> int32
        stloc count
        
        ldc.i4 0
        stloc completeCount
        ldc.i4 0
        stloc i
        
        while (i < count) {
            ldarg this
            ldfld TodoList.items
            ldloc i
            callvirt List<TodoItem>.get_Item(int32) -> TodoItem
            stloc item
            
            ldloc item
            callvirt TodoItem.IsComplete() -> bool
            
            if (stack) {
                ldloc completeCount
                ldc.i4 1
                add
                stloc completeCount
            }
            
            ldloc i
            ldc.i4 1
            add
            stloc i
        }
        
        ldloc completeCount
        ret
    }
}

// Entry point
class Program {
    static method Main() -> void {
        local todos: TodoList
        local item: TodoItem
        
        // Create todo list
        newobj TodoList.constructor()
        stloc todos
        
        // Add some items
        ldloc todos
        ldstr "Buy groceries"
        callvirt TodoList.Add(string) -> TodoItem
        pop
        
        ldloc todos
        ldstr "Write documentation"
        callvirt TodoList.Add(string) -> TodoItem
        stloc item
        
        ldloc todos
        ldstr "Fix bugs"
        callvirt TodoList.Add(string) -> TodoItem
        pop
        
        // Print initial list
        ldstr "\nInitial list:"
        call System.Console.WriteLine(string) -> void
        ldloc todos
        callvirt TodoList.PrintAll() -> void
        
        // Complete item with ID 2
        ldloc todos
        ldloc item
        callvirt TodoItem.GetId() -> int32
        callvirt TodoList.Complete(int32) -> bool
        pop
        
        // Print updated list
        ldstr "\nAfter completing 'Write documentation':"
        call System.Console.WriteLine(string) -> void
        ldloc todos
        callvirt TodoList.PrintAll() -> void
        
        // Print statistics
        ldstr "\nStatistics:"
        call System.Console.WriteLine(string) -> void
        
        ldstr "Completed: "
        ldloc todos
        callvirt TodoList.GetCompleteCount() -> int32
        call System.String.Concat(string, int32) -> string
        call System.Console.WriteLine(string) -> void
        
        ret
    }
}
```

## Output

When compiled and run, this program would output:

```
Initial list:
Todo List:
[ ] Buy groceries
[ ] Write documentation
[ ] Fix bugs

After completing 'Write documentation':
Todo List:
[ ] Buy groceries
[X] Write documentation
[ ] Fix bugs

Statistics:
Completed: 1
```

## Building with C# Builder API

Here's how you would construct this same program using the ObjectIR Builder API:

```csharp
var builder = new IRBuilder("TodoApp");

// IItem interface
builder.Interface("IItem")
    .Method("GetId", TypeReference.Int32)
    .Method("GetDescription", TypeReference.String)
    .Method("IsComplete", TypeReference.Bool)
    .EndInterface();

// TodoItem class
var todoItemType = TypeReference.FromName("TodoApp.TodoItem");

builder.Class("TodoItem")
    .Implements(TypeReference.FromName("TodoApp.IItem"))
    .Field("id", TypeReference.Int32).Access(AccessModifier.Private).EndField()
    .Field("description", TypeReference.String).Access(AccessModifier.Private).EndField()
    .Field("isComplete", TypeReference.Bool).Access(AccessModifier.Private).EndField()
    
    .Constructor()
        .Parameter("id", TypeReference.Int32)
        .Parameter("description", TypeReference.String)
        .Body()
            // Initialize fields...
            .Ret()
        .EndBody()
        .EndMethod()
    
    // ... more methods
    
    .EndClass();

// TodoList class
// ... similar pattern

// Program class
builder.Class("Program")
    .Method("Main", TypeReference.Void)
        .Static()
        .Body()
            // ... implementation
            .Ret()
        .EndBody()
        .EndMethod()
    .EndClass();

var module = builder.Build();
```

## Key Concepts Demonstrated

### 1. Interface Implementation
```
class TodoItem : IItem {
    method GetId() -> int32 implements IItem.GetId { ... }
}
```

### 2. Constructor with Parameters
```
constructor(id: int32, description: string) {
    ldarg this
    ldarg id
    stfld TodoItem.id
    ...
}
```

### 3. Generic Collections
```
field items: List<TodoItem>
```

### 4. Conditional Logic
```
if (isComplete) {
    ldstr "[X] "
} else {
    ldstr "[ ] "
}
```

### 5. Loops with Break Conditions
```
while (i < count) {
    // Find and remove item
    if (found) {
        ret  // Early return
    }
    i++
}
```

### 6. String Operations
```
call System.String.Concat(string, string) -> string
```

## Backend Considerations

When implementing backends for this code, consider:

### C# Backend
- Map `List<TodoItem>` to `System.Collections.Generic.List<TodoItem>`
- Interface implementation maps directly
- Constructor chaining if needed

### JavaScript Backend
- Map `List<T>` to JavaScript `Array`
- Implement interface checking with duck typing or symbols
- Constructor becomes regular function

### C++ Backend
- Map `List<T>` to `std::vector<T>`
- Use abstract base class for interface
- Reference counting or smart pointers for memory management

### Java Backend
- Map to `ArrayList<TodoItem>`
- Interface maps directly
- Automatic garbage collection

## Extensions

This example could be extended with:
- Persistence (save/load from file)
- Filtering (show only incomplete items)
- Due dates and priorities
- Categories/tags
- Async operations
- Unit tests

## Testing Strategy

To test this IR:

1. **Unit tests** for each class independently
2. **Integration tests** for the complete workflow
3. **Backend tests** to ensure correct compilation
4. **Runtime tests** to verify execution matches expectations

Example test:
```csharp
[Test]
public void TodoList_Add_CreatesNewItem()
{
    // Compile IR to target
    var compiled = backend.Compile(module);
    
    // Execute
    var result = Execute(compiled, "TodoList.Add", "Test item");
    
    // Verify
    Assert.NotNull(result);
    Assert.Equal(1, result.GetId());
}
```
