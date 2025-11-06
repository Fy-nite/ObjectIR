# Object Messaging System in ObjectIR

## Overview
The **Object Messaging System** in ObjectIR serves as the communication layer between instantiated objects and the runtime environment. Rather than invoking methods through direct function pointers or hardcoded dispatch tables, ObjectIR uses a dynamic messaging mechanism. Each method invocation, field access, or class operation is translated into a message that the runtime routes to the appropriate object instance and handler.

This allows for dynamic behavior similar to that seen in languages like Smalltalk, Objective-C, or modern virtual machine architectures such as the CLR and JVM.

---

## Core Concepts

### 1. **Messages**
Messages are the fundamental communication unit in ObjectIR. A message represents a call or request directed toward an object.

Each message includes:
- **Target:** The receiving object (`ObjectRef`)
- **Selector:** The name or identifier of the method/field being accessed (e.g., `"GetId"`)
- **Arguments:** A list of `Value` objects passed to the method
- **Response:** The result or acknowledgment of the operation

When a method such as `vm->InvokeMethod(obj, "AddItem", {...})` is called, the runtime constructs a message and delivers it to the target object.

---

### 2. **Dispatch Resolution**
When a message is sent to an object, the runtime performs **method resolution** to determine how to handle it:
1. The runtime queries the class definition associated with the object.
2. It checks whether the method exists in the class' method table.
3. If found, it executes the registered native or interpreted implementation.
4. If not found, it raises a **soft failure** — a controlled fallback mechanism.

Unlike hard crashes, a soft failure may return a safe response such as:
```cpp
Value("Method not found: AddItem")
```
This makes the runtime resilient against invalid calls and enhances safety during reflection or dynamic scripting.

---

### 3. **Soft Failure Handling**
A key design philosophy of ObjectIR is **non-destructive message failure**. When a message cannot be resolved (e.g., calling an undefined method), the runtime does not throw or terminate. Instead, it returns a message response indicating the failure.

This behavior enables runtime introspection and graceful degradation, especially useful when embedding ObjectIR in environments like games, VMs, or dynamic systems.

Example behavior:
```cpp
auto result = vm->InvokeMethod(todoList, "RemoveItem", { Value(1) });
if (result.IsString()) {
    std::cout << result.AsString() << std::endl; // "Method not found: RemoveItem"
}
```

---

### 4. **Native and Virtual Method Binding**
Methods in ObjectIR can be either:
- **Native Implementations:** Bound directly to C++ lambdas/functions using `.NativeImpl(...)`
- **Virtual Methods:** Defined within the IR or another interpreted layer, resolved dynamically at runtime

Messages are used in both cases — even native methods are invoked through message dispatch, allowing uniform handling across runtime boundaries.

---

### 5. **Cross-Class Communication**
Because ObjectIR’s messaging model is abstracted, an object in one class can send messages to another object dynamically, without compile-time type dependencies.

Example:
```cpp
auto user = vm->CreateObject(vm->GetClass("User"));
auto msgResult = vm->InvokeMethod(user, "GetProfile", {});
```
This mechanism allows building highly modular systems where class definitions can evolve independently.

---

### 6. **Benefits**
- **Dynamic Dispatch:** Supports reflection and dynamic typing.
- **Safe Fallbacks:** Avoids fatal runtime errors.
- **Extensible Runtime:** Enables custom object behaviors (e.g., proxies, interceptors).
- **Platform-Agnostic:** Abstracts interaction between host and virtual objects.
- **Debug-Friendly:** Easy to trace message flow for introspection.

---

## Example Message Flow
Below is a simplified conceptual flow for invoking a method through the messaging layer:

```cpp
vm->InvokeMethod(obj, "AddItem", { Value("Learn C++") });
```

1. **Message Construction:**
   - Selector: `"AddItem"`
   - Arguments: `["Learn C++"]`
   - Target: `obj`

2. **Dispatch Resolution:**
   - Runtime checks class metadata.
   - Finds `AddItem` in the method table.

3. **Execution:**
   - Calls the bound native lambda.
   - Returns a `Value` with the new item ID.

4. **Response Delivery:**
   - If successful → returns numeric ID.
   - If failed → returns string: `"Method not found: AddItem"`.

---

## Future Extensions
The Object Messaging System can evolve into:
- **Asynchronous Messaging:** Allowing queued or threaded message handling.
- **Event Hooks:** Objects can listen for message types globally.
- **Networked Messaging:** Serialize messages for remote object invocation.
- **Security Contexts:** Restrict message delivery between runtime zones.

---

## Summary
The ObjectIR Object Messaging System abstracts direct method calls into a dynamic message-passing architecture. It ensures runtime safety, flexibility, and introspection by treating every call, access, or operation as a routable message. This design philosophy makes ObjectIR well-suited for modular, reflective, and cross-platform runtime environments.

