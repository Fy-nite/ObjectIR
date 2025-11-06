# ObjectIR C++ Runtime: Feature Overview

This document summarizes the current features and capabilities of the ObjectIR C++ Runtime as implemented in the repository.

---

## Core Architecture
- **Virtual Machine (VM)**
  - Manages class registry, object creation, method invocation, and execution context stack.
  - Supports dynamic loading of modules from JSON (via `IRLoader`).
  - Provides APIs for both instance and static method invocation.

- **Type System**
  - Supports primitive types: `Int32`, `Int64`, `Float32`, `Float64`, `Bool`, `Void`, `String`, and generic `Object`.
  - Type references can be primitive or class-based, with support for generics.

- **Class Model**
  - Classes have fields, methods, base class, namespace, abstract/sealed flags, and interface support.
  - Methods can be static, virtual, abstract, or native (C++ function pointer).
  - Fields and methods are loaded from JSON IR.
  - Object instances are created with field storage and class/type info.

- **Object Model**
  - Objects store field values, class/type info, and optional base instance.
  - Field access and type checking (instanceof, base class traversal).

- **Method Execution**
  - Methods can have instructions (IR), parameters, locals, and native implementations.
  - Execution context manages stack, locals, arguments, and `this` pointer.
  - Instruction execution is delegated to `InstructionExecutor` (not detailed here).

- **Value Representation**
  - Stack-based value type supports all primitives, strings, and object references.
  - Safe conversion and type checking for values.

- **Generic Collections**
  - Base class for generic lists (`ListBase`) and typed list implementation (`List<T>`).
  - Supports size, get/set, add/remove, clear operations.

---

## Module Loading & Serialization
- **IRLoader**
  - Loads modules from JSON files or strings.
  - Parses types, classes, interfaces, structs, fields, methods, and instructions.
  - Registers loaded types/classes in the VM.

---

## Native C API (Interop)
- Exports C-compatible functions for use by C#/other runtimes:
  - VM creation/deletion
  - Module loading (file/string)
  - Instance creation
  - Method invocation (instance/static)
  - Value-to-string conversion
  - Error reporting
  - Resource cleanup (objects, values, strings)

---

## Error Handling
- Consistent error reporting via last-error string (thread-local).
- Exceptions are caught and reported across the C API boundary.

---

## Extensibility
- Designed for integration with C# backend and other host runtimes.
- Modular class/type/method system for future language features.

---

## Notable Limitations / TODOs
- Method argument marshalling is currently minimal (no argument passing in C API).
- Global function support is stubbed but not implemented.
- Instruction execution details are abstracted in `InstructionExecutor`.
- Advanced features (reflection, dynamic codegen, advanced collections) are not present yet.

---

## References
- See `objectir_runtime.hpp` for class/type/method definitions.
- See `ir_loader.hpp`/`.cpp` for module loading logic.
- See `runtime_c_api.cpp` for exported C API functions.
