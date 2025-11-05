# ObjectIR C++ Runtime Pipeline - Visual Guide

## The Complete Workflow

```
┌────────────────────────────────────────────────────────────────┐
│                    STEP 1: Build (C#)                          │
├────────────────────────────────────────────────────────────────┤
│                                                                │
│  var builder = new IRBuilder("Calculator");                    │
│  var module = builder                                          │
│      .Class("Calculator")                                      │
│          .Field("result", TypeReference.Int32())              │
│          .Method("Add", TypeReference.Int32())                │
│              .Parameter("a", TypeReference.Int32())           │
│              .Parameter("b", TypeReference.Int32())           │
│          .EndMethod()                                         │
│      .EndClass()                                              │
│      .Build();                                                │
│                                                                │
│  string json = module.DumpJson();                             │
│  File.WriteAllText("calculator.json", json);                  │
│                                                                │
└────────────┬──────────────────────────────────────────────────┘
             │
             │ Generates JSON
             ▼
┌────────────────────────────────────────────────────────────────┐
│                   STEP 2: JSON Format                          │
├────────────────────────────────────────────────────────────────┤
│                                                                │
│  {                                                            │
│    "Name": "Calculator",                                      │
│    "Version": "1.0.0",                                        │
│    "Types": [                                                 │
│      {                                                        │
│        "Kind": "Class",                                       │
│        "Name": "Calculator",                                  │
│        "Namespace": "",                                       │
│        "Fields": [                                            │
│          {"Name": "result", "Type": "int32"}                  │
│        ],                                                     │
│        "Methods": [                                           │
│          {                                                    │
│            "Name": "Add",                                     │
│            "ReturnType": "int32",                             │
│            "Parameters": [                                    │
│              {"Name": "a", "Type": "int32"},                  │
│              {"Name": "b", "Type": "int32"}                   │
│            ]                                                  │
│          }                                                    │
│        ]                                                      │
│      }                                                        │
│    ]                                                          │
│  }                                                            │
│                                                                │
└────────────┬──────────────────────────────────────────────────┘
             │
             │ IRLoader::LoadFromFile()
             ▼
┌────────────────────────────────────────────────────────────────┐
│                  STEP 3: Load in C++ Runtime                   │
├────────────────────────────────────────────────────────────────┤
│                                                                │
│  auto vm = IRLoader::LoadFromFile("calculator.json");          │
│                                                                │
│  ┌────────────────────────────────────────┐                  │
│  │        VirtualMachine (vm)             │                  │
│  ├────────────────────────────────────────┤                  │
│  │ Classes:                               │                  │
│  │  └─ Calculator                         │                  │
│  │     ├─ Fields: result (int32)          │                  │
│  │     └─ Methods: Add(int32, int32)      │                  │
│  └────────────────────────────────────────┘                  │
│                                                                │
│  auto calcClass = vm->GetClass("Calculator");                 │
│  auto instance = vm->CreateObject(calcClass);                 │
│                                                                │
└────────────┬──────────────────────────────────────────────────┘
             │
             │ Bind native implementations
             ▼
┌────────────────────────────────────────────────────────────────┐
│             STEP 4: Bind Native Methods (C++)                 │
├────────────────────────────────────────────────────────────────┤
│                                                                │
│  auto addMethod = calcClass->GetMethod("Add");                │
│  addMethod->SetNativeImpl(                                     │
│      [](ObjectRef self, const std::vector<Value>& args, ...) {│
│          int32_t a = args[0].AsInt32();                       │
│          int32_t b = args[1].AsInt32();                       │
│          return Value(a + b);                                 │
│      }                                                        │
│  );                                                           │
│                                                                │
│  ┌────────────────────────────────────────┐                  │
│  │        VirtualMachine (vm)             │                  │
│  ├────────────────────────────────────────┤                  │
│  │ Classes:                               │                  │
│  │  └─ Calculator                         │                  │
│  │     ├─ Fields: result (int32)          │                  │
│  │     └─ Methods:                        │                  │
│  │        └─ Add(int32, int32) ←─ BOUND   │                  │
│  └────────────────────────────────────────┘                  │
│                                                                │
└────────────┬──────────────────────────────────────────────────┘
             │
             │ vm->InvokeMethod()
             ▼
┌────────────────────────────────────────────────────────────────┐
│               STEP 5: Execute & Get Results                    │
├────────────────────────────────────────────────────────────────┤
│                                                                │
│  std::vector<Value> args = {                                  │
│      Value(15),  // int32 a                                   │
│      Value(27)   // int32 b                                   │
│  };                                                           │
│                                                                │
│  Value result = vm->InvokeMethod(instance, "Add", args);       │
│                                                                │
│  std::cout << "Result: " << result.AsInt32() << std::endl;     │
│  // Output: Result: 42                                        │
│                                                                │
│  // Access fields                                             │
│  instance->SetField("result", result);                         │
│  Value stored = instance->GetField("result");                  │
│                                                                │
└────────────────────────────────────────────────────────────────┘
```

## Component Interaction Diagram

```
┌──────────────────┐
│   Your Code      │
└────────┬─────────┘
         │ Uses
         ▼
┌──────────────────────────────┐
│   IRLoader                   │
├──────────────────────────────┤
│ • LoadFromFile()             │
│ • LoadFromString()           │
│ • ParseModule()              │
│ • LoadTypes()                │
│ • LoadClass/Interface/Struct │
└────────┬────────────────────┬┘
         │ Creates            │
         ▼                    ▼
  ┌──────────┐         ┌──────────────┐
  │ Virtual  │         │ Instruction  │
  │ Machine  │         │ Executor     │
  └────┬─────┘         └──────────────┘
       │ Registers          │ Interprets
       ▼                    ▼
  ┌──────────┐         ┌──────────────┐
  │ Classes  │         │ Instructions │
  │  • Name  │         │  • Stack ops │
  │  • Fields│         │  • Arithmetic│
  │  • Methods          │  • Comparison
  └────┬─────┘         │  • Control   │
       │ Creates         └──────────────┘
       ▼
  ┌──────────┐
  │ Objects  │
  │  • State │
  │  • Fields│
  └──────────┘
```

## Data Flow Diagram

```
┌─────────────┐
│ JSON File   │ calculator.json
└──────┬──────┘
       │ read
       ▼
┌─────────────────────────────────┐
│ Parse JSON                      │
│  • Extract types                │
│  • Extract methods              │
│  • Extract fields               │
└──────┬──────────────────────────┘
       │
       ▼
┌─────────────────────────────────┐
│ Create Runtime Metadata         │
│  • Class objects                │
│  • Method objects               │
│  • Field objects                │
└──────┬──────────────────────────┘
       │
       ▼
┌─────────────────────────────────┐
│ Register in VirtualMachine      │
│  • Add to class registry        │
└──────┬──────────────────────────┘
       │
       ▼
┌─────────────────────────────────┐
│ Bind Native Implementations     │
│  • Set NativeMethodImpl          │
└──────┬──────────────────────────┘
       │
       ▼
┌─────────────────────────────────┐
│ Execute Methods                 │
│  • InvokeMethod()               │
│  • Call native impl             │
│  • Return value                 │
└─────────────────────────────────┘
```

## Method Execution Flow

```
InvokeMethod(object, "Add", args)
         │
         ▼
    Get method from class
         │
         ▼
    Lookup native impl
         │
         ▼
    Create execution context
         │
         ▼
    Push arguments on stack
         │
         ▼
    Call native impl
    ┌──────────────────────┐
    │ C++ Lambda Function  │
    │ ┌──────────────────┐ │
    │ │ Pop a, b from    │ │
    │ │ Execute: a + b   │ │
    │ │ Push result      │ │
    │ └──────────────────┘ │
    └──────────────────────┘
         │
         ▼
    Return value
         │
         ▼
    Pop result from stack
         │
         ▼
    Return to caller
```

## Type System Overview

```
┌─────────────────────────────────────┐
│         TypeReference               │
├─────────────────────────────────────┤
│ Primitive:                          │
│  • int32                            │
│  • int64                            │
│  • float32 / float64                │
│  • bool                             │
│  • string                           │
│  • void                             │
│                                     │
│ Reference:                          │
│  • Object -> ClassRef               │
│  • User-defined classes             │
│                                     │
│ Namespace:                          │
│  • Namespace.ClassName              │
└─────────────────────────────────────┘
         │
         ▼
┌─────────────────────────────────────┐
│           Value                     │
├─────────────────────────────────────┤
│ Variant Union:                      │
│  • nullptr_t                        │
│  • int32_t                          │
│  • int64_t                          │
│  • float                            │
│  • double                           │
│  • bool                             │
│  • std::string                      │
│  • ObjectRef (shared_ptr)           │
└─────────────────────────────────────┘
```

## Class Hierarchy and Inheritance

```
┌──────────────────────────────────┐
│         Class                    │
├──────────────────────────────────┤
│ • Name: "Calculator"             │
│ • Namespace: "Examples"          │
│ • IsAbstract: false              │
│ • IsSealed: false                │
│ • BaseClass: nullptr             │
│                                  │
│ Fields:                          │
│ └─ result: int32                 │
│                                  │
│ Methods:                         │
│ └─ Add(int32, int32) -> int32    │
│    └─ NativeImpl: (bound)         │
│                                  │
│ Interfaces: (empty)              │
└──────────────────────────────────┘
         │ creates
         ▼
┌──────────────────────────────────┐
│    Object Instance               │
├──────────────────────────────────┤
│ • _class: -> Calculator class    │
│ • _fieldValues:                  │
│   ├─ "result": Value(42)         │
│   └─ ...                         │
│ • _baseInstance: nullptr         │
└──────────────────────────────────┘
```

## Module Structure

```
MyModule.json
├── Metadata
│   ├── Name: "MyModule"
│   ├── Version: "1.0.0"
│   └── Metadata: { }
│
├── Types: [
│   ├── Class 1
│   │   ├── Name, Namespace
│   │   ├── Fields: [ ]
│   │   ├── Methods: [ ]
│   │   └── Properties: [ ]
│   │
│   ├── Interface 1
│   │   ├── Name, Namespace
│   │   ├── Methods: [ ]
│   │   └── Properties: [ ]
│   │
│   └── Struct 1
│       ├── Name, Namespace
│       ├── Fields: [ ]
│       └── Methods: [ ]
│
└── Functions: [ ]
```

## Instruction Execution Pipeline

```
┌──────────────────────┐
│  JSON Instruction    │
│  {"opCode": "add",   │
│   "operand": null}   │
└──────┬───────────────┘
       │
       ▼
┌──────────────────────┐
│ Parse Instruction    │
│ OpCode: ADD          │
└──────┬───────────────┘
       │
       ▼
┌──────────────────────┐
│ Get Operands         │
│ Pop b from stack     │
│ Pop a from stack     │
└──────┬───────────────┘
       │
       ▼
┌──────────────────────┐
│ Execute Operation    │
│ result = a + b       │
└──────┬───────────────┘
       │
       ▼
┌──────────────────────┐
│ Push Result          │
│ Push result on stack │
└──────────────────────┘
```

## Error Handling Flow

```
LoadFromFile("module.json")
         │
         ▼
    Try to open file
         │
    ┌────┴────┐
    │          │
 Success    FileNotFound
    │          │
    ▼          ▼
 Parse      Exception:
 JSON       "Cannot open..."
    │
    ┌─── ParseError?
    │      │
    │      ▼
    │    Exception:
    │    "Failed to parse JSON"
    │      │
    │      ▼
    │    Throw
    │
    ├─── InvalidSchema?
    │      │
    │      ▼
    │    Exception:
    │    "Invalid type definition"
    │
    │
    └─── Success
           │
           ▼
         Return VirtualMachine
```

## Performance Characteristics

```
┌─────────────────────────────────────────────────────┐
│         Operation              Complexity           │
├─────────────────────────────────────────────────────┤
│ Load JSON module               O(n) - n = JSON size │
│ Create object instance         O(1)                 │
│ Invoke method                  O(1)                 │
│ Get field                      O(1) - hash lookup   │
│ Set field                      O(1) - hash insert   │
│ Execute instruction            O(1)                 │
│ Execute method body            O(m) - m = instrs    │
└─────────────────────────────────────────────────────┘
```

## See Also

- QUICK_START_PIPELINE.md - Get started in 5 minutes
- RUNTIME_PIPELINE.md - Complete documentation
- examples/pipeline_example.cpp - Working code
- include/ir_loader.hpp - API reference
- include/instruction_executor.hpp - Executor reference
