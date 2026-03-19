
# ObjectIR Documentation Repository

Welcome to the official documentation repository for **ObjectIR**, a high-level intermediate representation (IR) for object-oriented languages. This repository contains all specifications, guides, and reference materials for ObjectIR, serving as the central resource for users, implementers, and contributors.

---

## About ObjectIR

ObjectIR is a typed, object-oriented intermediate representation designed to bridge high-level OOP languages (such as C#, Java, and C++) with multiple target runtimes. It combines stack-based instructions with structured control flow, enabling advanced analysis, transformation, and cross-language interoperability.

## Spec
For the latest Language specification available, check out our website at [https://finite.ovh/PDF/ObjectIRV2.pdf](https://finite.ovh/PDF/ObjectIRV2.pdf) (external link).

**Note:** This repository contains only documentation and specifications. For source code, examples, and implementation details, see the [Lattice Research Runtime](https://github.com/Fy-nite/lattice) (external link).

---

## How to Use This Documentation

- **Start Here:** [Getting Started Guide](docs/GETTING_STARTED.md)
- **Formal Grammar:** [ObjectIR Grammar](docs/GRAMMAR.md)
- **Architecture Overview:** [System Architecture](docs/ARCHITECTURE.md)
- **Instruction Reference:** [VM Instructions (Text Format)](docs/VM_INSTRUCTIONS_TEXT_FORMAT.md) | [Instruction Serialization (JSON)](docs/INSTRUCTION_SERIALIZATION.md)
- **Module Formats:** [Module Serialization](docs/MODULE_DUMPING.md) | [FOB Binary Format](docs/OBJECTIR_FOB_SPEC.md)
- **Quick Reference:** [Construct Quick Reference](docs/CONSTRUCT_QUICK_REFERENCE.md)
- **All Documentation:** [docs/](docs/)
- **Examples:** [examples/](examples/)

For the latest updates, see the [CHANGELOG](docs/CHANGELOG.md) (if available).


---

## Key Documents

- [Getting Started](docs/GETTING_STARTED.md)
- [Architecture](docs/ARCHITECTURE.md)
- [Formal Grammar](docs/GRAMMAR.md)
- [VM Instructions (Text Format)](docs/VM_INSTRUCTIONS_TEXT_FORMAT.md)
- [Instruction Serialization (JSON)](docs/INSTRUCTION_SERIALIZATION.md)
- [Module Serialization](docs/MODULE_DUMPING.md)
- [FOB Binary Format](docs/OBJECTIR_FOB_SPEC.md)
- [Module Loader](docs/MODULE_LOADER.md)
- [Specification Index](docs/CONSTRUCT_COMPILER_DOCUMENTATION_INDEX.md)
- [Fortran 90 Compiler](docs/FORTRAN_90_COMPILER.md)


---

---

## Contributing to Documentation

Contributions to the documentation are welcome! To suggest changes, report issues, or propose new guides/specifications:

- [Open an Issue](https://github.com/objectir/objectir-docs/issues)
- [Start a Discussion](https://github.com/objectir/objectir-docs/discussions)
- See [CONTRIBUTING.md](docs/CONTRIBUTING.md) for guidelines (if available)

---

## License

This documentation is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.

---

## Inspiration

ObjectIR draws inspiration from:
- [LLVM IR](https://llvm.org/docs/LangRef.html): Modular architecture, SSA form concepts
- [.NET CIL](https://docs.microsoft.com/en-us/dotnet/standard/managed-code): Stack-based operations, rich type system
- [JVM Bytecode](https://docs.oracle.com/javase/specs/jvms/se8/html/jvms-6.html): Cross-platform vision
- [WebAssembly](https://webassembly.org/docs/): Structured control flow
- [MLIR](https://mlir.llvm.org/): Extensible dialect system

---

**Status:** Early Development — Documentation and specifications are evolving and subject to change.
