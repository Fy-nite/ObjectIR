# Contract language — quick reference

Contract is a tiny example language used for experimentation. This document
describes the current syntax, features, and how to compile/run with the
repository tools. It is intentionally small and focused on the constructs
used in the `examples/` folder.

## Files

- Source files use the `.ct` extension.

## Top-level

- `Contract Name { ... }` declares a contract namespace. Functions defined
  inside will be qualified as `Name.Func` in the emitter.

## Functions

- `fn foo(a: Int, b: Int) { ... }` declares a function. Parameter types are
  optional in the frontend; we parse names and emit signatures like `foo(a,b)`.

## Statements

- Expression statements: `IO.println("hi")`, arithmetic, comparisons.
- `var x = 10;` variable declaration.
- `if (cond) { ... } else: { ... }` control flow (basic form).
- `while (cond) { ... }` loop.
- `switch` with `case` and `else` branches is supported in a limited form.

## Expressions

- Integer literals: `123`
- String literals: `"hello"`
- Identifiers: `Foo`, `x`, `IO.println`
- Function calls: `f(1, 2)` or `IO.println("hi")`
- Binary operators: `+`, `-`, `<`, `<=`, `==`

## Example

```ct
Contract Program {
  fn main() {
    IO.println("Hello, world!");
  }
}
```

## Tooling

- Build the compiler and runtime with meson/ninja from the repo root:

```bash
meson setup build
ninja -C build -j 4
```

- Run the example by running the built binary (default behavior):

```bash
./build/Contract
```

- Ask the compiler for diagnostics on stdin (used by the LSP server):

```bash
printf "this is invalid\\n" | ./build/Contract --diagnose-stdin
```

## Editor integration

- This repository contains a minimal LSP server (`lsp/server.js`) and a small
  VS Code extension under `vscode-extension/` which launches it. The extension
  contributes syntax highlighting (TextMate grammar) and basic language
  activation on `.ct` files.

## Notes & Limitations

- The parser is intentionally small and permissive. Diagnostics are basic and
  focus on common structural problems (unterminated strings, unmatched braces,
  or top-level stray tokens).
- The bytecode VM is experimental and intended for learning and iteration.

If you'd like, I can expand this document with a formal grammar, more
examples, and a section describing the emitted IL and the VM instruction
set.
