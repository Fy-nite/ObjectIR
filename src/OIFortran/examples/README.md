# Fortran 90 Compiler Examples

This directory contains example Fortran 90 programs demonstrating various features of the OIFortran Fortran 90 compiler.

## Examples Overview

### 1. `geometry.f90` - Module with Derived Types

**Demonstrates:**
- MODULE definition
- USE statement to import module
- DERIVED TYPE definition
- Type component access with `%` operator
- FUNCTION with INTENT parameters
- Type instances and operations

**Key Features:**
- `point` derived type with x, y, z coordinates
- `distance()` function calculates distance between two points
- `magnitude()` function calculates distance from origin
- `translate()` function applies vector translation

**Compilation:**
```bash
oifortran geometry.f90 -o geometry.oir
```

**Expected Output:**
```
 Distance between p1 and p2: 5.19615
 Magnitude of p1: 3.74166
 Translated point: 5.00000 7.00000 9.00000
```

### 2. `array_operations.f90` - Arrays and Allocation

**Demonstrates:**
- ALLOCATE statement for dynamic memory allocation
- DEALLOCATE statement
- Multi-dimensional arrays (assumed-shape)
- Array operations (size, sum, do loops)
- FUNCTION returning allocatable arrays

**Key Features:**
- `matrix_sum()` calculates sum of matrix elements
- `print_matrix()` displays matrix contents
- `matrix_multiply()` performs matrix multiplication with allocation
- Dynamic memory management

**Compilation:**
```bash
oifortran array_operations.f90 -o array_ops.oir
```

**Features Used:**
- `allocate()` - runtime array allocation
- `deallocate()` - release allocated memory
- `size()` - get array dimensions
- Array slicing and do-loop iteration

### 3. `function_parameters.f90` - INTENT Parameters

**Demonstrates:**
- INTENT(IN) - input parameters (read-only)
- INTENT(OUT) - output parameters (modified)
- INTENT(INOUT) - bidirectional parameters
- Type parameters in functions
- Function return values with types

**Key Features:**
- `normalize()` - uses INOUT to modify parameter
- `dot_product()` - uses IN for read-only parameters
- `cross_product()` - returns derived type
- Derived type operations

**Compilation:**
```bash
oifortran function_parameters.f90 -o func_params.oir
```

**Expected Output:**
```
 Dot product (v1 . v2): 0.0
 Cross product (v1 x v2): 0.0 0.0 1.0
 Length of cross product: 1.00000
 Normalized cross product: 0.0 0.0 1.0
```

### 4. `dynamic_arrays.f90` - ALLOCATABLE and POINTER Arrays

**Demonstrates:**
- ALLOCATABLE attribute for dynamic arrays
- POINTER attribute and `=>` assignment
- TARGET attribute for pointer targets
- ALLOCATED() intrinsic function
- NULLIFY statement
- Memory management patterns

**Key Features:**
- `allocate_matrix()` - wrapper for allocation
- `deallocate_matrix()` - wrapper for deallocation
- `copy_matrix()` - allocate and copy array contents
- Pointer operations and safety checks

**Compilation:**
```bash
oifortran dynamic_arrays.f90 -o dynamic.oir
```

**Features Used:**
- `allocate()` - memory allocation
- `deallocate()` - memory deallocation
- `allocated()` - check allocation status
- `pointer => target` - pointer assignment
- `nullify()` - clear pointer

## Compilation and Usage

### Compiling Individual Files

```bash
cd /path/to/OIFortran
./oifortran examples/geometry.f90 -o geometry.oir
```

### Output Formats

The compiler supports multiple output formats:

```bash
# JSON format (default)
oifortran example.f90 -o example.json --format json

# Text format (human-readable)
oifortran example.f90 -o example.txt --format text

# BSON format (binary)
oifortran example.f90 -o example.bson --format bson
```

## Fortran 90 Features Used

| Feature | Example File | Purpose |
|---------|--------------|---------|
| MODULE | geometry.f90 | Code organization and encapsulation |
| Derived TYPE | geometry.f90, function_parameters.f90 | User-defined types |
| ALLOCATE | array_operations.f90, dynamic_arrays.f90 | Dynamic memory |
| DEALLOCATE | dynamic_arrays.f90 | Memory cleanup |
| FUNCTION | geometry.f90, array_operations.f90 | Procedures returning values |
| SUBROUTINE | array_operations.f90, dynamic_arrays.f90 | Procedures without return |
| INTENT | function_parameters.f90 | Parameter direction specification |
| Component access (%) | geometry.f90, function_parameters.f90 | Type member access |
| Array operations | array_operations.f90 | size(), sum() |
| ALLOCATABLE | dynamic_arrays.f90 | Dynamic arrays |
| POINTER | dynamic_arrays.f90 | Pointer variables |
| TARGET | dynamic_arrays.f90 | Pointer targets |

## Testing

These examples serve as test cases for:

1. **Lexer Testing**: Token recognition for all keywords and operators
2. **Parser Testing**: AST construction for Fortran 90 constructs
3. **Compiler Testing**: ObjectIR code generation
4. **Integration Testing**: End-to-end compilation

### Expected AST Elements

Each example should generate AST elements such as:

- `FortranModule` - for MODULE blocks
- `FortranDerivedType` - for TYPE definitions
- `FortranFunctionDefinition` - for FUNCTION procedures
- `FortranAttributedDeclarationStatement` - for declarations with attributes
- `FortranAllocateStatement` - for memory allocation
- `FortranComponentRefExpression` - for member access (%)

### Expected ObjectIR Elements

Compilation should produce ObjectIR:

- **Classes**: One for each module or derived type
- **Methods**: One for each function/subroutine
- **Fields**: For type components and module-level data
- **Instructions**: For assignments, array operations, calls

## Troubleshooting

### Common Issues

**Issue: "Unknown keyword"**
- Ensure the Fortran 90 lexer is properly configured
- Check keyword list in FortranLexer.cs

**Issue: Parser error on TYPE definition**
- Verify TYPE...END TYPE parsing is implemented
- Check token stream for proper keyword recognition

**Issue: Component access (%) not working**
- Check that Percent token is recognized by lexer
- Verify parser handles ComponentRefExpression
- Ensure compiler emits field access instructions

**Issue: ALLOCATE statement not recognized**
- Check that ALLOCATE keyword is in lexer
- Verify AllocateStatement parsing is implemented

## Contributing

When adding new examples:

1. Document the Fortran 90 features demonstrated
2. Include expected output for verification
3. Add to this README's feature table
4. Ensure no compiler warnings
5. Test both lexer and parser phases
6. Verify ObjectIR output is valid

## References

- **Fortran 90 Specification**: See `Contract/FORTRAN_90_SPEC.md`
- **Compiler Implementation**: See `docs/FORTRAN_90_COMPILER.md`
- **OIFortran Source**: See `src/OIFortran/Compiler/`
- **ObjectIR Architecture**: See `docs/ARCHITECTURE.md`

## Future Examples

Planned examples for additional features:

- **nested_modules.f90** - Module composition and USE statements
- **interface_blocks.f90** - Generic interfaces
- **operator_overloading.f90** - Custom operators (requires Fortran 2003)
- **character_strings.f90** - String operations
- **complex_numbers.f90** - Complex type and operations
- **implicit_none_patterns.f90** - Best practices with IMPLICIT NONE
- **assumed_shape_arrays.f90** - Passed array descriptors

---

**Last Updated**: November 2025  
**Status**: Active Development
