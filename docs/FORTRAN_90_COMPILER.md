# Fortran 90 Compiler Implementation Guide

**Document Status**: Implementation Guide for OIFortran Compiler  
**Date**: November 2025  
**Target**: Fortran 90 support in ObjectIR project

## Table of Contents

1. [Overview](#overview)
2. [Architecture](#architecture)
3. [Compilation Pipeline](#compilation-pipeline)
4. [Parser Implementation](#parser-implementation)
5. [Code Generation](#code-generation)
6. [Module System](#module-system)
7. [Type System](#type-system)
8. [Memory Management](#memory-management)
9. [Examples](#examples)
10. [Testing Strategy](#testing-strategy)

## Overview

The Fortran 90 compiler for ObjectIR extends the OIFortran compiler to support modern Fortran 90 language features. The implementation leverages the existing infrastructure (lexer, AST, builder) and adds new components for handling modules, derived types, advanced array operations, and memory management.

### Goals

- **Modularity**: Support MODULE construct for better code organization
- **Type Safety**: Implement derived types for structured data
- **Flexibility**: Support ALLOCATABLE and POINTER attributes
- **Interoperability**: Generate efficient ObjectIR that can be serialized and executed
- **Compatibility**: Maintain backward compatibility with Fortran 77 features
- **Performance**: Generate optimized ObjectIR code

### Key Features Implemented

- MODULE system with USE statements
- Derived type definitions (TYPE...END TYPE)
- Array operations with flexible bounds (DIMENSION, ALLOCATABLE, POINTER)
- Function definitions with INTENT parameters
- INTERFACE blocks for procedure declarations
- Memory allocation (ALLOCATE, DEALLOCATE, NULLIFY)
- Component access (%) for derived types
- Array subscripting and slicing

## Architecture

### Component Overview

```
┌─────────────────────────────────────────────┐
│  Fortran 90 Source Code (.f90)              │
└──────────────────┬──────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────┐
│  FortranLexer                               │ ← Tokenization
│  - Keywords recognition                     │ ← Operator matching
│  - String/number parsing                    │ ← Comment handling
└──────────────────┬──────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────┐
│  Token Stream (FortranToken[])              │
└──────────────────┬──────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────┐
│  FortranParser                              │ ← Syntax analysis
│  - Recursive descent parsing                │ ← AST construction
│  - MODULE/PROGRAM handling                  │ ← DERIVED TYPE parsing
│  - Expression parsing                       │ ← Attribute parsing
└──────────────────┬──────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────┐
│  Abstract Syntax Tree (AST)                 │
│  - FortranModule                            │
│  - FortranProgram                           │
│  - FortranDerivedType                       │
│  - FortranFunctionDefinition                │
│  - FortranStatement/Expression hierarchy    │
└──────────────────┬──────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────┐
│  FortranCompiler                            │ ← Code generation
│  - Module compilation                       │ ← Type mapping
│  - Statement emission                       │ ← Expression evaluation
│  - Memory management code gen               │ ← Symbol table management
└──────────────────┬──────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────┐
│  ObjectIR Module                            │
│  - Classes (for MODULEs)                    │
│  - Methods (for SUBROUTINEs/FUNCTIONs)     │
│  - Local allocations (for ARRAYs)           │
│  - Instructions (assignments, calls, etc.)  │
└──────────────────┬──────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────┐
│  Output Formats                             │
│  - JSON (Module.DumpJson())                 │
│  - Text (Module.DumpText())                 │
│  - BSON (serialization)                     │
└─────────────────────────────────────────────┘
```

### Key Classes

#### Lexer (FortranLexer.cs)

**Responsibilities:**
- Tokenize Fortran 90 source code
- Recognize keywords, operators, literals
- Handle comments and whitespace
- Support both Fortran-style (.EQ., .AND.) and modern (==, &&) operators

**Key Methods:**
- `Tokenize()`: Main entry point
- `NextToken()`: Get next token
- `LexIdentifier()`: Handle identifiers and keywords
- `LexNumber()`: Parse numeric literals (integer, real, complex)
- `LexString()`: Parse string literals
- `TryLexLogicalOperator()`: Match Fortran-style operators

**Fortran 90 Extensions:**
- Keywords: MODULE, USE, ONLY, CONTAINS, INTERFACE, TYPE, ALLOCATE, DEALLOCATE
- Operators: `=>` (pointer assignment), `%` (component access), `:` (array range)
- Attributes: DIMENSION, ALLOCATABLE, POINTER, TARGET, INTENT, OPTIONAL, PUBLIC, PRIVATE

#### Parser (FortranParser.cs)

**Responsibilities:**
- Parse Fortran 90 grammar into AST
- Handle MODULE constructs
- Parse derived type definitions
- Process function and subroutine declarations
- Parse attribute specifications

**Key Methods (to be implemented):**
- `ParseModule()`: MODULE...END MODULE
- `ParseDerivedType()`: TYPE...END TYPE
- `ParseUseStatement()`: USE module_name
- `ParseAttributedDeclaration()`: Type with attributes
- `ParseInterface()`: INTERFACE...END INTERFACE
- `ParseFunctionDefinition()`: FUNCTION declarations
- `ParseAllocateStatement()`: ALLOCATE and DEALLOCATE
- `ParseExpression()`: Extended for new operators

**Grammar Considerations:**
- Multi-word keywords (END PROGRAM, END MODULE, ELSE IF)
- Attribute parsing in declarations
- Array specification varieties (explicit, assumed-size, assumed-shape, deferred)
- Recursive descent suitable for Fortran syntax

#### Compiler (FortranCompiler.cs)

**Responsibilities:**
- Generate ObjectIR from AST
- Manage symbol tables and scopes
- Type checking and conversion
- Memory allocation code generation
- Method/procedure compilation

**Key Methods (to be implemented):**
- `CompileModule()`: Generate MODULE as ObjectIR class
- `CompileDerivedType()`: Generate TYPE as ObjectIR class
- `CompileFunction()`: Generate FUNCTION as ObjectIR method
- `CompileAllocate()`: Generate allocation instructions
- `EmitComponentAccess()`: Handle % operator
- `EmitArraySubscript()`: Handle array indexing
- `ManageTypeMapping()`: Map Fortran types to ObjectIR types

## Compilation Pipeline

### Phase 1: Lexical Analysis

```csharp
string source = File.ReadAllText("program.f90");
var lexer = new FortranLexer(source);
var tokens = lexer.Tokenize();
// tokens: IReadOnlyList<FortranToken>
```

**Output**: Token stream with classified tokens

### Phase 2: Syntax Analysis

```csharp
var parser = new FortranParser(tokens);
var program = parser.ParseProgram();  // FortranProgram or FortranModule
// AST structure ready for compilation
```

**Output**: Abstract Syntax Tree

### Phase 3: Semantic Analysis & Code Generation

```csharp
var compiler = new FortranCompiler(options);
Module irModule = compiler.Compile(astNode);
// ObjectIR module ready for serialization or execution
```

**Output**: ObjectIR Module

### Phase 4: Serialization

```csharp
string jsonOutput = irModule.DumpJson();
string textOutput = irModule.DumpText();
// Serialize or display the IR
```

## Parser Implementation

### Fortran 90 Program Structure

```fortran
[USE statements]*
[IMPLICIT statement]*
[TYPE definitions]*
[Declarations]*

[Internal procedures]
    [CONTAINS]
    [SUBROUTINE/FUNCTION definitions]*

END PROGRAM
```

### Key Parsing Patterns

#### 1. MODULE Parsing

```csharp
private FortranModule ParseModule()
{
    Consume(FortranTokenKind.KeywordModule, "Expected MODULE");
    string moduleName = Consume(FortranTokenKind.Identifier, "Expected module name").Text;
    
    var useStatements = new List<FortranUseStatement>();
    var declarations = new List<FortranStatement>();
    var subroutines = new List<FortranSubroutineDefinition>();
    var functions = new List<FortranFunctionDefinition>();
    var derivedTypes = new List<FortranDerivedType>();
    var interfaces = new List<FortranInterfaceBlock>();
    
    // Parse USE statements
    while (Check(FortranTokenKind.KeywordUse))
    {
        useStatements.Add(ParseUseStatement());
    }
    
    // Parse declarations, derived types
    while (!Check(FortranTokenKind.KeywordContains) && 
           !Check(FortranTokenKind.KeywordEndModule) &&
           !IsAtEnd)
    {
        if (Match(FortranTokenKind.KeywordType))
        {
            derivedTypes.Add(ParseDerivedType());
        }
        else if (IsTypeSpecifier(Current.Kind))
        {
            declarations.Add(ParseAttributedDeclaration());
        }
        // ... other cases
    }
    
    // Parse CONTAINS section (internal procedures)
    if (Match(FortranTokenKind.KeywordContains))
    {
        while (!Check(FortranTokenKind.KeywordEndModule) && !IsAtEnd)
        {
            if (Check(FortranTokenKind.KeywordSubroutine))
                subroutines.Add(ParseSubroutine());
            else if (Check(FortranTokenKind.KeywordFunction))
                functions.Add(ParseFunction());
        }
    }
    
    Consume(FortranTokenKind.KeywordEndModule, "Expected END MODULE");
    
    return new FortranModule(moduleName, useStatements, declarations, 
                            subroutines, functions, derivedTypes, interfaces);
}
```

#### 2. Derived Type Parsing

```csharp
private FortranDerivedType ParseDerivedType()
{
    // TYPE [::] type_name
    Consume(FortranTokenKind.KeywordType);
    
    Match(FortranTokenKind.DoubleColon);
    string typeName = Consume(FortranTokenKind.Identifier).Text;
    
    var components = new List<FortranTypeComponent>();
    
    // Parse components until END TYPE
    while (!Check(FortranTokenKind.KeywordEndType) && !IsAtEnd)
    {
        if (IsTypeSpecifier(Current.Kind))
        {
            var typeSpec = ParseTypeSpecifier();
            var compName = Consume(FortranTokenKind.Identifier).Text;
            
            IReadOnlyList<FortranArrayDimension>? dims = null;
            if (Match(FortranTokenKind.LParen))
            {
                dims = ParseArrayDimensions();
                Consume(FortranTokenKind.RParen);
            }
            
            components.Add(new FortranTypeComponent(compName, typeSpec, dims));
        }
    }
    
    Consume(FortranTokenKind.KeywordEndType, "Expected END TYPE");
    return new FortranDerivedType(typeName, components);
}
```

#### 3. Attributed Declaration Parsing

```csharp
private FortranAttributedDeclarationStatement ParseAttributedDeclaration()
{
    // type [, attr]* :: entity-decl-list
    var typeSpec = ParseTypeSpecifier();
    
    var attributes = new List<FortranDeclarationAttribute>();
    while (Match(FortranTokenKind.Comma))
    {
        if (Match(FortranTokenKind.KeywordDimension))
        {
            Consume(FortranTokenKind.LParen);
            attributes.Add(new FortranDimensionAttribute(ParseArrayDimensions()));
            Consume(FortranTokenKind.RParen);
        }
        else if (Match(FortranTokenKind.KeywordAllocatable))
        {
            attributes.Add(new FortranAllocatableAttribute());
        }
        else if (Match(FortranTokenKind.KeywordPointer))
        {
            attributes.Add(new FortranPointerAttribute());
        }
        else if (Match(FortranTokenKind.KeywordIntent))
        {
            Consume(FortranTokenKind.LParen);
            var intent = ParseIntentKind();
            Consume(FortranTokenKind.RParen);
            attributes.Add(new FortranIntentAttribute(intent));
        }
        // ... other attributes
    }
    
    Consume(FortranTokenKind.DoubleColon, "Expected :: after attributes");
    
    var entities = ParseEntityDeclarationList();
    
    return new FortranAttributedDeclarationStatement(typeSpec, attributes, entities);
}
```

## Code Generation

### Module Compilation

**Input:** FortranModule AST  
**Output:** ObjectIR Class

```csharp
public IRBuilder CompileModule(FortranModule module)
{
    var builder = new IRBuilder(module.Name);
    var classBuilder = builder.Class(module.Name)
        .Namespace(module.Name)
        .Access(AccessModifier.Public);
    
    // Compile derived types as inner classes
    foreach (var derivedType in module.DerivedTypes)
    {
        CompileDerivedType(classBuilder, derivedType);
    }
    
    // Compile module-level declarations
    foreach (var decl in module.Declarations)
    {
        // Generate static fields or properties
    }
    
    // Compile subroutines and functions
    foreach (var sub in module.Subroutines)
    {
        CompileSubroutine(classBuilder, sub);
    }
    
    foreach (var func in module.Functions)
    {
        CompileFunction(classBuilder, func);
    }
    
    classBuilder.EndClass();
    return builder;
}
```

### Derived Type Compilation

**Input:** FortranDerivedType  
**Output:** ObjectIR Class (nested)

```csharp
private void CompileDerivedType(ClassBuilder parent, FortranDerivedType derivedType)
{
    var typeClass = parent.Class(derivedType.Name)
        .Access(AccessModifier.Public);
    
    // Create field for each component
    foreach (var component in derivedType.Components)
    {
        var componentType = MapFortranTypeToObjectIR(component.Type);
        
        if (component.Dimensions != null)
        {
            // Array component: map to ObjectIR array type
            componentType = new ArrayTypeReference(componentType, component.Dimensions.Count);
        }
        
        typeClass.Field(component.Name, componentType)
            .Access(AccessModifier.Public);
    }
    
    typeClass.EndClass();
}
```

### Array Subscription Code Generation

**Input:** Array subscript expression  
**Output:** ObjectIR array access instruction

```csharp
private void EmitArraySubscript(FortranArraySubscriptExpression expr)
{
    // Emit the array reference
    EmitExpression(expr.Array);
    
    // For each subscript, emit index calculation
    foreach (var subscript in expr.Subscripts)
    {
        if (subscript is FortranExpressionSubscript exprSub)
        {
            EmitExpression(exprSub.Expression);
            _instructions.ArrayLoad();
        }
        else if (subscript is FortranRangeSubscript rangeSub)
        {
            // Range subscripting for slices
            if (rangeSub.Lower != null) EmitExpression(rangeSub.Lower);
            if (rangeSub.Upper != null) EmitExpression(rangeSub.Upper);
            if (rangeSub.Stride != null) EmitExpression(rangeSub.Stride);
            _instructions.ArraySlice();
        }
    }
}
```

### Memory Management

**ALLOCATE Statement:**

```csharp
private void EmitAllocate(FortranAllocateStatement stmt)
{
    foreach (var allocObj in stmt.Objects)
    {
        var variable = _locals[allocObj.Name];
        
        // Calculate total size from dimensions
        long totalSize = 1;
        foreach (var dim in allocObj.Dimensions ?? [])
        {
            // Evaluate dimension expression
            // totalSize *= evaluatedDimension;
        }
        
        // Emit allocation instruction
        _instructions.Alloc(variable, new IntegerLiteral(totalSize));
    }
}
```

**DEALLOCATE Statement:**

```csharp
private void EmitDeallocate(FortranDeallocateStatement stmt)
{
    foreach (var name in stmt.Names)
    {
        var variable = _locals[name];
        _instructions.Dealloc(variable);
    }
}
```

## Module System

### USE Statement Processing

The USE statement allows importing public entities from modules:

```fortran
USE my_module
USE my_module, ONLY: function1, subroutine2
```

**Compilation Strategy:**

1. Track module exports (PUBLIC declarations)
2. Create qualified references in consumer module
3. Generate lookup tables for rapid symbol resolution
4. Support renaming (future enhancement)

### Public/Private Access Control

```csharp
// Module-level declarations with access control
private Dictionary<string, (Entity, AccessModifier)> _moduleSymbols;

public void RegisterModuleEntity(string name, Entity entity, bool isPublic)
{
    var access = isPublic ? AccessModifier.Public : AccessModifier.Private;
    _moduleSymbols[name] = (entity, access);
}

public bool CanAccessEntity(string moduleName, string entityName, bool isExternal)
{
    if (!_moduleSymbols.TryGetValue(entityName, out var entry))
        return false;
    
    // Private entities inaccessible from outside
    if (entry.Item2 == AccessModifier.Private && isExternal)
        return false;
    
    return true;
}
```

## Type System

### Type Mapping

| Fortran 90 | ObjectIR | Notes |
|-----------|----------|-------|
| INTEGER | i32 | Default 4 bytes |
| INTEGER(8) | i64 | Extended precision |
| REAL | f64 | Double precision |
| REAL(4) | f32 | Single precision |
| COMPLEX | struct{f64, f64} | Real and imaginary |
| CHARACTER(n) | string | Fixed or variable length |
| LOGICAL | bool | Boolean type |
| TYPE(mytype) | class MyType | User-defined type |
| REAL(:) | array of f64 | Allocatable array |
| REAL(10,20) | matrix f64[10][20] | Fixed array |

### Type Checking

```csharp
private TypeReference ResolveType(FortranTypeSpec spec, 
                                  IReadOnlyList<FortranArrayDimension>? dimensions)
{
    var baseType = spec.Kind switch
    {
        FortranTypeKind.Integer => TypeReference.Int32,
        FortranTypeKind.Real => TypeReference.Float64,
        FortranTypeKind.Logical => TypeReference.Bool,
        FortranTypeKind.Character => TypeReference.String,
        _ => TypeReference.Void
    };
    
    // If array, wrap in array type
    if (dimensions != null)
    {
        return new ArrayTypeReference(baseType, dimensions.Count);
    }
    
    return baseType;
}
```

## Memory Management

### Allocation Tracking

The compiler maintains allocation information to generate appropriate cleanup code:

```csharp
private class AllocationInfo
{
    public string VariableName { get; set; }
    public TypeReference Type { get; set; }
    public bool IsAllocatable { get; set; }
    public bool IsPointer { get; set; }
    public Scope AllocationScope { get; set; }
}

private Stack<AllocationInfo> _activeAllocations = new();

private void TrackAllocation(string name, TypeReference type, bool isAllocatable)
{
    _activeAllocations.Push(new AllocationInfo
    {
        VariableName = name,
        Type = type,
        IsAllocatable = isAllocatable,
        AllocationScope = _currentScope
    });
}
```

### Scope-Based Cleanup

When exiting a scope (subroutine/function end), deallocate all scope-local allocations:

```csharp
private void GenerateCleanupCode()
{
    while (_activeAllocations.Count > 0)
    {
        var allocation = _activeAllocations.Peek();
        
        if (allocation.AllocationScope == _currentScope)
        {
            _instructions.Dealloc(
                new VariableReference(allocation.VariableName)
            );
            _activeAllocations.Pop();
        }
        else
        {
            break;
        }
    }
}
```

## Examples

### Example 1: Simple Module with Derived Type

**Fortran 90 Source:**

```fortran
module geometry
    implicit none
    private
    public :: point, distance
    
    type :: point
        real :: x, y, z
    end type point
    
contains
    
    real function distance(p1, p2)
        type(point), intent(in) :: p1, p2
        real :: dx, dy, dz
        
        dx = p2%x - p1%x
        dy = p2%y - p1%y
        dz = p2%z - p1%z
        
        distance = sqrt(dx**2 + dy**2 + dz**2)
    end function distance
    
end module geometry
```

**Compilation Steps:**

1. **Lexer**: Tokenizes MODULE, TYPE, REAL, etc.
2. **Parser**: Creates FortranModule with FortranDerivedType and FortranFunctionDefinition
3. **Compiler**: 
   - Creates geometry class in ObjectIR
   - Embeds Point class with x, y, z fields
   - Generates distance function with sqrt intrinsic call
   - Maps % component access to field access

**ObjectIR Output (conceptual):**

```
class geometry
    public class point
        public field real x
        public field real y
        public field real z
    end class
    
    public static method distance(p1: point, p2: point): real
        local dx: real
        local dy: real
        local dz: real
        
        dx = p2.x - p1.x
        dy = p2.y - p1.y
        dz = p2.z - p1.z
        
        return sqrt(dx * dx + dy * dy + dz * dz)
    end method
end class
```

### Example 2: Dynamic Arrays with ALLOCATE

**Fortran 90 Source:**

```fortran
program array_ops
    implicit none
    real, allocatable :: data(:,:)
    integer :: n, m, i, j
    
    n = 100
    m = 200
    
    allocate(data(n, m))
    
    do i = 1, n
        do j = 1, m
            data(i, j) = i * j
        end do
    end do
    
    print *, sum(data)
    
    deallocate(data)
    
end program array_ops
```

**Compilation Strategy:**

1. Declare `data` as allocatable 2D array
2. At allocation: compute total size (n*m*sizeof(real))
3. Generate alloc instruction with size
4. Track allocation for scope-based cleanup
5. Generate dealloc on deallocate or scope exit
6. Generate proper array indexing (row-major or column-major)

## Testing Strategy

### Unit Tests

**Parser Tests:**

```csharp
[TestClass]
public class Fortran90ParserTests
{
    [TestMethod]
    public void ParseModule_SimpleModule()
    {
        string source = @"
            module test_mod
            end module test_mod";
        
        var lexer = new FortranLexer(source);
        var parser = new FortranParser(lexer.Tokenize());
        var ast = parser.ParseProgram();
        
        Assert.IsInstanceOfType(ast, typeof(FortranModule));
        var mod = (FortranModule)ast;
        Assert.AreEqual("test_mod", mod.Name);
    }
    
    [TestMethod]
    public void ParseDerivedType_Point()
    {
        string source = @"
            type :: point
                real :: x, y
            end type";
        
        // Parse and verify structure
    }
    
    [TestMethod]
    public void ParseAllocateStatement()
    {
        string source = "allocate(array(10, 20))";
        // Parse and verify AST
    }
}
```

**Compiler Tests:**

```csharp
[TestClass]
public class Fortran90CompilerTests
{
    [TestMethod]
    public void Compile_ModuleWithDerivedType()
    {
        // Input: Fortran 90 module with type definition
        // Output: ObjectIR module with nested class
        // Verify: Class structure matches type definition
    }
    
    [TestMethod]
    public void Compile_ArrayAllocation()
    {
        // Input: ALLOCATE statement
        // Output: Alloc instruction in ObjectIR
        // Verify: Size calculation is correct
    }
}
```

### Integration Tests

**End-to-End Tests:**

```csharp
[TestMethod]
public void E2E_GeometryModule()
{
    string source = File.ReadAllText("geometry.f90");
    
    var compiler = new FortranLanguageCompiler();
    var module = compiler.CompileSource(source);
    
    string json = module.DumpJson();
    
    // Verify:
    // - Module has geometry class
    // - point type exists
    // - distance function compiled
    // - JSON is valid
    Assert.IsTrue(json.Contains("\"geometry\""));
    Assert.IsTrue(json.Contains("\"point\""));
    Assert.IsTrue(json.Contains("\"distance\""));
}
```

### Example Test Files

Create test Fortran 90 files:

- `test_module.f90` - Simple module
- `test_derived_type.f90` - Type definitions
- `test_arrays.f90` - Array operations
- `test_allocation.f90` - Dynamic memory
- `test_functions.f90` - Function definitions
- `test_interface.f90` - Interface blocks

## Implementation Roadmap

### Phase 1: Foundation (Current)
- ✅ Extend lexer with Fortran 90 keywords
- ✅ Extend AST with Fortran 90 classes
- 🔄 Update parser for MODULE/TYPE/FUNCTION

### Phase 2: Core Features
- Parser for MODULE constructs
- Parser for DERIVED TYPE
- Parser for USE statements
- Parser for attributed declarations
- Compiler for derived types (as ObjectIR classes)
- Compiler for module-level code generation

### Phase 3: Advanced Features
- ALLOCATE/DEALLOCATE code generation
- INTERFACE block support
- Generic procedures
- Operator overloading
- Array operations and slicing

### Phase 4: Optimization & Polish
- Type checking improvements
- Error messages
- Performance optimization
- Documentation and examples

## Troubleshooting

### Common Issues

**Issue: Keywords not recognized**
- Ensure lexer has keyword entries
- Check case sensitivity (should be case-insensitive)
- Verify token kind enum has the keyword

**Issue: Parser crashes on MODULE**
- Check that ParseModule is called
- Verify all END keywords are matched
- Check USE statement parsing

**Issue: Component access (%) not working**
- Ensure Percent token is created by lexer
- Check parser handles ComponentRefExpression
- Verify compiler emits field access

**Issue: ALLOCATE generates wrong code**
- Check dimension parsing
- Verify size calculation
- Ensure alloc instruction parameters

## References

- **Fortran 90 Specification**: FORTRAN_90_SPEC.md (Contract/)
- **ObjectIR Architecture**: ARCHITECTURE.md (docs/)
- **Grammar Reference**: GRAMMAR.md (docs/)
- **OIFortran Compiler**: src/OIFortran/
- **ObjectIR Core**: src/ObjectIR.Core/

## Future Enhancements

- Fortran 2003+ features (parameterized types, bind(C))
- Coarray support for parallelism
- Enhanced array operations (WHERE, FORALL)
- Custom operator definitions
- Module procedures with internal state
- Performance profiling and optimization
- Debugging symbols and line information

---

**Last Updated**: November 2025  
**Maintainers**: ObjectIR Project Team
