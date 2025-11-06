# Fortran 90 Specification for OIFortran Compiler

Status: specification — This document defines the Fortran 90 language features supported by the OIFortran compiler as part of the ObjectIR project. It includes formal grammar (EBNF), lexical rules, AST structures, and compilation semantics.

## Goals for Fortran 90 Support

- Support modern Fortran 90 language features introduced in 1990 which provide significant improvements over Fortran 77.
- Enable modular programming through MODULE constructs.
- Support advanced type definitions via DERIVED TYPES (records/structures).
- Enable flexible memory management with ALLOCATABLE and POINTER attributes.
- Support multi-dimensional arrays with flexible bounds.
- Provide interfaces for better type checking and generic programming.
- Maintain backward compatibility with Fortran 77 constructs.
- Generate efficient ObjectIR intermediate representation.

## Overview of Fortran 90 Features

### Core Language Elements

- **Source files**: UTF-8 text, extension `.f90` or `.f95` (free-form format)
- **Top-level constructs**: PROGRAM, MODULE, SUBROUTINE, FUNCTION
- **Type system**: INTEGER, REAL, COMPLEX, CHARACTER, LOGICAL, derived types
- **Data attributes**: DIMENSION, PARAMETER, ALLOCATABLE, POINTER, INTENT, TARGET, VALUE
- **Control flow**: IF/ELSE, SELECT CASE, DO loops, EXIT, CYCLE
- **Procedures**: Subroutines, functions, interfaces, generic procedures
- **Modules**: MODULE definitions with PUBLIC/PRIVATE access control
- **Derived types**: TYPE definitions for user-defined composite data
- **Intrinsic functions**: Extensive set of built-in functions and operations

## Lexical Grammar

### Tokens

All whitespace between tokens is ignored except inside strings:

- **IDENTIFIER**: `[A-Za-z_][A-Za-z0-9_]*`
- **INTEGER**: `[0-9]+` or `[0-9]+_kind` where kind is INTEGER or identifier
- **REAL**: `[0-9]+.[0-9]*` or `[0-9]*.[0-9]+` with optional `E/e[+-]?[0-9]+` exponent
- **COMPLEX**: `([REAL, REAL)` in parentheses
- **CHARACTER**: Single or double quoted strings with escape sequences
- **LOGICAL**: `.TRUE.` or `.FALSE.`

### Keywords (Case-Insensitive)

Core language:
- `PROGRAM`, `MODULE`, `SUBROUTINE`, `FUNCTION`, `END`
- `IMPLICIT`, `NONE`, `CONTAINS`
- `USE`, `ONLY`, `INTERFACE`, `OPERATOR`

Data types:
- `INTEGER`, `REAL`, `COMPLEX`, `CHARACTER`, `LOGICAL`
- `DIMENSION`, `PARAMETER`, `ALLOCATABLE`, `POINTER`, `TARGET`, `VALUE`
- `INTENT`, `IN`, `OUT`, `INOUT`, `OPTIONAL`
- `PUBLIC`, `PRIVATE`, `PROTECTED`

Control flow:
- `IF`, `THEN`, `ELSE`, `ELSE IF`, `END IF`
- `SELECT`, `CASE`, `DEFAULT`, `END SELECT`
- `DO`, `WHILE`, `EXIT`, `CYCLE`, `END DO`
- `RETURN`, `STOP`

User-defined types:
- `TYPE`, `END TYPE`
- `DERIVED`

Memory operations:
- `ALLOCATE`, `DEALLOCATE`, `NULLIFY`
- `ASSOCIATED`, `ALLOCATED`

### Operators

- Arithmetic: `+`, `-`, `*`, `/`, `**` (power)
- Relational: `==`, `/=`, `<`, `<=`, `>`, `>=`
- Logical: `.AND.`, `.OR.`, `.NOT.`, `.EQV.`, `.NEQV.`
- Assignment: `=`
- Array: `:` (range operator), `::` (scope)
- Component access: `%`

### Comments

- `!` to end-of-line (free-form format)

All tokens are case-insensitive for keywords, but case-sensitive for identifiers (by convention).

## EBNF Grammar (Fortran 90)

```ebnf
Start ::= ProgramUnit+

ProgramUnit ::= ProgramStatement | ModuleStatement | SubroutineStatement | FunctionStatement | ExternalStatement

(* Program units *)
ProgramStatement ::= 'PROGRAM' IDENTIFIER [UseStmt]* [ImplicitStmt]* [Declaration]* [ExecutableConstruct]* [ContainsClause]* 'END' ['PROGRAM' [IDENTIFIER]]

ModuleStatement ::= 'MODULE' IDENTIFIER [UseStmt]* [ImplicitStmt]* [Declaration]* [ContainsClause]? 'END' ['MODULE' [IDENTIFIER]]

ContainsClause ::= 'CONTAINS' (SubroutineStatement | FunctionStatement | InterfaceStatement)*

SubroutineStatement ::= 'SUBROUTINE' IDENTIFIER '(' [ParameterList]? ')' [Attr]* 
                       [UseStmt]* [ImplicitStmt]* [Declaration]* [ExecutableConstruct]* [InternalSubprogramPart]
                       'END' ['SUBROUTINE' [IDENTIFIER]]

FunctionStatement ::= 'FUNCTION' IDENTIFIER '(' [ParameterList]? ')' [TypeSpec] [Attr]*
                    [UseStmt]* [ImplicitStmt]* [Declaration]* [ExecutableConstruct]* [InternalSubprogramPart]
                    'END' ['FUNCTION' [IDENTIFIER]]

ExternalStatement ::= 'EXTERNAL' IDENTIFIER (',' IDENTIFIER)*

(* Module components *)
UseStmt ::= 'USE' ModuleName [ModuleNature]? [ONLY ':' UseList]?
ModuleNature ::= ',INTRINSIC' | ',NON_INTRINSIC'
UseList ::= UseName (',' UseName)*
UseName ::= IDENTIFIER ['=>' IDENTIFIER]

InterfaceStatement ::= 'INTERFACE' [OPERATOR | ASSIGNMENT]? [IDENTIFIER]?
                     (InterfaceBody | ProcedureStatement)+
                     'END' 'INTERFACE' [OPERATOR | ASSIGNMENT]? [IDENTIFIER]?

InterfaceBody ::= InterfaceSpecification | ProcedureStatement
ProcedureStatement ::= 'PROCEDURE' '(' IDENTIFIER [, IDENTIFIER]* ')'

(* Type declarations *)
TypeSpec ::= ('INTEGER' | 'REAL' | 'COMPLEX' | 'CHARACTER' | 'LOGICAL') [KindParam]?
           | 'TYPE' '(' DerivedTypeName ')'

KindParam ::= '(' 'KIND' '=' Expression | Expression ')'

Declaration ::= TypeSpec [Attr]* ':' ':' EntityDeclList EOL
              | TypeDef
              | 'IMPLICIT' TypeSpec '(' LetterRange (',' LetterRange)* ')'
              | 'IMPLICIT' 'NONE'

Attr ::= 'DIMENSION' '(' ArraySpec ')'
       | 'PARAMETER'
       | 'ALLOCATABLE'
       | 'POINTER'
       | 'TARGET'
       | 'VALUE'
       | 'INTENT' '(' IntentType ')'
       | 'OPTIONAL'
       | 'PUBLIC'
       | 'PRIVATE'
       | 'PROTECTED'
       | 'CONTIGUOUS'
       | 'ASYNCHRONOUS'
       | 'VOLATILE'
       | 'BIND' '(' 'C' [',' NAME '=' Expression]? ')'

IntentType ::= 'IN' | 'OUT' | 'INOUT'

EntityDeclList ::= EntityDecl (',' EntityDecl)*

EntityDecl ::= IDENTIFIER [ArraySpec]? [CharLength]? ['=' Initialization]
             | IDENTIFIER '=>' NULL '(' ')'

ArraySpec ::= ExplicitShapeSpec | AssumedShapeSpec | AssumedSizeSpec | DeferredShapeSpec

ExplicitShapeSpec ::= (Expression ':')? Expression (',' (Expression ':')? Expression)*
AssumedShapeSpec ::= (':' [Lower ':' Upper]?) (',' (':' [Lower ':' Upper]?))* 
DeferredShapeSpec ::= (':') (',' ':')*
AssumedSizeSpec ::= (ExplicitShapeSpec ',')? ('*')

CharLength ::= '*' (Expression | '*')

TypeDef ::= 'TYPE' ['::'] IDENTIFIER [TypeAttr]?
           ('!' comment)*
           (ComponentDecl EOL)+
           'END' 'TYPE' [IDENTIFIER]

TypeAttr ::= ',' ('PUBLIC' | 'PRIVATE' | 'SEQUENCE')

ComponentDecl ::= TypeSpec [ComponentAttr]* (',' ComponentAttr)* '::' ComponentDeclList

ComponentAttr ::= 'DIMENSION' '(' ArraySpec ')'
                | 'POINTER'
                | 'ALLOCATABLE'
                | 'INTENT' '(' IntentType ')'
                | 'OPTIONAL'

ComponentDeclList ::= ComponentName ['*' CharLength] ['=' ComponentInit] (',' ComponentName ['*' CharLength] ['=' ComponentInit])*

(* Parameters *)
ParameterList ::= Parameter (',' Parameter)*
Parameter ::= [Intent]? [Optional]? TypeSpec? '::' IDENTIFIER [ArraySpec]?

Intent ::= 'INTENT' '(' ('IN' | 'OUT' | 'INOUT') ')'
Optional ::= 'OPTIONAL'

(* Executable constructs *)
ExecutableConstruct ::= ActionStmt
                      | ControlConstruct

ActionStmt ::= AssignmentStmt
             | ProcedureCallStmt
             | ReturnStmt
             | CycleStmt
             | ExitStmt
             | StopStmt
             | AllocateStmt
             | DeallocateStmt

AssignmentStmt ::= Variable '=' Expression

ProcedureCallStmt ::= 'CALL' ProcedureName ['(' [ArgList]? ')']

ReturnStmt ::= 'RETURN' [Expression]?

CycleStmt ::= 'CYCLE' [ConstructName]?

ExitStmt ::= 'EXIT' [ConstructName]?

StopStmt ::= 'STOP' [Expression]?

AllocateStmt ::= 'ALLOCATE' '(' AllocationList [',' AllocOpt]* ')'

DeallocateStmt ::= 'DEALLOCATE' '(' PointerObjectList [',' AllocOpt]* ')'

ControlConstruct ::= IfConstruct
                   | CaseConstruct
                   | DoConstruct
                   | WhileConstruct

IfConstruct ::= ['label:'] 'IF' '(' Condition ')' 'THEN' 
               (ExecutableConstruct)*
               ('ELSE IF' '(' Condition ')' 'THEN' (ExecutableConstruct)*)*
               ['ELSE' (ExecutableConstruct)*]
               'END IF' [ConstructName]

CaseConstruct ::= ['label:'] 'SELECT' 'CASE' '(' Expression ')'
                 ('CASE' '(' CaseValueRange ')' (ExecutableConstruct)*)+
                 ['CASE DEFAULT' (ExecutableConstruct)*]
                 'END SELECT' [ConstructName]

DoConstruct ::= ['label:'] 'DO' [LoopControl] [ConstructName] ':']
               (ExecutableConstruct)*
               'END DO' [ConstructName]

LoopControl ::= IDENTIFIER '=' Expression ',' Expression [',' Expression]
              | 'WHILE' '(' Condition ')'

WhileConstruct ::= ['label:'] 'DO WHILE' '(' Condition ')' [ConstructName] ':']
                  (ExecutableConstruct)*
                  'END DO' [ConstructName]

(* Expressions *)
Expression ::= OrExpr

OrExpr ::= AndExpr ('.OR.' AndExpr)*
AndExpr ::= NotExpr ('.AND.' NotExpr)*
NotExpr ::= ['.NOT.'] EquivalenceExpr
EquivalenceExpr ::= RelationalExpr (('.EQV.' | '.NEQV.') RelationalExpr)*
RelationalExpr ::= ConcatenationExpr (RelOp ConcatenationExpr)*
ConcatenationExpr ::= AdditiveExpr ('//' AdditiveExpr)*
AdditiveExpr ::= MultiplicativeExpr (('+' | '-') MultiplicativeExpr)*
MultiplicativeExpr ::= PowerExpr (('*' | '/') PowerExpr)*
PowerExpr ::= PrimaryExpr ('**' PrimaryExpr)*

PrimaryExpr ::= Literal
              | Variable
              | FunctionReference
              | '(' Expression ')'
              | ArrayConstructor
              | ComponentRef

Variable ::= IDENTIFIER [PartRef]*
PartRef ::= '%' IDENTIFIER
          | '(' ArraySection ')'
          | '[' CoIndices ']'

ArraySection ::= [Subscript] (':' [Subscript])? 
               | [Subscript] ':' [Subscript] ':' [Stride]

FunctionReference ::= ProcedureName '(' [ArgList]? ')'

ArgList ::= Arg (',' Arg)*
Arg ::= [IDENTIFIER '='] Expression

Literal ::= IntLiteral
          | RealLiteral
          | ComplexLiteral
          | CharLiteral
          | LogicalLiteral

RelOp ::= '==' | '/=' | '<' | '<=' | '>' | '>='

LetterRange ::= Letter '-' Letter
Letter ::= [A-Z]

DerivedTypeName ::= IDENTIFIER
ModuleName ::= IDENTIFIER
ProcedureName ::= IDENTIFIER
ConstructName ::= IDENTIFIER
```

## AST Structure

The AST representation in ObjectIR consists of:

```
ProgramUnit
  ├── Program
  │   ├── name: string
  │   ├── useStatements: UseStatement[]
  │   ├── implicitStmt: ImplicitStatement
  │   ├── declarations: Declaration[]
  │   ├── statements: ExecutableConstruct[]
  │   └── internalSubprograms: (Subroutine | Function)[]
  │
  ├── Module
  │   ├── name: string
  │   ├── publicPrivate: AccessModifier
  │   ├── useStatements: UseStatement[]
  │   ├── declarations: Declaration[]
  │   ├── interfaces: Interface[]
  │   ├── subroutines: Subroutine[]
  │   ├── functions: Function[]
  │   └── derivedTypes: DerivedType[]
  │
  ├── Subroutine
  │   ├── name: string
  │   ├── parameters: Parameter[]
  │   ├── declarations: Declaration[]
  │   ├── statements: ExecutableConstruct[]
  │   ├── internalSubprograms: (Subroutine | Function)[]
  │   └── interface: Interface (optional)
  │
  └── Function
      ├── name: string
      ├── returnType: TypeSpec
      ├── parameters: Parameter[]
      ├── declarations: Declaration[]
      ├── statements: ExecutableConstruct[]
      ├── internalSubprograms: (Subroutine | Function)[]
      └── interface: Interface (optional)

Declaration
  ├── TypeDeclaration
  │   ├── typeSpec: TypeSpec
  │   ├── attributes: Attribute[]
  │   └── entities: Entity[]
  │
  ├── TypeDefinition
  │   ├── name: string
  │   ├── attributes: TypeAttribute[]
  │   ├── components: Component[]
  │   └── bindingProcedures: BindingProcedure[]
  │
  ├── ImplicitStatement
  │   └── rules: (TypeSpec, LetterRanges)[]
  │
  └── ImplicitNoneStatement

TypeSpec
  ├── INTEGER [kind]
  ├── REAL [kind]
  ├── COMPLEX [kind]
  ├── CHARACTER [len]
  ├── LOGICAL [kind]
  └── DERIVED (DerivedTypeName)

Attribute
  ├── DIMENSION (ArraySpec)
  ├── PARAMETER
  ├── ALLOCATABLE
  ├── POINTER
  ├── TARGET
  ├── VALUE
  ├── INTENT (IN | OUT | INOUT)
  ├── OPTIONAL
  ├── PUBLIC
  ├── PRIVATE
  ├── PROTECTED
  ├── CONTIGUOUS
  ├── ASYNCHRONOUS
  ├── VOLATILE
  └── BIND_C

ArraySpec
  ├── ExplicitShape (lower:upper)*
  ├── AssumedShape (:)*
  ├── AssumedSize
  └── DeferredShape

ExecutableConstruct
  ├── IfConstruct
  │   ├── condition: Expression
  │   ├── thenStmts: ExecutableConstruct[]
  │   ├── elseIfClauses: (Expression, ExecutableConstruct[])[]
  │   └── elseStmts: ExecutableConstruct[]
  │
  ├── SelectCaseConstruct
  │   ├── selectExpr: Expression
  │   ├── cases: (CaseValueRange[], ExecutableConstruct[])[]
  │   └── defaultCase: ExecutableConstruct[]
  │
  ├── DoConstruct
  │   ├── loopControl: LoopControl (optional)
  │   ├── statements: ExecutableConstruct[]
  │   └── constructName: string
  │
  ├── AssignmentStatement
  │   ├── target: Variable
  │   └── value: Expression
  │
  ├── ProcedureCallStatement
  │   ├── procedureName: string
  │   └── arguments: Argument[]
  │
  ├── AllocationStatement
  │   ├── allocationList: AllocationObject[]
  │   └── allocateOptions: AllocateOption[]
  │
  ├── DeallocationStatement
  │   ├── pointerList: Variable[]
  │   └── deallocateOptions: DeallocateOption[]
  │
  ├── ReturnStatement
  │   └── value: Expression (optional, for functions)
  │
  ├── CycleStatement
  │   └── constructName: string (optional)
  │
  ├── ExitStatement
  │   └── constructName: string (optional)
  │
  └── StopStatement
      └── code: Expression (optional)

Interface
  ├── name: string (optional)
  ├── isOperator: boolean
  ├── isAssignment: boolean
  └── procedures: Procedure[]
```

## Compilation Semantics

### Module System

Modules provide namespace and encapsulation:
- Each MODULE is compiled as a separate class in ObjectIR
- PUBLIC/PRIVATE access control maps to ObjectIR AccessModifier
- USE statements create qualified references to module entities
- Module initialization happens in a module-level initialization method

### Type System

Type declarations map to ObjectIR types:
- Fortran types map to ObjectIR: INTEGER→i32, REAL→f64, CHARACTER→string, LOGICAL→bool
- Derived types compile to ObjectIR CLASS definitions
- Array types compile to appropriate array representations
- ALLOCATABLE and POINTER attributes generate heap allocation instructions

### Procedure Compilation

Subroutines and functions:
- Compile to ObjectIR methods with appropriate signatures
- Parameters become method parameters with INTENT mapping to attributes
- Local variables become local allocations
- RETURN statements generate ObjectIR return instructions
- OPTIONAL parameters require runtime presence checks

### Array Handling

Arrays in Fortran 90:
- Explicit shape: compile with known dimensions
- Assumed shape: pass descriptor information through module interface
- Assumed size: treat as dynamic-length arrays
- Deferred shape: compile as pointers to allocated memory
- Array operations use Fortran 90 array intrinsics

### Memory Management

- ALLOCATE statements generate ObjectIR memory allocation calls
- DEALLOCATE statements generate ObjectIR memory deallocation calls
- POINTER attributes require reference tracking
- Automatic memory cleanup for scope-based allocations

## Intrinsic Functions

Fortran 90 provides extensive intrinsic functions categorized as:

### Numeric Functions
- `ABS`, `SIGN`, `MOD`, `FLOAT`, `AINT`, `NINT`, `MAX`, `MIN`
- `SQRT`, `EXP`, `LOG`, `LOG10`, `SIN`, `COS`, `TAN`, `ASIN`, `ACOS`, `ATAN`
- `SINH`, `COSH`, `TANH`

### Type Conversion
- `INT`, `REAL`, `DBLE`, `CMPLX`, `ICHAR`, `CHAR`

### Character Functions
- `LEN`, `INDEX`, `TRIM`, `ADJUSTL`, `ADJUSTR`, `REPEAT`, `LEN_TRIM`
- `SCAN`, `VERIFY`, `TRANSFER`

### Logical Functions
- `LOGICAL`

### Array Functions
- `SUM`, `PRODUCT`, `MAXVAL`, `MINVAL`
- `ALL`, `ANY`
- `COUNT`, `SHAPE`, `SIZE`, `LBOUND`, `UBOUND`
- `RESHAPE`, `TRANSPOSE`, `MATMUL`, `DOT_PRODUCT`
- `CSHIFT`, `EOSHIFT`, `PACK`, `UNPACK`, `SPREAD`, `MERGE`

### Inquiry Functions
- `ALLOCATED`, `ASSOCIATED`, `PRESENT`
- `KIND`, `PRECISION`, `RANGE`, `DIGITS`, `MAXEXPONENT`, `MINEXPONENT`

## Example Fortran 90 Code

### Simple Module

```fortran
module math_functions
  implicit none
  private
  
  public :: add, subtract, multiply
  
contains
  
  function add(a, b) result(res)
    real, intent(in) :: a, b
    real :: res
    res = a + b
  end function add
  
  function subtract(a, b) result(res)
    real, intent(in) :: a, b
    real :: res
    res = a - b
  end function subtract
  
  function multiply(a, b) result(res)
    real, intent(in) :: a, b
    real :: res
    res = a * b
  end function multiply
  
end module math_functions
```

### Derived Type Example

```fortran
module geometry
  implicit none
  
  type :: point
    real :: x, y, z
  end type point
  
  type :: vector
    type(point) :: start, end
  end type vector
  
contains
  
  subroutine print_point(p)
    type(point), intent(in) :: p
    print *, 'Point: (', p%x, ',', p%y, ',', p%z, ')'
  end subroutine print_point
  
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

### Arrays and Allocation

```fortran
program array_example
  implicit none
  real, allocatable :: data(:,:)
  integer :: n, m, i, j
  
  n = 100
  m = 200
  
  allocate(data(n, m))
  
  do i = 1, n
    do j = 1, m
      data(i,j) = i * j
    end do
  end do
  
  print *, 'Sum of all elements:', sum(data)
  print *, 'Maximum:', maxval(data)
  print *, 'Minimum:', minval(data)
  
  deallocate(data)
  
end program array_example
```

## Migration Path from Fortran 77

The OIFortran compiler supports incremental migration:

1. **Phase 1**: Continue using Fortran 77 features (basic types, fixed format)
2. **Phase 2**: Adopt free-form format and IMPLICIT NONE
3. **Phase 3**: Use derived types for structured data
4. **Phase 4**: Modularize code with MODULE system
5. **Phase 5**: Use advanced array operations and allocation
6. **Phase 6**: Adopt interfaces and generic procedures

Each phase is fully backward compatible, allowing gradual modernization of existing code.

## Limitations and Future Enhancements

### Current Limitations

- Coarrays (for parallelism) not yet supported
- Fortran 2003+ features not yet included
- Assumed-length CHARACTER parameters require special handling
- User-defined operators limited scope in initial implementation

### Future Enhancement Areas

- Fortran 2003: Parameterized derived types, bind(C) interoperability
- Fortran 2008: Coarrays, submodules, asynchronous I/O
- Optimization: Array vectorization, SIMD operations
- Debugging: Enhanced error messages and type checking
- Interoperability: C/C++ foreign function interface improvements

## References

- [Fortran 90 Standard](https://wg5-ral.upc.es/iso/Fortran-90-1991.pdf)
- [Modern Fortran Explained](https://global.oup.com/academic/product/modern-fortran-explained-9780198811886)
- ObjectIR IR Specification: See `docs/ARCHITECTURE.md`
- ObjectIR Core Module: See `src/ObjectIR.Core`
