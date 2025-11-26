# Self-Hosting Bootstrap Strategy

## Overview
To self-host the ObjectIR compilers, we need a multi-stage bootstrap process that breaks circular dependencies.

## Stage 1: Bootstrap Compiler (C# → IR)

### Current Challenge
- CSharpFrontend.cs depends on `ObjectIR.Core` types
- Can't compile it because Roslyn doesn't have ObjectIR.Core as a reference during analysis
- This creates a circular dependency

### Solution: Bootstrap Phase
Create a minimal C# representation that can be compiled in isolation:

1. **Extract core compilation logic** from RoslynCompiler/CompilerService into a "pure" version
   - Remove ObjectIR.Core dependencies (temporarily)
   - Generate generic IR structure (JSON/XML representation)
   - Focus only on type extraction and member analysis

2. **Compile this bootstrap version with our current compiler**
   ```bash
   dotnet run -- CSharpFrontend_Bootstrap.cs -o bin/ -m CSharpBootstrap -f json
   ```

3. **Result**: JSON/IR representation of the compiler itself

## Stage 2: Self-Hosted Compilation

Once we have the IR representation of the compiler:

### Approach A: Runtime Interpretation
- Load the generated IR representation
- Use it as metadata to understand the compiler's structure
- Can then analyze the full CSharpFrontend (with ObjectIR.Core refs) through semantic understanding

### Approach B: IR-to-Executable Compilation
- Take the IR representation of CSharpFrontend
- Compile it back to executable form using ObjectIR.Runtime or ObjectIR.CppRuntime
- This becomes a "self-hosted" version of the compiler

### Approach C: Multi-Pass Compilation
1. **Pass 1**: Compile CSharpFrontend stub (no dependencies) → IR
2. **Pass 2**: Use IR + reflection to resolve ObjectIR.Core → Full IR with dependencies
3. **Pass 3**: Compile the Fortran frontend using the enhanced IR resolver

## Recommended Implementation Path

### Phase 1: Bootstrap Extraction (Next)
Extract these to independent files that can compile:
- `TypeExtractor.cs` - Pure type/member analysis (no ObjectIR.Core refs)
- `IREmitter.cs` - IR generation logic (generic structures)
- `CompilerOptions.cs` - Already independent ✓
- `CommandLineParser.cs` - Already independent ✓

### Phase 2: Bootstrap Compilation
```bash
# Compile the bootstrap set
csharp-to-objectir TypeExtractor.cs IREmitter.cs -m ObjectIRBootstrap -f json -o bootstrap/
```

### Phase 3: Self-Hosting
Create `ObjectIRCompiler.cs` that:
- Loads the bootstrap IR
- Uses it to understand compiler structure
- Can now handle full compilation with dependencies resolved

### Phase 4: Fortran Compiler
```bash
# Use self-hosted compiler to compile Fortran frontend
objectir-compiler OIFortran/*.cs -m FortranCompiler -f ir
```

## Assembly Resolution Strategy

### Current Limitation
```
Error: ObjectIR.Core not referenced during analysis
```

### Solution: Post-Compilation Resolution
1. Compile without ObjectIR.Core reference → basic IR
2. Scan compiled IR for unresolved type references
3. Load ObjectIR.Core assembly and resolve references
4. Generate complete IR with all dependencies mapped

### Implementation
```csharp
public class MultiPassCompiler
{
    // Pass 1: Compile without project references
    var basicIR = CompileWithoutDependencies(sourceFiles);
    
    // Pass 2: Resolve unresolved types
    var resolvedIR = ResolveTypes(basicIR, new[] {
        Assembly.Load("ObjectIR.Core"),
        Assembly.Load("ObjectIR.CSharpFrontend")
    });
    
    // Pass 3: Generate final output
    return EmitIR(resolvedIR);
}
```

## Benefits

1. **True Self-Hosting**: Compiler can compile itself
2. **Bootstrap Capability**: Can bootstrap new compilers (Fortran, etc.)
3. **IR Transparency**: All compiler structures available as data
4. **Portable**: Compiled IR can run on any ObjectIR runtime

## Challenges & Mitigations

| Challenge | Mitigation |
|-----------|-----------|
| Circular dependencies | Multi-pass compilation with post-resolution |
| Missing assembly refs | Type scanning + runtime resolution |
| Complex type hierarchies | Preserve full qualified names in IR |
| Performance | Cache resolved types, use incremental compilation |

## Next Steps

1. Extract bootstrap-compatible components
2. Implement multi-pass compiler logic  
3. Test bootstrap compilation
4. Verify Fortran compiler can be self-hosted
5. Document final self-hosting pattern
