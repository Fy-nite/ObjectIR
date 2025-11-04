# Module Composition Guide

## Overview

**Module Composition** allows you to combine multiple ObjectIR modules into a single unified module. This is useful for:

- **Merging separate projects** into a unified codebase
- **Resolving dependencies** across module boundaries
- **Managing large codebases** by splitting into logical components
- **Validating consistency** across module interfaces
- **Generating combined code** from multiple sources

## Core Concepts

### ModuleComposer

The `ModuleComposer` orchestrates the composition process:

```csharp
using ObjectIR.Core.Composition;

var composer = new ModuleComposer();
composer.AddModule(module1);
composer.AddModule(module2);
composer.AddModule(module3);

var composedModule = composer.Compose("UnifiedApp", "1.0.0");
```

### DependencyResolver

The `DependencyResolver` analyzes type dependencies across modules:

- Resolves type references to their definitions
- Detects circular dependencies
- Performs topological sorting
- Builds dependency graphs

## Quick Start

### 1. Create Multiple Modules

```csharp
// Module 1: Core types
var coreModule = new Module("Core", "1.0.0");
var entityClass = coreModule.DefineClass("Entity");
entityClass.DefineField("id", TypeReference.Int32);
entityClass.DefineField("name", TypeReference.String);

// Module 2: Business logic
var businessModule = new Module("Business", "1.0.0");
var userClass = businessModule.DefineClass("User");
userClass.BaseType = new TypeReference("Entity");
userClass.DefineField("email", TypeReference.String);
```

### 2. Compose the Modules

```csharp
var composer = new ModuleComposer();
composer.AddModule(coreModule);
composer.AddModule(businessModule);

// Validate before composing
var validation = composer.Validate();
if (validation.HasErrors)
{
    foreach (var error in validation.Errors)
        Console.WriteLine($"Error: {error}");
    return;
}

// Compose into unified module
var composed = composer.Compose("UnifiedApp", "2.0.0");
```

### 3. Use the Composed Module

```csharp
// Generate code
var generator = new CSharpCodeGenerator();
var code = generator.Generate(composed);
File.WriteAllText("UnifiedApp.cs", code);

// Serialize to JSON
var json = new ModuleSerializer(composed).DumpToJson();
File.WriteAllText("UnifiedApp.json", json);
```

## Validation

The composition system validates:

### Type Conflicts
- No duplicate type names across modules
- Unresolved type references are caught

### Dependency Issues
- Missing base classes are warned about
- Missing interfaces are detected
- Circular dependencies are identified

### Example

```csharp
var validation = composer.Validate();

if (validation.HasErrors)
{
    foreach (var error in validation.Errors)
        Console.WriteLine($"ERROR: {error}");
}

if (validation.HasWarnings)
{
    foreach (var warning in validation.Warnings)
        Console.WriteLine($"WARNING: {warning}");
}
```

## Dependency Management

### Resolve Type References

```csharp
var resolver = new DependencyResolver();
resolver.RegisterModule(module1);
resolver.RegisterModule(module2);

var typeRef = new TypeReference("MyClass");
var typeDef = resolver.ResolveType(typeRef);
```

### Get Type Dependencies

```csharp
var dependencies = resolver.GetDependencies(myType);
foreach (var dep in dependencies)
{
    Console.WriteLine($"Depends on: {dep.Name}");
}
```

### Topological Sort

```csharp
var sorted = resolver.TopologicalSort();
// Types are sorted by dependency order
// Base classes come before derived classes
```

### Find Circular Dependencies

```csharp
var cycles = resolver.FindCircularDependencies();
if (cycles.Any())
{
    Console.WriteLine("Circular dependencies detected!");
    foreach (var cycle in cycles)
    {
        Console.WriteLine($"  {string.Join(" -> ", cycle.Select(t => t.Name))}");
    }
}
```

## Advanced Usage

### Namespace Linking

```csharp
var composer = new ModuleComposer();
composer.AddModule(module1);
composer.AddModule(module2);

// Link namespaces for aliasing
composer.LinkNamespace("OldNamespace", "Module2", "NewNamespace");
```

### Dependency Graph

```csharp
var graph = composer.GetDependencyGraph();

foreach (var (typeName, dependencies) in graph)
{
    Console.WriteLine($"{typeName}:");
    foreach (var dep in dependencies)
    {
        Console.WriteLine($"  -> {dep}");
    }
}
```

### Composition Report

```csharp
var report = composer.GenerateReport();
Console.WriteLine(report);

// Output:
// === Module Composition Report ===
// Total modules: 3
// Total types: 15
// Total functions: 42
//
// Module: Core (v1.0.0)
//   Types: 5
//     - Entity (Class)
//     - IEntity (Interface)
//   ...
```

## Metadata Preservation

Composition merges metadata from all modules:

```csharp
var module1 = new Module("Module1");
module1.Metadata["Author"] = "Team A";
module1.Metadata["Version"] = "1.0";

var module2 = new Module("Module2");
module2.Metadata["Author"] = "Team B";

var composed = composer.Compose("Unified");

// Composed module contains:
// - "Module1.Author" = "Team A"
// - "Module1.Version" = "1.0"
// - "Module2.Author" = "Team B"
// - "ComposedFrom" = "Module1, Module2"
// - "CompositionTime" = "2024-11-05T..."
```

## Complete Example

See `ObjectIR.Examples/ModuleCompositionExample.cs` for a complete working example demonstrating:

- Creating modules with class hierarchies
- Resolving dependencies
- Validating composition
- Generating dependency graphs
- Saving composed modules

## Error Handling

### Handling Composition Errors

```csharp
try
{
    var validation = composer.Validate();
    if (validation.HasErrors)
        throw new InvalidOperationException(string.Join("\n", validation.Errors));
    
    var composed = composer.Compose("App");
}
catch (InvalidOperationException ex)
{
    Console.WriteLine($"Composition failed: {ex.Message}");
}
```

### Resolving Common Issues

**Problem**: "Type 'X' is defined in multiple modules"
- **Solution**: Ensure each type has a unique name across all modules

**Problem**: "Base class 'X' not found"
- **Solution**: Add the module containing the base class to the composition

**Problem**: Circular dependencies detected
- **Solution**: Refactor your type hierarchy to remove cycles

## Performance Considerations

For large compositions:

1. **Lazy resolution**: Types are only resolved when needed
2. **Caching**: Type definitions are cached by the resolver
3. **Incremental composition**: Compose only the modules you need

## Best Practices

1. **Validate Early**: Always validate before composing
   ```csharp
   var validation = composer.Validate();
   if (!validation.HasErrors) {
       var composed = composer.Compose("App");
   }
   ```

2. **Review Warnings**: Check composition warnings for potential issues
   ```csharp
   if (validation.HasWarnings) {
       foreach (var warning in validation.Warnings)
           logger.Warn(warning);
   }
   ```

3. **Document Dependencies**: Keep track of module relationships
   ```csharp
   // Module1 depends on Core
   // Module2 depends on Core and Data
   composer.AddModule(coreModule);
   composer.AddModule(dataModule);
   composer.AddModule(module1);
   composer.AddModule(module2);
   ```

4. **Organize Hierarchically**: Group related types in modules
   ```csharp
   // Good: Logical grouping
   var dataModule = new Module("Data");     // Entity, Repository
   var serviceModule = new Module("Service"); // Service, Handler
   
   // Less ideal: Random grouping
   ```

5. **Version Metadata**: Track composition versions
   ```csharp
   var composed = composer.Compose("App", "3.0.0");
   composed.Metadata["SourceModules"] = 
       new[] { "Core/1.0", "Data/2.1", "Business/1.5" };
   ```

## Related Topics

- [Instruction Serialization](INSTRUCTION_SERIALIZATION.md)
- [Module Serialization](../docs/MODULE_DUMPING.md)
- [IR Architecture](ARCHITECTURE.md)
