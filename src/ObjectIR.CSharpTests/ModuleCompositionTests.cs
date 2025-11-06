using Xunit;
using ObjectIR.Core.IR;
using ObjectIR.Core.Composition;

namespace ObjectIR.CSharpTests;

public class ModuleCompositionTests
{
    [Fact]
    public void AddModule_SuccessfullyRegistersModule()
    {
        // Arrange
        var composer = new ModuleComposer();
        var module = new Module("TestModule");

        // Act
        composer.AddModule(module);

        // Assert
        Assert.Single(composer.GetModules());
    }

    [Fact]
    public void AddModule_ThrowsOnDuplicate()
    {
        // Arrange
        var composer = new ModuleComposer();
        var module1 = new Module("DuplicateModule");
        var module2 = new Module("DuplicateModule");

        // Act & Assert
        composer.AddModule(module1);
        Assert.Throws<InvalidOperationException>(() => composer.AddModule(module2));
    }

    [Fact]
    public void Compose_CreatesCompositeModule()
    {
        // Arrange
        var composer = new ModuleComposer();
        var module1 = new Module("Module1");
        module1.DefineClass("ClassA");
        var module2 = new Module("Module2");
        module2.DefineClass("ClassB");

        composer.AddModule(module1);
        composer.AddModule(module2);

        // Act
        var composite = composer.Compose("ComposedModule", "1.0.0");

        // Assert
        Assert.Equal("ComposedModule", composite.Name);
        Assert.Equal(2, composite.Types.Count);
    }

    [Fact]
    public void Validate_AcceptsValidComposition()
    {
        // Arrange
        var composer = new ModuleComposer();
        var module1 = new Module("Module1");
        module1.DefineClass("Animal");
        
        var module2 = new Module("Module2");
        var dogClass = module2.DefineClass("Dog");
        dogClass.BaseType = TypeReference.FromName("Animal");

        composer.AddModule(module1);
        composer.AddModule(module2);

        // Act
        var validation = composer.Validate();

        // Assert
        Assert.False(validation.HasErrors);
    }

    [Fact]
    public void Validate_WarnsOnUnresolvedDependency()
    {
        // Arrange
        var composer = new ModuleComposer();
        var module = new Module("TestModule");
        var classA = module.DefineClass("ClassA");
        classA.BaseType = TypeReference.FromName("NonExistentClass");

        composer.AddModule(module);

        // Act
        var validation = composer.Validate();

        // Assert
        Assert.True(validation.HasWarnings);
    }

    [Fact]
    public void Compose_PreserversMetadata()
    {
        // Arrange
        var composer = new ModuleComposer();
        var module = new Module("SourceModule");
        module.Metadata["Author"] = "TestAuthor";
        module.Metadata["Version"] = "1.0.0";

        composer.AddModule(module);

        // Act
        var composite = composer.Compose("ComposedModule");

        // Assert
        Assert.True(composite.Metadata.ContainsKey("SourceModule.Author"));
        Assert.Equal("TestAuthor", composite.Metadata["SourceModule.Author"]);
    }

    [Fact]
    public void Compose_SetsCompositionMetadata()
    {
        // Arrange
        var composer = new ModuleComposer();
        var module1 = new Module("Module1");
        var module2 = new Module("Module2");

        composer.AddModule(module1);
        composer.AddModule(module2);

        // Act
        var composite = composer.Compose("ComposedModule");

        // Assert
        Assert.True(composite.Metadata.ContainsKey("ComposedFrom"));
        Assert.Contains("Module1", (string)composite.Metadata["ComposedFrom"]);
        Assert.Contains("Module2", (string)composite.Metadata["ComposedFrom"]);
        Assert.True(composite.Metadata.ContainsKey("CompositionTime"));
    }

    [Fact]
    public void GenerateReport_IncludesAllModules()
    {
        // Arrange
        var composer = new ModuleComposer();
        var module1 = new Module("Module1");
        module1.DefineClass("ClassA");
        var module2 = new Module("Module2");
        module2.DefineClass("ClassB");

        composer.AddModule(module1);
        composer.AddModule(module2);

        // Act
        var report = composer.GenerateReport();

        // Assert
        Assert.Contains("Module1", report);
        Assert.Contains("Module2", report);
        Assert.Contains("ClassA", report);
        Assert.Contains("ClassB", report);
    }

    [Fact]
    public void GetDependencyGraph_BuildsCorrectly()
    {
        // Arrange
        var composer = new ModuleComposer();
        var module = new Module("TestModule");
        var baseClass = module.DefineClass("Animal");
        var derivedClass = module.DefineClass("Dog");
        derivedClass.BaseType = TypeReference.FromName("Animal");

        composer.AddModule(module);

        // Act
        var graph = composer.GetDependencyGraph();

        // Assert
        Assert.True(graph.ContainsKey("Dog"));
        Assert.Contains("Animal", graph["Dog"]);
    }
}
