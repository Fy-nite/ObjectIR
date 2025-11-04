using Xunit;
using ObjectIR.Core.IR;
using ObjectIR.Core.Builder;
using ObjectIR.Core.Serialization;
using System.IO;

namespace ObjectIR.Tests;

/// <summary>
/// Integration tests combining multiple components
/// </summary>
public class ModuleLoaderIntegrationTests
{
    private readonly string _testDirectory = Path.Combine(Path.GetTempPath(), "ObjectIR_Integration_Tests");

    public ModuleLoaderIntegrationTests()
    {
        Directory.CreateDirectory(_testDirectory);
    }

    private void Cleanup()
    {
        if (Directory.Exists(_testDirectory))
        {
            Directory.Delete(_testDirectory, true);
        }
    }

    [Fact]
    public void CompleteWorkflow_BuildSerializeLoadModule()
    {
        try
        {
            // Arrange
            var builder = new IRBuilder("WorkflowModule");
            builder.Class("WorkflowClass")
                .Field("id", TypeReference.Int32).EndField()
                .Field("items", TypeReference.List(TypeReference.String)).EndField();
            var originalModule = builder.Build();

            // Act - Serialize to JSON
            var loader = new ModuleLoader();
            var jsonPath = Path.Combine(_testDirectory, "workflow.json");
            loader.SaveToJsonFile(originalModule, jsonPath);

            // Act - Load from JSON
            var loadedModule = loader.LoadFromJsonFile(jsonPath);

            // Assert
            Assert.Equal(originalModule.Name, loadedModule.Name);
            Assert.Equal(originalModule.Types.Count, loadedModule.Types.Count);
        }
        finally
        {
            Cleanup();
        }
    }

    [Fact]
    public void TextFormatRoundTrip_ParseAndSerialize()
    {
        // Arrange
        var textInput = @"
module RoundTripModule

class RoundTripClass {
    field id: int32
    field name: string
}
";
        var loader = new ModuleLoader();

        // Act
        var module = loader.LoadFromText(textInput);
        var json = module.DumpJson();
        var text = module.DumpText();

        // Assert
        Assert.NotNull(module);
        Assert.NotEmpty(json);
        Assert.NotEmpty(text);
        Assert.Contains("RoundTripModule", json);
        Assert.Contains("RoundTripModule", text);
    }

    [Fact]
    public void ComplexModule_WithMultipleTypes()
    {
        // Arrange
        var builder = new IRBuilder("ComplexModule");
        
        // Add interface
        builder.Interface("IEntity");
        
        // Add classes
        builder.Class("User")
            .Field("id", TypeReference.Int32).EndField()
            .Field("name", TypeReference.String).EndField()
            .Field("email", TypeReference.String).EndField();
        
        builder.Class("Product")
            .Field("id", TypeReference.Int32).EndField()
            .Field("title", TypeReference.String).EndField()
            .Field("price", TypeReference.Float64).EndField()
            .Field("tags", TypeReference.Set(TypeReference.String)).EndField();
        
        builder.Struct("Point")
            .Field("x", TypeReference.Int32)
            .Field("y", TypeReference.Int32)
            .EndStruct();
        
        var module = builder.Build();

        // Assert
        Assert.Equal(4, module.Types.Count);
        Assert.Single(module.Types.OfType<InterfaceDefinition>());
        Assert.Equal(2, module.Types.OfType<ClassDefinition>().Count());
        Assert.Single(module.Types.OfType<StructDefinition>());
    }

    [Fact]
    public void CachePerformance_MultipleLoadsUseCache()
    {
        // Arrange
        var loader = new ModuleLoader();
        var textFormat = @"
module CachePerformanceModule

class CacheClass {
    field id: int32
}
";

        // Act
        var module1 = loader.LoadFromText(textFormat);
        var cached = loader.GetCachedModule("CachePerformanceModule");
        var allCached = loader.GetAllCachedModules();

        // Assert
        Assert.Same(module1, cached);
        Assert.Single(allCached);
        Assert.True(allCached.ContainsKey("CachePerformanceModule"));
    }

        [Fact]
    public void BatchOperations_LoadAndSaveMultipleModules()
    {
        try
        {
            // Arrange
            var loader = new ModuleLoader();
            var builder1 = new IRBuilder("ModuleA");
            builder1.Class("ClassA")
                .Field("field1", TypeReference.String).EndField()
                .EndClass();

            var builder2 = new IRBuilder("ModuleB");
            builder2.Class("ClassB")
                .Field("field2", TypeReference.Int32).EndField()
                .EndClass();

            var modules = new[] { builder1.Build(), builder2.Build() };

            // Act - Save using JSON format (text format is display-only)
            var jsonFiles = new List<string>();
            foreach (var module in modules)
            {
                var jsonPath = Path.Combine(_testDirectory, $"{module.Name}.json");
                loader.SaveToJsonFile(module, jsonPath);
                jsonFiles.Add(jsonPath);
            }

            // Create fresh loader and load them back
            var loader2 = new ModuleLoader();
            var loadedModules = new List<Module>();
            foreach (var jsonPath in jsonFiles)
            {
                var loaded = loader2.LoadFromJsonFile(jsonPath);
                loadedModules.Add(loaded);
            }

            // Assert
            Assert.Equal(2, loadedModules.Count);
            Assert.Equal("ModuleA", loadedModules[0].Name);
            Assert.Equal("ModuleB", loadedModules[1].Name);
            Assert.Single(loadedModules[0].Types);
            Assert.Single(loadedModules[1].Types);
        }
        finally
        {
            Cleanup();
        }
    }

    [Fact]
    public void ErrorHandling_InvalidJsonContent()
    {
        try
        {
            // Arrange
            var loader = new ModuleLoader();
            var invalidJsonPath = Path.Combine(_testDirectory, "invalid.json");
            File.WriteAllText(invalidJsonPath, "{ invalid json content ]");

            // Act & Assert - Should throw JsonException
            var ex = Assert.Throws<System.Text.Json.JsonException>(() => loader.LoadFromJsonFile(invalidJsonPath));
            Assert.NotNull(ex);
        }
        finally
        {
            Cleanup();
        }
    }

    [Fact]
    public void ModuleData_StructurePreserved()
    {
        // Arrange
        var builder = new IRBuilder("DataModule");
        builder.Class("DataClass")
            .Field("id", TypeReference.Int32).EndField()
            .Field("items", TypeReference.List(TypeReference.String)).EndField()
            .Method("GetId", TypeReference.Int32)
                .Parameter("idx", TypeReference.Int32)
                .EndMethod();
        var module = builder.Build();

        // Act
        var data = module.Dump();

        // Assert
        Assert.NotNull(data);
        Assert.Equal("DataModule", data.Name);
        Assert.Single(data.Types);
        var typeData = data.Types[0];
        Assert.Equal("DataClass", typeData.Name);
        Assert.Equal(2, typeData.Fields.Length);
        Assert.Single(typeData.Methods);
    }

    [Fact]
    public void TypeReferences_ComplexGenericTypes()
    {
        // Arrange
        var loader = new ModuleLoader();
        var textFormat = @"
module ComplexGenericModule

class GenericContainer {
    field simple: int32
    field list: List<int32>
    field dict: Dict<string, int32>
    field set: Set<string>
    field optional: Optional<int32>
}
";

        // Act
        var module = loader.LoadFromText(textFormat);
        var classDef = module.Types[0] as ClassDefinition;

        // Assert
        Assert.NotNull(classDef);
        Assert.Equal(5, classDef.Fields.Count);
        
        // Verify each field type
        Assert.Empty(classDef.Fields[0].Type.GenericArguments); // primitive
        Assert.NotEmpty(classDef.Fields[1].Type.GenericArguments);  // List
        Assert.NotEmpty(classDef.Fields[2].Type.GenericArguments);  // Dict
        Assert.NotEmpty(classDef.Fields[3].Type.GenericArguments);  // Set
        Assert.NotEmpty(classDef.Fields[4].Type.GenericArguments);  // Optional
    }
}
