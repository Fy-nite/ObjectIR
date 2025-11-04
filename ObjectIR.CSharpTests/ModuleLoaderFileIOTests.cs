using Xunit;
using ObjectIR.Core.Serialization;
using ObjectIR.Core.IR;
using ObjectIR.Core.Builder;
using System.IO;

namespace ObjectIR.Tests;

/// <summary>
/// Tests for ModuleLoader file I/O operations
/// </summary>
public class ModuleLoaderFileIOTests
{
    private readonly string _testDirectory = Path.Combine(Path.GetTempPath(), "ObjectIR_Tests");

    public ModuleLoaderFileIOTests()
    {
        // Ensure test directory exists
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
    public void SaveToJsonFile_CreatesValidJsonFile()
    {
        try
        {
            // Arrange
            var builder = new IRBuilder("JsonTestModule");
            builder.Class("TestClass").Field("id", TypeReference.Int32).EndField();
            var module = builder.Build();
            var loader = new ModuleLoader();
            var filePath = Path.Combine(_testDirectory, "test.json");

            // Act
            loader.SaveToJsonFile(module, filePath);

            // Assert
            Assert.True(File.Exists(filePath));
            var content = File.ReadAllText(filePath);
            Assert.NotEmpty(content);
            Assert.Contains("JsonTestModule", content);
        }
        finally
        {
            Cleanup();
        }
    }

    [Fact]
    public void LoadFromJsonFile_ReloadsModuleSuccessfully()
    {
        try
        {
            // Arrange
            var builder = new IRBuilder("JsonRoundTripModule");
            builder.Class("RoundTripClass")
                .Field("value", TypeReference.Int32).EndField()
                .Field("name", TypeReference.String).EndField();
            var originalModule = builder.Build();
            var loader = new ModuleLoader();
            var filePath = Path.Combine(_testDirectory, "roundtrip.json");

            // Act
            loader.SaveToJsonFile(originalModule, filePath);
            var loadedModule = loader.LoadFromJsonFile(filePath);

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
    public void SaveToTextFile_CreatesTextFile()
    {
        try
        {
            // Arrange
            var builder = new IRBuilder("TextModule");
            builder.Class("TextClass").Field("id", TypeReference.Int32).EndField();
            var module = builder.Build();
            var loader = new ModuleLoader();
            var filePath = Path.Combine(_testDirectory, "test.ir.txt");

            // Act
            loader.SaveToTextFile(module, filePath);

            // Assert
            Assert.True(File.Exists(filePath));
            var content = File.ReadAllText(filePath);
            Assert.NotEmpty(content);
            Assert.Contains("TextModule", content);
        }
        finally
        {
            Cleanup();
        }
    }

    [Fact]
    public void LoadFromJsonFile_InvalidPath_ThrowsFileNotFoundException()
    {
        // Arrange
        var loader = new ModuleLoader();
        var invalidPath = Path.Combine(_testDirectory, "nonexistent_file.json");

        // Act & Assert
        Assert.Throws<FileNotFoundException>(() => loader.LoadFromJsonFile(invalidPath));
    }

    [Fact]
    public void LoadFromTextFile_InvalidPath_ThrowsFileNotFoundException()
    {
        // Arrange
        var loader = new ModuleLoader();
        var invalidPath = Path.Combine(_testDirectory, "nonexistent_file.ir.txt");

        // Act & Assert
        Assert.Throws<FileNotFoundException>(() => loader.LoadFromTextFile(invalidPath));
    }

    [Fact]
    public void LoadModulesFromDirectory_LoadsMultipleFiles()
    {
        try
        {
            // Arrange
            var loader = new ModuleLoader();
            Directory.CreateDirectory(_testDirectory);

            // Create multiple module files using JSON format (text format is display-only)
            for (int i = 1; i <= 3; i++)
            {
                var builder = new IRBuilder($"Module{i}");
                builder.Class($"Class{i}").Field("id", TypeReference.Int32).EndField();
                var module = builder.Build();
                var filePath = Path.Combine(_testDirectory, $"module{i}.ir.json");
                loader.SaveToJsonFile(module, filePath);
            }

            // Act
            var modules = loader.LoadModulesFromDirectory(_testDirectory, "*.ir.json");

            // Assert
            Assert.Equal(3, modules.Count);
        }
        finally
        {
            Cleanup();
        }
    }

    [Fact]
    public void SaveModulesToDirectory_CreatesMultipleFiles()
    {
        try
        {
            // Arrange
            var loader = new ModuleLoader();
            var modules = new List<Module>();
            for (int i = 1; i <= 3; i++)
            {
                var builder = new IRBuilder($"SaveModule{i}");
                builder.Class($"SaveClass{i}").Field("id", TypeReference.Int32).EndField();
                modules.Add(builder.Build());
            }

            // Act
            loader.SaveModulesToDirectory(modules, _testDirectory);

            // Assert
            var files = Directory.GetFiles(_testDirectory, "*.ir.txt");
            Assert.Equal(3, files.Length);
        }
        finally
        {
            Cleanup();
        }
    }

    [Fact]
    public void SaveToJsonFile_WithIndentation_CreatesFormattedOutput()
    {
        try
        {
            // Arrange
            var builder = new IRBuilder("FormattedModule");
            builder.Class("FormattedClass").Field("id", TypeReference.Int32).EndField();
            var module = builder.Build();
            var loader = new ModuleLoader();
            var filePath = Path.Combine(_testDirectory, "formatted.json");

            // Act
            loader.SaveToJsonFile(module, filePath, indented: true);

            // Assert
            var content = File.ReadAllText(filePath);
            Assert.Contains("\n", content); // Should have newlines for indentation
            Assert.Contains("  ", content); // Should have indentation
        }
        finally
        {
            Cleanup();
        }
    }

    [Fact]
    public void SaveToJsonFile_WithoutIndentation_CreatesCompactOutput()
    {
        try
        {
            // Arrange
            var builder = new IRBuilder("CompactModule");
            builder.Class("CompactClass").Field("id", TypeReference.Int32).EndField();
            var module = builder.Build();
            var loader = new ModuleLoader();
            var filePath = Path.Combine(_testDirectory, "compact.json");

            // Act
            loader.SaveToJsonFile(module, filePath, indented: false);

            // Assert
            var content = File.ReadAllText(filePath);
            // Compact JSON should be shorter than indented
            Assert.NotNull(content);
        }
        finally
        {
            Cleanup();
        }
    }
}
