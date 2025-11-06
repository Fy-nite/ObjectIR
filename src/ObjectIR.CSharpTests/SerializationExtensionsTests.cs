using Xunit;
using ObjectIR.Core.Serialization;
using ObjectIR.Core.IR;
using ObjectIR.Core.Builder;

namespace ObjectIR.Tests;

/// <summary>
/// Tests for SerializationExtensions functionality
/// </summary>
public class SerializationExtensionsTests
{
    [Fact]
    public void Module_DumpJson_ReturnsValidJson()
    {
        // Arrange
        var builder = new IRBuilder("SerializeModule");
        builder.Class("SerializeClass").Field("id", TypeReference.Int32).EndField();
        var module = builder.Build();

        // Act
        var json = module.DumpJson();

        // Assert
        Assert.NotNull(json);
        Assert.NotEmpty(json);
        Assert.Contains("SerializeModule", json);
    }

    [Fact]
    public void Module_DumpText_ReturnsReadableText()
    {
        // Arrange
        var builder = new IRBuilder("TextDumpModule");
        builder.Class("TextDumpClass").Field("id", TypeReference.Int32).EndField();
        var module = builder.Build();

        // Act
        var text = module.DumpText();

        // Assert
        Assert.NotNull(text);
        Assert.Contains("TextDumpModule", text);
        Assert.Contains("TextDumpClass", text);
    }

    [Fact]
    public void Module_Dump_ReturnsModuleData()
    {
        // Arrange
        var builder = new IRBuilder("DumpModule");
        builder.Class("DumpClass").Field("id", TypeReference.Int32).EndField();
        var module = builder.Build();

        // Act
        var data = module.Dump();

        // Assert
        Assert.NotNull(data);
        Assert.Equal("DumpModule", data.Name);
    }

    [Fact]
    public void Module_DumpTypes_ReturnsTypeArray()
    {
        // Arrange
        var builder = new IRBuilder("DumpTypesModule");
        builder.Class("Class1").Field("id", TypeReference.Int32).EndField();
        builder.Class("Class2").Field("name", TypeReference.String).EndField();
        var module = builder.Build();

        // Act
        var types = module.DumpTypes();

        // Assert
        Assert.NotNull(types);
        Assert.Equal(2, types.Length);
    }

    [Fact]
    public void Module_DumpFunctions_ReturnsFunctionArray()
    {
        // Arrange
        var builder = new IRBuilder("DumpFunctionsModule");
        var module = builder.Build();

        // Act
        var functions = module.DumpFunctions();

        // Assert
        Assert.NotNull(functions);
        // Empty module has no functions
        Assert.Empty(functions);
    }

    [Fact]
    public void Module_DumpJson_WithIndentation_IsReadable()
    {
        // Arrange
        var builder = new IRBuilder("IndentedModule");
        builder.Class("IndentedClass").Field("id", TypeReference.Int32).EndField();
        var module = builder.Build();

        // Act
        var json = module.DumpJson(indented: true);

        // Assert
        Assert.Contains("\n", json);
        Assert.Contains("  ", json);
    }

    [Fact]
    public void Module_DumpJson_WithoutIndentation_IsCompact()
    {
        // Arrange
        var builder = new IRBuilder("CompactModule");
        builder.Class("CompactClass").Field("id", TypeReference.Int32).EndField();
        var module = builder.Build();

        // Act
        var jsonCompact = module.DumpJson(indented: false);
        var jsonIndented = module.DumpJson(indented: true);

        // Assert
        Assert.NotNull(jsonCompact);
        Assert.NotNull(jsonIndented);
        Assert.True(jsonIndented.Length > jsonCompact.Length);
    }
}
