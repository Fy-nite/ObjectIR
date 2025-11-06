using Xunit;
using ObjectIR.Core.Serialization;
using ObjectIR.Core.IR;
using ObjectIR.Core.Builder;

namespace ObjectIR.Tests;

/// <summary>
/// Tests for ModuleLoader text format parsing
/// </summary>
public class ModuleLoaderTextParsingTests
{
    [Fact]
    public void LoadFromText_SimpleModule_CreatesModuleSuccessfully()
    {
        // Arrange
        var textFormat = @"
module TestModule

class SimpleClass {
    field value: int32
}
";
        var loader = new ModuleLoader();

        // Act
        var module = loader.LoadFromText(textFormat);

        // Assert
        Assert.NotNull(module);
        Assert.Equal("TestModule", module.Name);
        Assert.Single(module.Types);
        Assert.Equal("SimpleClass", module.Types[0].Name);
    }

    [Fact]
    public void LoadFromText_MultipleClasses_ParsesAllTypes()
    {
        // Arrange
        var textFormat = @"
module MultiClassModule

class Class1 {
    field id: int32
}

class Class2 {
    field name: string
}

class Class3 {
    field active: bool
}
";
        var loader = new ModuleLoader();

        // Act
        var module = loader.LoadFromText(textFormat);

        // Assert
        Assert.Equal(3, module.Types.Count);
        Assert.Collection(module.Types,
            t => Assert.Equal("Class1", t.Name),
            t => Assert.Equal("Class2", t.Name),
            t => Assert.Equal("Class3", t.Name));
    }

    [Fact]
    public void LoadFromText_ClassWithMultipleFields_ParsesAllFields()
    {
        // Arrange
        var textFormat = @"
module FieldTestModule

class FieldClass {
    field field1: int32
    field field2: string
    field field3: bool
    field field4: float64
}
";
        var loader = new ModuleLoader();

        // Act
        var module = loader.LoadFromText(textFormat);

        // Assert
        Assert.Single(module.Types);
        var classDef = module.Types[0] as ClassDefinition;
        Assert.NotNull(classDef);
        Assert.Equal(4, classDef.Fields.Count);
    }

    [Fact]
    public void LoadFromText_GenericTypes_ParsesGenericFields()
    {
        // Arrange
        var textFormat = @"
module GenericModule

class GenericClass {
    field items: List<int32>
    field mapping: Dict<string, int32>
    field unique: Set<string>
}
";
        var loader = new ModuleLoader();

        // Act
        var module = loader.LoadFromText(textFormat);

        // Assert
        Assert.Single(module.Types);
        var classDef = module.Types[0] as ClassDefinition;
        Assert.NotNull(classDef);
        Assert.Equal(3, classDef.Fields.Count);
        
        // Verify generic types
        Assert.True(classDef.Fields[0].Type.GenericArguments.Count > 0);
        Assert.True(classDef.Fields[1].Type.GenericArguments.Count > 0);
    }

    [Fact]
    public void LoadFromText_InvalidSyntax_ThrowsFormatException()
    {
        // Arrange
        var invalidText = "invalid syntax without module declaration";
        var loader = new ModuleLoader();

        // Act & Assert
        Assert.Throws<FormatException>(() => loader.LoadFromText(invalidText));
    }

    [Fact]
    public void LoadFromText_EmptyText_ThrowsArgumentException()
    {
        // Arrange
        var emptyText = "";
        var loader = new ModuleLoader();

        // Act & Assert
        Assert.Throws<ArgumentException>(() => loader.LoadFromText(emptyText));
    }

    [Fact]
    public void LoadFromText_WithCommentsAndWhitespace_ParsesSuccessfully()
    {
        // Arrange
        var textFormat = @"
// This is a comment
module CommentModule

// Another comment
class CommentClass {
    // Field comment
    field id: int32
}
";
        var loader = new ModuleLoader();

        // Act
        var module = loader.LoadFromText(textFormat);

        // Assert
        Assert.NotNull(module);
        Assert.Equal("CommentModule", module.Name);
    }

    [Fact]
    public void LoadFromText_AllPrimitiveTypes_ParsesSuccessfully()
    {
        // Arrange
        var textFormat = @"
module PrimitiveModule

class PrimitiveClass {
    field f1: bool
    field f2: int8
    field f3: int16
    field f4: int32
    field f5: int64
    field f6: uint8
    field f7: uint16
    field f8: uint32
    field f9: uint64
    field f10: float32
    field f11: float64
    field f12: char
    field f13: string
}
";
        var loader = new ModuleLoader();

        // Act
        var module = loader.LoadFromText(textFormat);

        // Assert
        var classDef = module.Types[0] as ClassDefinition;
        Assert.NotNull(classDef);
        Assert.Equal(13, classDef.Fields.Count);
    }

    [Fact]
    public void LoadFromText_MethodWithParameters_ParsesSignature()
    {
        // Arrange
        var textFormat = @"
module MethodModule

class MethodClass {
    method Add(a: int32, b: int32) -> int32 {
        ldarg a
        ldarg b
        add
        ret
    }
}
";
        var loader = new ModuleLoader();

        // Act
        var module = loader.LoadFromText(textFormat);

        // Assert
        var classDef = module.Types[0] as ClassDefinition;
        Assert.NotNull(classDef);
        Assert.Single(classDef.Methods);
        Assert.Equal("Add", classDef.Methods[0].Name);
        Assert.Equal(2, classDef.Methods[0].Parameters.Count);
    }

    [Fact]
    public void LoadFromText_MethodWithReturnType_ParsesReturnType()
    {
        // Arrange
        var textFormat = @"
module ReturnTypeModule

class ReturnClass {
    method GetValue() -> int32 {
        ldc.i4 42
        ret
    }
    
    method GetString() -> string {
        ldstr ""hello""
        ret
    }
}
";
        var loader = new ModuleLoader();

        // Act
        var module = loader.LoadFromText(textFormat);

        // Assert
        var classDef = module.Types[0] as ClassDefinition;
        Assert.NotNull(classDef);
        Assert.Equal(2, classDef.Methods.Count);
        Assert.NotEqual(TypeReference.Void, classDef.Methods[0].ReturnType);
        Assert.NotEqual(TypeReference.Void, classDef.Methods[1].ReturnType);
    }
}
