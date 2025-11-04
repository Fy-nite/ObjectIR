using Xunit;
using ObjectIR.Core.Builder;
using ObjectIR.Core.IR;

namespace ObjectIR.Tests;

/// <summary>
/// Tests for IR Builder API
/// </summary>
public class IRBuilderTests
{
    [Fact]
    public void IRBuilder_CreateModule_HasCorrectName()
    {
        // Arrange & Act
        var builder = new IRBuilder("TestModule");
        var module = builder.Build();

        // Assert
        Assert.NotNull(module);
        Assert.Equal("TestModule", module.Name);
    }

    [Fact]
    public void IRBuilder_Class_CreatesClassDefinition()
    {
        // Arrange & Act
        var builder = new IRBuilder("ClassModule");
        builder.Class("TestClass");
        var module = builder.Build();

        // Assert
        Assert.Single(module.Types);
        Assert.Equal("TestClass", module.Types[0].Name);
    }

    [Fact]
    public void IRBuilder_MultipleClasses_CreatesAllClasses()
    {
        // Arrange & Act
        var builder = new IRBuilder("MultiClassModule");
        builder.Class("Class1");
        builder.Class("Class2");
        builder.Class("Class3");
        var module = builder.Build();

        // Assert
        Assert.Equal(3, module.Types.Count);
    }

    [Fact]
    public void ClassBuilder_Field_AddsFieldToClass()
    {
        // Arrange
        var builder = new IRBuilder("FieldModule");
        builder.Class("FieldClass")
            .Field("testField", TypeReference.Int32).EndField();
        var module = builder.Build();

        // Assert
        var classDef = module.Types[0] as ClassDefinition;
        Assert.NotNull(classDef);
        Assert.Single(classDef.Fields);
        Assert.Equal("testField", classDef.Fields[0].Name);
    }

    [Fact]
    public void ClassBuilder_MultipleFields_AddsAllFields()
    {
        // Arrange
        var builder = new IRBuilder("MultiFieldModule");
        builder.Class("MultiFieldClass")
            .Field("field1", TypeReference.Int32).EndField()
            .Field("field2", TypeReference.String).EndField()
            .Field("field3", TypeReference.Bool).EndField();
        var module = builder.Build();

        // Assert
        var classDef = module.Types[0] as ClassDefinition;
        Assert.NotNull(classDef);
        Assert.Equal(3, classDef.Fields.Count);
    }

    [Fact]
    public void FieldBuilder_WithModifiers_SetsFieldProperties()
    {
        // Arrange
        var builder = new IRBuilder("ModifierModule");
        builder.Class("ModifierClass")
            .Field("staticField", TypeReference.Int32)
                .Static()
                .ReadOnly()
                .EndField();
        var module = builder.Build();

        // Assert
        var classDef = module.Types[0] as ClassDefinition;
        Assert.NotNull(classDef);
        var field = classDef.Fields[0];
        Assert.True(field.IsStatic);
        Assert.True(field.IsReadOnly);
    }

    [Fact]
    public void ClassBuilder_Method_AddsMethodToClass()
    {
        // Arrange
        var builder = new IRBuilder("MethodModule");
        builder.Class("MethodClass")
            .Method("TestMethod", TypeReference.Void).EndMethod();
        var module = builder.Build();

        // Assert
        var classDef = module.Types[0] as ClassDefinition;
        Assert.NotNull(classDef);
        Assert.Single(classDef.Methods);
        Assert.Equal("TestMethod", classDef.Methods[0].Name);
    }

    [Fact]
    public void MethodBuilder_WithParameters_AddsParameters()
    {
        // Arrange
        var builder = new IRBuilder("ParameterModule");
        builder.Class("ParameterClass")
            .Method("Add", TypeReference.Int32)
                .Parameter("a", TypeReference.Int32)
                .Parameter("b", TypeReference.Int32)
                .EndMethod();
        var module = builder.Build();

        // Assert
        var classDef = module.Types[0] as ClassDefinition;
        Assert.NotNull(classDef);
        var method = classDef.Methods[0];
        Assert.Equal(2, method.Parameters.Count);
    }

    [Fact]
    public void MethodBuilder_WithLocal_AddsLocalVariable()
    {
        // Arrange
        var builder = new IRBuilder("LocalModule");
        builder.Class("LocalClass")
            .Method("LocalMethod", TypeReference.Int32)
                .Local("result", TypeReference.Int32)
                .EndMethod();
        var module = builder.Build();

        // Assert
        var classDef = module.Types[0] as ClassDefinition;
        Assert.NotNull(classDef);
        var method = classDef.Methods[0];
        Assert.Single(method.Locals);
    }

    [Fact]
    public void ClassBuilder_WithNamespace_SetsNamespace()
    {
        // Arrange
        var builder = new IRBuilder("NamespaceModule");
        builder.Class("NamespacedClass")
            .Namespace("TestNamespace");
        var module = builder.Build();

        // Assert
        var classDef = module.Types[0];
        Assert.Equal("TestNamespace", classDef.Namespace);
    }

    [Fact]
    public void IRBuilder_Interface_CreatesInterfaceDefinition()
    {
        // Arrange & Act
        var builder = new IRBuilder("InterfaceModule");
        builder.Interface("ITestInterface");
        var module = builder.Build();

        // Assert
        Assert.Single(module.Types);
        Assert.IsType<InterfaceDefinition>(module.Types[0]);
        Assert.Equal("ITestInterface", module.Types[0].Name);
    }

    [Fact]
    public void IRBuilder_Struct_CreatesStructDefinition()
    {
        // Arrange & Act
        var builder = new IRBuilder("StructModule");
        builder.Struct("TestStruct");
        var module = builder.Build();

        // Assert
        Assert.Single(module.Types);
        Assert.IsType<StructDefinition>(module.Types[0]);
        Assert.Equal("TestStruct", module.Types[0].Name);
    }

    [Fact]
    public void TypeReference_PrimitiveTypes_AreCorrect()
    {
        // Assert
        Assert.NotNull(TypeReference.Int32);
        Assert.NotNull(TypeReference.String);
        Assert.NotNull(TypeReference.Bool);
        Assert.NotNull(TypeReference.Void);
    }

    [Fact]
    public void TypeReference_List_CreatesGenericType()
    {
        // Act
        var listType = TypeReference.List(TypeReference.Int32);

        // Assert
        Assert.NotNull(listType);
        Assert.Single(listType.GenericArguments);
    }

    [Fact]
    public void TypeReference_Dict_CreatesGenericType()
    {
        // Act
        var dictType = TypeReference.Dict(TypeReference.String, TypeReference.Int32);

        // Assert
        Assert.NotNull(dictType);
        Assert.Equal(2, dictType.GenericArguments.Count);
    }
}
