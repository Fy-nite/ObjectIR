using Xunit;
using ObjectIR.Core.IR;
using ObjectIR.Core.Serialization;
using System.Text.Json;

namespace ObjectIR.CSharpTests;

public class InstructionSerializationTests
{
    [Fact]
    public void SerializeLoadArgInstruction_ProducesValidJson()
    {
        // Arrange
        var instr = new LoadArgInstruction("x");

        // Act
        var doc = InstructionSerializer.SerializeInstruction(instr);
        var opCode = doc.RootElement.GetProperty("opCode").GetString();

        // Assert
        Assert.Equal("ldarg", opCode);
        Assert.Equal("x", doc.RootElement.GetProperty("operand").GetProperty("argumentName").GetString());
    }

    [Fact]
    public void SerializeLoadLocalInstruction_ProducesValidJson()
    {
        // Arrange
        var instr = new LoadLocalInstruction("result");

        // Act
        var doc = InstructionSerializer.SerializeInstruction(instr);
        var opCode = doc.RootElement.GetProperty("opCode").GetString();

        // Assert
        Assert.Equal("ldloc", opCode);
        Assert.Equal("result", doc.RootElement.GetProperty("operand").GetProperty("localName").GetString());
    }

    [Fact]
    public void SerializeLoadConstantInstruction_ProducesValidJson()
    {
        // Arrange
        var instr = new LoadConstantInstruction(42, TypeReference.Int32);

        // Act
        var doc = InstructionSerializer.SerializeInstruction(instr);
        var opCode = doc.RootElement.GetProperty("opCode").GetString();

        // Assert
        Assert.Equal("ldc", opCode);
        Assert.Equal("42", doc.RootElement.GetProperty("operand").GetProperty("value").GetString());
    }

    [Fact]
    public void SerializeLoadNullInstruction_ProducesValidJson()
    {
        // Arrange
        var instr = new LoadNullInstruction();

        // Act
        var doc = InstructionSerializer.SerializeInstruction(instr);
        var opCode = doc.RootElement.GetProperty("opCode").GetString();

        // Assert
        Assert.Equal("ldnull", opCode);
    }

    [Fact]
    public void SerializeArithmeticInstruction_ProducesValidJson()
    {
        // Arrange
        var instr = new ArithmeticInstruction(ArithmeticOp.Add);

        // Act
        var doc = InstructionSerializer.SerializeInstruction(instr);
        var opCode = doc.RootElement.GetProperty("opCode").GetString();

        // Assert
        Assert.Equal("add", opCode);
    }

    [Fact]
    public void SerializeComparisonInstruction_ProducesValidJson()
    {
        // Arrange
        var instr = new ComparisonInstruction(ComparisonOp.Equal);

        // Act
        var doc = InstructionSerializer.SerializeInstruction(instr);
        var opCode = doc.RootElement.GetProperty("opCode").GetString();

        // Assert
        Assert.Equal("ceq", opCode);
    }

    [Fact]
    public void SerializeReturnInstruction_ProducesValidJson()
    {
        // Arrange
        var instr = new ReturnInstruction(null);

        // Act
        var doc = InstructionSerializer.SerializeInstruction(instr);
        var opCode = doc.RootElement.GetProperty("opCode").GetString();

        // Assert
        Assert.Equal("ret", opCode);
    }

    [Fact]
    public void DeserializeLoadArgInstruction_RecreatesInstruction()
    {
        // Arrange
        var original = new LoadArgInstruction("myArg");
        var doc = InstructionSerializer.SerializeInstruction(original);

        // Act
        var deserialized = (LoadArgInstruction)InstructionSerializer.DeserializeInstruction(doc.RootElement);

        // Assert
        Assert.Equal("myArg", deserialized.ArgumentName);
    }

    [Fact]
    public void DeserializeLoadLocalInstruction_RecreatesInstruction()
    {
        // Arrange
        var original = new LoadLocalInstruction("myLocal");
        var doc = InstructionSerializer.SerializeInstruction(original);

        // Act
        var deserialized = (LoadLocalInstruction)InstructionSerializer.DeserializeInstruction(doc.RootElement);

        // Assert
        Assert.Equal("myLocal", deserialized.LocalName);
    }

    [Fact]
    public void DeserializeLoadConstantInstruction_RecreatesInstruction()
    {
        // Arrange
        var original = new LoadConstantInstruction(42, TypeReference.Int32);
        var doc = InstructionSerializer.SerializeInstruction(original);

        // Act
        var deserialized = (LoadConstantInstruction)InstructionSerializer.DeserializeInstruction(doc.RootElement);

        // Assert
        Assert.Equal(42, deserialized.Value);
    }

    [Fact]
    public void RoundTrip_SimpleInstructions_PreservesData()
    {
        // Arrange
        var instructions = new InstructionList();
        instructions.Emit(new LoadArgInstruction("a"));
        instructions.Emit(new LoadArgInstruction("b"));
        instructions.Emit(new ArithmeticInstruction(ArithmeticOp.Add));
        instructions.Emit(new StoreLocalInstruction("result"));

        // Act
        var serialized = InstructionSerializer.SerializeInstructions(instructions);
        var deserialized = InstructionSerializer.DeserializeInstructions(serialized);

        // Assert
        Assert.Equal(4, deserialized.Count);
        Assert.IsType<LoadArgInstruction>(deserialized[0]);
        Assert.IsType<LoadArgInstruction>(deserialized[1]);
        Assert.IsType<ArithmeticInstruction>(deserialized[2]);
        Assert.IsType<StoreLocalInstruction>(deserialized[3]);
    }

    [Fact]
    public void SerializeNewObjectInstruction_IncludesTypeName()
    {
        // Arrange
        var typeRef = TypeReference.FromName("MyClass");
        var instr = new NewObjectInstruction(typeRef);

        // Act
        var doc = InstructionSerializer.SerializeInstruction(instr);

        // Assert
        Assert.Equal("newobj", doc.RootElement.GetProperty("opCode").GetString());
        Assert.Equal("MyClass", doc.RootElement.GetProperty("operand").GetProperty("type").GetString());
    }

    [Fact]
    public void SerializeCastInstruction_IncludesTargetType()
    {
        // Arrange
        var typeRef = TypeReference.FromName("Animal");
        var instr = new CastInstruction(typeRef);

        // Act
        var doc = InstructionSerializer.SerializeInstruction(instr);

        // Assert
        Assert.Equal("castclass", doc.RootElement.GetProperty("opCode").GetString());
        Assert.Equal("Animal", doc.RootElement.GetProperty("operand").GetProperty("targetType").GetString());
    }

    [Fact]
    public void SerializeDupInstruction_HasNoOperand()
    {
        // Arrange
        var instr = new DupInstruction();

        // Act
        var doc = InstructionSerializer.SerializeInstruction(instr);

        // Assert
        Assert.Equal("dup", doc.RootElement.GetProperty("opCode").GetString());
        Assert.False(doc.RootElement.TryGetProperty("operand", out var operand) && operand.ValueKind != JsonValueKind.Null);
    }

    [Fact]
    public void SerializePopInstruction_HasNoOperand()
    {
        // Arrange
        var instr = new PopInstruction();

        // Act
        var doc = InstructionSerializer.SerializeInstruction(instr);

        // Assert
        Assert.Equal("pop", doc.RootElement.GetProperty("opCode").GetString());
    }

    [Fact]
    public void RoundTrip_ComplexInstructions_PreservesTypes()
    {
        // Arrange
        var instructions = new InstructionList();
        instructions.Emit(new LoadConstantInstruction(10, TypeReference.Int32));
        instructions.Emit(new LoadConstantInstruction("hello", TypeReference.String));
        instructions.Emit(new NewObjectInstruction(TypeReference.FromName("MyClass")));

        // Act
        var serialized = InstructionSerializer.SerializeInstructions(instructions);
        var deserialized = InstructionSerializer.DeserializeInstructions(serialized);

        // Assert
        Assert.Equal(3, deserialized.Count);
        Assert.IsType<LoadConstantInstruction>(deserialized[0]);
        Assert.IsType<LoadConstantInstruction>(deserialized[1]);
        Assert.IsType<NewObjectInstruction>(deserialized[2]);
    }

    [Fact]
    public void SerializeAllArithmeticOps()
    {
        // Arrange
        var ops = new[] { ArithmeticOp.Add, ArithmeticOp.Sub, ArithmeticOp.Mul, ArithmeticOp.Div, ArithmeticOp.Rem };
        var expected = new[] { "add", "sub", "mul", "div", "rem" };

        // Act & Assert
        for (int i = 0; i < ops.Length; i++)
        {
            var instr = new ArithmeticInstruction(ops[i]);
            var doc = InstructionSerializer.SerializeInstruction(instr);
            var opCode = doc.RootElement.GetProperty("opCode").GetString();
            Assert.Equal(expected[i], opCode);
        }
    }

    [Fact]
    public void SerializeAllComparisonOps()
    {
        // Arrange
        var ops = new[] { ComparisonOp.Equal, ComparisonOp.Greater, ComparisonOp.Less };
        var expected = new[] { "ceq", "cgt", "clt" };

        // Act & Assert
        for (int i = 0; i < ops.Length; i++)
        {
            var instr = new ComparisonInstruction(ops[i]);
            var doc = InstructionSerializer.SerializeInstruction(instr);
            var opCode = doc.RootElement.GetProperty("opCode").GetString();
            Assert.Equal(expected[i], opCode);
        }
    }
}
