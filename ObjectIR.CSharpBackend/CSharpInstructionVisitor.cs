using System.Text;
using ObjectIR.Core.IR;

namespace ObjectIR.CSharpBackend;

/// <summary>
/// Visits IR instructions and generates C# code
/// </summary>
internal class CSharpInstructionVisitor : IInstructionVisitor
{
    private readonly CSharpCodeGenerator _generator;
    private readonly Stack<string> _stack = new();
    private int _tempVarCounter = 0;

    public CSharpInstructionVisitor(CSharpCodeGenerator generator)
 {
        _generator = generator;
    }

    public void Visit(LoadArgInstruction instruction)
    {
     _stack.Push(instruction.ArgumentName);
    }

public void Visit(LoadLocalInstruction instruction)
    {
        _stack.Push(instruction.LocalName);
    }

    public void Visit(LoadFieldInstruction instruction)
    {
        var obj = PopOrTemp();
        _stack.Push($"{obj}.{instruction.Field.Name}");
  }

    public void Visit(LoadStaticFieldInstruction instruction)
  {
        _stack.Push($"{instruction.Field.DeclaringType.Name}.{instruction.Field.Name}");
   }

    public void Visit(LoadConstantInstruction instruction)
    {
        var formattedValue = FormatValue(instruction.Value, instruction.Type);
        _stack.Push(formattedValue);
    }

    public void Visit(LoadNullInstruction instruction)
    {
   _stack.Push("null");
    }

    public void Visit(StoreArgInstruction instruction)
    {
        var value = Pop();
     _generator.WriteLine($"{instruction.ArgumentName} = {value};");
    }

    public void Visit(StoreLocalInstruction instruction)
 {
     var value = Pop();
     _generator.WriteLine($"{instruction.LocalName} = {value};");
  }

   public void Visit(StoreFieldInstruction instruction)
    {
        var value = Pop();
        var obj = Pop();
        _generator.WriteLine($"{obj}.{instruction.Field.Name} = {value};");
    }

    public void Visit(StoreStaticFieldInstruction instruction)
    {
      var value = Pop();
        _generator.WriteLine($"{instruction.Field.DeclaringType.Name}.{instruction.Field.Name} = {value};");
    }

    public void Visit(ArithmeticInstruction instruction)
    {
  var right = Pop();
   var left = Pop();
        
    var op = instruction.Operation switch
        {
    ArithmeticOp.Add => "+",
  ArithmeticOp.Sub => "-",
           ArithmeticOp.Mul => "*",
          ArithmeticOp.Div => "/",
          ArithmeticOp.Rem => "%",
         ArithmeticOp.And => "&",
      ArithmeticOp.Or => "|",
            ArithmeticOp.Xor => "^",
    ArithmeticOp.Shl => "<<",
    ArithmeticOp.Shr => ">>",
           _ => "+"
        };
        
  _stack.Push($"({left} {op} {right})");
    }

  public void Visit(ComparisonInstruction instruction)
    {
     var right = Pop();
        var left = Pop();
        
  var op = instruction.Operation switch
      {
      ComparisonOp.Equal => "==",
           ComparisonOp.NotEqual => "!=",
           ComparisonOp.Greater => ">",
    ComparisonOp.GreaterOrEqual => ">=",
           ComparisonOp.Less => "<",
     ComparisonOp.LessOrEqual => "<=",
          _ => "=="
        };
       
       _stack.Push($"({left} {op} {right})");
    }

    public void Visit(CallInstruction instruction)
    {
        var args = PopArguments(instruction.Method.ParameterTypes.Count);
    var argString = string.Join(", ", args);
   
        if (instruction.Method.ReturnType.Name != "void")
{
     var tempVar = GetTempVar();
    var declaringType = instruction.Method.DeclaringType.Name;
       _generator.WriteLine($"var {tempVar} = {declaringType}.{instruction.Method.Name}({argString});");
        _stack.Push(tempVar);
        }
        else
        {
  var declaringType = instruction.Method.DeclaringType.Name;
            _generator.WriteLine($"{declaringType}.{instruction.Method.Name}({argString});");
        }
    }

    public void Visit(CallVirtualInstruction instruction)
    {
        var args = PopArguments(instruction.Method.ParameterTypes.Count);
        var obj = Pop();
        var argString = string.Join(", ", args);
        
        if (instruction.Method.ReturnType.Name != "void")
        {
     var tempVar = GetTempVar();
_generator.WriteLine($"var {tempVar} = {obj}.{instruction.Method.Name}({argString});");
           _stack.Push(tempVar);
   }
        else
  {
        _generator.WriteLine($"{obj}.{instruction.Method.Name}({argString});");
 }
   }

  public void Visit(NewObjectInstruction instruction)
    {
       var tempVar = GetTempVar();
        _generator.WriteLine($"var {tempVar} = new {instruction.Type.Name}();");
        _stack.Push(tempVar);
}

    public void Visit(NewArrayInstruction instruction)
  {
        var size = Pop();
   var tempVar = GetTempVar();
  _generator.WriteLine($"var {tempVar} = new {instruction.ElementType.Name}[{size}];");
        _stack.Push(tempVar);
  }

    public void Visit(CastInstruction instruction)
    {
   var value = Pop();
        _stack.Push($"(({instruction.TargetType.Name}){value})");
    }

    public void Visit(IsInstanceInstruction instruction)
    {
      var value = Pop();
    _stack.Push($"({value} is {instruction.TargetType.Name})");
    }

   public void Visit(ReturnInstruction instruction)
    {
        if (instruction.Value != null)
     {
      instruction.Value.Accept(this);
          var value = Pop();
      _generator.WriteLine($"return {value};");
        }
        else
        {
  _generator.WriteLine("return;");
  }
    }

    public void Visit(DupInstruction instruction)
    {
        if (_stack.TryPeek(out var value))
        {
    _stack.Push(value);
        }
    }

    public void Visit(PopInstruction instruction)
    {
     if (_stack.Count > 0)
       {
  _stack.Pop();
      }
    }

    public void Visit(ConversionInstruction instruction)
 {
  var value = Pop();
    var targetType = FormatCSharpType(instruction.TargetType);
        _stack.Push($"(({targetType}){value})");
    }

    public void Visit(IfInstruction instruction)
    {
        if (instruction.Condition is StackCondition)
        {
      VisitIfStackCondition(instruction);
      }
   else if (instruction.Condition is BinaryCondition bc)
   {
            VisitIfBinaryCondition(instruction, bc);
        }
        else if (instruction.Condition is ExpressionCondition ec)
        {
  VisitIfExpressionCondition(instruction, ec);
        }
  else
     {
         _generator.WriteLine("// Unknown condition type");
        }
    }

    private void VisitIfStackCondition(IfInstruction instruction)
    {
        var condition = Pop();
 _generator.WriteLine($"if ({condition})");
        _generator.WriteLine("{");
        _generator.IncreaseIndent();
        
    foreach (var instr in instruction.ThenBlock)
       {
  instr.Accept(this);
 }
    
        _generator.DecreaseIndent();
        
        if (instruction.ElseBlock != null && instruction.ElseBlock.Count > 0)
        {
         _generator.WriteLine("}");
  _generator.WriteLine("else");
            _generator.WriteLine("{");
       _generator.IncreaseIndent();
         
  foreach (var instr in instruction.ElseBlock)
          {
  instr.Accept(this);
            }
         
       _generator.DecreaseIndent();
           _generator.WriteLine("}");
        }
        else
    {
     _generator.WriteLine("}");
   }
    }

    private void VisitIfBinaryCondition(IfInstruction instruction, BinaryCondition bc)
    {
   var right = Pop();
  var left = Pop();
        
  var op = bc.Operation switch
       {
          ComparisonOp.Equal => "==",
       ComparisonOp.NotEqual => "!=",
        ComparisonOp.Greater => ">",
      ComparisonOp.GreaterOrEqual => ">=",
   ComparisonOp.Less => "<",
          ComparisonOp.LessOrEqual => "<=",
     _ => "=="
   };
       
    _generator.WriteLine($"if ({left} {op} {right})");
     _generator.WriteLine("{");
   _generator.IncreaseIndent();
   
        foreach (var instr in instruction.ThenBlock)
{
           instr.Accept(this);
        }
   
        _generator.DecreaseIndent();
  
       if (instruction.ElseBlock != null && instruction.ElseBlock.Count > 0)
    {
          _generator.WriteLine("}");
        _generator.WriteLine("else");
   _generator.WriteLine("{");
        _generator.IncreaseIndent();
        
         foreach (var instr in instruction.ElseBlock)
          {
         instr.Accept(this);
           }
 
    _generator.DecreaseIndent();
           _generator.WriteLine("}");
        }
       else
      {
         _generator.WriteLine("}");
        }
    }

    private void VisitIfExpressionCondition(IfInstruction instruction, ExpressionCondition ec)
    {
      ec.Expression.Accept(this);
        var condition = Pop();
        
      _generator.WriteLine($"if ({condition})");
        _generator.WriteLine("{");
        _generator.IncreaseIndent();
        
     foreach (var instr in instruction.ThenBlock)
        {
      instr.Accept(this);
       }
        
  _generator.DecreaseIndent();

       if (instruction.ElseBlock != null && instruction.ElseBlock.Count > 0)
        {
            _generator.WriteLine("}");
    _generator.WriteLine("else");
   _generator.WriteLine("{");
        _generator.IncreaseIndent();
        
      foreach (var instr in instruction.ElseBlock)
      {
       instr.Accept(this);
     }
        
      _generator.DecreaseIndent();
   _generator.WriteLine("}");
 }
        else
        {
        _generator.WriteLine("}");
  }
    }

 public void Visit(WhileInstruction instruction)
  {
       if (instruction.Condition is StackCondition)
        {
       VisitWhileStackCondition(instruction);
        }
        else if (instruction.Condition is BinaryCondition bc)
        {
  VisitWhileBinaryCondition(instruction, bc);
        }
        else if (instruction.Condition is ExpressionCondition ec)
        {
      VisitWhileExpressionCondition(instruction, ec);
     }
     else
        {
    _generator.WriteLine("// Unknown condition type");
        }
    }

    private void VisitWhileStackCondition(WhileInstruction instruction)
    {
        var condition = Pop();
  _generator.WriteLine($"while ({condition})");
    _generator.WriteLine("{");
        _generator.IncreaseIndent();
   
        foreach (var instr in instruction.Body)
        {
           instr.Accept(this);
        }
        
  _generator.DecreaseIndent();
        _generator.WriteLine("}");
    }

    private void VisitWhileBinaryCondition(WhileInstruction instruction, BinaryCondition bc)
    {
      var right = Pop();
        var left = Pop();
        
        var op = bc.Operation switch
     {
      ComparisonOp.Equal => "==",
      ComparisonOp.NotEqual => "!=",
         ComparisonOp.Greater => ">",
      ComparisonOp.GreaterOrEqual => ">=",
       ComparisonOp.Less => "<",
         ComparisonOp.LessOrEqual => "<=",
           _ => "=="
       };
        
  _generator.WriteLine($"while ({left} {op} {right})");
        _generator.WriteLine("{");
        _generator.IncreaseIndent();
        
        foreach (var instr in instruction.Body)
        {
      instr.Accept(this);
        }
        
  _generator.DecreaseIndent();
        _generator.WriteLine("}");
    }

    private void VisitWhileExpressionCondition(WhileInstruction instruction, ExpressionCondition ec)
    {
        ec.Expression.Accept(this);
 var condition = Pop();
        
        _generator.WriteLine($"while ({condition})");
        _generator.WriteLine("{");
     _generator.IncreaseIndent();
        
        foreach (var instr in instruction.Body)
        {
      instr.Accept(this);
  }
      
  _generator.DecreaseIndent();
  _generator.WriteLine("}");
    }

    public void Visit(BreakInstruction instruction)
    {
     _generator.WriteLine("break;");
    }

    public void Visit(ContinueInstruction instruction)
    {
      _generator.WriteLine("continue;");
    }

    public void Visit(TryInstruction instruction)
    {
        _generator.WriteLine("try");
   _generator.WriteLine("{");
        _generator.IncreaseIndent();
        
        foreach (var instr in instruction.TryBlock)
        {
        instr.Accept(this);
        }
        
        _generator.DecreaseIndent();
        _generator.WriteLine("}");
     
        foreach (var catchClause in instruction.CatchClauses)
  {
  var exceptionType = FormatCSharpType(catchClause.ExceptionType);
         _generator.WriteLine($"catch ({exceptionType} {catchClause.VariableName})");
       _generator.WriteLine("{");
          _generator.IncreaseIndent();
     
   foreach (var instr in catchClause.Body)
        {
            instr.Accept(this);
         }
 
     _generator.DecreaseIndent();
      _generator.WriteLine("}");
       }
       
        if (instruction.FinallyBlock != null)
        {
     _generator.WriteLine("finally");
           _generator.WriteLine("{");
           _generator.IncreaseIndent();
        
      foreach (var instr in instruction.FinallyBlock)
            {
       instr.Accept(this);
         }
        
      _generator.DecreaseIndent();
           _generator.WriteLine("}");
       }
    }

    public void Visit(ThrowInstruction instruction)
 {
 var value = Pop();
        _generator.WriteLine($"throw {value};");
    }

    private string Pop()
    {
        return _stack.Count > 0 ? _stack.Pop() : "null";
    }

    private string PopOrTemp()
    {
        if (_stack.Count > 0)
        {
            return _stack.Pop();
        }
      
  var tempVar = GetTempVar();
        return tempVar;
    }

  private List<string> PopArguments(int count)
    {
        var args = new List<string>();
        for (int i = 0; i < count; i++)
{
  args.Insert(0, Pop());
  }
        return args;
    }

   private string GetTempVar()
    {
        return $"__temp{_tempVarCounter++}";
    }

    private string FormatValue(object? value, TypeReference type)
    {
        if (value == null) return "null";
        if (value is bool b) return b ? "true" : "false";
  if (value is string s) return $"\"{EscapeString(s)}\"";
        if (value is char c) return $"'{c}'";
       if (value is float f) return $"{f}f";
 return value.ToString() ?? "null";
    }

   private string EscapeString(string str)
    {
        return str.Replace("\\", "\\\\")
          .Replace("\"", "\\\"")
       .Replace("\n", "\\n")
       .Replace("\r", "\\r")
       .Replace("\t", "\\t");
    }

    private string FormatCSharpType(TypeReference type)
    {
        if (type.Name == "void") return "void";
   if (type.Name == "bool") return "bool";
 if (type.Name == "int8") return "sbyte";
        if (type.Name == "uint8") return "byte";
        if (type.Name == "int16") return "short";
      if (type.Name == "uint16") return "ushort";
        if (type.Name == "int32") return "int";
      if (type.Name == "uint32") return "uint";
     if (type.Name == "int64") return "long";
     if (type.Name == "uint64") return "ulong";
       if (type.Name == "float32") return "float";
  if (type.Name == "float64") return "double";
        if (type.Name == "char") return "char";
        if (type.Name == "string") return "string";

  var qualified = string.IsNullOrEmpty(type.Namespace) ? type.Name : $"{type.Namespace}.{type.Name}";

        if (type.GenericArguments.Count > 0)
     {
          qualified += $"<{string.Join(", ", type.GenericArguments.Select(FormatCSharpType))}>";
  }

  if (type.IsArray) qualified += "[]";
        if (type.IsPointer) qualified += "*";
        if (type.IsReference) qualified += "&";

       return qualified;
    }
}
