using System;
using System.Collections.Generic;
using ObjectIR.Core.Builder;
using ObjectIR.Core.IR;

namespace ObjectIR.Examples;

/// <summary>
/// Example showing how to build a simple Calculator class using the IR builder
/// </summary>
public class CalculatorExample
{
    public static Module BuildCalculator()
    {
        var builder = new IRBuilder("CalculatorApp");

        // Define the Calculator class
        builder.Class("Calculator")
            .Namespace("MyApp")
            .Field("history", TypeReference.List(TypeReference.Int32))
                .Access(AccessModifier.Private)
                .EndField()
            .Field("lastResult", TypeReference.Int32)
                .Access(AccessModifier.Private)
                .EndField()
            
            // Constructor
            .Constructor()
                .Body()
                    // Initialize history field with new List<int32>
                    .Newobj(TypeReference.List(TypeReference.Int32))
                    .Stfld(new FieldReference(
                        TypeReference.FromName("MyApp.Calculator"),
                        "history",
                        TypeReference.List(TypeReference.Int32)))
                    
                    // Initialize lastResult to 0
                    .LdcI4(0)
                    .Stfld(new FieldReference(
                        TypeReference.FromName("MyApp.Calculator"),
                        "lastResult",
                        TypeReference.Int32))
                    
                    .Ret()
                .EndBody()
                .EndMethod()
            
            // Add method
            .Method("Add", TypeReference.Int32)
                .Parameter("a", TypeReference.Int32)
                .Parameter("b", TypeReference.Int32)
                .Local("result", TypeReference.Int32)
                .Body()
                    // result = a + b
                    .Ldarg("a")
                    .Ldarg("b")
                    .Add()
                    .Dup()  // Duplicate for storing
                    .Stloc("result")
                    
                    // history.Add(result)
                    .Ldarg("this")
                    .Ldfld(new FieldReference(
                        TypeReference.FromName("MyApp.Calculator"),
                        "history",
                        TypeReference.List(TypeReference.Int32)))
                    .Ldloc("result")
                    .Callvirt(new MethodReference(
                        TypeReference.List(TypeReference.Int32),
                        "Add",
                        TypeReference.Void,
                        new List<TypeReference> { TypeReference.Int32 }))
                    
                    // this.lastResult = result
                    .Ldarg("this")
                    .Ldloc("result")
                    .Stfld(new FieldReference(
                        TypeReference.FromName("MyApp.Calculator"),
                        "lastResult",
                        TypeReference.Int32))
                    
                    // return result
                    .Ldloc("result")
                    .Ret()
                .EndBody()
                .EndMethod()
            
            // GetAverage method
            .Method("GetAverage", TypeReference.Float32)
                .Local("count", TypeReference.Int32)
                .Local("sum", TypeReference.Int32)
                .Local("i", TypeReference.Int32)
                .Body()
                    // count = history.Count
                    .Ldarg("this")
                    .Ldfld(new FieldReference(
                        TypeReference.FromName("MyApp.Calculator"),
                        "history",
                        TypeReference.List(TypeReference.Int32)))
                    .Callvirt(new MethodReference(
                        TypeReference.List(TypeReference.Int32),
                        "get_Count",
                        TypeReference.Int32,
                        new List<TypeReference>()))
                    .Stloc("count")
                    
                    // if (count == 0) return 0.0
                    .Ldloc("count")
                    .LdcI4(0)
                    .Ceq()
                    .If(Condition.Stack(), 
                        then => then.LdcR4(0.0f).Ret())
                    
                    // sum = 0
                    .LdcI4(0)
                    .Stloc("sum")
                    
                    // i = 0
                    .LdcI4(0)
                    .Stloc("i")
                    
                    // while (i < count)
                    .While(Condition.Binary(ComparisonOp.Less), 
                        body => body
                            // sum += history[i]
                            .Ldarg("this")
                            .Ldfld(new FieldReference(
                                TypeReference.FromName("MyApp.Calculator"),
                                "history",
                                TypeReference.List(TypeReference.Int32)))
                            .Ldloc("i")
                            .Callvirt(new MethodReference(
                                TypeReference.List(TypeReference.Int32),
                                "get_Item",
                                TypeReference.Int32,
                                new List<TypeReference> { TypeReference.Int32 }))
                            .Ldloc("sum")
                            .Add()
                            .Stloc("sum")
                            
                            // i++
                            .Ldloc("i")
                            .LdcI4(1)
                            .Add()
                            .Stloc("i"))
                    
                    // return sum / count (as float)
                    .Ldloc("sum")
                    // TODO: Add conversion instruction
                    .Ldloc("count")
                    // TODO: Add conversion instruction
                    .Div()
                    .Ret()
                .EndBody()
                .EndMethod()
            
            .EndClass();

        return builder.Build();
    }

    public static void PrintModule(Module module)
    {
        Console.WriteLine($"Module: {module.Name} v{module.Version}");
        Console.WriteLine();

        foreach (var type in module.Types)
        {
            if (type is ClassDefinition classDef)
            {
                Console.WriteLine($"class {classDef.GetQualifiedName()} {{");
                
                foreach (var field in classDef.Fields)
                {
                    Console.WriteLine($"  field {field.Name}: {field.Type}");
                }
                
                Console.WriteLine();
                
                foreach (var method in classDef.Methods)
                {
                    var methodName = method.IsConstructor ? "constructor" : $"method {method.Name}";
                    var parameters = string.Join(", ", method.Parameters.Select(p => $"{p.Name}: {p.Type}"));
                    Console.WriteLine($"  {methodName}({parameters}) -> {method.ReturnType} {{");
                    
                    foreach (var local in method.Locals)
                    {
                        Console.WriteLine($"    local {local.Name}: {local.Type}");
                    }
                    
                    if (method.Locals.Count > 0)
                        Console.WriteLine();
                    
                    PrintInstructions(method.Instructions, 4);
                    
                    Console.WriteLine("  }");
                    Console.WriteLine();
                }
                
                Console.WriteLine("}");
            }
        }
    }

    private static void PrintInstructions(IEnumerable<Instruction> instructions, int indent)
    {
        var indentStr = new string(' ', indent);
        
        foreach (var inst in instructions)
        {
            switch (inst)
            {
                case LoadArgInstruction ldarg:
                    Console.WriteLine($"{indentStr}ldarg {ldarg.ArgumentName}");
                    break;
                case LoadLocalInstruction ldloc:
                    Console.WriteLine($"{indentStr}ldloc {ldloc.LocalName}");
                    break;
                case LoadFieldInstruction ldfld:
                    Console.WriteLine($"{indentStr}ldfld {ldfld.Field}");
                    break;
                case LoadConstantInstruction ldc:
                    Console.WriteLine($"{indentStr}{inst.OpCode.ToString().ToLower()} {ldc.Value}");
                    break;
                case StoreLocalInstruction stloc:
                    Console.WriteLine($"{indentStr}stloc {stloc.LocalName}");
                    break;
                case StoreFieldInstruction stfld:
                    Console.WriteLine($"{indentStr}stfld {stfld.Field}");
                    break;
                case ArithmeticInstruction arith:
                    Console.WriteLine($"{indentStr}{arith.Operation.ToString().ToLower()}");
                    break;
                case ComparisonInstruction cmp:
                    Console.WriteLine($"{indentStr}c{cmp.Operation.ToString().ToLower()}");
                    break;
                case CallInstruction call:
                    Console.WriteLine($"{indentStr}call {call.Method.GetSignature()}");
                    break;
                case CallVirtualInstruction callvirt:
                    Console.WriteLine($"{indentStr}callvirt {callvirt.Method.GetSignature()}");
                    break;
                case NewObjectInstruction newobj:
                    Console.WriteLine($"{indentStr}newobj {newobj.Type}");
                    break;
                case DupInstruction:
                    Console.WriteLine($"{indentStr}dup");
                    break;
                case ReturnInstruction:
                    Console.WriteLine($"{indentStr}ret");
                    break;
                case IfInstruction ifInst:
                    Console.WriteLine($"{indentStr}if ({ifInst.Condition}) {{");
                    PrintInstructions(ifInst.ThenBlock, indent + 2);
                    if (ifInst.ElseBlock != null)
                    {
                        Console.WriteLine($"{indentStr}}} else {{");
                        PrintInstructions(ifInst.ElseBlock, indent + 2);
                    }
                    Console.WriteLine($"{indentStr}}}");
                    break;
                case WhileInstruction whileInst:
                    Console.WriteLine($"{indentStr}while ({whileInst.Condition}) {{");
                    PrintInstructions(whileInst.Body, indent + 2);
                    Console.WriteLine($"{indentStr}}}");
                    break;
                default:
                    Console.WriteLine($"{indentStr}{inst.OpCode.ToString().ToLower()}");
                    break;
            }
        }
    }
}
