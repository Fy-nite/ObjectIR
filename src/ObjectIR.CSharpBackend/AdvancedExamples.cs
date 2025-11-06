using ObjectIR.Core.IR;
using ObjectIR.CSharpBackend;

/// <summary>
/// Advanced examples demonstrating the ObjectIR C# backend capabilities
/// </summary>
public static class AdvancedExamples
{
    /// <summary>
    /// Example: Generate an enum type
    /// </summary>
    public static string GenerateEnumExample()
    {
        var module = new Module("Examples");
        
  var statusEnum = new EnumDefinition("Status");
statusEnum.Namespace = "Examples";
        statusEnum.UnderlyingType = TypeReference.Int32;
        statusEnum.DefineMember("Active", 0);
   statusEnum.DefineMember("Inactive", 1);
        statusEnum.DefineMember("Pending", 2);
        
        module.Types.Add(statusEnum);
        
     var generator = new CSharpCodeGenerator();
  return generator.Generate(module);
    }
    
    /// <summary>
  /// Example: Generate a class with generic type parameters and constraints
    /// </summary>
    public static string GenerateGenericClassExample()
   {
        var module = new Module("Collections");
        
 var listClass = new ClassDefinition("SimpleList");
        listClass.Namespace = "Collections";
     
   // Add generic parameter with constraint
        var tParam = new GenericParameter("T");
        tParam.Constraints.Add(TypeConstraint.Class());
   listClass.GenericParameters.Add(tParam);
     
        // Add a field
 var itemsField = listClass.DefineField("_items", TypeReference.List(TypeReference.FromName("T")));
        itemsField.Access = AccessModifier.Private;
        
   // Add an Add method
        var addMethod = listClass.DefineMethod("Add", TypeReference.Void);
        addMethod.DefineParameter("item", TypeReference.FromName("T"));
 
        module.Types.Add(listClass);
        
  var generator = new CSharpCodeGenerator();
      return generator.Generate(module);
    }
    
    /// <summary>
   /// Example: Generate a struct implementing an interface
    /// </summary>
    public static string GenerateStructExample()
    {
     var module = new Module("Geometry");

       var pointStruct = new StructDefinition("Point3D");
        pointStruct.Namespace = "Geometry";
  pointStruct.Interfaces.Add(TypeReference.FromName("System.IEquatable").MakeGenericType(TypeReference.FromName("Point3D")));
    
 // Add fields
   pointStruct.DefineField("X", TypeReference.Float64).Access = AccessModifier.Public;
        pointStruct.DefineField("Y", TypeReference.Float64).Access = AccessModifier.Public;
     pointStruct.DefineField("Z", TypeReference.Float64).Access = AccessModifier.Public;
        
        // Add an Equals method
      var equalsMethod = new MethodDefinition("Equals", TypeReference.Bool);
   equalsMethod.Access = AccessModifier.Public;
       equalsMethod.IsOverride = true;
     equalsMethod.DefineParameter("obj", TypeReference.String);

        pointStruct.Methods.Add(equalsMethod);
   
 module.Types.Add(pointStruct);
        
        var generator = new CSharpCodeGenerator();
        return generator.Generate(module);
    }
    
 /// <summary>
    /// Example: Generate an interface with multiple methods and properties
    /// </summary>
    public static string GenerateInterfaceExample()
   {
   var module = new Module("Contracts");
        
     var dataInterface = new InterfaceDefinition("IDataService");
     dataInterface.Namespace = "Contracts";
        
    // Add a property
        var isReadOnlyProp = new PropertyDefinition("IsReadOnly", TypeReference.Bool);
        isReadOnlyProp.Access = AccessModifier.Public;
       dataInterface.Properties.Add(isReadOnlyProp);
        
 // Add methods
        var queryMethod = dataInterface.DefineMethod("Query", TypeReference.List(TypeReference.String));
       queryMethod.DefineParameter("filter", TypeReference.String);
        
        var saveMethod = dataInterface.DefineMethod("Save", TypeReference.Void);
      saveMethod.DefineParameter("data", TypeReference.FromName("System.Object"));

        module.Types.Add(dataInterface);
        
  var generator = new CSharpCodeGenerator();
        return generator.Generate(module);
    }
    
    /// <summary>
    /// Example: Generate control flow with if-else and loops
   /// </summary>
    public static string GenerateControlFlowExample()
    {
    var module = new Module("Logic");
        
      var logicClass = new ClassDefinition("Processor");
      logicClass.Namespace = "Logic";
        logicClass.Access = AccessModifier.Public;
  
      // Add a method with control flow
      var processMethod = logicClass.DefineMethod("ProcessData", TypeReference.Bool);
  processMethod.Access = AccessModifier.Public;
        processMethod.DefineParameter("count", TypeReference.Int32);
     
   // Define locals
        processMethod.DefineLocal("result", TypeReference.Bool);
     processMethod.DefineLocal("i", TypeReference.Int32);
   
        // Add instructions for if statement
 var ifInstr = new IfInstruction(Condition.Stack());
      ifInstr.ThenBlock.EmitLoadConstant(true, TypeReference.Bool);
    ifInstr.ThenBlock.EmitStoreLocal("result");
        
        processMethod.Instructions.Emit(ifInstr);
        
        // Add while loop
        var whileInstr = new WhileInstruction(Condition.Stack());
 whileInstr.Body.EmitLoadLocal("i");
 whileInstr.Body.EmitLoadConstant(1, TypeReference.Int32);
        whileInstr.Body.EmitLoadLocal("i");
   
 processMethod.Instructions.Emit(whileInstr);
        processMethod.Instructions.EmitLoadLocal("result");
        processMethod.Instructions.EmitReturn();
        
        logicClass.Methods.Add(processMethod);
      module.Types.Add(logicClass);
        
 var generator = new CSharpCodeGenerator();
     return generator.Generate(module);
    }
    
    /// <summary>
    /// Example: Generate try-catch-finally
    /// </summary>
    public static string GenerateTryCatchExample()
   {
      var module = new Module("ErrorHandling");
        
   var handlerClass = new ClassDefinition("FileHandler");
  handlerClass.Namespace = "ErrorHandling";
        
      var readMethod = handlerClass.DefineMethod("ReadFile", TypeReference.String);
        readMethod.DefineParameter("filePath", TypeReference.String);
        
      // Create try-catch-finally instruction
    var tryInstr = new TryInstruction();
    
      // Try block
     tryInstr.TryBlock.EmitLoadArg("filePath");
        
    // Catch block
      var catchClause = new CatchClause(TypeReference.FromName("System.IO.IOException"), "ex");
        catchClause.Body.EmitLoadConstant("Error reading file", TypeReference.String);
   tryInstr.CatchClauses.Add(catchClause);
    
        // Finally block
     tryInstr.FinallyBlock = new InstructionList();
      tryInstr.FinallyBlock.EmitLoadConstant("File operation complete", TypeReference.String);
        
   readMethod.Instructions.Emit(tryInstr);
        
        handlerClass.Methods.Add(readMethod);
   module.Types.Add(handlerClass);
   
        var generator = new CSharpCodeGenerator();
  return generator.Generate(module);
    }
}
