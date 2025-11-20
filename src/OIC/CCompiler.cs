namespace OIC;

using ObjectIR.Core.Builder;
using ObjectIR.Core.IR;
using ObjectIR.Core.Serialization;
using System.Collections.Generic;
using System.Linq;

/// <summary>
/// Compiler that converts C AST to ObjectIR modules
/// </summary>
public class CCompiler
{
    private IRBuilder? _moduleBuilder;
    private Dictionary<string, TypeReference> _typeMap;
    private Dictionary<string, TypeReference> _typedefs;

    public CCompiler()
    {
        _typeMap = new Dictionary<string, TypeReference>();
        _typedefs = new Dictionary<string, TypeReference>();
        InitializeTypeMap();
    }

    private void InitializeTypeMap()
    {
        _typeMap["void"] = TypeReference.Void;
        _typeMap["char"] = TypeReference.Int8;
        _typeMap["short"] = TypeReference.Int16;
        _typeMap["int"] = TypeReference.Int32;
        _typeMap["long"] = TypeReference.Int64;
        _typeMap["float"] = TypeReference.Float32;
        _typeMap["double"] = TypeReference.Float64;
        _typeMap["unsigned"] = TypeReference.UInt32;
        _typeMap["signed"] = TypeReference.Int32;
    }

    public Module Compile(CProgram program)
    {
        _moduleBuilder = new IRBuilder("CProgram");

        // Create a main class to hold all functions
        var classBuilder = _moduleBuilder.Class("Program");

        // Process declarations
        foreach (var decl in program.Declarations)
        {
            if (decl is CFunctionDeclaration funcDecl)
            {
                CompileFunction(classBuilder, funcDecl);
            }
            else if (decl is CTypedef typedef)
            {
                RegisterTypedef(typedef);
            }
            else if (decl is CDeclarationList declList)
            {
                CompileGlobalVariable(classBuilder, declList);
            }
        }

        classBuilder.EndClass();
        return _moduleBuilder.Build();
    }

    private void RegisterTypedef(CTypedef typedef)
    {
        var typeRef = ResolveType(typedef.Specifiers);
        var name = typedef.Declarator.Name;
        if (name != null)
        {
            _typedefs[name] = typeRef;
        }
    }

    private void CompileGlobalVariable(ClassBuilder classBuilder, CDeclarationList declList)
    {
        var type = ResolveType(declList.Specifiers);

        foreach (var declarator in declList.Declarators)
        {
            var name = declarator.Declarator.Name ?? "var";
            classBuilder.Field(name, type);
        }
    }

    private void CompileFunction(ClassBuilder classBuilder, CFunctionDeclaration funcDecl)
    {
        var returnType = ResolveType(funcDecl.ReturnTypeSpecifiers);

        var methodBuilder = classBuilder
            .Method(funcDecl.Name, returnType)
            .Access(AccessModifier.Public)
            .Static();

        // Add parameters
        foreach (var param in funcDecl.Parameters)
        {
            var paramType = ResolveType(param.Specifiers);
            var paramName = param.Declarator?.Name ?? "param";
            methodBuilder.Parameter(paramName, paramType);
        }

        // Compile body
        if (funcDecl.Body.Statements.Count > 0 || funcDecl.Body.Declarations.Count > 0)
        {
            CompileCompoundStatement(methodBuilder, funcDecl.Body);
        }
        else
        {
            // Empty body - just return
            methodBuilder.Body().Ret();
        }

        methodBuilder.EndMethod();
    }

    private void CompileCompoundStatement(MethodBuilder methodBuilder, CCompoundStatement stmt)
    {
        var body = methodBuilder.Body();

        // Add local declarations
        foreach (var decl in stmt.Declarations)
        {
            if (decl is CDeclarationList declList)
            {
                var type = ResolveType(declList.Specifiers);
                foreach (var declarator in declList.Declarators)
                {
                    var name = declarator.Declarator.Name ?? "local";
                    methodBuilder.Local(name, type);
                }
            }
        }

        // Compile statements
        foreach (var statement in stmt.Statements)
        {
            CompileStatement(body, statement);
        }

        // Add default return only if last statement isn't a return
        if (stmt.Statements.Count == 0 || !(stmt.Statements[^1] is CReturnStatement))
        {
            body.Ret();
        }
    }

    private void CompileStatement(InstructionBuilder body, CStatement statement)
    {
        switch (statement)
        {
            case CExpressionStatement exprStmt:
                if (exprStmt.Expression != null)
                {
                    CompileExpression(body, exprStmt.Expression);
                    body.Pop();
                }
                break;

            case CReturnStatement retStmt:
                if (retStmt.Expression != null)
                {
                    CompileExpression(body, retStmt.Expression);
                }
                body.Ret();
                break;

            case CEmptyStatement:
                break;

            default:
                // Other statement types not yet fully implemented
                break;
        }
    }

    private void CompileExpression(InstructionBuilder body, CExpression expr)
    {
        switch (expr)
        {
            case CIntLiteral intLit:
                body.LdcI4((int)intLit.Value);
                break;

            case CFloatLiteral floatLit:
                body.LdcR4((float)floatLit.Value);
                break;

            case CStringLiteral strLit:
                body.Ldstr(strLit.Value);
                break;

            case CIdentifier ident:
                body.Ldarg(ident.Name);
                break;

            case CBinaryOp binOp:
                CompileBinaryOp(body, binOp);
                break;

            case CParenthesized paren:
                CompileExpression(body, paren.Expression);
                break;

            case CFunctionCall funcCall:
                CompileFunctionCall(body, funcCall);
                break;

            default:
                // Unsupported expression type - load 0
                body.LdcI4(0);
                break;
        }
    }

    private void CompileFunctionCall(InstructionBuilder body, CFunctionCall call)
    {
        // Get function name
        if (call.Function is not CIdentifier funcIdent)
        {
            // For now, only handle simple identifier calls
            body.LdcI4(0);
            return;
        }

        string funcName = funcIdent.Name;

        // Handle built-in functions
        if (funcName == "printf")
        {
            CompilePrintfCall(body, call);
        }
        else if (funcName == "scanf")
        {
            CompileScanfCall(body, call);
        }
        else if (funcName == "puts")
        {
            CompilePutsCall(body, call);
        }
        else if (funcName == "strlen")
        {
            CompileStrlenCall(body, call);
        }
        else
        {
            // Regular function call - would need runtime support
            // For now, compile arguments and call
            var paramTypes = new List<TypeReference>();
            foreach (var arg in call.Arguments)
            {
                CompileExpression(body, arg);
                paramTypes.Add(TypeReference.Int32); // Assume int32 for now
            }
            var method = new MethodReference(
                TypeReference.FromName("OIC.Program"),
                funcName,
                TypeReference.Int32,
                paramTypes
            );
            body.Call(method);
        }
    }

    private void CompilePrintfCall(InstructionBuilder body, CFunctionCall call)
    {
        // printf(format, args...) -> Console.WriteLine(formatted)
        if (call.Arguments.Count == 0)
            return;

        // For now, simple support: just print the arguments
        if (call.Arguments[0] is CStringLiteral formatStr)
        {
            if (call.Arguments.Count >= 2)
            {
                // Load argument (skip format)
                CompileExpression(body, call.Arguments[1]);
            }
            else
            {
                // Just format string - print it
                body.Ldstr(formatStr.Value);
            }
        }
        else
        {
            CompileExpression(body, call.Arguments[0]);
        }
        
        var method = new MethodReference(
            TypeReference.FromName("System.Console"),
            "WriteLine",
            TypeReference.Void,
            new List<TypeReference> { TypeReference.String }
        );
        body.Call(method);
    }

    private void CompileScanfCall(InstructionBuilder body, CFunctionCall call)
    {
        // scanf(format, ...) -> Console.ReadLine()
        var method = new MethodReference(
            TypeReference.FromName("System.Console"),
            "ReadLine",
            TypeReference.String,
            new List<TypeReference>()
        );
        body.Call(method);
    }

    private void CompilePutsCall(InstructionBuilder body, CFunctionCall call)
    {
        // puts(str) -> Console.WriteLine(str)
        if (call.Arguments.Count > 0)
        {
            CompileExpression(body, call.Arguments[0]);
        }
        
        var method = new MethodReference(
            TypeReference.FromName("System.Console"),
            "WriteLine",
            TypeReference.Void,
            new List<TypeReference> { TypeReference.String }
        );
        body.Call(method);
    }

    private void CompileStrlenCall(InstructionBuilder body, CFunctionCall call)
    {
        // strlen(str) -> str.Length
        // For now, just load the string and return 0
        if (call.Arguments.Count > 0)
        {
            CompileExpression(body, call.Arguments[0]);
            body.Pop();
        }
        body.LdcI4(0);
    }

    private void CompileBinaryOp(InstructionBuilder body, CBinaryOp op)
    {
        CompileExpression(body, op.Left);
        CompileExpression(body, op.Right);

        switch (op.Op)
        {
            case "+": body.Add(); break;
            case "-": body.Sub(); break;
            case "*": body.Mul(); break;
            case "/": body.Div(); break;
            case "==": body.Ceq(); break;
            case "<": body.Clt(); break;
            case ">": body.Cgt(); break;
        }
    }

    private TypeReference ResolveType(List<CSpecifier> specifiers)
    {
        if (specifiers.Count == 0)
            return TypeReference.Int32;

        // Check for typedefs
        foreach (var spec in specifiers)
        {
            if (spec is CTypeSpecifier typeSpec && _typedefs.TryGetValue(typeSpec.Name, out var typeRef))
                return typeRef;
        }

        // Build type from specifiers
        var isUnsigned = false;
        var isLong = false;
        var isShort = false;
        var isDouble = false;
        var isChar = false;

        foreach (var spec in specifiers)
        {
            if (spec is CTypeSpecifier typeSpec)
            {
                switch (typeSpec.Name)
                {
                    case "void": return TypeReference.Void;
                    case "char": isChar = true; break;
                    case "int": break; // int is default
                    case "float": return TypeReference.Float32;
                    case "double": isDouble = true; break;
                    case "short": isShort = true; break;
                    case "long": isLong = true; break;
                    case "unsigned": isUnsigned = true; break;
                    case "signed": isUnsigned = false; break;
                }
            }
        }

        // Handle char type (for char* support)
        if (isChar)
            return TypeReference.Int8; // char is int8

        if (isDouble)
            return TypeReference.Float64;

        if (isLong)
            return isUnsigned ? TypeReference.UInt64 : TypeReference.Int64;

        if (isShort)
            return isUnsigned ? TypeReference.UInt16 : TypeReference.Int16;

        return isUnsigned ? TypeReference.UInt32 : TypeReference.Int32;
    }
}

/// <summary>
/// High-level C to ObjectIR compiler
/// </summary>
public class CLanguageCompiler
{
    /// <summary>
    /// Compiles C source code to an ObjectIR module
    /// </summary>
    public Module CompileSource(string sourceCode)
    {
        // Lexical analysis
        var lexer = new CLexer(sourceCode);
        var tokens = lexer.Lex().ToList();

        // Syntax analysis
        var parser = new CParser(tokens);
        var program = parser.Parse();

        // Code generation
        var compiler = new CCompiler();
        var module = compiler.Compile(program);

        return module;
    }

    /// <summary>
    /// Compiles C source code and returns as JSON
    /// </summary>
    public string CompileSourceToJson(string sourceCode)
    {
        var module = CompileSource(sourceCode);
        return module.DumpJson();
    }

    /// <summary>
    /// Compiles C source code and returns as text dump
    /// </summary>
    public string CompileSourceToText(string sourceCode)
    {
        var module = CompileSource(sourceCode);
        return module.DumpText();
    }

    /// <summary>
    /// Compiles C source code and returns as IR code (assembly-like format)
    /// </summary>
    public string CompileSourceToIRCode(string sourceCode)
    {
        var module = CompileSource(sourceCode);
        return module.Serialize().DumpToIRCode();
    }

    /// <summary>
    /// Compiles C source code and returns as BSON (binary format)
    /// </summary>
    public byte[] CompileSourceToBson(string sourceCode)
    {
        var module = CompileSource(sourceCode);
        return module.DumpBson();
    }

    /// <summary>
    /// Compiles C source code and returns as CSV format
    /// </summary>
    public string CompileSourceToCsv(string sourceCode)
    {
        var module = CompileSource(sourceCode);
        return module.DumpCsv();
    }

    /// <summary>
    /// Compiles C source code and returns as Markdown format
    /// </summary>
    public string CompileSourceToMarkdown(string sourceCode)
    {
        var module = CompileSource(sourceCode);
        return module.DumpMarkdown();
    }

    /// <summary>
    /// Compiles C source code and returns as YAML format
    /// </summary>
    public string CompileSourceToYaml(string sourceCode)
    {
        var module = CompileSource(sourceCode);
        return module.DumpYaml();
    }

    /// <summary>
    /// Compiles C source code and saves to multiple format files in the specified directory
    /// </summary>
    public void SaveToAllFormats(string sourceCode, string outputDirectory)
    {
        var module = CompileSource(sourceCode);

        if (!Directory.Exists(outputDirectory))
            Directory.CreateDirectory(outputDirectory);

        // Text formats
        File.WriteAllText(Path.Combine(outputDirectory, "module.json"), module.DumpJson());
        File.WriteAllText(Path.Combine(outputDirectory, "module.txt"), module.DumpText());
        File.WriteAllText(Path.Combine(outputDirectory, "module.ir"), CompileSourceToIRCode(sourceCode));
        File.WriteAllText(Path.Combine(outputDirectory, "module.csv"), module.DumpCsv());
        File.WriteAllText(Path.Combine(outputDirectory, "module.md"), module.DumpMarkdown());
        File.WriteAllText(Path.Combine(outputDirectory, "module.yaml"), module.DumpYaml());
        File.WriteAllText(Path.Combine(outputDirectory, "module_report.txt"), module.GenerateSummaryReport());

        // Binary formats
        File.WriteAllBytes(Path.Combine(outputDirectory, "module.bson"), module.DumpBson());

        // Try FOB format, but skip if it fails (requires entry point)
        try
        {
            File.WriteAllBytes(Path.Combine(outputDirectory, "module.fob"), module.DumpFob());
        }
        catch
        {
            // FOB format requires valid entry point, so we skip it silently
        }
    }
}

