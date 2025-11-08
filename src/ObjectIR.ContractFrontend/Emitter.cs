using ObjectIR.ContractFrontend.AST;
using ObjectIR.Core.IR;

namespace ObjectIR.ContractFrontend;

public class Emitter
{
    public Module Emit(ContractUnit unit, string moduleName = "ContractModule")
    {
        var module = new Module(moduleName);
        foreach (var contract in unit.Contracts)
        {
            var classDef = module.DefineClass(contract.Name);
            classDef.Namespace = "Contract";
            classDef.IsSealed = true;

            foreach (var fn in contract.Functions)
            {
                var method = classDef.DefineMethod(fn.Name, TypeReference.Void);
                method.IsStatic = true;
                foreach (var p in fn.Parameters)
                {
                    method.DefineParameter(p.Name, TypeReference.Int32); // simplistic typing
                }
                EmitBlock(fn.Body, method);
                // Ensure terminating return if none present
                if (!method.Instructions.Any(i => i.OpCode == OpCode.Ret))
                {
                    method.Instructions.EmitReturn();
                }
            }
        }
        return module;
    }

    private void EmitBlock(BlockStmt block, MethodDefinition method)
    {
        foreach (var stmt in block.Statements)
        {
            EmitStatement(stmt, method);
        }
    }

    private void EmitStatement(Stmt stmt, MethodDefinition method)
    {
        switch (stmt)
        {
            case ExprStmt es:
                EmitExpr(es.Expression, method);
                break;
            case VarDeclStmt vds:
                if (vds.Initializer != null)
                {
                    EmitExpr(vds.Initializer, method);
                    // Pop value (no locals yet)
                    method.Instructions.EmitPop();
                }
                break;
            case ReturnStmt rs:
                if (rs.Expression != null)
                {
                    EmitExpr(rs.Expression, method);
                }
                method.Instructions.EmitReturn();
                break;
            case IfStmt ifs:
                // Minimal if: evaluate condition then (no else) — placeholder
                EmitExpr(ifs.Condition, method);
                method.Instructions.EmitPop();
                EmitBlock(ifs.Then, method);
                if (ifs.Else != null) EmitBlock(ifs.Else, method); // naive
                break;
            case WhileStmt ws:
                // Placeholder: emit condition and body once
                EmitExpr(ws.Condition, method);
                method.Instructions.EmitPop();
                EmitBlock(ws.Body, method);
                break;
        }
    }

    private void EmitExpr(Expr expr, MethodDefinition method)
    {
        switch (expr)
        {
            case IntLiteralExpr ile:
                method.Instructions.EmitLoadConstant(ile.Value, TypeReference.Int32);
                break;
            case StringLiteralExpr sle:
                method.Instructions.EmitLoadConstant(sle.Value, TypeReference.String);
                break;
            case IdentifierExpr id:
                // Treat bare identifier as string constant if quoted? fallback no-op
                break;
            case BinaryExpr be:
                EmitExpr(be.Left, method);
                EmitExpr(be.Right, method);
                switch (be.Op)
                {
                    case "+": method.Instructions.EmitAdd(); break;
                    case "-": method.Instructions.EmitSub(); break;
                    case "<": method.Instructions.EmitCompareLess(); break;
                    case "==": method.Instructions.EmitCompareEqual(); break;
                }
                break;
            case CallExpr call:
                // Support IO.println(expr)
                if (call.Target is IdentifierExpr tid && tid.Name.StartsWith("IO.println"))
                {
                    if (call.Arguments.Count == 1)
                    {
                        EmitExpr(call.Arguments[0], method);
                        var consoleType = TypeReference.FromName("System.Console");
                        var writeLineRef = new MethodReference(consoleType, "WriteLine", TypeReference.Void, new List<TypeReference>{ TypeReference.String });
                        method.Instructions.EmitCall(writeLineRef);
                    }
                }
                break;
        }
    }
}
