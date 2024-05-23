package TypeCheckerTools

import scala.collection.mutable.Map
import ParserTools._


object TypeChecker {

    // def makeEnv(args: List[(TypeNode, Var)]): Map[Var, (Type, Int)] = {
    //     args.foldLeft(Map[Var, (Type, Int)]()) { (accum, cur) =>
    //         val (typeNode, varNode) = cur
    //         if (accum.contains(varNode)) {
    //             throw TypeErrorException("Duplicate variable name: " + varNode.name)
    //         }
    //         accum + (varNode -> astNodeTypeToTypeCheckerType(typeNode))
    //     }
    // }

    // def makeEnvWithScopeLevel(args: List[(TypeNode, Var)], scopeLevel: Int): Map[Var, (Type, Int)] = {
    //     makeEnv(args).map { case (varNode, typ) => 
    //         varNode -> (typ, scopeLevel)
    //     }
    // }


    def makeFunctionMap(prog: ProgramNode): Map[String, (List[TypeNode], TypeNode)] = {
        prog.fncs.foldLeft(Map[String, (List[TypeNode], TypeNode)]()) { (accum, funcDef) =>
            val FuncDefNode(name, params, returnType, _) = funcDef
            if (accum.contains(name)) {
                throw new TypeErrorException("Duplicate function name: " + name)
            }
            accum + (name -> (params.map(_._1), returnType))
        }
    }


    def typeCheck(prgrm: ProgramNode): Unit = {
        val functionMapping = makeFunctionMap(prgrm)
        prgrm match {
            case ProgramNode(lst: List[FuncDefNode])=> typeCheckProgram(lst, Map.empty[Var, (Type, Int)], functionMapping)
        }
    }

    def typeOfExp(ex: ExpNode, scopeLevel: Int, env: Map[Var, (Type, Int)], funcs: Map[String, (List[TypeNode], TypeNode)]): Type = ex match {
        case IntExpNode(_) => IntType
        case TrueExpNode | FalseExpNode => BooleanType
        case VarExpNode(vr: Var) => env.get(vr) match {
                case Some((t, _)) => t
                case None => throw new TypeErrorException("Variable Undefined or not in scope")
            }
        case OpExpNode(op, e1, e2) => {
            val e1Type: Type = typeOfExp(e1, scopeLevel, env, funcs)
            val e2Type: Type = typeOfExp(e2, scopeLevel, env, funcs)
            (e1Type, op, e2Type) match {
                case (IntType, PlusOperatorNode, IntType) => IntType
                case (IntType, MinusOperatorNode, IntType) => IntType
                case (IntType, MultiplyOperatorNode, IntType) => IntType
                case (IntType, DivideOperatorNode, IntType) => IntType
                case (IntType, LessThanOperatorNode, IntType) => BooleanType
                case (IntType, GreaterThanOperatorNode, IntType) => BooleanType
                case (IntType, DoubleEqualsOperatorNode, IntType) => BooleanType
                case _ => throw new TypeErrorException("Type Error: operation operands not well typed")
            }
        }
        case CallExpNode(vr: Var, params: List[ExpNode]) => env.get(vr) match {
            case Some(FunctionType(expectedParams: List[Type], rtrn: Type), _) => {
                val typesList: List[Type] = params.map(expNode => typeOfExp(expNode, scopeLevel, env, funcs))
                if (typesList == expectedParams) {
                    rtrn
                } else {
                    throw new TypeErrorException("Type Error: Function paramater mismatch")
                }
            }
            case Some(_) => throw new TypeErrorException("Type Error: Function Type expected for call")
            case None => throw new TypeErrorException("Type Error: function type expected for call")
        }
        case FuncExpNode(params: List[(TypeNode, Var)], stmt: StmtNode) => {
            val newEnv: Map[Var, (Type, Int)] = typeCheckVarParamList(params, scopeLevel, env)
            val (rtnType: Option[Type], _) = typeCheckStmt(stmt, scopeLevel + 1, newEnv, funcs)
            rtnType match {
                case Some(rtn) => paramsToFunctionType(params, rtn)
                case None => throw new TypeErrorException("Type Error: Stmt in function Must have return a value")
            }
        }
    }

    def typeCheckStmt(stmt: StmtNode, scopeLevel: Int, env: Map[Var, (Type, Int)], funcs: Map[String, (List[TypeNode], TypeNode)]): (Option[Type], Map[Var, (Type, Int)]) = stmt match {
        case VarDefStmtNode(t: TypeNode, vr: Var, value: ExpNode) => {
            val typeExp: Type = typeOfExp(value, scopeLevel, env, funcs)
            
            val expectedType: Type = astNodeTypeToTypeCheckerType(t)
            if (typeExp == expectedType) {
                env.get(vr) match {
                    case Some(_, `scopeLevel`) => throw new TypeErrorException("Variable in same scope")
                    case _ => env += (vr -> (expectedType, scopeLevel))
                }
                (None, env)
            } else {
                throw new TypeErrorException("Type Mismatch: Variable Type does not match Expression Type: " + vr + " != " + t + " != " + value)
            }
        }
        case AssertStmtNode(cond: ExpNode) => {
            val typeExp: Type = typeOfExp(cond, scopeLevel, env, funcs)
            if (typeExp == BooleanType) {
                (None, env)
            } else {
                throw new TypeErrorException("Type Mismatch: Assert Exp must be boolean")
            }
        }
        case ReturnStmtNode(exp: ExpNode) => (Some(typeOfExp(exp, scopeLevel, env, funcs)), env)
        case ChoiceStmtNode(c1: StmtNode, c2: StmtNode) => {
            val (rtnType1, _) = typeCheckStmt(c1, scopeLevel + 1, env, funcs)
            val (rtnType2, _) = typeCheckStmt(c2, scopeLevel + 1, env, funcs)
            val rtn: Option[Type] = rtnType1 match {
                case Some(typ1) => rtnType2 match {
                    case Some(typ2) => {
                        if(typ2 == typ1) {
                            Some(typ1)
                        } else {
                            None
                        }
                    }
                    case None => None
                }
                case None => None
            }
            (rtn, env)
        }

        case BlockStmtNode(lst: List[StmtNode]) => {
            // val (rtn: Option[Type], _) = lst.foldLeft(env)((accum, cur)) => {
            //     typeCheck(cur, accum)
            // }
            val (rtn: Type, _) = typeCheckListOfStmt(lst, scopeLevel + 1, env, funcs, None)
            (Some(rtn), env)
        }
    }

    def typeCheckFunction(func: FuncDefNode, scopeLevel: Int, env: Map[Var, (Type, Int)], funcs: Map[String, (List[TypeNode], TypeNode)]): Map[Var, (Type, Int)] = func match {
        case FuncDefNode(name: String, params: List[(TypeNode, Var)], rtntype: TypeNode, stmt: StmtNode) => {
            val envWithFunc: Map[Var, (Type, Int)] = env + (Var(name) -> (FunctionType(params.map((tn, _) => astNodeTypeToTypeCheckerType(tn)), astNodeTypeToTypeCheckerType(rtntype)), scopeLevel))
            val envWithParams: Map[Var, (Type, Int)] = typeCheckVarParamList(params, 0, envWithFunc)
            val (stmtRtn, _) = typeCheckStmt(stmt, 0, envWithParams, funcs)
            stmtRtn match   {
                case Some(stmtRtnV) => {
                    if (stmtRtnV == astNodeTypeToTypeCheckerType(rtntype)) {
                        env
                    } else {
                        throw new TypeErrorException("Type Mismatch: Return Type must match stmt Return")
                    }
                }
                case None => throw new TypeErrorException("Type Error: Unexpected Error oops lol")
            }
            
        }
        
    }

    def typeCheckProgram(fncs: List[FuncDefNode], env: Map[Var, (Type, Int)], funcs: Map[String, (List[TypeNode], TypeNode)]): Map[Var, (Type, Int)] = {
        
        if (fncs.isEmpty) {
            env
        } else {
            typeCheckFunction(fncs.head, 0, env, funcs)
            typeCheckProgram(fncs.tail, env, funcs)
        }
    }

    def typeCheckListOfStmt(lst: List[StmtNode], scopeLevel: Int, env: Map[Var, (Type, Int)], funcs: Map[String, (List[TypeNode], TypeNode)], rtn: Option[Type]): (Type, Map[Var, (Type, Int)]) = {
        if (lst.isEmpty) {
            rtn match {
                case Some(r) => (r, env)
                case None => throw new TypeErrorException("Type Error: return stmt must be present")
            }
        } else {
            if (rtn != None) {
                throw new TypeErrorException("Type Error: Cannot add additional stmts after return")
            } else {
                val (rtn: Option[Type], newEnv: Map[Var, (Type, Int)] ) = typeCheckStmt(lst.head, scopeLevel, env, funcs)
                typeCheckListOfStmt(lst.tail, scopeLevel, newEnv, funcs, rtn)
            }
        }
    }

    def typeCheckVarParamList(lst: List[(TypeNode, Var)], scopeLevel: Int, env: Map[Var, (Type, Int)]): Map[Var, (Type, Int)] = {
        if (lst.isEmpty) {
            env
        } else {
            val (tNode: TypeNode, vr: Var) = lst.head
            env += (vr -> (astNodeTypeToTypeCheckerType(tNode), scopeLevel))
            typeCheckVarParamList(lst.tail, scopeLevel, env)
        }
    }

    def paramsToFunctionType(params: List[(TypeNode, Var)], returnType: Type): FunctionType = {
        val paramTypes = params.map{ case (typeNode, _) => astNodeTypeToTypeCheckerType(typeNode) }
        FunctionType(paramTypes, returnType)
    }


    def astNodeTypeToTypeCheckerType(typ: TypeNode): Type = typ match {
        case IntTypeNode => IntType
        case BooleanTypeNode => BooleanType
        case FunctionTypeNode(param: List[TypeNode], rtn: TypeNode) => FunctionType(param.map(astNodeTypeToTypeCheckerType), astNodeTypeToTypeCheckerType(rtn))
    }

}


