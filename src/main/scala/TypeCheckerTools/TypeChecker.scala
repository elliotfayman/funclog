package TypeCheckerTools

import scala.collection.mutable.Map
import ParserTools._


object TypeChecker {

    def typeCheck(prgrm: ProgramNode): Unit = prgrm match {
        case ProgramNode(lst: List[FuncDefNode])=> typeCheckProgram(lst, Map.empty[Var, Type])
    }

    def typeOfExp(ex: ExpNode, env: Map[Var, Type]): Type = ex match {
        case IntExpNode(_) => IntType
        case TrueExpNode | FalseExpNode => BooleanType
        case VarExpNode(vr: Var) => env.get(vr) match {
                case Some(t) => t
                case None => throw new Exception("Variable Undefined or not in scope")
            }
        case OpExpNode(op, e1, e2) => {
            val e1Type: Type = typeOfExp(e1, env)
            val e2Type: Type = typeOfExp(e2, env)
            (e1Type, op, e2Type) match {
                case (IntType, PlusOperatorNode, IntType) => IntType
                case (IntType, MinusOperatorNode, IntType) => IntType
                case (IntType, MultiplyOperatorNode, IntType) => IntType
                case (IntType, DivideOperatorNode, IntType) => IntType
                case (IntType, LessThanOperatorNode, IntType) => BooleanType
                case (IntType, GreaterThanOperatorNode, IntType) => BooleanType
                case (IntType, DoubleEqualsOperatorNode, IntType) => BooleanType
                case _ => throw new Exception("Type Error: operation operands not well typed")
            }
        }
        case CallExpNode(vr: Var, params: List[ExpNode]) => env.get(vr) match {
            case Some(FunctionType(expectedParams: List[Type], rtrn: Type)) => {
                val typesList: List[Type] = params.map(expNode => typeOfExp(expNode, env))
                if (typesList == expectedParams) {
                    rtrn
                } else {
                    throw new Exception("Type Error: Function paramater mismatch")
                }
            }
            case Some(_) => throw new Exception("Type Error: Function Type expected for call")
            case None => throw new Exception("Type Error: function type expected for call")
        }
        case FuncExpNode(params: List[(TypeNode, Var)], stmt: StmtNode) => {
            val newEnv: Map[Var, Type] = typeCheckVarParamList(params, env)
            val (rtnType: Option[Type], _) = typeCheckStmt(stmt, newEnv)
            rtnType match {
                case Some(rtn) => rtn
                case None => throw new Exception("Type Error: Stmt in function Must have return a value")
            }
        }
    }

    def typeCheckStmt(stmt: StmtNode, env: Map[Var, Type]): (Option[Type], Map[Var, Type]) = stmt match {
        case VarDefStmtNode(t: TypeNode, vr: Var, value: ExpNode) => {
            val typeExp: Type = typeOfExp(value, env)
            val expectedType: Type = astNodeTypeToTypeCheckerType(t)
            if (typeExp == expectedType) {
                env += (vr -> expectedType)
                (None, env)
            } else {
                throw new Exception("Type Mismatch: Variable Type does not match Expression Type ")
            }
        }
        case AssertStmtNode(cond: ExpNode) => {
            val typeExp: Type = typeOfExp(cond, env)
            if (typeExp == BooleanType) {
                (None, env)
            } else {
                throw new Exception("Type Mismatch: Assert Exp must be boolean")
            }
        }
        case ReturnStmtNode(exp: ExpNode) => (Some(typeOfExp(exp, env)), env)
        case ChoiceStmtNode(c1: StmtNode, c2: StmtNode) => {
            typeCheckStmt(c1, env)
            typeCheckStmt(c2, env)
            (None, env)
        }

        case BlockStmtNode(lst: List[StmtNode]) => {
            // val (rtn: Option[Type], _) = lst.foldLeft(env)((accum, cur)) => {
            //     typeCheck(cur, accum)
            // }
            val (rtn: Type, _) = typeCheckListOfStmt(lst, env, None)
            (Some(rtn), env)
        }
    }

    def typeCheckFunction(func: FuncDefNode, env: Map[Var, Type]): Map[Var, Type] = func match {
        case FuncDefNode(name: String, params: List[(TypeNode, Var)], rtntype: TypeNode, stmt: StmtNode) => {
            val envWithFunc: Map[Var, Type] = env + (Var(name) -> FunctionType(params.map((tn, _) => astNodeTypeToTypeCheckerType(tn)), astNodeTypeToTypeCheckerType(rtntype)))
            val envWithParams: Map[Var, Type] = typeCheckVarParamList(params, envWithFunc)
            val (stmtRtn, _) = typeCheckStmt(stmt, envWithParams)
            stmtRtn match   {
                case Some(stmtRtnV) => {
                    if (stmtRtnV == astNodeTypeToTypeCheckerType(rtntype)) {
                        env
                    } else {
                        print(stmtRtnV)
                        print(astNodeTypeToTypeCheckerType(rtntype))
                        throw new Exception("Type Mismatch: Return Type must match stmt Return")
                    }
                }
                case None => throw new Exception("Type Error: Unexpected Error oops lol")
            }
            
        }
        
    }

    def typeCheckProgram(fncs: List[FuncDefNode], env: Map[Var, Type]): Map[Var, Type] = {
        if (fncs.isEmpty) {
            env
        } else {
            typeCheckFunction(fncs.head, env)
            typeCheckProgram(fncs.tail, env)
        }
    }

    def typeCheckListOfStmt(lst: List[StmtNode], env: Map[Var, Type], rtn: Option[Type]): (Type, Map[Var, Type]) = {
        if (lst.isEmpty) {
            rtn match {
                case Some(r) => (r, env)
                case None => throw new Exception("Type Error: return stmt must be present")
            }
        } else {
            if (rtn != None) {
                throw new Exception("Type Error: Cannot add additional stmts after return")
            } else {
                val (rtn: Option[Type], newEnv: Map[Var, Type] ) = typeCheckStmt(lst.head, env)
                typeCheckListOfStmt(lst.tail, newEnv, rtn)
            }
        }
    }

    def typeCheckVarParamList(lst: List[(TypeNode, Var)], env: Map[Var, Type]): Map[Var, Type] = {
        if (lst.isEmpty) {
            env
        } else {
            val (tNode: TypeNode, vr: Var) = lst.head
            env += (vr -> astNodeTypeToTypeCheckerType(tNode))
            typeCheckVarParamList(lst.tail, env)
        }
    }

    def astNodeTypeToTypeCheckerType(typ: TypeNode): Type = typ match {
        case IntTypeNode => IntType
        case BooleanTypeNode => BooleanType
        case FunctionTypeNode(param: List[TypeNode], rtn: TypeNode) => FunctionType(param.map(astNodeTypeToTypeCheckerType), astNodeTypeToTypeCheckerType(rtn))
    }

}


