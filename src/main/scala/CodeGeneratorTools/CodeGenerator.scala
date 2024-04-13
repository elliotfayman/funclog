package CodeGeneratorTools

import scala.collection.BuildFrom.buildFromString
import ParserTools._

object CodeGenerator {

    def codeGenExp(exp: ExpNode): String = exp match {
        case VarExpNode(vr) => varConversion(vr)
        case IntExpNode(value) => value.toString
        case TrueExpNode => "true"
        case FalseExpNode => "false"
        case OpExpNode(op, e1, e2) => {
            val s1: String = "(" + codeGenExp(e1) + ")"
            val s2: String = "(" + codeGenExp(e2) + ")"
            val opStr: String = op match {
                case PlusOperatorNode => "+"
                case MinusOperatorNode => "-"
                case MultiplyOperatorNode => "*"
                case DivideOperatorNode => "/"
                case LessThanOperatorNode => "<"
                case GreaterThanOperatorNode => ">"
                case DoubleEqualsOperatorNode => "=="
            }
            s1 + opStr + s2
        }
        case CallExpNode(vr, params) => ""
        case FuncExpNode(params, stmt) => ""
    }

    def codeGenStmt(stmt: StmtNode, rtrnStr: String, currList: List[String] = List("")): List[String] = stmt match {
        case VarDefStmtNode(_, vr, value) =>
            val expStr: String = codeGenExp(value)
            currList.map(cl => cl + varConversion(vr) + " is " + expStr + "\n")

        case AssertStmtNode(cond) =>
            currList.map(cl => cl + codeGenExp(cond) + "\n")

        case ReturnStmtNode(value) =>
            currList.map(cl => cl + rtrnStr + " is " + codeGenExp(value) + "\n")

        case BlockStmtNode(stmts) =>
            stmts.foldLeft(currList) { (acc, s) => codeGenStmt(s, rtrnStr, acc) }

        case ChoiceStmtNode(c1, c2) =>
            val list1 = codeGenStmt(c1, rtrnStr, currList)
            val list2 = codeGenStmt(c2, rtrnStr, currList)
            list1 ++ list2 // Concatenate the results from each choice path
        }
    
    def codeGenFuncDef(fnc: FuncDefNode): String = fnc match {
        case FuncDefNode(name, params, _, stmt) => {
            val funcName = "func" + "_" + name + "(" + params.map { case (_, vr) => varConversion(vr) }.mkString(", ") + ", "
            val rtrnStr = "RtnV" + fnc.id.toString
            val stmtString = codeGenStmt(stmt, rtrnStr)
            
            val prog = stmtString.foldLeft("")((accum, stmts) => s"$funcName$rtrnStr) :-\n$stmts.\n\n")
            
            prog    
        }
    }

    def varConversion(vr: Var): String =  "X" + vr.name
}