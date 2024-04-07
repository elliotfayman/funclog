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

    def codeGenStmt(stmt: StmtNode, rtrnStr: String): String = stmt match {
        case VarDefStmtNode(_, vr, value) => {
            val expStr: String = codeGenExp(value)
            varConversion(vr) + " is " + expStr
        }
        case AssertStmtNode(cond) => codeGenExp(cond)
        case ReturnStmtNode(value) => {
            rtrnStr + " is " + codeGenExp(value)
        }
        case BlockStmtNode(stmts) => ""
        case ChoiceStmtNode(c1, c2) => ""
    }
    
    def codeGenFuncDef(fnc: FuncDefNode): String = fnc match {
        case FuncDefNode(name, params, _, stmt) => {
            val funcName = "func" + name + "(" + params.map { case (_, vr) => varConversion(vr) }.mkString(", ") + ", "
            val rtrnStr = "RtnV" + fnc.id.toString
            val stmtString = codeGenStmt(stmt, rtrnStr)
        
            s"$funcName$rtrnStr) :- $stmtString."
        }
    }

    def varConversion(vr: Var): String =  "X" + vr.name
}