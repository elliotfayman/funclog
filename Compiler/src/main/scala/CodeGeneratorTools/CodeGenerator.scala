package CodeGeneratorTools

import scala.collection.BuildFrom.buildFromString
import ParserTools._

object CodeGenerator {
    var FunctionMap = Map.empty[Var, (String, String, String)]
    var FunctionSigMap = Map.empty[Var, String]
    var VarValueMap = Map.empty[Var, ExpNode]
    var FunctionList = List.empty[String]
    var prefixExps = List.empty[String]
    var latestFuncExp = ("", "", "")

    def codeGenExp(exp: ExpNode): (String, List[String]) = exp match {
        case VarExpNode(vr) => (varConversion(vr), List.empty)
        case IntExpNode(value) => (value.toString, List.empty)
        case TrueExpNode => ("true", List.empty)
        case FalseExpNode => ("false", List.empty)
        case OpExpNode(op, e1, e2) => {
            val (exp1Str, exp1PrefixExp) = codeGenExp(e1)
            val (exp2Str, exp2PrefixExp) = codeGenExp(e2)
            val s1: String = "(" + exp1Str + ")"
            val s2: String = "(" + exp2Str + ")"
            val opStr: String = op match {
                case PlusOperatorNode => "+"
                case MinusOperatorNode => "-"
                case MultiplyOperatorNode => "*"
                case DivideOperatorNode => "/"
                case LessThanOperatorNode => "<"
                case GreaterThanOperatorNode => ">"
                case DoubleEqualsOperatorNode => "=="
            }
            (s1 + opStr + s2, exp1PrefixExp ++ exp2PrefixExp)
        }
        case CallExpNode(vr, params) => {
            val result = FunctionMap.get(vr).getOrElse(("MEMEMEME", "AMAMAMAM", "AAAAAA"))
            
            val funcVarParamsToFuncExpParamMap = VarValueMap.get(vr).flatMap {
                case FuncExpNode(prms, stmt) =>
                    // Extract Var objects directly if the parameter is a VarExpNode, else use a placeholder or null
                    val keyVars = params.collect {
                        case VarExpNode(vr) => vr  // Collect only VarExpNode variables
                    }
        
                    // We only want to create a map if the sizes match to ensure each key has a corresponding value
                    if (keyVars.size == prms.size) {
                        Some(prms.map(_._2).zip(keyVars).toMap)  // Create a map pairing prms Vars to the extracted keys from params
                    } else {
                        None  // If sizes don't match, return None to indicate a mismatch or error
                    }
                case _ => None
            }.getOrElse(Map.empty[Var, Var])  // Return an empty map if no function or mismatch
            val value = VarValueMap.get(vr) match {
                case Some(func) => func match {
                    case  FuncExpNode(prms, stmt) => {
                        createFunc(func, prms, stmt, funcVarParamsToFuncExpParamMap)
                    }
                }
                case None => ""
            }

            val (expParams, prefixExps) = params.foldLeft(("", List.empty[String])) {
                case ((accumStr, prefixList), exp) =>
                // Check if the expression is a variable and if it maps to a FuncExpNode
                val skipExpression = exp match {
                    case VarExpNode(vr) => VarValueMap.get(vr) match {
                        case Some(FuncExpNode(_, _)) => true // Skip if it's a FuncExpNode
                        case _ => false
                    }
                    case _ => false
                }

                if (skipExpression) {
                    // If the expression should be skipped, just pass the accumulators forward
                    (accumStr, prefixList)
                } else {
                    // Otherwise, process the expression as usual
                    val (expStr, expPrefix) = codeGenExp(exp)
                    val newStr = if (accumStr.isEmpty) expStr else accumStr + ", " + expStr
                    val newPrefixList = prefixList ++ expPrefix
                    (newStr, newPrefixList)
                }
            }

            (result._3 , List((result._1 + "(" + List(expParams, result._3).filter(_.nonEmpty).mkString(", ") + ")")))
        }
        case FuncExpNode(params, stmt) => {
            val res = createFunc(exp, params, stmt)
            (res, List())
        }

    }


    def createFunc(funcNode: ExpNode, params: List[(TypeNode, Var)], stmt: StmtNode, funcInternalParamMap: Map[Var, Var]=Map.empty[Var, Var]): String = {
        val (funcs, _) = params.foldLeft((List.empty[(TypeNode, Var)], 0)) { case ((acc, index), item) =>
            item match {
                case (node: FunctionTypeNode, vr) => 
                    //VarValueMap = VarValueMap + (vr -> Unit)
                    val funcSig = funcInternalParamMap.get(vr) match {
                        case Some(outerVR) => FunctionSigMap.get(outerVR) match {
                            case Some(str) => str
                            case None => "lel"
                        }
                        case None => "llll"
                    }
                    FunctionMap = FunctionMap + (vr -> (funcSig, "call", "R" + index.toString))
                    ((acc :+ (node, vr)), index + 1)  // Correctly return the updated tuple
                case _ => 
                    (acc, index + 1)  // Return unchanged list and incremented index
            }
        }

        val funcLambdaMap: Map[(TypeNode, Var), String] = funcs.foldLeft(Map.empty[(TypeNode, Var), String]) { (acc, func) =>
            acc + (func -> ("lambda" + func._1.id.toString))
        }
        val funcName = "lambda_func" + funcNode.id.toString
        val returnStr = "Rtn" + funcNode.id.toString
        val (funcParams, _) = (params.foldLeft(List.empty[String], 0) { case ((acc, index), param) =>
            funcLambdaMap.get(param) match {
                case Some(_) => 
                    (acc, index + 1)
                case None => 
                    (acc :+ varConversion(param._2), index + 1)
            }
        })
        
        val funcParamsAndRtn = (funcParams ++ List(returnStr)).mkString(", ")
        
        val funcSig = funcName + "(" + funcParamsAndRtn + ")"
        val stmtString = codeGenStmt(stmt, returnStr).mkString(", ")
        val func = funcSig + " :- " + stmtString + ".\n"
        if (!func.contains("llll")) {
            FunctionList = FunctionList :+ func
        }
        
        latestFuncExp = (funcName, "(" + funcParamsAndRtn + ")", returnStr)
        "lambda" + funcNode.id.toString
    }


    def codeGenStmt(stmt: StmtNode, rtrnStr: String): List[String] = stmt match {
        case VarDefStmtNode(varType, vr, value) =>
            val (expStr, prefixExp) = codeGenExp(value)
            FunctionMap = FunctionMap + (vr -> latestFuncExp)
            VarValueMap = VarValueMap + (vr -> value)
            FunctionSigMap = FunctionSigMap + (vr -> latestFuncExp._1)
            varType match {
                case typ: FunctionTypeNode => prefixExp ++ List("true")
                case _ => {
                    prefixExp ++ List(varConversion(vr) + " is " + expStr)
                }
            }
        case AssertStmtNode(cond) =>
            val (expStr, prefixExp) = codeGenExp(cond)
            prefixExp ++ List(expStr)
        case ReturnStmtNode(value) =>
            val (expStr, prefixExp) = codeGenExp(value)
            prefixExp ++ List(rtrnStr + " is " + expStr)
        case BlockStmtNode(stmts) =>
            stmts.foldLeft(List(): List[String]) { (acc, st) => acc :+ {codeGenStmt(st, rtrnStr).mkString(", ")} }
        case ChoiceStmtNode(c1, c2) =>
            val list1 = codeGenStmt(c1, rtrnStr)
            val list2 = codeGenStmt(c2, rtrnStr)
            List("(" + list1.mkString(", ") + " ; " +  list2.mkString(", ") + ")")
        }
    
    def codeGenFuncDef(fnc: FuncDefNode): String = fnc match {
        case FuncDefNode(name, params, _, stmt) => {
            val funcName = "func" + "_" + name + "(" + params.map { case (_, vr) => varConversion(vr) }.mkString(", ") + ", "
            val rtrnStr = "RtnV" + fnc.id.toString
            val stmtString = codeGenStmt(stmt, rtrnStr).mkString(", ")
            val prog = s"$funcName$rtrnStr) :- $stmtString.\n"
            val prepend = FunctionList.mkString
            prepend ++ prog    
        }
    }

    def codeGenProg(prg: ProgramNode): String = {
        prg.fncs.foldLeft("") { (accum, func) => accum + codeGenFuncDef(func) + "\n" }
    }

    def varConversion(vr: Var): String =  "X" + vr.name
}