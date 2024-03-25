package ParserTools

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{Reader, Position, NoPosition}
import TokenizerTools._

// Define Token Reader
case class TokenReader(tokens: Seq[Token]) extends Reader[Token] {
  override def first: Token = tokens.head
  override def atEnd: Boolean = tokens.isEmpty
  override def pos: Position = NoPosition
  override def rest: Reader[Token] = TokenReader(tokens.tail)
}

object Parser extends Parsers {
  override type Elem = Token

  // Assuming PlusToken represents the '+' operation
  def op: Parser[OperatorNode] = (
    accept("plus token", { case PlusToken => PlusOperatorNode }) |
    accept("minus token", { case MinusToken => MinusOperatorNode }) |
    accept("multiply token", { case MultiplyToken => MultiplyOperatorNode }) | 
    accept("divide token", { case DivideToken => DivideOperatorNode }) | 
    accept("less than token", { case LessThanToken => LessThanOperatorNode }) | 
    accept("greater than token", { case GreaterThanToken => GreaterThanOperatorNode }) | 
    accept("double equals token", { case DoubleEqualsToken => DoubleEqualsOperatorNode })
  )

  def types: Parser[TypeNode] = intType | booleanType | funcType

  def intType: Parser[IntTypeNode.type] = accept("int type token", { case IntTypeToken => IntTypeNode })

  def booleanType: Parser[BooleanTypeNode.type] = accept("boolean type token", { case BooleanTypeToken => BooleanTypeNode })

  def funcType: Parser[FunctionTypeNode] = 
    (LeftParenToken ~> ArrowToken ~> typeParams ~ types <~ RightParenToken) ^^ {
      case typePar ~ rtrn => FunctionTypeNode(typePar, rtrn)
    }

  def typeParams: Parser[List[TypeNode]] = 
    (LeftParenToken ~> rep(types) <~ RightParenToken) ^^ {
      case typeParams => typeParams
    }

  // Parser for integer tokens
  def intExpNode: Parser[IntExpNode] = acceptMatch("integer token", { case IntToken(value) => IntExpNode(value)})

  def varExpNode: Parser[VarExpNode] = acceptMatch("identfier token", { case IdentifierToken(name) => VarExpNode(Var(name))})

  def trueExpNode: Parser[TrueExpNode.type] = accept("true token", { case TrueToken => TrueExpNode})
  
  def falseExpNode: Parser[FalseExpNode.type] = accept("false token", { case FalseToken => FalseExpNode})


  // Recursive parser for expressions
  def exp: Parser[ExpNode] = intExpNode | opExp | varExpNode | trueExpNode | falseExpNode 

  // Parser for a simple expression like `(+ 1 2)` or nested expressions like `(+ 1 (+ 1 2))`
  def opExp: Parser[OpExpNode] = LeftParenToken ~> op ~ exp ~ exp <~ RightParenToken ^^ {
    case op ~ e1 ~ e2 => OpExpNode(op, e1, e2)
  }

  def stmt: Parser[StmtNode] = vardef | assert | retn | blockStmt | choiceStmt | callStmt
  
  def varDefName: Parser[Var] = acceptMatch("identifier token", { case IdentifierToken(name) => Var(name)})

  def vardef: Parser[VarDefStmtNode] = LeftParenToken ~> VarDefToken ~> types ~ varDefName ~ exp <~ RightParenToken ^^ {
    case t ~ vr ~ e1 => VarDefStmtNode(t, vr, e1)
  }

  def assert: Parser[AssertStmtNode] = LeftParenToken ~> AssertToken ~> exp <~ RightParenToken ^^ AssertStmtNode.apply


  def retn: Parser[ReturnStmtNode] = LeftParenToken ~> ReturnToken ~> exp <~ RightParenToken ^^ ReturnStmtNode.apply

  def blockStmt: Parser[BlockStmtNode] = LeftParenToken ~> BlockToken ~> rep(stmt) <~ RightParenToken ^^ BlockStmtNode.apply

  def choiceStmt: Parser[ChoiceStmtNode] = LeftParenToken ~> ChoiceToken ~> stmt ~ stmt <~ RightParenToken ^^ {
    case stmt1 ~ stmt2 => ChoiceStmtNode(stmt1, stmt2)
  }
  
  def callStmt: Parser[CallStmtNode] = LeftParenToken ~> CallToken ~> varDefName ~ rep(exp) <~ RightParenToken ^^ {
    case varName ~ params => CallStmtNode(varName, params)
  }

  def funcDef: Parser[FuncDefNode] = 
    (LeftParenToken ~> FuncDefToken ~> varDefName ~ paramsList ~ types ~ stmt <~ RightParenToken) ^^ {
      case idnt ~ params ~ rtntype ~ body => FuncDefNode(idnt.name, params, rtntype, body)
    }

  def paramsList: Parser[List[(TypeNode, Var)]] = LeftParenToken ~> rep(paramPair) <~ RightParenToken

  def paramPair: Parser[(TypeNode, Var)] = types ~ varDefName ^^ {
      case tpe ~ vr => (tpe, vr)
    }

  def prgrm: Parser[ProgramNode] = rep(funcDef) ^^ ProgramNode.apply
}
