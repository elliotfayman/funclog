package ParserTools

sealed trait Node

//Type Nodes
sealed trait TypeNode extends Node
case object IntTypeNode extends TypeNode
case object BooleanTypeNode extends TypeNode
case class FunctionTypeNode(param: List[TypeNode], rtn: TypeNode) extends TypeNode

//Operator Nodes
sealed trait OperatorNode extends Node
case object PlusOperatorNode extends OperatorNode
case object MinusOperatorNode extends OperatorNode
case object MultiplyOperatorNode extends OperatorNode
case object DivideOperatorNode extends OperatorNode
case object LessThanOperatorNode extends OperatorNode
case object GreaterThanOperatorNode extends OperatorNode
case object DoubleEqualsOperatorNode extends OperatorNode

sealed trait ExpNode extends Node
case class VarExpNode(vr: Var) extends ExpNode
case class IntExpNode(value: Int) extends ExpNode
case object TrueExpNode extends ExpNode
case object FalseExpNode extends ExpNode
case class OpExpNode(op: OperatorNode, e1: ExpNode, e2: ExpNode) extends ExpNode
//case class FuncExpNode(params: List[ExpNode], stmt: StmtNode) extends ExpNode

sealed trait StmtNode extends Node
case class VarDefStmtNode(t: TypeNode, vr: Var, value: ExpNode) extends StmtNode
case class AssertStmtNode(cond: ExpNode) extends StmtNode
case class ReturnStmtNode(value: ExpNode) extends StmtNode
case class BlockStmtNode(stmts: List[StmtNode]) extends StmtNode
case class ChoiceStmtNode(c1: StmtNode, c2: StmtNode) extends StmtNode
case class CallStmtNode(vr: Var, params: List[ExpNode]) extends StmtNode

case class Var(name: String)

case class FuncDefNode(name: String, params: List[(TypeNode, Var)], rtntype: TypeNode, stmt: StmtNode) extends Node

case class ProgramNode(fncs: List[FuncDefNode]) extends Node