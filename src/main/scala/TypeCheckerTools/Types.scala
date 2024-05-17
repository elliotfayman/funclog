package TypeCheckerTools

sealed trait Type

//types
case object IntType extends Type
case object BooleanType extends Type
case class FunctionType(param: List[Type], rtn: Type) extends Type

sealed trait ReturnAnalysis
case object NoReturn extends ReturnAnalysis
case object MaybeReturns extends ReturnAnalysis
case object DefinitelyReturns extends ReturnAnalysis

case class TypeErrorException(msg: String) extends Exception(msg)