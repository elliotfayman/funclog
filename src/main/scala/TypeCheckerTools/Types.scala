package TypeCheckerTools

sealed trait Type

//types
case object IntType extends Type
case object BooleanType extends Type
case class FunctionType(param: List[Type], rtn: Type) extends Type

