package TypeCheckerTools

sealed trait Type

//types
case object IntType extends Type
case object BooleanType extends Type
case class FunctionType(param: List[Type], rtn: Type) extends Type

/*
 int function(int x) {
 if (x ==0) return o
 else return function(x+1)

}

return function()
*/