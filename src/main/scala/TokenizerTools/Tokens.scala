package TokenizerTools

sealed trait Token

//Basic Data types
case class IntToken(value: Int) extends Token
case object TrueToken extends Token
case object FalseToken extends Token

//Identifiers
case class IdentifierToken(value: String) extends Token

//Operators
case object PlusToken extends Token
case object MinusToken extends Token
case object MultiplyToken extends Token
case object DivideToken extends Token
case object LeftParenToken extends Token
case object RightParenToken extends Token
case object EqualsToken extends Token
case object DoubleEqualsToken extends Token
case object ArrowToken extends Token
case object LessThanToken extends Token
case object GreaterThanToken extends Token

//Reserved words
case object AssertToken extends Token
case object ChoiceToken extends Token
case object ReturnToken extends Token
case object CallToken extends Token
case object BlockToken extends Token
case object FuncDefToken extends Token
case object VarDefToken extends Token

// Tokenization Exception
class TokenizationException(message: String) extends Exception(message)
