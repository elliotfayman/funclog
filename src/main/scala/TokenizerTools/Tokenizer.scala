package TokenizerTools

import scala.annotation.tailrec

object Tokenizer {

  val reservedWordMap = Map(
    "assert" -> AssertToken,
    "choice" -> ChoiceToken,
    "return" -> ReturnToken,
    "call" -> CallToken,
    "block" -> BlockToken,
    "def" -> FuncDefToken,
    "var" -> VarDefToken
  )

  def tokenize(source: String): List[Token] = {
    RecTokinize(source.toList, List.empty[Token])
  }

  @tailrec
  def RecTokinize(input: List[Char], accum: List[Token]): List[Token] = {
    val (_, inputWithoutLeadingSpaces) = takeWhileAndGetAfter(input)(_.isWhitespace)
    if (inputWithoutLeadingSpaces.isEmpty) {
      accum
    } else {
      val (token, rest) = readToken(inputWithoutLeadingSpaces)
      RecTokinize(rest, accum :+ token)
    }
  }
  
  def readToken(input: List[Char]): (Token, List[Char]) = {
    
    tokenizeSymbol(input)
      .orElse(tokenizeInteger(input))
      .orElse(tokenizeWord(input))
      .getOrElse(throw new TokenizationException("Unrecognized token"))
  }
  
  def tokenizeSymbol(input: List[Char]): Option[(Token, List[Char])] = {
    input match {
      case '=' :: '=' :: rest => Some((DoubleEqualsToken), rest)
      case '=' :: '>' :: rest => Some((ArrowToken), rest)
      case '='        :: rest => Some((EqualsToken), rest)
      case '+'        :: rest => Some((PlusToken), rest)
      case '-'        :: rest => Some((MinusToken), rest)
      case '*'        :: rest => Some((MultiplyToken), rest)
      case '/'        :: rest => Some((DivideToken), rest)
      case '('        :: rest => Some((LeftParenToken), rest)
      case ')'        :: rest => Some((RightParenToken), rest)
      case '<'        :: rest => Some((LessThanToken), rest)
      case '>'        :: rest => Some((GreaterThanToken), rest)
      case _                  => None
    }
  }
  
  def tokenizeInteger(input: List[Char]): Option[(IntToken, List[Char])] = {
    val (num, rest) = takeWhileAndGetAfter(input)(_.isDigit)
    if (num.nonEmpty) {
      rest match {
        case c :: other if c.isLetter => throw TokenizationException("Numbers cannot directly be followed by Charecters")
        case _ => Some((IntToken(num.mkString.toInt), rest))
      }
    } else {
      None
    }
  }

  def tokenizeWord(input: List[Char]): Option[(Token, List[Char])] = {
    input match {
      case c :: rest if c.isLetter => {
        val (wrd, rest) = takeWhileAndGetAfter(input)(_.isLetterOrDigit)
        val word = wrd.mkString
        val token = reservedWordMap.get(word).getOrElse(IdentifierToken(word))
        Some((token, rest))
      }
      case _ => None
    }
  }
  
  def takeWhileAndGetAfter[A](input: List[A])(check: (A) => Boolean): (List[A], List[A]) = {
    val take = input.takeWhile(check)
    (take, input.drop(take.size))
  }
}