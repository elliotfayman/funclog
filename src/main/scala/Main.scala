import TokenizerTools._
import ParserTools._
@main def Main(): Unit =
  val tokens = TokenizerTools.Tokenizer.tokenize("(def foo (int x int y) int (return (+ x y)))")
  val reader = TokenReader(tokens)
  val result = ParserTools.Parser.prgrm(reader)
  print(result)

