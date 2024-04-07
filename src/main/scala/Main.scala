import TokenizerTools._
import ParserTools._
import TypeCheckerTools._
import CodeGeneratorTools._

@main def Main(): Unit =
  val tokens = TokenizerTools.Tokenizer.tokenize("(def foo (int x int y) int (return 1))")
  val reader = TokenReader(tokens)
  val result = ParserTools.Parser.prgrm(reader)

  result match {
    case Parser.Success(prgrmNode, _) => { 
      TypeChecker.typeCheck(prgrmNode)
      print("\n")
      print(CodeGenerator.codeGenFuncDef(prgrmNode.fncs.head))
      print("\n")
    }
    case Parser.Failure(msg, _) => throw new Exception(msg)
    case Parser.Error(msg, _) => throw new Exception(msg)
  }
  

