import TokenizerTools._
import ParserTools._
import TypeCheckerTools._
import CodeGeneratorTools._
//(def f (int x (=> (int) int) transform) int (return (call transform x)))
//(def f (int X) int (return X))
//(def f (int X) int (block (return X)))
//(def f (int X) int (choice (return X) (return X)))
@main def Main(): Unit =
  val tokens = TokenizerTools.Tokenizer.tokenize("(def f (int X) int (choice (return X) (return X)))")
  val reader = TokenReader(tokens)
  val result = ParserTools.Parser.prgrm(reader)
//(=> (int y) int)
//
  result match {
    case Parser.Success(prgrmNode, _) => { 
      if (!prgrmNode.fncs.isEmpty) {
        //TypeChecker.typeCheck(prgrmNode)
        print("\n")
        print(CodeGenerator.codeGenFuncDef(prgrmNode.fncs.head))
        print("\n")
      } else {
        throw new Exception("Parse Error")
      }
      
    }
    case Parser.Failure(msg, _) => throw new Exception(msg)
    case Parser.Error(msg, _) => throw new Exception(msg)
  }
  

