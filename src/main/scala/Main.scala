import TokenizerTools._
import ParserTools._
import TypeCheckerTools._
import CodeGeneratorTools._
import scala.io.Source
import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets

//(def f (int x (=> (int) int) transform) int (return (call transform x)))
//(def f (int X) int (return X))
//(def f (int X) int (block (return X)))
//(def f (int X) int (choice (return X) (return X)))
//(def f (int X) int (choice (block (var ( =>  (int) int) Z ( =>  (int Inner) (return Inner))) (var (=> ((=> (int) int)) int) Y ( => (( =>  (int) int) Func) (return (call Func (5) ) ) ) ) (return (call Y (Z)))) (return X)))


@main def Main(filename: String): Unit = {
  // Read the file content
  println(filename)
  val fileContent = Source.fromFile(filename).getLines.mkString("\n")
  val tokens = Tokenizer.tokenize(fileContent)
  val reader = TokenReader(tokens)
  val result = Parser.prgrm(reader)

  result match {
    case Parser.Success(prgrmNode, _) => {
      if (prgrmNode.fncs.nonEmpty) {
        TypeChecker.typeCheck(prgrmNode)
        val output = CodeGenerator.codeGenProg(prgrmNode)
        // Write the result to a new .pl file
        val outputFile = filename.replaceAll("\\.funclog$", ".pl")
        Files.write(Paths.get(outputFile), output.getBytes(StandardCharsets.UTF_8))
        println(s"Output written to $outputFile")
      } else {
        throw new Exception("Parse Error: No functions defined")
      }
    }
    case Parser.Failure(msg, _) => throw new Exception("Parsing failed: " + msg)
    case Parser.Error(msg, _) => throw new Exception("Parsing error: " + msg)
  }
}
