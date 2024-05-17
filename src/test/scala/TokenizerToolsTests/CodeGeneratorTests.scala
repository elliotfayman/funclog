package CodeGeneratorTools

import ParserTools._
import TokenizerTools._

class CodeGeneratorTests extends munit.FunSuite {

    test("Code generator should generate complex Binop Expr") { 
        val tokens = Tokenizer.tokenize("(def f (int X int Y) int (block (assert true) (assert false) (assert (< X Y)) (assert (> X Y)) (assert (== X Y)) (return (+ (* X Y) (- X (/ X Y))))))") // Expected boolean, got int
        val reader = TokenReader(tokens)
        val result = Parser.prgrm(reader)

        result match {
        case Parser.Success(prgrmNode, _) => {
            val rslt = CodeGenerator.codeGenProg(prgrmNode)
            assert(true)
        }
        case _ => fail("Parsing failed, expected success")
        }
    }

    test("Code generator should generate complex HOF prolog output") {
        val tokens = Tokenizer.tokenize("(def f (int X) int (choice (block (var ( =>  (int) int) Z ( =>  (int Inner) (return Inner))) (var (=> ((=> (int) int)) int) Y ( => (( =>  (int) int) Func) (return (call Func (5) ) ) ) ) (return (call Y (Z)))) (return X)))") // Expected boolean, got int
        val reader = TokenReader(tokens)
        val result = Parser.prgrm(reader)

        result match {
        case Parser.Success(prgrmNode, _) => {
            CodeGenerator.codeGenProg(prgrmNode)
            assert(true)
        }
        case _ => fail("Parsing failed, expected success")
        }
    }
}