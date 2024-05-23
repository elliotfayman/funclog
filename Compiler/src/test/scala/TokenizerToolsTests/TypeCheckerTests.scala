// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
package TypeCheckerTools

import ParserTools._
import TokenizerTools._

class TypeCheckerTests extends munit.FunSuite {
    test("Type checker detects simple function return mismatch") {
        val tokens = Tokenizer.tokenize("(def f (int X) bool (return X))") // Expected boolean, got int
        val reader = TokenReader(tokens)
        val result = Parser.prgrm(reader)
        
        result match {
        case Parser.Success(prgrmNode, _) =>
            intercept[TypeErrorException] {
            TypeChecker.typeCheck(prgrmNode) // Should throw TypeMismatchException
            }
        case _ => fail("Parsing failed, expected success")
        }
    }

    test("Type checker detects two functions with same name") {
        val tokens = Tokenizer.tokenize("(def f (int X) int (return X)) (def f (int X) bool (return X))") // Expected boolean, got int
        val reader = TokenReader(tokens)
        val result = Parser.prgrm(reader)
        
        result match {
        case Parser.Success(prgrmNode, _) =>
            intercept[TypeErrorException] {
            TypeChecker.typeCheck(prgrmNode) // Should throw TypeMismatchException
            }
        case _ => fail("Parsing failed, expected success")
        }
    }

    test("Type checker detects variable not in scope") {
        val tokens = Tokenizer.tokenize("(def f (int X) int (return Z))") // Expected boolean, got int
        val reader = TokenReader(tokens)
        val result = Parser.prgrm(reader)
        
        result match {
        case Parser.Success(prgrmNode, _) =>
            intercept[TypeErrorException] {
            TypeChecker.typeCheck(prgrmNode) // Should throw TypeMismatchException
            }
        case _ => fail("Parsing failed, expected success")
        }
    }

    test("Type checker detects op expression with mismatch types") {
        val tokens = Tokenizer.tokenize("(def f (int X) int (return (+ X true)))") // Expected boolean, got int
        val reader = TokenReader(tokens)
        val result = Parser.prgrm(reader)
        
        result match {
        case Parser.Success(prgrmNode, _) =>
            intercept[TypeErrorException] {
            TypeChecker.typeCheck(prgrmNode) // Should throw TypeMismatchException
            }
        case _ => fail("Parsing failed, expected success")
        }
    }

    test("Type checker succefully checks complex binop") { 
        val tokens = Tokenizer.tokenize("(def f (int X) int (return (+ X (* (- 1 2) (/ 4 5)))))")
        val reader = TokenReader(tokens)
        val result = Parser.prgrm(reader)
        
        result match {
        case Parser.Success(prgrmNode, _) =>
            TypeChecker.typeCheck(prgrmNode)
            assert(true) // Pass if no exceptions
        case _ => fail("Parsing failed, expected success")
        }
    }

    test("Type checker detects two of the same vars declared in same scope") {
        val tokens = Tokenizer.tokenize("(def f (int X) int (block (var int Y 1) (var int Y 2) (return X)))") // Expected boolean, got int
        val reader = TokenReader(tokens)
        val result = Parser.prgrm(reader)
        
        result match {
        case Parser.Success(prgrmNode, _) =>
            intercept[TypeErrorException] {
            TypeChecker.typeCheck(prgrmNode) // Should throw TypeMismatchException
            }
        case _ => fail("Parsing failed, expected success")
        }
    }

    test("Type checker detects assert expression is not type bool") {
        val tokens = Tokenizer.tokenize("(def f (int X) int (block (assert 2) (return X)))") // Expected boolean, got int
        val reader = TokenReader(tokens)
        val result = Parser.prgrm(reader)
        
        result match {
        case Parser.Success(prgrmNode, _) =>
            intercept[TypeErrorException] {
            TypeChecker.typeCheck(prgrmNode) // Should throw TypeMismatchException
            }
        case _ => fail("Parsing failed, expected success")
        }
    }

    test("Type checker detects single choice branch doesnt have return") {
        val tokens = Tokenizer.tokenize("(def f (int X) int (choice (assert 2) (return X)))") // Expected boolean, got int
        val reader = TokenReader(tokens)
        val result = Parser.prgrm(reader)
        
        result match {
        case Parser.Success(prgrmNode, _) =>
            intercept[TypeErrorException] {
            TypeChecker.typeCheck(prgrmNode) // Should throw TypeMismatchException
            }
        case _ => fail("Parsing failed, expected success")
        }
    }
    

    test("Type checker passes multiple complex binop asserts") {
        val tokens = Tokenizer.tokenize("(def f (int X) int (block (assert (< 2 3)) (assert (== 2 2)) (assert (> 3 2)) (return X)))") // Expected boolean, got int
        val reader = TokenReader(tokens)
        val result = Parser.prgrm(reader)
        
        result match {
        case Parser.Success(prgrmNode, _) => {
            TypeChecker.typeCheck(prgrmNode) // Should throw TypeMismatchException
            assert(true)
        }
        case _ => fail("Parsing failed, expected success")
        }
    }

    test("Type checker passes simple choice") {
        val tokens = Tokenizer.tokenize("(def f (int X) int (choice (return X) (return X)))") // Expected boolean, got int
        val reader = TokenReader(tokens)
        val result = Parser.prgrm(reader)
        
        result match {
        case Parser.Success(prgrmNode, _) => {
            TypeChecker.typeCheck(prgrmNode) // Should throw TypeMismatchException
            assert(true)
        }
        case _ => fail("Parsing failed, expected success")
        }
    }

    test("Type checker rejects var expecter type and given exp type") {
        val tokens = Tokenizer.tokenize("(def f (int X) int (choice (return X) (var int X true)))") // Expected boolean, got int
        val reader = TokenReader(tokens)
        val result = Parser.prgrm(reader)
        
        result match {
        case Parser.Success(prgrmNode, _) => {
            intercept[TypeErrorException] {
                TypeChecker.typeCheck(prgrmNode) // Should throw TypeMismatchException
            }
        }
        case _ => fail("Parsing failed, expected success")
        }
    }

    test("Type checker passes Function expression decleration and call") {
        val tokens = Tokenizer.tokenize("(def f (int X) int (choice (block (var ( =>  (int) int) Z ( =>  (int Inner) (return Inner))) (var (=> ((=> (int) int)) int) Y ( => (( =>  (int) int) Func) (return (call Func (5) ) ) ) ) (return (call Y (Z)))) (return X)))") // Expected boolean, got int
        val reader = TokenReader(tokens)
        val result = Parser.prgrm(reader)
        
        result match {
        case Parser.Success(prgrmNode, _) => {
            TypeChecker.typeCheck(prgrmNode) // Should throw TypeMismatchException
            assert(true)
        }
        case _ => fail("Parsing failed, expected success")
        }
    }

    test("Type checker rejects func expression func call param mismatch") {
        val tokens = Tokenizer.tokenize("(def f (int X) int (choice (block (var ( =>  (int) int) Z ( =>  (int Inner) (return Inner))) (var (=> ((=> (int) int)) int) Y ( => (( =>  (int) int) Func) (return (call Func (5) ) ) ) ) (return (call Y (Z 2)))) (return X)))") // Expected boolean, got int
        val reader = TokenReader(tokens)
        val result = Parser.prgrm(reader)
        
        result match {
        case Parser.Success(prgrmNode, _) => {
            intercept[TypeErrorException] {
                TypeChecker.typeCheck(prgrmNode) // Should throw TypeMismatchException
            }
        }
        case _ => fail("Parsing failed, expected success")
        }
    }

    test("Type checker rejects func call not func") {
        val tokens = Tokenizer.tokenize("(def f (int X) int (choice (block (var ( =>  (int) int) Z ( =>  (int Inner) (return Inner))) (var (=> ((=> (int) int)) int) Y ( => (( =>  (int) int) Func) (return (call Func (5) ) ) ) ) (return (call X (Z 2)))) (return X)))") // Expected boolean, got int
        val reader = TokenReader(tokens)
        val result = Parser.prgrm(reader)
        
        result match {
        case Parser.Success(prgrmNode, _) => {
            intercept[TypeErrorException] {
                TypeChecker.typeCheck(prgrmNode) // Should throw TypeMismatchException
            }
        }
        case _ => fail("Parsing failed, expected success")
        }
    }

    test("Type checker rejects func call var does not exist") {
        val tokens = Tokenizer.tokenize("(def f (int X) int (choice (block (var ( =>  (int) int) Z ( =>  (int Inner) (return Inner))) (var (=> ((=> (int) int)) int) Y ( => (( =>  (int) int) Func) (return (call Func (5) ) ) ) ) (return (call P (Z 2)))) (return X)))") // Expected boolean, got int
        val reader = TokenReader(tokens)
        val result = Parser.prgrm(reader)
        
        result match {
        case Parser.Success(prgrmNode, _) => {
            intercept[TypeErrorException] {
                TypeChecker.typeCheck(prgrmNode) // Should throw TypeMismatchException
            }
        }
        case _ => fail("Parsing failed, expected success")
        }
    }

    test("Type checker rejects becuase variable being called not a function type") {
        val tokens = Tokenizer.tokenize("(def f (int X) int (return (call X ())))") // Expected boolean, got int
        val reader = TokenReader(tokens)
        val result = Parser.prgrm(reader)
        
        result match {
        case Parser.Success(prgrmNode, _) => {
            intercept[TypeErrorException] {
                TypeChecker.typeCheck(prgrmNode) // Should throw TypeMismatchException
            }
        }
        case _ => fail("Parsing failed, expected success")
        }
    }

    test("Type checker rejects becuase singleton block must contain return") {
        val tokens = Tokenizer.tokenize("(def f (int X) int (block (assert true) (assert true)))") // Expected boolean, got int
        val reader = TokenReader(tokens)
        val result = Parser.prgrm(reader)
        
        result match {
        case Parser.Success(prgrmNode, _) => {
            intercept[TypeErrorException] {
                TypeChecker.typeCheck(prgrmNode) // Should throw TypeMismatchException
            }
        }
        case _ => fail("Parsing failed, expected success")
        }
    }

    test("Type checker rejects becuase singleton block must not contain multiple returns") {
        val tokens = Tokenizer.tokenize("(def f (int X) int (block (return 1) (return 2)))") // Expected boolean, got int
        val reader = TokenReader(tokens)
        val result = Parser.prgrm(reader)
        
        result match {
        case Parser.Success(prgrmNode, _) => {
            intercept[TypeErrorException] {
                TypeChecker.typeCheck(prgrmNode) // Should throw TypeMismatchException
            }
        }
        case _ => fail("Parsing failed, expected success")
        }
    }

    test("Type checker rejects func exp without returns") {
        val tokens = Tokenizer.tokenize("(def f (int X) int (choice (block (var ( =>  (int) int) Z ( =>  (int Inner) (block (assert true)))) (var (=> ((=> (int) int)) int) Y ( => (( =>  (int) int) Func) (return (call Func (5) ) ) ) ) (return (call Y (Z)))) (return X)))") // Expected boolean, got int
        val reader = TokenReader(tokens)
        val result = Parser.prgrm(reader)
        
        result match {
        case Parser.Success(prgrmNode, _) => {
            intercept[TypeErrorException] {
                TypeChecker.typeCheck(prgrmNode) // Should throw TypeMismatchException
            }
        }
        case _ => fail("Parsing failed, expected success")
        }
    }

    test("Type checker detects single choice branch doesnt have return v2") {
        val tokens = Tokenizer.tokenize("(def f (int X) int (choice (return 2) (assert true)))") // Expected boolean, got int
        val reader = TokenReader(tokens)
        val result = Parser.prgrm(reader)
        
        result match {
        case Parser.Success(prgrmNode, _) =>
            intercept[TypeErrorException] {
            TypeChecker.typeCheck(prgrmNode) // Should throw TypeMismatchException
            }
        case _ => fail("Parsing failed, expected success")
        }
    }

    test("Type checker detects function doesnt have return") {
        val tokens = Tokenizer.tokenize("(def f (int X) int (assert true))") // Expected boolean, got int
        val reader = TokenReader(tokens)
        val result = Parser.prgrm(reader)
        
        result match {
        case Parser.Success(prgrmNode, _) =>
            intercept[TypeErrorException] {
            TypeChecker.typeCheck(prgrmNode) // Should throw TypeMismatchException
            }
        case _ => fail("Parsing failed, expected success")
        }
    }

    test("Type checker choice branch must have equal return types") {
        val tokens = Tokenizer.tokenize("(def f (int X) int (choice (return true) (return 2)))") // Expected boolean, got int
        val reader = TokenReader(tokens)
        val result = Parser.prgrm(reader)
        
        result match {
        case Parser.Success(prgrmNode, _) =>
            intercept[TypeErrorException] {
            TypeChecker.typeCheck(prgrmNode) // Should throw TypeMismatchException
            }
        case _ => fail("Parsing failed, expected success")
        }
    }

    test("Type checker choice branch must have return type") {
        val tokens = Tokenizer.tokenize("(def f (int X) int (choice (assert true) (return 2)))") // Expected boolean, got int
        val reader = TokenReader(tokens)
        val result = Parser.prgrm(reader)
        
        result match {
        case Parser.Success(prgrmNode, _) =>
            intercept[TypeErrorException] {
            TypeChecker.typeCheck(prgrmNode) // Should throw TypeMismatchException
            }
        case _ => fail("Parsing failed, expected success")
        }
    }

    test("Type checker func expr must have return type") {
        val tokens = Tokenizer.tokenize("(def f (int X) int (block (var (=> () int) P (=> () (assert true))) (return 2)))") // Expected boolean, got int
        val reader = TokenReader(tokens)
        val result = Parser.prgrm(reader)
        
        result match {
        case Parser.Success(prgrmNode, _) =>
            intercept[TypeErrorException] {
            TypeChecker.typeCheck(prgrmNode) // Should throw TypeMismatchException
            }
        case _ => fail("Parsing failed, expected success")
        }
    }
    
}