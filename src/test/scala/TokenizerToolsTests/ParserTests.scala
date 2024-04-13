// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
package ParserTools

import TokenizerTools._

class TokenizerTests extends munit.FunSuite {
    test("Testing Generation of function with int return") {
        val tokens = List(LeftParenToken, FuncDefToken, IdentifierToken("func"), LeftParenToken, RightParenToken, IntTypeToken, LeftParenToken, ReturnToken, IntToken(1), RightParenToken, RightParenToken)
        val reader = TokenReader(tokens)
        val parseResult = ParserTools.Parser.prgrm(reader)
        val expected = ProgramNode(List(FuncDefNode("func", List(), IntTypeNode, ReturnStmtNode(IntExpNode(1)))))

        val obtained = parseResult match {
            case Parser.Success(prgrmNode, _) => prgrmNode
            case _ => None
        }

        assertEquals(obtained, expected)
    }

    test("Testing Generation of function with boolean true return") {
        val tokens = List(LeftParenToken, FuncDefToken, IdentifierToken("func"), LeftParenToken, RightParenToken, BooleanTypeToken, LeftParenToken, ReturnToken, TrueToken, RightParenToken, RightParenToken)
        val reader = TokenReader(tokens)
        val parseResult = ParserTools.Parser.prgrm(reader)
        val expected = ProgramNode(List(FuncDefNode("func", List(), BooleanTypeNode, ReturnStmtNode(TrueExpNode))))

        val obtained = parseResult match {
            case Parser.Success(prgrmNode, _) => prgrmNode
            case _ => None
        }

        assertEquals(obtained, expected)
    }

    test("Testing Generation of function with boolean false return") {
        val tokens = List(LeftParenToken, FuncDefToken, IdentifierToken("func"), LeftParenToken, RightParenToken, BooleanTypeToken, LeftParenToken, ReturnToken, FalseToken, RightParenToken, RightParenToken)
        val reader = TokenReader(tokens)
        val parseResult = ParserTools.Parser.prgrm(reader)
        val expected = ProgramNode(List(FuncDefNode("func", List(), BooleanTypeNode, ReturnStmtNode(FalseExpNode))))

        val obtained = parseResult match {
            case Parser.Success(prgrmNode, _) => prgrmNode
            case _ => None
        }

        assertEquals(obtained, expected)
    }

    test("Testing Generation of function with boolean false return") {
        val tokens = List(LeftParenToken, FuncDefToken, IdentifierToken("func"), LeftParenToken, RightParenToken, BooleanTypeToken, LeftParenToken, ReturnToken, LeftParenToken, CallToken, IdentifierToken("func2"), LeftParenToken, RightParenToken, RightParenToken, RightParenToken, RightParenToken)
        val reader = TokenReader(tokens)
        val parseResult = ParserTools.Parser.prgrm(reader)
        val expected = ProgramNode(List(FuncDefNode("func", List(), BooleanTypeNode, ReturnStmtNode(CallExpNode(Var("func2"), List())))))

        val obtained = parseResult match {
            case Parser.Success(prgrmNode, _) => prgrmNode
            case _ => None
        }

        assertEquals(obtained, expected)
    }

    test("Testing Generation of function with Function return") {
        val tokens = List(LeftParenToken, FuncDefToken, IdentifierToken("func"), LeftParenToken, RightParenToken, LeftParenToken, ArrowToken, LeftParenToken, RightParenToken, IntTypeToken, RightParenToken, LeftParenToken, ReturnToken, LeftParenToken, ArrowToken, LeftParenToken, RightParenToken, LeftParenToken, ReturnToken, IntToken(1), RightParenToken, RightParenToken, RightParenToken, RightParenToken)
        val reader = TokenReader(tokens)
        val parseResult = ParserTools.Parser.prgrm(reader)
        val expected = ProgramNode(List(FuncDefNode("func", List(), FunctionTypeNode(List(), IntTypeNode), ReturnStmtNode(FuncExpNode(List(), ReturnStmtNode(IntExpNode(1)))))))

        val obtained = parseResult match {
            case Parser.Success(prgrmNode, _) => prgrmNode
            case _ => None
        }
        assertEquals(obtained, expected)
    }

    test("Testing Generation of function with int Param") {
        val tokens = List(LeftParenToken, FuncDefToken, IdentifierToken("func"), LeftParenToken, IntTypeToken, IdentifierToken("X"), RightParenToken, IntTypeToken, LeftParenToken, ReturnToken, IdentifierToken("X"), RightParenToken, RightParenToken)
        val reader = TokenReader(tokens)
        val parseResult = ParserTools.Parser.prgrm(reader)
        val expected = ProgramNode(List(FuncDefNode("func", List((IntTypeNode, Var("X"))), IntTypeNode, ReturnStmtNode(VarExpNode(Var("X"))))))

        val obtained = parseResult match {
            case Parser.Success(prgrmNode, _) => prgrmNode
            case _ => None
        }
        assertEquals(obtained, expected)
    }

    test("Testing Generation of function with int Param") {
        val tokens = List(LeftParenToken, FuncDefToken, IdentifierToken("func"), LeftParenToken, IntTypeToken, IdentifierToken("X"), RightParenToken, IntTypeToken, LeftParenToken, ReturnToken, IdentifierToken("X"), RightParenToken, RightParenToken)
        val reader = TokenReader(tokens)
        val parseResult = ParserTools.Parser.prgrm(reader)
        val expected = ProgramNode(List(FuncDefNode("func", List((IntTypeNode, Var("X"))), IntTypeNode, ReturnStmtNode(VarExpNode(Var("X"))))))

        val obtained = parseResult match {
            case Parser.Success(prgrmNode, _) => prgrmNode
            case _ => None
        }
        assertEquals(obtained, expected)
    }

    test("Testing Generation of function with Plus Binop") {
        val tokens = List(LeftParenToken, FuncDefToken, IdentifierToken("func"), LeftParenToken, IntTypeToken, IdentifierToken("X"), RightParenToken, IntTypeToken, LeftParenToken, ReturnToken, LeftParenToken, PlusToken, IntToken(1), IntToken(2), RightParenToken, RightParenToken, RightParenToken)
        val reader = TokenReader(tokens)
        val parseResult = ParserTools.Parser.prgrm(reader)
        val expected = ProgramNode(List(FuncDefNode("func", List((IntTypeNode, Var("X"))), IntTypeNode, ReturnStmtNode(OpExpNode(PlusOperatorNode, IntExpNode(1), IntExpNode(2))))))

        val obtained = parseResult match {
            case Parser.Success(prgrmNode, _) => prgrmNode
            case _ => None
        }
        assertEquals(obtained, expected)
    }

    test("Testing Generation of function with Minus Binop") {
        val tokens = List(LeftParenToken, FuncDefToken, IdentifierToken("func"), LeftParenToken, IntTypeToken, IdentifierToken("X"), RightParenToken, IntTypeToken, LeftParenToken, ReturnToken, LeftParenToken, MinusToken, IntToken(1), IntToken(2), RightParenToken, RightParenToken, RightParenToken)
        val reader = TokenReader(tokens)
        val parseResult = ParserTools.Parser.prgrm(reader)
        val expected = ProgramNode(List(FuncDefNode("func", List((IntTypeNode, Var("X"))), IntTypeNode, ReturnStmtNode(OpExpNode(MinusOperatorNode, IntExpNode(1), IntExpNode(2))))))

        val obtained = parseResult match {
            case Parser.Success(prgrmNode, _) => prgrmNode
            case _ => None
        }
        assertEquals(obtained, expected)
    }

    test("Testing Generation of function with Multiply Binop") {
        val tokens = List(LeftParenToken, FuncDefToken, IdentifierToken("func"), LeftParenToken, IntTypeToken, IdentifierToken("X"), RightParenToken, IntTypeToken, LeftParenToken, ReturnToken, LeftParenToken, MultiplyToken, IntToken(1), IntToken(2), RightParenToken, RightParenToken, RightParenToken)
        val reader = TokenReader(tokens)
        val parseResult = ParserTools.Parser.prgrm(reader)
        val expected = ProgramNode(List(FuncDefNode("func", List((IntTypeNode, Var("X"))), IntTypeNode, ReturnStmtNode(OpExpNode(MultiplyOperatorNode, IntExpNode(1), IntExpNode(2))))))

        val obtained = parseResult match {
            case Parser.Success(prgrmNode, _) => prgrmNode
            case _ => None
        }
        assertEquals(obtained, expected)
    }

    test("Testing Generation of function with Divide Binop") {
        val tokens = List(LeftParenToken, FuncDefToken, IdentifierToken("func"), LeftParenToken, IntTypeToken, IdentifierToken("X"), RightParenToken, IntTypeToken, LeftParenToken, ReturnToken, LeftParenToken, DivideToken, IntToken(1), IntToken(2), RightParenToken, RightParenToken, RightParenToken)
        val reader = TokenReader(tokens)
        val parseResult = ParserTools.Parser.prgrm(reader)
        val expected = ProgramNode(List(FuncDefNode("func", List((IntTypeNode, Var("X"))), IntTypeNode, ReturnStmtNode(OpExpNode(DivideOperatorNode, IntExpNode(1), IntExpNode(2))))))

        val obtained = parseResult match {
            case Parser.Success(prgrmNode, _) => prgrmNode
            case _ => None
        }
        assertEquals(obtained, expected)
    }

    test("Testing Generation of function with double equals Binop") {
        val tokens = List(LeftParenToken, FuncDefToken, IdentifierToken("func"), LeftParenToken, IntTypeToken, IdentifierToken("X"), RightParenToken, IntTypeToken, LeftParenToken, ReturnToken, LeftParenToken, DoubleEqualsToken, IntToken(1), IntToken(2), RightParenToken, RightParenToken, RightParenToken)
        val reader = TokenReader(tokens)
        val parseResult = ParserTools.Parser.prgrm(reader)
        val expected = ProgramNode(List(FuncDefNode("func", List((IntTypeNode, Var("X"))), IntTypeNode, ReturnStmtNode(OpExpNode(DoubleEqualsOperatorNode, IntExpNode(1), IntExpNode(2))))))

        val obtained = parseResult match {
            case Parser.Success(prgrmNode, _) => prgrmNode
            case _ => None
        }
        assertEquals(obtained, expected)
    }

    test("Testing Generation of function with less than Binop") {
        val tokens = List(LeftParenToken, FuncDefToken, IdentifierToken("func"), LeftParenToken, IntTypeToken, IdentifierToken("X"), RightParenToken, BooleanTypeToken, LeftParenToken, ReturnToken, LeftParenToken, LessThanToken, IntToken(1), IntToken(2), RightParenToken, RightParenToken, RightParenToken)
        val reader = TokenReader(tokens)
        val parseResult = ParserTools.Parser.prgrm(reader)
        val expected = ProgramNode(List(FuncDefNode("func", List((IntTypeNode, Var("X"))), BooleanTypeNode, ReturnStmtNode(OpExpNode(LessThanOperatorNode, IntExpNode(1), IntExpNode(2))))))

        val obtained = parseResult match {
            case Parser.Success(prgrmNode, _) => prgrmNode
            case _ => None
        }
        assertEquals(obtained, expected)
    }

    test("Testing Generation of function with greater than Binop") {
        val tokens = List(LeftParenToken, FuncDefToken, IdentifierToken("func"), LeftParenToken, IntTypeToken, IdentifierToken("X"), RightParenToken, BooleanTypeToken, LeftParenToken, ReturnToken, LeftParenToken, GreaterThanToken, IntToken(1), IntToken(2), RightParenToken, RightParenToken, RightParenToken)
        val reader = TokenReader(tokens)
        val parseResult = ParserTools.Parser.prgrm(reader)
        val expected = ProgramNode(List(FuncDefNode("func", List((IntTypeNode, Var("X"))), BooleanTypeNode, ReturnStmtNode(OpExpNode(GreaterThanOperatorNode, IntExpNode(1), IntExpNode(2))))))

        val obtained = parseResult match {
            case Parser.Success(prgrmNode, _) => prgrmNode
            case _ => None
        }
        assertEquals(obtained, expected)
    }

    test("Testing Generation of function with assert") {
        val tokens = List(LeftParenToken, FuncDefToken, IdentifierToken("func"), LeftParenToken, IntTypeToken, IdentifierToken("X"), RightParenToken, BooleanTypeToken, LeftParenToken, AssertToken, LeftParenToken, GreaterThanToken, IntToken(1), IntToken(2), RightParenToken, RightParenToken, RightParenToken)
        val reader = TokenReader(tokens)
        val parseResult = ParserTools.Parser.prgrm(reader)
        val expected = ProgramNode(List(FuncDefNode("func", List((IntTypeNode, Var("X"))), BooleanTypeNode, AssertStmtNode(OpExpNode(GreaterThanOperatorNode, IntExpNode(1), IntExpNode(2))))))

        val obtained = parseResult match {
            case Parser.Success(prgrmNode, _) => prgrmNode
            case _ => None
        }
        assertEquals(obtained, expected)
    }

    test("Testing Generation of function with var def") {
        val tokens = List(LeftParenToken, FuncDefToken, IdentifierToken("func"), LeftParenToken, IntTypeToken, IdentifierToken("X"), RightParenToken, BooleanTypeToken, LeftParenToken, VarDefToken, IntTypeToken, IdentifierToken("X"), IntToken(1), RightParenToken, RightParenToken)
        val reader = TokenReader(tokens)
        val parseResult = ParserTools.Parser.prgrm(reader)
        val expected = ProgramNode(List(FuncDefNode("func", List((IntTypeNode, Var("X"))), BooleanTypeNode, VarDefStmtNode(IntTypeNode, Var("X"), IntExpNode(1)))))

        val obtained = parseResult match {
            case Parser.Success(prgrmNode, _) => prgrmNode
            case _ => None
        }
        assertEquals(obtained, expected)
    }

    test("Testing Generation of function with choice") {
        val tokens = List(LeftParenToken, FuncDefToken, IdentifierToken("func"), LeftParenToken, IntTypeToken, IdentifierToken("X"), RightParenToken, BooleanTypeToken, LeftParenToken, ChoiceToken, LeftParenToken, ReturnToken, IntToken(1), RightParenToken, LeftParenToken, ReturnToken, IntToken(2), RightParenToken, RightParenToken, RightParenToken)
        val reader = TokenReader(tokens)
        val parseResult = ParserTools.Parser.prgrm(reader)
        val expected = ProgramNode(List(FuncDefNode("func", List((IntTypeNode, Var("X"))), BooleanTypeNode, ChoiceStmtNode(ReturnStmtNode(IntExpNode(1)), ReturnStmtNode(IntExpNode(2))))))

        val obtained = parseResult match {
            case Parser.Success(prgrmNode, _) => prgrmNode
            case _ => None
        }
        assertEquals(obtained, expected)
    }

    test("Testing Generation of function with block") {
        val tokens = List(LeftParenToken, FuncDefToken, IdentifierToken("func"), LeftParenToken, IntTypeToken, IdentifierToken("X"), RightParenToken, BooleanTypeToken, LeftParenToken, BlockToken, LeftParenToken, AssertToken, TrueToken, RightParenToken, LeftParenToken, ReturnToken, IntToken(2), RightParenToken, RightParenToken, RightParenToken)
        val reader = TokenReader(tokens)
        val parseResult = ParserTools.Parser.prgrm(reader)
        val expected = ProgramNode(List(FuncDefNode("func", List((IntTypeNode, Var("X"))), BooleanTypeNode, BlockStmtNode(List(AssertStmtNode(TrueExpNode), ReturnStmtNode(IntExpNode(2)))))))

        val obtained = parseResult match {
            case Parser.Success(prgrmNode, _) => prgrmNode
            case _ => None
        }
        assertEquals(obtained, expected)
    }
}