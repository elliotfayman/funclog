// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
package TokenizerTools

class TokenizerTests extends munit.FunSuite {
    test("Testing Tokenization of Mixed Tokens 0") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * / ( == reserved1 12355 ) = helloword < 5 > ident1231asdad =>")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DivideToken, LeftParenToken, DoubleEqualsToken, IdentifierToken("reserved1"), IntToken(12355), RightParenToken, EqualsToken, IdentifierToken("helloword"), LessThanToken, IntToken(5), GreaterThanToken, IdentifierToken("ident1231asdad"), ArrowToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 1") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * / ( > 12355 = ) => ident1231asdad reserved1 < == helloword 5")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DivideToken, LeftParenToken, GreaterThanToken, IntToken(12355), EqualsToken, RightParenToken, ArrowToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), LessThanToken, DoubleEqualsToken, IdentifierToken("helloword"), IntToken(5))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 2") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * / ( reserved1 == 5 ) < 12355 helloword ident1231asdad > => =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DivideToken, LeftParenToken, IdentifierToken("reserved1"), DoubleEqualsToken, IntToken(5), RightParenToken, LessThanToken, IntToken(12355), IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), GreaterThanToken, ArrowToken, EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 3") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * / ) ( = < == 5 reserved1 > ident1231asdad 12355 helloword =>")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DivideToken, RightParenToken, LeftParenToken, EqualsToken, LessThanToken, DoubleEqualsToken, IntToken(5), IdentifierToken("reserved1"), GreaterThanToken, IdentifierToken("ident1231asdad"), IntToken(12355), IdentifierToken("helloword"), ArrowToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 4") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * / ) == reserved1 ident1231asdad ( 12355 < 5 => = helloword >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DivideToken, RightParenToken, DoubleEqualsToken, IdentifierToken("reserved1"), IdentifierToken("ident1231asdad"), LeftParenToken, IntToken(12355), LessThanToken, IntToken(5), ArrowToken, EqualsToken, IdentifierToken("helloword"), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 5") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * / ) > 12355 => ( ident1231asdad == helloword reserved1 5 < =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DivideToken, RightParenToken, GreaterThanToken, IntToken(12355), ArrowToken, LeftParenToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), IntToken(5), LessThanToken, EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 6") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * / ) reserved1 == helloword ( ident1231asdad => < 12355 > 5 =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DivideToken, RightParenToken, IdentifierToken("reserved1"), DoubleEqualsToken, IdentifierToken("helloword"), LeftParenToken, IdentifierToken("ident1231asdad"), ArrowToken, LessThanToken, IntToken(12355), GreaterThanToken, IntToken(5), EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 7") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * / = ( ) 12355 => == < ident1231asdad 5 > helloword reserved1")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DivideToken, EqualsToken, LeftParenToken, RightParenToken, IntToken(12355), ArrowToken, DoubleEqualsToken, LessThanToken, IdentifierToken("ident1231asdad"), IntToken(5), GreaterThanToken, IdentifierToken("helloword"), IdentifierToken("reserved1"))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 8") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * / = == ident1231asdad ( => ) helloword reserved1 5 12355 > <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DivideToken, EqualsToken, DoubleEqualsToken, IdentifierToken("ident1231asdad"), LeftParenToken, ArrowToken, RightParenToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), IntToken(5), IntToken(12355), GreaterThanToken, LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 9") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * / = > 12355 5 ) == helloword => reserved1 < ident1231asdad (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DivideToken, EqualsToken, GreaterThanToken, IntToken(12355), IntToken(5), RightParenToken, DoubleEqualsToken, IdentifierToken("helloword"), ArrowToken, IdentifierToken("reserved1"), LessThanToken, IdentifierToken("ident1231asdad"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 10") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * / = reserved1 => ) == > ident1231asdad 5 < ( helloword 12355")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DivideToken, EqualsToken, IdentifierToken("reserved1"), ArrowToken, RightParenToken, DoubleEqualsToken, GreaterThanToken, IdentifierToken("ident1231asdad"), IntToken(5), LessThanToken, LeftParenToken, IdentifierToken("helloword"), IntToken(12355))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 11") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * / == ( ) reserved1 => 12355 5 helloword ident1231asdad > < =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DivideToken, DoubleEqualsToken, LeftParenToken, RightParenToken, IdentifierToken("reserved1"), ArrowToken, IntToken(12355), IntToken(5), IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), GreaterThanToken, LessThanToken, EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 12") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * / == = ident1231asdad => ) 5 12355 < reserved1 > helloword (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DivideToken, DoubleEqualsToken, EqualsToken, IdentifierToken("ident1231asdad"), ArrowToken, RightParenToken, IntToken(5), IntToken(12355), LessThanToken, IdentifierToken("reserved1"), GreaterThanToken, IdentifierToken("helloword"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 13") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * / == > 12355 ident1231asdad ) reserved1 => 5 = ( helloword <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DivideToken, DoubleEqualsToken, GreaterThanToken, IntToken(12355), IdentifierToken("ident1231asdad"), RightParenToken, IdentifierToken("reserved1"), ArrowToken, IntToken(5), EqualsToken, LeftParenToken, IdentifierToken("helloword"), LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 14") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * / == reserved1 => < ) helloword = ident1231asdad 5 12355 > (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DivideToken, DoubleEqualsToken, IdentifierToken("reserved1"), ArrowToken, LessThanToken, RightParenToken, IdentifierToken("helloword"), EqualsToken, IdentifierToken("ident1231asdad"), IntToken(5), IntToken(12355), GreaterThanToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 15") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * / => ( ) helloword < = > 12355 reserved1 5 ident1231asdad ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DivideToken, ArrowToken, LeftParenToken, RightParenToken, IdentifierToken("helloword"), LessThanToken, EqualsToken, GreaterThanToken, IntToken(12355), IdentifierToken("reserved1"), IntToken(5), IdentifierToken("ident1231asdad"), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 16") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * / => = ident1231asdad > == ) ( reserved1 12355 < helloword 5")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DivideToken, ArrowToken, EqualsToken, IdentifierToken("ident1231asdad"), GreaterThanToken, DoubleEqualsToken, RightParenToken, LeftParenToken, IdentifierToken("reserved1"), IntToken(12355), LessThanToken, IdentifierToken("helloword"), IntToken(5))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 17") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * / => > 5 ( == = helloword ident1231asdad reserved1 12355 < )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DivideToken, ArrowToken, GreaterThanToken, IntToken(5), LeftParenToken, DoubleEqualsToken, EqualsToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IntToken(12355), LessThanToken, RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 18") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * / => reserved1 == 12355 = < helloword ) 5 > ident1231asdad (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DivideToken, ArrowToken, IdentifierToken("reserved1"), DoubleEqualsToken, IntToken(12355), EqualsToken, LessThanToken, IdentifierToken("helloword"), RightParenToken, IntToken(5), GreaterThanToken, IdentifierToken("ident1231asdad"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 19") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * / < ( = == > 5 ident1231asdad reserved1 => ) helloword 12355")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DivideToken, LessThanToken, LeftParenToken, EqualsToken, DoubleEqualsToken, GreaterThanToken, IntToken(5), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), ArrowToken, RightParenToken, IdentifierToken("helloword"), IntToken(12355))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 20") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * / < = ident1231asdad 5 == 12355 > helloword reserved1 => ) (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DivideToken, LessThanToken, EqualsToken, IdentifierToken("ident1231asdad"), IntToken(5), DoubleEqualsToken, IntToken(12355), GreaterThanToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), ArrowToken, RightParenToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 21") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * / < > 5 = == ident1231asdad 12355 ) reserved1 => helloword (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DivideToken, LessThanToken, GreaterThanToken, IntToken(5), EqualsToken, DoubleEqualsToken, IdentifierToken("ident1231asdad"), IntToken(12355), RightParenToken, IdentifierToken("reserved1"), ArrowToken, IdentifierToken("helloword"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 22") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * / < reserved1 == ident1231asdad = helloword => 12355 ) ( 5 >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DivideToken, LessThanToken, IdentifierToken("reserved1"), DoubleEqualsToken, IdentifierToken("ident1231asdad"), EqualsToken, IdentifierToken("helloword"), ArrowToken, IntToken(12355), RightParenToken, LeftParenToken, IntToken(5), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 23") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * / > ( = < 12355 ) => helloword ident1231asdad reserved1 5 ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DivideToken, GreaterThanToken, LeftParenToken, EqualsToken, LessThanToken, IntToken(12355), RightParenToken, ArrowToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IntToken(5), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 24") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * / > = ident1231asdad helloword => ) == < 5 12355 reserved1 (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DivideToken, GreaterThanToken, EqualsToken, IdentifierToken("ident1231asdad"), IdentifierToken("helloword"), ArrowToken, RightParenToken, DoubleEqualsToken, LessThanToken, IntToken(5), IntToken(12355), IdentifierToken("reserved1"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 25") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * / > < 5 => == = ( ident1231asdad 12355 ) helloword reserved1")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DivideToken, GreaterThanToken, LessThanToken, IntToken(5), ArrowToken, DoubleEqualsToken, EqualsToken, LeftParenToken, IdentifierToken("ident1231asdad"), IntToken(12355), RightParenToken, IdentifierToken("helloword"), IdentifierToken("reserved1"))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 26") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * / > reserved1 => ( < == helloword ident1231asdad 5 12355 = )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DivideToken, GreaterThanToken, IdentifierToken("reserved1"), ArrowToken, LeftParenToken, LessThanToken, DoubleEqualsToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), IntToken(5), IntToken(12355), EqualsToken, RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 27") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * / 12355 ( = 5 < > helloword == reserved1 => ident1231asdad )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DivideToken, IntToken(12355), LeftParenToken, EqualsToken, IntToken(5), LessThanToken, GreaterThanToken, IdentifierToken("helloword"), DoubleEqualsToken, IdentifierToken("reserved1"), ArrowToken, IdentifierToken("ident1231asdad"), RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 28") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * / 12355 = helloword ) < 5 reserved1 > == ( ident1231asdad =>")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DivideToken, IntToken(12355), EqualsToken, IdentifierToken("helloword"), RightParenToken, LessThanToken, IntToken(5), IdentifierToken("reserved1"), GreaterThanToken, DoubleEqualsToken, LeftParenToken, IdentifierToken("ident1231asdad"), ArrowToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 29") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * / 12355 < 5 reserved1 == ident1231asdad => helloword > = ) (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DivideToken, IntToken(12355), LessThanToken, IntToken(5), IdentifierToken("reserved1"), DoubleEqualsToken, IdentifierToken("ident1231asdad"), ArrowToken, IdentifierToken("helloword"), GreaterThanToken, EqualsToken, RightParenToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 30") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * / 12355 reserved1 => = < helloword > ) 5 == ident1231asdad (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DivideToken, IntToken(12355), IdentifierToken("reserved1"), ArrowToken, EqualsToken, LessThanToken, IdentifierToken("helloword"), GreaterThanToken, RightParenToken, IntToken(5), DoubleEqualsToken, IdentifierToken("ident1231asdad"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 31") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * / 5 ( = ident1231asdad > ) < reserved1 => == helloword 12355")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DivideToken, IntToken(5), LeftParenToken, EqualsToken, IdentifierToken("ident1231asdad"), GreaterThanToken, RightParenToken, LessThanToken, IdentifierToken("reserved1"), ArrowToken, DoubleEqualsToken, IdentifierToken("helloword"), IntToken(12355))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 32") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * / 5 = helloword => > ) == ident1231asdad reserved1 12355 < (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DivideToken, IntToken(5), EqualsToken, IdentifierToken("helloword"), ArrowToken, GreaterThanToken, RightParenToken, DoubleEqualsToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IntToken(12355), LessThanToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 33") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * / 5 < 12355 helloword => = ) == reserved1 > ident1231asdad (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DivideToken, IntToken(5), LessThanToken, IntToken(12355), IdentifierToken("helloword"), ArrowToken, EqualsToken, RightParenToken, DoubleEqualsToken, IdentifierToken("reserved1"), GreaterThanToken, IdentifierToken("ident1231asdad"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 34") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * / 5 reserved1 => < > == ( ident1231asdad = ) helloword 12355")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DivideToken, IntToken(5), IdentifierToken("reserved1"), ArrowToken, LessThanToken, GreaterThanToken, DoubleEqualsToken, LeftParenToken, IdentifierToken("ident1231asdad"), EqualsToken, RightParenToken, IdentifierToken("helloword"), IntToken(12355))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 35") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * / reserved1 ( == ) 12355 > helloword ident1231asdad 5 < => =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DivideToken, IdentifierToken("reserved1"), LeftParenToken, DoubleEqualsToken, RightParenToken, IntToken(12355), GreaterThanToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), IntToken(5), LessThanToken, ArrowToken, EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 36") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * / reserved1 = helloword > < 12355 ident1231asdad ) => == 5 (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DivideToken, IdentifierToken("reserved1"), EqualsToken, IdentifierToken("helloword"), GreaterThanToken, LessThanToken, IntToken(12355), IdentifierToken("ident1231asdad"), RightParenToken, ArrowToken, DoubleEqualsToken, IntToken(5), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 37") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * / reserved1 < 5 ) > ident1231asdad 12355 => = ( helloword ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DivideToken, IdentifierToken("reserved1"), LessThanToken, IntToken(5), RightParenToken, GreaterThanToken, IdentifierToken("ident1231asdad"), IntToken(12355), ArrowToken, EqualsToken, LeftParenToken, IdentifierToken("helloword"), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 38") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * / reserved1 5 => 12355 < helloword == ident1231asdad > = ) (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DivideToken, IdentifierToken("reserved1"), IntToken(5), ArrowToken, IntToken(12355), LessThanToken, IdentifierToken("helloword"), DoubleEqualsToken, IdentifierToken("ident1231asdad"), GreaterThanToken, EqualsToken, RightParenToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 39") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * / ident1231asdad ( == => 5 ) 12355 < reserved1 > helloword =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DivideToken, IdentifierToken("ident1231asdad"), LeftParenToken, DoubleEqualsToken, ArrowToken, IntToken(5), RightParenToken, IntToken(12355), LessThanToken, IdentifierToken("reserved1"), GreaterThanToken, IdentifierToken("helloword"), EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 40") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * / ident1231asdad = helloword 5 > ) => 12355 == ( reserved1 <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DivideToken, IdentifierToken("ident1231asdad"), EqualsToken, IdentifierToken("helloword"), IntToken(5), GreaterThanToken, RightParenToken, ArrowToken, IntToken(12355), DoubleEqualsToken, LeftParenToken, IdentifierToken("reserved1"), LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 41") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * / ident1231asdad < 5 == 12355 = ) helloword reserved1 > => (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DivideToken, IdentifierToken("ident1231asdad"), LessThanToken, IntToken(5), DoubleEqualsToken, IntToken(12355), EqualsToken, RightParenToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), GreaterThanToken, ArrowToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 42") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * / ident1231asdad 5 => helloword > == ) = 12355 < reserved1 (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DivideToken, IdentifierToken("ident1231asdad"), IntToken(5), ArrowToken, IdentifierToken("helloword"), GreaterThanToken, DoubleEqualsToken, RightParenToken, EqualsToken, IntToken(12355), LessThanToken, IdentifierToken("reserved1"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 43") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * / helloword ( == > 5 12355 ) reserved1 => = ident1231asdad <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DivideToken, IdentifierToken("helloword"), LeftParenToken, DoubleEqualsToken, GreaterThanToken, IntToken(5), IntToken(12355), RightParenToken, IdentifierToken("reserved1"), ArrowToken, EqualsToken, IdentifierToken("ident1231asdad"), LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 44") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * / helloword == ( ) 5 12355 ident1231asdad reserved1 > < => =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DivideToken, IdentifierToken("helloword"), DoubleEqualsToken, LeftParenToken, RightParenToken, IntToken(5), IntToken(12355), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), GreaterThanToken, LessThanToken, ArrowToken, EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 45") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * / helloword < 5 > 12355 reserved1 ident1231asdad ) == = => (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DivideToken, IdentifierToken("helloword"), LessThanToken, IntToken(5), GreaterThanToken, IntToken(12355), IdentifierToken("reserved1"), IdentifierToken("ident1231asdad"), RightParenToken, DoubleEqualsToken, EqualsToken, ArrowToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 46") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * / helloword 5 < ) 12355 ident1231asdad > => = ( reserved1 ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DivideToken, IdentifierToken("helloword"), IntToken(5), LessThanToken, RightParenToken, IntToken(12355), IdentifierToken("ident1231asdad"), GreaterThanToken, ArrowToken, EqualsToken, LeftParenToken, IdentifierToken("reserved1"), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 47") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( / ) => reserved1 ident1231asdad = 12355 helloword 5 > < ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, DivideToken, RightParenToken, ArrowToken, IdentifierToken("reserved1"), IdentifierToken("ident1231asdad"), EqualsToken, IntToken(12355), IdentifierToken("helloword"), IntToken(5), GreaterThanToken, LessThanToken, DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 48") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( / => ) < ident1231asdad == 5 > reserved1 12355 helloword =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, DivideToken, ArrowToken, RightParenToken, LessThanToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken, IntToken(5), GreaterThanToken, IdentifierToken("reserved1"), IntToken(12355), IdentifierToken("helloword"), EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 49") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( / > reserved1 ident1231asdad 5 == => 12355 = ) helloword <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, DivideToken, GreaterThanToken, IdentifierToken("reserved1"), IdentifierToken("ident1231asdad"), IntToken(5), DoubleEqualsToken, ArrowToken, IntToken(12355), EqualsToken, RightParenToken, IdentifierToken("helloword"), LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 50") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( / reserved1 > => ident1231asdad < = helloword 5 12355 == )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, DivideToken, IdentifierToken("reserved1"), GreaterThanToken, ArrowToken, IdentifierToken("ident1231asdad"), LessThanToken, EqualsToken, IdentifierToken("helloword"), IntToken(5), IntToken(12355), DoubleEqualsToken, RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 51") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( ) / => helloword reserved1 12355 == < 5 > ident1231asdad =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, RightParenToken, DivideToken, ArrowToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), IntToken(12355), DoubleEqualsToken, LessThanToken, IntToken(5), GreaterThanToken, IdentifierToken("ident1231asdad"), EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 52") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( ) => / 12355 ident1231asdad reserved1 = 5 < == helloword >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, RightParenToken, ArrowToken, DivideToken, IntToken(12355), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), EqualsToken, IntToken(5), LessThanToken, DoubleEqualsToken, IdentifierToken("helloword"), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 53") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( ) > ident1231asdad / reserved1 5 helloword 12355 < => == =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, RightParenToken, GreaterThanToken, IdentifierToken("ident1231asdad"), DivideToken, IdentifierToken("reserved1"), IntToken(5), IdentifierToken("helloword"), IntToken(12355), LessThanToken, ArrowToken, DoubleEqualsToken, EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 54") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( ) reserved1 > 12355 ident1231asdad helloword 5 = => == < /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, RightParenToken, IdentifierToken("reserved1"), GreaterThanToken, IntToken(12355), IdentifierToken("ident1231asdad"), IdentifierToken("helloword"), IntToken(5), EqualsToken, ArrowToken, DoubleEqualsToken, LessThanToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 55") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( = / < == helloword ) reserved1 5 > => ident1231asdad 12355")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, EqualsToken, DivideToken, LessThanToken, DoubleEqualsToken, IdentifierToken("helloword"), RightParenToken, IdentifierToken("reserved1"), IntToken(5), GreaterThanToken, ArrowToken, IdentifierToken("ident1231asdad"), IntToken(12355))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 56") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( = => / reserved1 helloword == 12355 ident1231asdad 5 > < )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, EqualsToken, ArrowToken, DivideToken, IdentifierToken("reserved1"), IdentifierToken("helloword"), DoubleEqualsToken, IntToken(12355), IdentifierToken("ident1231asdad"), IntToken(5), GreaterThanToken, LessThanToken, RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 57") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( = > ident1231asdad == helloword => 12355 ) 5 < reserved1 /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, EqualsToken, GreaterThanToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken, IdentifierToken("helloword"), ArrowToken, IntToken(12355), RightParenToken, IntToken(5), LessThanToken, IdentifierToken("reserved1"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 58") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( = reserved1 > ident1231asdad helloword => == 12355 ) / 5 <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, EqualsToken, IdentifierToken("reserved1"), GreaterThanToken, IdentifierToken("ident1231asdad"), IdentifierToken("helloword"), ArrowToken, DoubleEqualsToken, IntToken(12355), RightParenToken, DivideToken, IntToken(5), LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 59") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( == / < > helloword 5 = ident1231asdad reserved1 12355 => )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, DoubleEqualsToken, DivideToken, LessThanToken, GreaterThanToken, IdentifierToken("helloword"), IntToken(5), EqualsToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IntToken(12355), ArrowToken, RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 60") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( == => / helloword ident1231asdad 5 = < 12355 > reserved1 )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, DoubleEqualsToken, ArrowToken, DivideToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), IntToken(5), EqualsToken, LessThanToken, IntToken(12355), GreaterThanToken, IdentifierToken("reserved1"), RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 61") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( == > ident1231asdad < helloword reserved1 / 12355 = ) 5 =>")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, DoubleEqualsToken, GreaterThanToken, IdentifierToken("ident1231asdad"), LessThanToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), DivideToken, IntToken(12355), EqualsToken, RightParenToken, IntToken(5), ArrowToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 62") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( == reserved1 12355 / helloword ident1231asdad 5 > < => = )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, DoubleEqualsToken, IdentifierToken("reserved1"), IntToken(12355), DivideToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), IntToken(5), GreaterThanToken, LessThanToken, ArrowToken, EqualsToken, RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 63") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( => / < reserved1 ) = helloword > 5 12355 ident1231asdad ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, ArrowToken, DivideToken, LessThanToken, IdentifierToken("reserved1"), RightParenToken, EqualsToken, IdentifierToken("helloword"), GreaterThanToken, IntToken(5), IntToken(12355), IdentifierToken("ident1231asdad"), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 64") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( => == ) < / > ident1231asdad reserved1 12355 = helloword 5")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, ArrowToken, DoubleEqualsToken, RightParenToken, LessThanToken, DivideToken, GreaterThanToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IntToken(12355), EqualsToken, IdentifierToken("helloword"), IntToken(5))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 65") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( => > ident1231asdad reserved1 / == 12355 helloword 5 < = )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, ArrowToken, GreaterThanToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), DivideToken, DoubleEqualsToken, IntToken(12355), IdentifierToken("helloword"), IntToken(5), LessThanToken, EqualsToken, RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 66") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( => reserved1 12355 == / > 5 = ident1231asdad < helloword )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, ArrowToken, IdentifierToken("reserved1"), IntToken(12355), DoubleEqualsToken, DivideToken, GreaterThanToken, IntToken(5), EqualsToken, IdentifierToken("ident1231asdad"), LessThanToken, IdentifierToken("helloword"), RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 67") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( < / => helloword ) 5 > reserved1 == = ident1231asdad 12355")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, LessThanToken, DivideToken, ArrowToken, IdentifierToken("helloword"), RightParenToken, IntToken(5), GreaterThanToken, IdentifierToken("reserved1"), DoubleEqualsToken, EqualsToken, IdentifierToken("ident1231asdad"), IntToken(12355))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 68") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( < == ) 12355 / ident1231asdad => helloword reserved1 5 > =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, LessThanToken, DoubleEqualsToken, RightParenToken, IntToken(12355), DivideToken, IdentifierToken("ident1231asdad"), ArrowToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), IntToken(5), GreaterThanToken, EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 69") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( < > helloword / ) ident1231asdad == => 5 12355 reserved1 =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, LessThanToken, GreaterThanToken, IdentifierToken("helloword"), DivideToken, RightParenToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken, ArrowToken, IntToken(5), IntToken(12355), IdentifierToken("reserved1"), EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 70") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( < reserved1 12355 > ) / = ident1231asdad => == helloword 5")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, LessThanToken, IdentifierToken("reserved1"), IntToken(12355), GreaterThanToken, RightParenToken, DivideToken, EqualsToken, IdentifierToken("ident1231asdad"), ArrowToken, DoubleEqualsToken, IdentifierToken("helloword"), IntToken(5))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 71") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( > / < = == ) helloword ident1231asdad reserved1 5 12355 =>")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, GreaterThanToken, DivideToken, LessThanToken, EqualsToken, DoubleEqualsToken, RightParenToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IntToken(5), IntToken(12355), ArrowToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 72") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( > == ) reserved1 = => helloword < 5 12355 ident1231asdad /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, GreaterThanToken, DoubleEqualsToken, RightParenToken, IdentifierToken("reserved1"), EqualsToken, ArrowToken, IdentifierToken("helloword"), LessThanToken, IntToken(5), IntToken(12355), IdentifierToken("ident1231asdad"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 73") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( > < helloword = ) => reserved1 5 == / ident1231asdad 12355")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, GreaterThanToken, LessThanToken, IdentifierToken("helloword"), EqualsToken, RightParenToken, ArrowToken, IdentifierToken("reserved1"), IntToken(5), DoubleEqualsToken, DivideToken, IdentifierToken("ident1231asdad"), IntToken(12355))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 74") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( > reserved1 12355 ident1231asdad ) => < helloword 5 == = /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, GreaterThanToken, IdentifierToken("reserved1"), IntToken(12355), IdentifierToken("ident1231asdad"), RightParenToken, ArrowToken, LessThanToken, IdentifierToken("helloword"), IntToken(5), DoubleEqualsToken, EqualsToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 75") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( 12355 / < => = reserved1 5 == ident1231asdad > helloword )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, IntToken(12355), DivideToken, LessThanToken, ArrowToken, EqualsToken, IdentifierToken("reserved1"), IntToken(5), DoubleEqualsToken, IdentifierToken("ident1231asdad"), GreaterThanToken, IdentifierToken("helloword"), RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 76") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( 12355 == ) helloword = reserved1 < 5 => / ident1231asdad >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, IntToken(12355), DoubleEqualsToken, RightParenToken, IdentifierToken("helloword"), EqualsToken, IdentifierToken("reserved1"), LessThanToken, IntToken(5), ArrowToken, DivideToken, IdentifierToken("ident1231asdad"), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 77") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( 12355 < helloword => ) ident1231asdad = reserved1 5 > == /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, IntToken(12355), LessThanToken, IdentifierToken("helloword"), ArrowToken, RightParenToken, IdentifierToken("ident1231asdad"), EqualsToken, IdentifierToken("reserved1"), IntToken(5), GreaterThanToken, DoubleEqualsToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 78") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( 12355 reserved1 5 / == ) => < ident1231asdad > helloword =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, IntToken(12355), IdentifierToken("reserved1"), IntToken(5), DivideToken, DoubleEqualsToken, RightParenToken, ArrowToken, LessThanToken, IdentifierToken("ident1231asdad"), GreaterThanToken, IdentifierToken("helloword"), EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 79") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( 5 / < 12355 == = ) ident1231asdad > => helloword reserved1")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, IntToken(5), DivideToken, LessThanToken, IntToken(12355), DoubleEqualsToken, EqualsToken, RightParenToken, IdentifierToken("ident1231asdad"), GreaterThanToken, ArrowToken, IdentifierToken("helloword"), IdentifierToken("reserved1"))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 80") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( 5 == = ) < => helloword ident1231asdad reserved1 12355 > /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, IntToken(5), DoubleEqualsToken, EqualsToken, RightParenToken, LessThanToken, ArrowToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IntToken(12355), GreaterThanToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 81") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( 5 < helloword 12355 = == ident1231asdad ) > => reserved1 /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, IntToken(5), LessThanToken, IdentifierToken("helloword"), IntToken(12355), EqualsToken, DoubleEqualsToken, IdentifierToken("ident1231asdad"), RightParenToken, GreaterThanToken, ArrowToken, IdentifierToken("reserved1"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 82") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( 5 reserved1 12355 = == < ident1231asdad > ) / helloword =>")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, IntToken(5), IdentifierToken("reserved1"), IntToken(12355), EqualsToken, DoubleEqualsToken, LessThanToken, IdentifierToken("ident1231asdad"), GreaterThanToken, RightParenToken, DivideToken, IdentifierToken("helloword"), ArrowToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 83") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( reserved1 / < ident1231asdad == 12355 > helloword 5 => = )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, IdentifierToken("reserved1"), DivideToken, LessThanToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken, IntToken(12355), GreaterThanToken, IdentifierToken("helloword"), IntToken(5), ArrowToken, EqualsToken, RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 84") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( reserved1 == = < => ident1231asdad 12355 ) 5 > helloword /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, IdentifierToken("reserved1"), DoubleEqualsToken, EqualsToken, LessThanToken, ArrowToken, IdentifierToken("ident1231asdad"), IntToken(12355), RightParenToken, IntToken(5), GreaterThanToken, IdentifierToken("helloword"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 85") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( reserved1 < helloword ident1231asdad = 5 == > ) / 12355 =>")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, IdentifierToken("reserved1"), LessThanToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), EqualsToken, IntToken(5), DoubleEqualsToken, GreaterThanToken, RightParenToken, DivideToken, IntToken(12355), ArrowToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 86") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( reserved1 5 12355 => == / = helloword ident1231asdad > < )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, IdentifierToken("reserved1"), IntToken(5), IntToken(12355), ArrowToken, DoubleEqualsToken, DivideToken, EqualsToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), GreaterThanToken, LessThanToken, RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 87") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( ident1231asdad / > ) < == => 12355 reserved1 5 helloword =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, IdentifierToken("ident1231asdad"), DivideToken, GreaterThanToken, RightParenToken, LessThanToken, DoubleEqualsToken, ArrowToken, IntToken(12355), IdentifierToken("reserved1"), IntToken(5), IdentifierToken("helloword"), EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 88") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( ident1231asdad == = 12355 < => / reserved1 > ) helloword 5")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken, EqualsToken, IntToken(12355), LessThanToken, ArrowToken, DivideToken, IdentifierToken("reserved1"), GreaterThanToken, RightParenToken, IdentifierToken("helloword"), IntToken(5))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 89") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( ident1231asdad > / = < => helloword reserved1 5 12355 == )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, IdentifierToken("ident1231asdad"), GreaterThanToken, DivideToken, EqualsToken, LessThanToken, ArrowToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), IntToken(5), IntToken(12355), DoubleEqualsToken, RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 90") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( ident1231asdad 5 12355 > == => helloword ) < = reserved1 /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, IdentifierToken("ident1231asdad"), IntToken(5), IntToken(12355), GreaterThanToken, DoubleEqualsToken, ArrowToken, IdentifierToken("helloword"), RightParenToken, LessThanToken, EqualsToken, IdentifierToken("reserved1"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 91") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( helloword / > == < 5 reserved1 12355 = ) ident1231asdad =>")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, IdentifierToken("helloword"), DivideToken, GreaterThanToken, DoubleEqualsToken, LessThanToken, IntToken(5), IdentifierToken("reserved1"), IntToken(12355), EqualsToken, RightParenToken, IdentifierToken("ident1231asdad"), ArrowToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 92") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( helloword == = reserved1 < 5 > ident1231asdad 12355 => ) /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, IdentifierToken("helloword"), DoubleEqualsToken, EqualsToken, IdentifierToken("reserved1"), LessThanToken, IntToken(5), GreaterThanToken, IdentifierToken("ident1231asdad"), IntToken(12355), ArrowToken, RightParenToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 93") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( helloword > / => < ident1231asdad 12355 = 5 == reserved1 )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, IdentifierToken("helloword"), GreaterThanToken, DivideToken, ArrowToken, LessThanToken, IdentifierToken("ident1231asdad"), IntToken(12355), EqualsToken, IntToken(5), DoubleEqualsToken, IdentifierToken("reserved1"), RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 94") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ( helloword 5 12355 ident1231asdad => / == > = ) reserved1 <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LeftParenToken, IdentifierToken("helloword"), IntToken(5), IntToken(12355), IdentifierToken("ident1231asdad"), ArrowToken, DivideToken, DoubleEqualsToken, GreaterThanToken, EqualsToken, RightParenToken, IdentifierToken("reserved1"), LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 95") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) / ( 12355 > 5 == => helloword ident1231asdad reserved1 < =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, DivideToken, LeftParenToken, IntToken(12355), GreaterThanToken, IntToken(5), DoubleEqualsToken, ArrowToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), LessThanToken, EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 96") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) / => < ( 5 > == 12355 ident1231asdad reserved1 helloword =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, DivideToken, ArrowToken, LessThanToken, LeftParenToken, IntToken(5), GreaterThanToken, DoubleEqualsToken, IntToken(12355), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IdentifierToken("helloword"), EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 97") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) / 12355 ( 5 > < = ident1231asdad => == helloword reserved1")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, DivideToken, IntToken(12355), LeftParenToken, IntToken(5), GreaterThanToken, LessThanToken, EqualsToken, IdentifierToken("ident1231asdad"), ArrowToken, DoubleEqualsToken, IdentifierToken("helloword"), IdentifierToken("reserved1"))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 98") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) / reserved1 ident1231asdad = > < helloword 5 12355 => == (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, DivideToken, IdentifierToken("reserved1"), IdentifierToken("ident1231asdad"), EqualsToken, GreaterThanToken, LessThanToken, IdentifierToken("helloword"), IntToken(5), IntToken(12355), ArrowToken, DoubleEqualsToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 99") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) ( / 12355 reserved1 > 5 helloword == < => ident1231asdad =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, LeftParenToken, DivideToken, IntToken(12355), IdentifierToken("reserved1"), GreaterThanToken, IntToken(5), IdentifierToken("helloword"), DoubleEqualsToken, LessThanToken, ArrowToken, IdentifierToken("ident1231asdad"), EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 100") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) ( => < == 5 ident1231asdad reserved1 12355 = / helloword >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, LeftParenToken, ArrowToken, LessThanToken, DoubleEqualsToken, IntToken(5), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IntToken(12355), EqualsToken, DivideToken, IdentifierToken("helloword"), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 101") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) ( 12355 / ident1231asdad > helloword < reserved1 5 => == =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, LeftParenToken, IntToken(12355), DivideToken, IdentifierToken("ident1231asdad"), GreaterThanToken, IdentifierToken("helloword"), LessThanToken, IdentifierToken("reserved1"), IntToken(5), ArrowToken, DoubleEqualsToken, EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 102") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) ( reserved1 ident1231asdad => 12355 / > == 5 < helloword =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, LeftParenToken, IdentifierToken("reserved1"), IdentifierToken("ident1231asdad"), ArrowToken, IntToken(12355), DivideToken, GreaterThanToken, DoubleEqualsToken, IntToken(5), LessThanToken, IdentifierToken("helloword"), EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 103") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) = / 12355 helloword 5 == < reserved1 => ( ident1231asdad >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, EqualsToken, DivideToken, IntToken(12355), IdentifierToken("helloword"), IntToken(5), DoubleEqualsToken, LessThanToken, IdentifierToken("reserved1"), ArrowToken, LeftParenToken, IdentifierToken("ident1231asdad"), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 104") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) = => < 12355 reserved1 == ( helloword ident1231asdad 5 > /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, EqualsToken, ArrowToken, LessThanToken, IntToken(12355), IdentifierToken("reserved1"), DoubleEqualsToken, LeftParenToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), IntToken(5), GreaterThanToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 105") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) = 12355 ( / reserved1 > => < ident1231asdad 5 helloword ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, EqualsToken, IntToken(12355), LeftParenToken, DivideToken, IdentifierToken("reserved1"), GreaterThanToken, ArrowToken, LessThanToken, IdentifierToken("ident1231asdad"), IntToken(5), IdentifierToken("helloword"), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 106") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) = reserved1 ident1231asdad > 12355 < / 5 == ( helloword =>")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, EqualsToken, IdentifierToken("reserved1"), IdentifierToken("ident1231asdad"), GreaterThanToken, IntToken(12355), LessThanToken, DivideToken, IntToken(5), DoubleEqualsToken, LeftParenToken, IdentifierToken("helloword"), ArrowToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 107") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) == / 5 = reserved1 12355 helloword ident1231asdad > < => (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, DoubleEqualsToken, DivideToken, IntToken(5), EqualsToken, IdentifierToken("reserved1"), IntToken(12355), IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), GreaterThanToken, LessThanToken, ArrowToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 108") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) == => < reserved1 5 ident1231asdad helloword ( > = 12355 /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, DoubleEqualsToken, ArrowToken, LessThanToken, IdentifierToken("reserved1"), IntToken(5), IdentifierToken("ident1231asdad"), IdentifierToken("helloword"), LeftParenToken, GreaterThanToken, EqualsToken, IntToken(12355), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 109") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) == 12355 ( => reserved1 helloword 5 > = / ident1231asdad <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, DoubleEqualsToken, IntToken(12355), LeftParenToken, ArrowToken, IdentifierToken("reserved1"), IdentifierToken("helloword"), IntToken(5), GreaterThanToken, EqualsToken, DivideToken, IdentifierToken("ident1231asdad"), LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 110") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) == reserved1 ident1231asdad 5 12355 / < helloword > => = (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, DoubleEqualsToken, IdentifierToken("reserved1"), IdentifierToken("ident1231asdad"), IntToken(5), IntToken(12355), DivideToken, LessThanToken, IdentifierToken("helloword"), GreaterThanToken, ArrowToken, EqualsToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 111") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) => / 5 < ident1231asdad = 12355 == reserved1 > helloword (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, ArrowToken, DivideToken, IntToken(5), LessThanToken, IdentifierToken("ident1231asdad"), EqualsToken, IntToken(12355), DoubleEqualsToken, IdentifierToken("reserved1"), GreaterThanToken, IdentifierToken("helloword"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 112") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) => == < helloword reserved1 = > 5 ( / ident1231asdad 12355")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, ArrowToken, DoubleEqualsToken, LessThanToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), EqualsToken, GreaterThanToken, IntToken(5), LeftParenToken, DivideToken, IdentifierToken("ident1231asdad"), IntToken(12355))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 113") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) => 12355 ( > ident1231asdad < = helloword reserved1 5 == /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, ArrowToken, IntToken(12355), LeftParenToken, GreaterThanToken, IdentifierToken("ident1231asdad"), LessThanToken, EqualsToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), IntToken(5), DoubleEqualsToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 114") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) => reserved1 helloword / 5 > = == 12355 < ident1231asdad (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, ArrowToken, IdentifierToken("reserved1"), IdentifierToken("helloword"), DivideToken, IntToken(5), GreaterThanToken, EqualsToken, DoubleEqualsToken, IntToken(12355), LessThanToken, IdentifierToken("ident1231asdad"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 115") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) < / 5 12355 ident1231asdad reserved1 ( > == = helloword =>")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, LessThanToken, DivideToken, IntToken(5), IntToken(12355), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), LeftParenToken, GreaterThanToken, DoubleEqualsToken, EqualsToken, IdentifierToken("helloword"), ArrowToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 116") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) < == > ( ident1231asdad reserved1 helloword 5 12355 => = /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, LessThanToken, DoubleEqualsToken, GreaterThanToken, LeftParenToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IdentifierToken("helloword"), IntToken(5), IntToken(12355), ArrowToken, EqualsToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 117") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) < 12355 ( reserved1 ident1231asdad helloword 5 = => == > /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, LessThanToken, IntToken(12355), LeftParenToken, IdentifierToken("reserved1"), IdentifierToken("ident1231asdad"), IdentifierToken("helloword"), IntToken(5), EqualsToken, ArrowToken, DoubleEqualsToken, GreaterThanToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 118") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) < reserved1 helloword = ident1231asdad / 12355 > == ( 5 =>")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, LessThanToken, IdentifierToken("reserved1"), IdentifierToken("helloword"), EqualsToken, IdentifierToken("ident1231asdad"), DivideToken, IntToken(12355), GreaterThanToken, DoubleEqualsToken, LeftParenToken, IntToken(5), ArrowToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 119") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) > / 5 ident1231asdad helloword = < reserved1 12355 => == (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, GreaterThanToken, DivideToken, IntToken(5), IdentifierToken("ident1231asdad"), IdentifierToken("helloword"), EqualsToken, LessThanToken, IdentifierToken("reserved1"), IntToken(12355), ArrowToken, DoubleEqualsToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 120") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) > == < => helloword = 5 ( reserved1 12355 ident1231asdad /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, GreaterThanToken, DoubleEqualsToken, LessThanToken, ArrowToken, IdentifierToken("helloword"), EqualsToken, IntToken(5), LeftParenToken, IdentifierToken("reserved1"), IntToken(12355), IdentifierToken("ident1231asdad"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 121") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) > 12355 ( helloword ident1231asdad => == 5 = / reserved1 <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, GreaterThanToken, IntToken(12355), LeftParenToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), ArrowToken, DoubleEqualsToken, IntToken(5), EqualsToken, DivideToken, IdentifierToken("reserved1"), LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 122") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) > reserved1 helloword => ident1231asdad < ( 5 12355 == = /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, GreaterThanToken, IdentifierToken("reserved1"), IdentifierToken("helloword"), ArrowToken, IdentifierToken("ident1231asdad"), LessThanToken, LeftParenToken, IntToken(5), IntToken(12355), DoubleEqualsToken, EqualsToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 123") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) 12355 / reserved1 ( helloword 5 == => > < ident1231asdad =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, IntToken(12355), DivideToken, IdentifierToken("reserved1"), LeftParenToken, IdentifierToken("helloword"), IntToken(5), DoubleEqualsToken, ArrowToken, GreaterThanToken, LessThanToken, IdentifierToken("ident1231asdad"), EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 124") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) 12355 == < 5 helloword ident1231asdad / > = ( reserved1 =>")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, IntToken(12355), DoubleEqualsToken, LessThanToken, IntToken(5), IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), DivideToken, GreaterThanToken, EqualsToken, LeftParenToken, IdentifierToken("reserved1"), ArrowToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 125") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) 12355 > = ( helloword ident1231asdad reserved1 5 < => == /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, IntToken(12355), GreaterThanToken, EqualsToken, LeftParenToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IntToken(5), LessThanToken, ArrowToken, DoubleEqualsToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 126") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) 12355 reserved1 helloword 5 / ( ident1231asdad == < => > =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, IntToken(12355), IdentifierToken("reserved1"), IdentifierToken("helloword"), IntToken(5), DivideToken, LeftParenToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken, LessThanToken, ArrowToken, GreaterThanToken, EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 127") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) 5 / reserved1 => ( == ident1231asdad 12355 < = helloword >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, IntToken(5), DivideToken, IdentifierToken("reserved1"), ArrowToken, LeftParenToken, DoubleEqualsToken, IdentifierToken("ident1231asdad"), IntToken(12355), LessThanToken, EqualsToken, IdentifierToken("helloword"), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 128") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) 5 == < helloword / => 12355 ident1231asdad reserved1 > = (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, IntToken(5), DoubleEqualsToken, LessThanToken, IdentifierToken("helloword"), DivideToken, ArrowToken, IntToken(12355), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), GreaterThanToken, EqualsToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 129") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) 5 > = < / 12355 reserved1 == ident1231asdad => helloword (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, IntToken(5), GreaterThanToken, EqualsToken, LessThanToken, DivideToken, IntToken(12355), IdentifierToken("reserved1"), DoubleEqualsToken, IdentifierToken("ident1231asdad"), ArrowToken, IdentifierToken("helloword"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 130") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) 5 ident1231asdad / ( = 12355 < reserved1 => == helloword >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, IntToken(5), IdentifierToken("ident1231asdad"), DivideToken, LeftParenToken, EqualsToken, IntToken(12355), LessThanToken, IdentifierToken("reserved1"), ArrowToken, DoubleEqualsToken, IdentifierToken("helloword"), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 131") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) reserved1 / 5 > ( ident1231asdad == helloword 12355 < => =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, IdentifierToken("reserved1"), DivideToken, IntToken(5), GreaterThanToken, LeftParenToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken, IdentifierToken("helloword"), IntToken(12355), LessThanToken, ArrowToken, EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 132") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) reserved1 == > ( / helloword => < 5 12355 ident1231asdad =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, IdentifierToken("reserved1"), DoubleEqualsToken, GreaterThanToken, LeftParenToken, DivideToken, IdentifierToken("helloword"), ArrowToken, LessThanToken, IntToken(5), IntToken(12355), IdentifierToken("ident1231asdad"), EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 133") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) reserved1 > = 5 ( / == ident1231asdad < => helloword 12355")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, IdentifierToken("reserved1"), GreaterThanToken, EqualsToken, IntToken(5), LeftParenToken, DivideToken, DoubleEqualsToken, IdentifierToken("ident1231asdad"), LessThanToken, ArrowToken, IdentifierToken("helloword"), IntToken(12355))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 134") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) reserved1 ident1231asdad / == = ( helloword 5 12355 > < =>")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, IdentifierToken("reserved1"), IdentifierToken("ident1231asdad"), DivideToken, DoubleEqualsToken, EqualsToken, LeftParenToken, IdentifierToken("helloword"), IntToken(5), IntToken(12355), GreaterThanToken, LessThanToken, ArrowToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 135") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) ident1231asdad / 5 reserved1 = == helloword => > < 12355 (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, IdentifierToken("ident1231asdad"), DivideToken, IntToken(5), IdentifierToken("reserved1"), EqualsToken, DoubleEqualsToken, IdentifierToken("helloword"), ArrowToken, GreaterThanToken, LessThanToken, IntToken(12355), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 136") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) ident1231asdad == > => ( < reserved1 5 = / helloword 12355")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken, GreaterThanToken, ArrowToken, LeftParenToken, LessThanToken, IdentifierToken("reserved1"), IntToken(5), EqualsToken, DivideToken, IdentifierToken("helloword"), IntToken(12355))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 137") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) ident1231asdad > = helloword ( < 12355 reserved1 5 => == /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, IdentifierToken("ident1231asdad"), GreaterThanToken, EqualsToken, IdentifierToken("helloword"), LeftParenToken, LessThanToken, IntToken(12355), IdentifierToken("reserved1"), IntToken(5), ArrowToken, DoubleEqualsToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 138") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) ident1231asdad reserved1 / < = 12355 > == 5 => helloword (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), DivideToken, LessThanToken, EqualsToken, IntToken(12355), GreaterThanToken, DoubleEqualsToken, IntToken(5), ArrowToken, IdentifierToken("helloword"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 139") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) helloword / reserved1 ( == 5 < 12355 => = ident1231asdad >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, IdentifierToken("helloword"), DivideToken, IdentifierToken("reserved1"), LeftParenToken, DoubleEqualsToken, IntToken(5), LessThanToken, IntToken(12355), ArrowToken, EqualsToken, IdentifierToken("ident1231asdad"), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 140") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) helloword == > 12355 ( ident1231asdad = reserved1 5 < => /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, IdentifierToken("helloword"), DoubleEqualsToken, GreaterThanToken, IntToken(12355), LeftParenToken, IdentifierToken("ident1231asdad"), EqualsToken, IdentifierToken("reserved1"), IntToken(5), LessThanToken, ArrowToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 141") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) helloword > == ( => / < 12355 reserved1 5 ident1231asdad =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, IdentifierToken("helloword"), GreaterThanToken, DoubleEqualsToken, LeftParenToken, ArrowToken, DivideToken, LessThanToken, IntToken(12355), IdentifierToken("reserved1"), IntToken(5), IdentifierToken("ident1231asdad"), EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 142") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ) helloword reserved1 / 12355 == = ( 5 < => ident1231asdad >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, RightParenToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), DivideToken, IntToken(12355), DoubleEqualsToken, EqualsToken, LeftParenToken, IntToken(5), LessThanToken, ArrowToken, IdentifierToken("ident1231asdad"), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 143") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = / ( ident1231asdad => < == helloword reserved1 5 12355 > )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, DivideToken, LeftParenToken, IdentifierToken("ident1231asdad"), ArrowToken, LessThanToken, DoubleEqualsToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), IntToken(5), IntToken(12355), GreaterThanToken, RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 144") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = / => 12355 ident1231asdad == < helloword ) 5 > reserved1 (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, DivideToken, ArrowToken, IntToken(12355), IdentifierToken("ident1231asdad"), DoubleEqualsToken, LessThanToken, IdentifierToken("helloword"), RightParenToken, IntToken(5), GreaterThanToken, IdentifierToken("reserved1"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 145") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = / 12355 => < == 5 ident1231asdad reserved1 ) ( helloword >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, DivideToken, IntToken(12355), ArrowToken, LessThanToken, DoubleEqualsToken, IntToken(5), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), RightParenToken, LeftParenToken, IdentifierToken("helloword"), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 146") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = / ident1231asdad ( helloword => 12355 > reserved1 5 < == )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, DivideToken, IdentifierToken("ident1231asdad"), LeftParenToken, IdentifierToken("helloword"), ArrowToken, IntToken(12355), GreaterThanToken, IdentifierToken("reserved1"), IntToken(5), LessThanToken, DoubleEqualsToken, RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 147") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = ( / ident1231asdad > => reserved1 12355 == 5 < helloword )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, LeftParenToken, DivideToken, IdentifierToken("ident1231asdad"), GreaterThanToken, ArrowToken, IdentifierToken("reserved1"), IntToken(12355), DoubleEqualsToken, IntToken(5), LessThanToken, IdentifierToken("helloword"), RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 148") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = ( => 5 / < helloword > reserved1 == ) ident1231asdad 12355")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, LeftParenToken, ArrowToken, IntToken(5), DivideToken, LessThanToken, IdentifierToken("helloword"), GreaterThanToken, IdentifierToken("reserved1"), DoubleEqualsToken, RightParenToken, IdentifierToken("ident1231asdad"), IntToken(12355))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 149") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = ( 12355 => 5 < / == helloword ident1231asdad reserved1 > )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, LeftParenToken, IntToken(12355), ArrowToken, IntToken(5), LessThanToken, DivideToken, DoubleEqualsToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), GreaterThanToken, RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 150") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = ( ident1231asdad ) == > => < 12355 reserved1 5 helloword /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, LeftParenToken, IdentifierToken("ident1231asdad"), RightParenToken, DoubleEqualsToken, GreaterThanToken, ArrowToken, LessThanToken, IntToken(12355), IdentifierToken("reserved1"), IntToken(5), IdentifierToken("helloword"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 151") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = ) / ident1231asdad 5 < => ( reserved1 > == helloword 12355")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, RightParenToken, DivideToken, IdentifierToken("ident1231asdad"), IntToken(5), LessThanToken, ArrowToken, LeftParenToken, IdentifierToken("reserved1"), GreaterThanToken, DoubleEqualsToken, IdentifierToken("helloword"), IntToken(12355))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 152") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = ) => 5 == > < helloword ident1231asdad reserved1 12355 ( /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, RightParenToken, ArrowToken, IntToken(5), DoubleEqualsToken, GreaterThanToken, LessThanToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IntToken(12355), LeftParenToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 153") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = ) 12355 => ident1231asdad < > helloword ( 5 == reserved1 /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, RightParenToken, IntToken(12355), ArrowToken, IdentifierToken("ident1231asdad"), LessThanToken, GreaterThanToken, IdentifierToken("helloword"), LeftParenToken, IntToken(5), DoubleEqualsToken, IdentifierToken("reserved1"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 154") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = ) ident1231asdad ( < > 5 reserved1 12355 == / helloword =>")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, RightParenToken, IdentifierToken("ident1231asdad"), LeftParenToken, LessThanToken, GreaterThanToken, IntToken(5), IdentifierToken("reserved1"), IntToken(12355), DoubleEqualsToken, DivideToken, IdentifierToken("helloword"), ArrowToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 155") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = == / ident1231asdad helloword < 5 > reserved1 12355 => ) (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, DoubleEqualsToken, DivideToken, IdentifierToken("ident1231asdad"), IdentifierToken("helloword"), LessThanToken, IntToken(5), GreaterThanToken, IdentifierToken("reserved1"), IntToken(12355), ArrowToken, RightParenToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 156") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = == => 5 > < helloword 12355 ( reserved1 ) ident1231asdad /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, DoubleEqualsToken, ArrowToken, IntToken(5), GreaterThanToken, LessThanToken, IdentifierToken("helloword"), IntToken(12355), LeftParenToken, IdentifierToken("reserved1"), RightParenToken, IdentifierToken("ident1231asdad"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 157") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = == 12355 < / 5 ( > ident1231asdad => ) helloword reserved1")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, DoubleEqualsToken, IntToken(12355), LessThanToken, DivideToken, IntToken(5), LeftParenToken, GreaterThanToken, IdentifierToken("ident1231asdad"), ArrowToken, RightParenToken, IdentifierToken("helloword"), IdentifierToken("reserved1"))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 158") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = == ident1231asdad ( 12355 > ) => helloword reserved1 5 < /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, DoubleEqualsToken, IdentifierToken("ident1231asdad"), LeftParenToken, IntToken(12355), GreaterThanToken, RightParenToken, ArrowToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), IntToken(5), LessThanToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 159") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = => / helloword ) 12355 < == > reserved1 5 ident1231asdad (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, ArrowToken, DivideToken, IdentifierToken("helloword"), RightParenToken, IntToken(12355), LessThanToken, DoubleEqualsToken, GreaterThanToken, IdentifierToken("reserved1"), IntToken(5), IdentifierToken("ident1231asdad"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 160") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = => == 5 reserved1 > < / ident1231asdad ) ( helloword 12355")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, ArrowToken, DoubleEqualsToken, IntToken(5), IdentifierToken("reserved1"), GreaterThanToken, LessThanToken, DivideToken, IdentifierToken("ident1231asdad"), RightParenToken, LeftParenToken, IdentifierToken("helloword"), IntToken(12355))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 161") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = => 12355 < ) 5 > helloword ident1231asdad reserved1 == ( /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, ArrowToken, IntToken(12355), LessThanToken, RightParenToken, IntToken(5), GreaterThanToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), DoubleEqualsToken, LeftParenToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 162") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = => ident1231asdad ( reserved1 > 12355 helloword ) < == 5 /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, ArrowToken, IdentifierToken("ident1231asdad"), LeftParenToken, IdentifierToken("reserved1"), GreaterThanToken, IntToken(12355), IdentifierToken("helloword"), RightParenToken, LessThanToken, DoubleEqualsToken, IntToken(5), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 163") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = < / helloword => 12355 reserved1 5 > ) ( ident1231asdad ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, LessThanToken, DivideToken, IdentifierToken("helloword"), ArrowToken, IntToken(12355), IdentifierToken("reserved1"), IntToken(5), GreaterThanToken, RightParenToken, LeftParenToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 164") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = < == 5 helloword > ident1231asdad => reserved1 12355 ) ( /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, LessThanToken, DoubleEqualsToken, IntToken(5), IdentifierToken("helloword"), GreaterThanToken, IdentifierToken("ident1231asdad"), ArrowToken, IdentifierToken("reserved1"), IntToken(12355), RightParenToken, LeftParenToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 165") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = < 12355 => > reserved1 / 5 ) ident1231asdad == helloword (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, LessThanToken, IntToken(12355), ArrowToken, GreaterThanToken, IdentifierToken("reserved1"), DivideToken, IntToken(5), RightParenToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken, IdentifierToken("helloword"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 166") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = < ident1231asdad ) / 5 == > reserved1 => ( helloword 12355")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, LessThanToken, IdentifierToken("ident1231asdad"), RightParenToken, DivideToken, IntToken(5), DoubleEqualsToken, GreaterThanToken, IdentifierToken("reserved1"), ArrowToken, LeftParenToken, IdentifierToken("helloword"), IntToken(12355))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 167") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = > / helloword 12355 5 == ) ident1231asdad reserved1 < => (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, GreaterThanToken, DivideToken, IdentifierToken("helloword"), IntToken(12355), IntToken(5), DoubleEqualsToken, RightParenToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), LessThanToken, ArrowToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 168") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = > == reserved1 ( 5 < ) => ident1231asdad 12355 helloword /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, GreaterThanToken, DoubleEqualsToken, IdentifierToken("reserved1"), LeftParenToken, IntToken(5), LessThanToken, RightParenToken, ArrowToken, IdentifierToken("ident1231asdad"), IntToken(12355), IdentifierToken("helloword"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 169") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = > 12355 => reserved1 5 < / ident1231asdad ) ( helloword ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, GreaterThanToken, IntToken(12355), ArrowToken, IdentifierToken("reserved1"), IntToken(5), LessThanToken, DivideToken, IdentifierToken("ident1231asdad"), RightParenToken, LeftParenToken, IdentifierToken("helloword"), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 170") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = > ident1231asdad ) == 5 12355 helloword reserved1 < => ( /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, GreaterThanToken, IdentifierToken("ident1231asdad"), RightParenToken, DoubleEqualsToken, IntToken(5), IntToken(12355), IdentifierToken("helloword"), IdentifierToken("reserved1"), LessThanToken, ArrowToken, LeftParenToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 171") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = 12355 / helloword reserved1 > 5 ident1231asdad ) => == < (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, IntToken(12355), DivideToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), GreaterThanToken, IntToken(5), IdentifierToken("ident1231asdad"), RightParenToken, ArrowToken, DoubleEqualsToken, LessThanToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 172") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = 12355 == reserved1 => 5 helloword > < ( / ident1231asdad )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, IntToken(12355), DoubleEqualsToken, IdentifierToken("reserved1"), ArrowToken, IntToken(5), IdentifierToken("helloword"), GreaterThanToken, LessThanToken, LeftParenToken, DivideToken, IdentifierToken("ident1231asdad"), RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 173") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = 12355 > => helloword reserved1 / < ident1231asdad 5 == ) (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, IntToken(12355), GreaterThanToken, ArrowToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), DivideToken, LessThanToken, IdentifierToken("ident1231asdad"), IntToken(5), DoubleEqualsToken, RightParenToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 174") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = 12355 ident1231asdad ) < reserved1 ( > == 5 => helloword /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, IntToken(12355), IdentifierToken("ident1231asdad"), RightParenToken, LessThanToken, IdentifierToken("reserved1"), LeftParenToken, GreaterThanToken, DoubleEqualsToken, IntToken(5), ArrowToken, IdentifierToken("helloword"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 175") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = 5 ( / ) ident1231asdad < > reserved1 => == helloword 12355")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, IntToken(5), LeftParenToken, DivideToken, RightParenToken, IdentifierToken("ident1231asdad"), LessThanToken, GreaterThanToken, IdentifierToken("reserved1"), ArrowToken, DoubleEqualsToken, IdentifierToken("helloword"), IntToken(12355))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 176") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = 5 == reserved1 > ident1231asdad => ( helloword 12355 < ) /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, IntToken(5), DoubleEqualsToken, IdentifierToken("reserved1"), GreaterThanToken, IdentifierToken("ident1231asdad"), ArrowToken, LeftParenToken, IdentifierToken("helloword"), IntToken(12355), LessThanToken, RightParenToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 177") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = 5 > < ( ident1231asdad 12355 ) == reserved1 => helloword /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, IntToken(5), GreaterThanToken, LessThanToken, LeftParenToken, IdentifierToken("ident1231asdad"), IntToken(12355), RightParenToken, DoubleEqualsToken, IdentifierToken("reserved1"), ArrowToken, IdentifierToken("helloword"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 178") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = 5 ident1231asdad ) 12355 reserved1 > / < == ( helloword =>")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, IntToken(5), IdentifierToken("ident1231asdad"), RightParenToken, IntToken(12355), IdentifierToken("reserved1"), GreaterThanToken, DivideToken, LessThanToken, DoubleEqualsToken, LeftParenToken, IdentifierToken("helloword"), ArrowToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 179") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = reserved1 ( / => ident1231asdad 5 helloword 12355 > < == )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, IdentifierToken("reserved1"), LeftParenToken, DivideToken, ArrowToken, IdentifierToken("ident1231asdad"), IntToken(5), IdentifierToken("helloword"), IntToken(12355), GreaterThanToken, LessThanToken, DoubleEqualsToken, RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 180") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = reserved1 == 5 ident1231asdad 12355 helloword > ( => ) < /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, IdentifierToken("reserved1"), DoubleEqualsToken, IntToken(5), IdentifierToken("ident1231asdad"), IntToken(12355), IdentifierToken("helloword"), GreaterThanToken, LeftParenToken, ArrowToken, RightParenToken, LessThanToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 181") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = reserved1 > < == helloword / 5 12355 ) ( ident1231asdad =>")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, IdentifierToken("reserved1"), GreaterThanToken, LessThanToken, DoubleEqualsToken, IdentifierToken("helloword"), DivideToken, IntToken(5), IntToken(12355), RightParenToken, LeftParenToken, IdentifierToken("ident1231asdad"), ArrowToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 182") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = reserved1 ident1231asdad ) helloword 5 ( < 12355 > => == /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, IdentifierToken("reserved1"), IdentifierToken("ident1231asdad"), RightParenToken, IdentifierToken("helloword"), IntToken(5), LeftParenToken, LessThanToken, IntToken(12355), GreaterThanToken, ArrowToken, DoubleEqualsToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 183") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = ident1231asdad ( / > helloword => 12355 == 5 < reserved1 )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, IdentifierToken("ident1231asdad"), LeftParenToken, DivideToken, GreaterThanToken, IdentifierToken("helloword"), ArrowToken, IntToken(12355), DoubleEqualsToken, IntToken(5), LessThanToken, IdentifierToken("reserved1"), RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 184") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = ident1231asdad == reserved1 / helloword < => 12355 ) ( 5 >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken, IdentifierToken("reserved1"), DivideToken, IdentifierToken("helloword"), LessThanToken, ArrowToken, IntToken(12355), RightParenToken, LeftParenToken, IntToken(5), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 185") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = ident1231asdad > < 12355 helloword => ( reserved1 5 == ) /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, IdentifierToken("ident1231asdad"), GreaterThanToken, LessThanToken, IntToken(12355), IdentifierToken("helloword"), ArrowToken, LeftParenToken, IdentifierToken("reserved1"), IntToken(5), DoubleEqualsToken, RightParenToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 186") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = ident1231asdad reserved1 == ( helloword 12355 ) => > < 5 /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), DoubleEqualsToken, LeftParenToken, IdentifierToken("helloword"), IntToken(12355), RightParenToken, ArrowToken, GreaterThanToken, LessThanToken, IntToken(5), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 187") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = helloword ( / 5 ident1231asdad reserved1 ) > => == 12355 <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, IdentifierToken("helloword"), LeftParenToken, DivideToken, IntToken(5), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), RightParenToken, GreaterThanToken, ArrowToken, DoubleEqualsToken, IntToken(12355), LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 188") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = helloword == reserved1 ) ident1231asdad 5 12355 > < => ( /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, IdentifierToken("helloword"), DoubleEqualsToken, IdentifierToken("reserved1"), RightParenToken, IdentifierToken("ident1231asdad"), IntToken(5), IntToken(12355), GreaterThanToken, LessThanToken, ArrowToken, LeftParenToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 189") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = helloword > < ident1231asdad / ( reserved1 == 12355 => 5 )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, IdentifierToken("helloword"), GreaterThanToken, LessThanToken, IdentifierToken("ident1231asdad"), DivideToken, LeftParenToken, IdentifierToken("reserved1"), DoubleEqualsToken, IntToken(12355), ArrowToken, IntToken(5), RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 190") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * = helloword reserved1 == < / ) 5 12355 => ( ident1231asdad >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, EqualsToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), DoubleEqualsToken, LessThanToken, DivideToken, RightParenToken, IntToken(5), IntToken(12355), ArrowToken, LeftParenToken, IdentifierToken("ident1231asdad"), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 191") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == / ) = ( => 12355 reserved1 helloword ident1231asdad 5 > <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, DivideToken, RightParenToken, EqualsToken, LeftParenToken, ArrowToken, IntToken(12355), IdentifierToken("reserved1"), IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), IntToken(5), GreaterThanToken, LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 192") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == / => ident1231asdad 12355 ( > 5 = reserved1 < helloword )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, DivideToken, ArrowToken, IdentifierToken("ident1231asdad"), IntToken(12355), LeftParenToken, GreaterThanToken, IntToken(5), EqualsToken, IdentifierToken("reserved1"), LessThanToken, IdentifierToken("helloword"), RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 193") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == / 12355 5 ) ( reserved1 < ident1231asdad => = helloword >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, DivideToken, IntToken(12355), IntToken(5), RightParenToken, LeftParenToken, IdentifierToken("reserved1"), LessThanToken, IdentifierToken("ident1231asdad"), ArrowToken, EqualsToken, IdentifierToken("helloword"), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 194") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == / ident1231asdad => 5 ( reserved1 = helloword 12355 > < )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, DivideToken, IdentifierToken("ident1231asdad"), ArrowToken, IntToken(5), LeftParenToken, IdentifierToken("reserved1"), EqualsToken, IdentifierToken("helloword"), IntToken(12355), GreaterThanToken, LessThanToken, RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 195") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == ( ) = < / helloword > 12355 reserved1 5 ident1231asdad =>")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, LeftParenToken, RightParenToken, EqualsToken, LessThanToken, DivideToken, IdentifierToken("helloword"), GreaterThanToken, IntToken(12355), IdentifierToken("reserved1"), IntToken(5), IdentifierToken("ident1231asdad"), ArrowToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 196") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == ( => ident1231asdad reserved1 ) / = 5 > < helloword 12355")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, LeftParenToken, ArrowToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), RightParenToken, DivideToken, EqualsToken, IntToken(5), GreaterThanToken, LessThanToken, IdentifierToken("helloword"), IntToken(12355))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 197") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == ( 12355 5 => ) / helloword ident1231asdad reserved1 > < =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, LeftParenToken, IntToken(12355), IntToken(5), ArrowToken, RightParenToken, DivideToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), GreaterThanToken, LessThanToken, EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 198") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == ( ident1231asdad => helloword ) = reserved1 < 12355 > 5 /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, LeftParenToken, IdentifierToken("ident1231asdad"), ArrowToken, IdentifierToken("helloword"), RightParenToken, EqualsToken, IdentifierToken("reserved1"), LessThanToken, IntToken(12355), GreaterThanToken, IntToken(5), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 199") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == ) ( = 12355 => > ident1231asdad reserved1 < / helloword 5")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, RightParenToken, LeftParenToken, EqualsToken, IntToken(12355), ArrowToken, GreaterThanToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), LessThanToken, DivideToken, IdentifierToken("helloword"), IntToken(5))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 200") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == ) => helloword / = 12355 5 ident1231asdad reserved1 > < (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, RightParenToken, ArrowToken, IdentifierToken("helloword"), DivideToken, EqualsToken, IntToken(12355), IntToken(5), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), GreaterThanToken, LessThanToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 201") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == ) 12355 5 > ( reserved1 < = ident1231asdad => helloword /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, RightParenToken, IntToken(12355), IntToken(5), GreaterThanToken, LeftParenToken, IdentifierToken("reserved1"), LessThanToken, EqualsToken, IdentifierToken("ident1231asdad"), ArrowToken, IdentifierToken("helloword"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 202") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == ) ident1231asdad < ( = reserved1 > 5 => / helloword 12355")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, RightParenToken, IdentifierToken("ident1231asdad"), LessThanToken, LeftParenToken, EqualsToken, IdentifierToken("reserved1"), GreaterThanToken, IntToken(5), ArrowToken, DivideToken, IdentifierToken("helloword"), IntToken(12355))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 203") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == = ( ) reserved1 => helloword < ident1231asdad 5 12355 > /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, EqualsToken, LeftParenToken, RightParenToken, IdentifierToken("reserved1"), ArrowToken, IdentifierToken("helloword"), LessThanToken, IdentifierToken("ident1231asdad"), IntToken(5), IntToken(12355), GreaterThanToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 204") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == = => helloword ) < / > 12355 reserved1 5 ident1231asdad (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, EqualsToken, ArrowToken, IdentifierToken("helloword"), RightParenToken, LessThanToken, DivideToken, GreaterThanToken, IntToken(12355), IdentifierToken("reserved1"), IntToken(5), IdentifierToken("ident1231asdad"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 205") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == = 12355 5 ident1231asdad ) ( / reserved1 < => helloword >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, EqualsToken, IntToken(12355), IntToken(5), IdentifierToken("ident1231asdad"), RightParenToken, LeftParenToken, DivideToken, IdentifierToken("reserved1"), LessThanToken, ArrowToken, IdentifierToken("helloword"), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 206") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == = ident1231asdad < => ) ( helloword reserved1 5 12355 > /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, EqualsToken, IdentifierToken("ident1231asdad"), LessThanToken, ArrowToken, RightParenToken, LeftParenToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), IntToken(5), IntToken(12355), GreaterThanToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 207") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == => ( ) helloword < > ident1231asdad = 5 12355 reserved1 /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, ArrowToken, LeftParenToken, RightParenToken, IdentifierToken("helloword"), LessThanToken, GreaterThanToken, IdentifierToken("ident1231asdad"), EqualsToken, IntToken(5), IntToken(12355), IdentifierToken("reserved1"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 208") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == => = helloword > ) 12355 reserved1 5 ( / ident1231asdad <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, ArrowToken, EqualsToken, IdentifierToken("helloword"), GreaterThanToken, RightParenToken, IntToken(12355), IdentifierToken("reserved1"), IntToken(5), LeftParenToken, DivideToken, IdentifierToken("ident1231asdad"), LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 209") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == => 12355 reserved1 / = 5 > helloword ident1231asdad < ) (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, ArrowToken, IntToken(12355), IdentifierToken("reserved1"), DivideToken, EqualsToken, IntToken(5), GreaterThanToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), LessThanToken, RightParenToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 210") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == => ident1231asdad < 12355 ) reserved1 > ( 5 = helloword /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, ArrowToken, IdentifierToken("ident1231asdad"), LessThanToken, IntToken(12355), RightParenToken, IdentifierToken("reserved1"), GreaterThanToken, LeftParenToken, IntToken(5), EqualsToken, IdentifierToken("helloword"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 211") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == < ( = ) > helloword 12355 reserved1 => / ident1231asdad 5")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, LessThanToken, LeftParenToken, EqualsToken, RightParenToken, GreaterThanToken, IdentifierToken("helloword"), IntToken(12355), IdentifierToken("reserved1"), ArrowToken, DivideToken, IdentifierToken("ident1231asdad"), IntToken(5))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 212") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == < = helloword 5 => / ) ident1231asdad reserved1 12355 > (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, LessThanToken, EqualsToken, IdentifierToken("helloword"), IntToken(5), ArrowToken, DivideToken, RightParenToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IntToken(12355), GreaterThanToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 213") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == < 12355 reserved1 ) => ( = > ident1231asdad 5 helloword /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, LessThanToken, IntToken(12355), IdentifierToken("reserved1"), RightParenToken, ArrowToken, LeftParenToken, EqualsToken, GreaterThanToken, IdentifierToken("ident1231asdad"), IntToken(5), IdentifierToken("helloword"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 214") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == < ident1231asdad => reserved1 = ) / 5 > ( helloword 12355")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, LessThanToken, IdentifierToken("ident1231asdad"), ArrowToken, IdentifierToken("reserved1"), EqualsToken, RightParenToken, DivideToken, IntToken(5), GreaterThanToken, LeftParenToken, IdentifierToken("helloword"), IntToken(12355))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 215") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == > ( = < 12355 => helloword ident1231asdad reserved1 5 ) /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, GreaterThanToken, LeftParenToken, EqualsToken, LessThanToken, IntToken(12355), ArrowToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IntToken(5), RightParenToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 216") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == > = helloword ident1231asdad => < reserved1 ( 12355 ) 5 /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, GreaterThanToken, EqualsToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), ArrowToken, LessThanToken, IdentifierToken("reserved1"), LeftParenToken, IntToken(12355), RightParenToken, IntToken(5), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 217") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == > 12355 reserved1 => = 5 ident1231asdad < ( / helloword )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, GreaterThanToken, IntToken(12355), IdentifierToken("reserved1"), ArrowToken, EqualsToken, IntToken(5), IdentifierToken("ident1231asdad"), LessThanToken, LeftParenToken, DivideToken, IdentifierToken("helloword"), RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 218") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == > ident1231asdad < / => reserved1 12355 helloword 5 = ) (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, GreaterThanToken, IdentifierToken("ident1231asdad"), LessThanToken, DivideToken, ArrowToken, IdentifierToken("reserved1"), IntToken(12355), IdentifierToken("helloword"), IntToken(5), EqualsToken, RightParenToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 219") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == 12355 ( = 5 < helloword > ) reserved1 => ident1231asdad /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, IntToken(12355), LeftParenToken, EqualsToken, IntToken(5), LessThanToken, IdentifierToken("helloword"), GreaterThanToken, RightParenToken, IdentifierToken("reserved1"), ArrowToken, IdentifierToken("ident1231asdad"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 220") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == 12355 => / ) 5 ( > ident1231asdad < = helloword reserved1")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, IntToken(12355), ArrowToken, DivideToken, RightParenToken, IntToken(5), LeftParenToken, GreaterThanToken, IdentifierToken("ident1231asdad"), LessThanToken, EqualsToken, IdentifierToken("helloword"), IdentifierToken("reserved1"))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 221") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == 12355 > reserved1 5 => ( ) helloword ident1231asdad < = /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, IntToken(12355), GreaterThanToken, IdentifierToken("reserved1"), IntToken(5), ArrowToken, LeftParenToken, RightParenToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), LessThanToken, EqualsToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 222") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == 12355 ident1231asdad < ) > = ( => reserved1 5 helloword /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, IntToken(12355), IdentifierToken("ident1231asdad"), LessThanToken, RightParenToken, GreaterThanToken, EqualsToken, LeftParenToken, ArrowToken, IdentifierToken("reserved1"), IntToken(5), IdentifierToken("helloword"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 223") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == 5 ( = ident1231asdad > < / reserved1 => ) helloword 12355")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, IntToken(5), LeftParenToken, EqualsToken, IdentifierToken("ident1231asdad"), GreaterThanToken, LessThanToken, DivideToken, IdentifierToken("reserved1"), ArrowToken, RightParenToken, IdentifierToken("helloword"), IntToken(12355))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 224") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == 5 => / < 12355 > helloword ident1231asdad reserved1 = ) (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, IntToken(5), ArrowToken, DivideToken, LessThanToken, IntToken(12355), GreaterThanToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), EqualsToken, RightParenToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 225") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == 5 > reserved1 helloword => < ident1231asdad ( = ) 12355 /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, IntToken(5), GreaterThanToken, IdentifierToken("reserved1"), IdentifierToken("helloword"), ArrowToken, LessThanToken, IdentifierToken("ident1231asdad"), LeftParenToken, EqualsToken, RightParenToken, IntToken(12355), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 226") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == 5 ident1231asdad < => > reserved1 12355 = ( / helloword )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, IntToken(5), IdentifierToken("ident1231asdad"), LessThanToken, ArrowToken, GreaterThanToken, IdentifierToken("reserved1"), IntToken(12355), EqualsToken, LeftParenToken, DivideToken, IdentifierToken("helloword"), RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 227") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == reserved1 ( => / 12355 helloword > ident1231asdad 5 < = )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, IdentifierToken("reserved1"), LeftParenToken, ArrowToken, DivideToken, IntToken(12355), IdentifierToken("helloword"), GreaterThanToken, IdentifierToken("ident1231asdad"), IntToken(5), LessThanToken, EqualsToken, RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 228") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == reserved1 => / 12355 5 ( > = ident1231asdad < helloword )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, IdentifierToken("reserved1"), ArrowToken, DivideToken, IntToken(12355), IntToken(5), LeftParenToken, GreaterThanToken, EqualsToken, IdentifierToken("ident1231asdad"), LessThanToken, IdentifierToken("helloword"), RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 229") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == reserved1 > ident1231asdad ( 12355 ) => 5 = / helloword <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, IdentifierToken("reserved1"), GreaterThanToken, IdentifierToken("ident1231asdad"), LeftParenToken, IntToken(12355), RightParenToken, ArrowToken, IntToken(5), EqualsToken, DivideToken, IdentifierToken("helloword"), LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 230") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == reserved1 ident1231asdad < 12355 > ) ( helloword 5 => = /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, IdentifierToken("reserved1"), IdentifierToken("ident1231asdad"), LessThanToken, IntToken(12355), GreaterThanToken, RightParenToken, LeftParenToken, IdentifierToken("helloword"), IntToken(5), ArrowToken, EqualsToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 231") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == ident1231asdad ( => = 5 > ) < reserved1 12355 helloword /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, IdentifierToken("ident1231asdad"), LeftParenToken, ArrowToken, EqualsToken, IntToken(5), GreaterThanToken, RightParenToken, LessThanToken, IdentifierToken("reserved1"), IntToken(12355), IdentifierToken("helloword"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 232") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == ident1231asdad => / reserved1 12355 > ( 5 = ) helloword <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, IdentifierToken("ident1231asdad"), ArrowToken, DivideToken, IdentifierToken("reserved1"), IntToken(12355), GreaterThanToken, LeftParenToken, IntToken(5), EqualsToken, RightParenToken, IdentifierToken("helloword"), LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 233") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == ident1231asdad > reserved1 = 12355 < helloword 5 => ) ( /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, IdentifierToken("ident1231asdad"), GreaterThanToken, IdentifierToken("reserved1"), EqualsToken, IntToken(12355), LessThanToken, IdentifierToken("helloword"), IntToken(5), ArrowToken, RightParenToken, LeftParenToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 234") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == ident1231asdad reserved1 < helloword > 12355 5 ( = ) => /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), LessThanToken, IdentifierToken("helloword"), GreaterThanToken, IntToken(12355), IntToken(5), LeftParenToken, EqualsToken, RightParenToken, ArrowToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 235") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == helloword ( => > 5 ident1231asdad 12355 < ) / reserved1 =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, IdentifierToken("helloword"), LeftParenToken, ArrowToken, GreaterThanToken, IntToken(5), IdentifierToken("ident1231asdad"), IntToken(12355), LessThanToken, RightParenToken, DivideToken, IdentifierToken("reserved1"), EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 236") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == helloword => ( / reserved1 ) 12355 ident1231asdad 5 > < =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, IdentifierToken("helloword"), ArrowToken, LeftParenToken, DivideToken, IdentifierToken("reserved1"), RightParenToken, IntToken(12355), IdentifierToken("ident1231asdad"), IntToken(5), GreaterThanToken, LessThanToken, EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 237") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == helloword > reserved1 < 5 ( => ) 12355 = ident1231asdad /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, IdentifierToken("helloword"), GreaterThanToken, IdentifierToken("reserved1"), LessThanToken, IntToken(5), LeftParenToken, ArrowToken, RightParenToken, IntToken(12355), EqualsToken, IdentifierToken("ident1231asdad"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 238") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * == helloword reserved1 > ( 5 = => 12355 ) / ident1231asdad <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, DoubleEqualsToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), GreaterThanToken, LeftParenToken, IntToken(5), EqualsToken, ArrowToken, IntToken(12355), RightParenToken, DivideToken, IdentifierToken("ident1231asdad"), LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 239") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => / ) < reserved1 ident1231asdad > = helloword 5 12355 == (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, DivideToken, RightParenToken, LessThanToken, IdentifierToken("reserved1"), IdentifierToken("ident1231asdad"), GreaterThanToken, EqualsToken, IdentifierToken("helloword"), IntToken(5), IntToken(12355), DoubleEqualsToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 240") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => / < ) == ident1231asdad 5 = > reserved1 12355 helloword (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, DivideToken, LessThanToken, RightParenToken, DoubleEqualsToken, IdentifierToken("ident1231asdad"), IntToken(5), EqualsToken, GreaterThanToken, IdentifierToken("reserved1"), IntToken(12355), IdentifierToken("helloword"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 241") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => / 12355 ident1231asdad reserved1 5 > ( < = ) helloword ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, DivideToken, IntToken(12355), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IntToken(5), GreaterThanToken, LeftParenToken, LessThanToken, EqualsToken, RightParenToken, IdentifierToken("helloword"), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 242") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => / ident1231asdad 12355 == reserved1 5 helloword > < = ) (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, DivideToken, IdentifierToken("ident1231asdad"), IntToken(12355), DoubleEqualsToken, IdentifierToken("reserved1"), IntToken(5), IdentifierToken("helloword"), GreaterThanToken, LessThanToken, EqualsToken, RightParenToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 243") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => ( ) < helloword reserved1 ident1231asdad 5 = > == 12355 /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, LeftParenToken, RightParenToken, LessThanToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), IdentifierToken("ident1231asdad"), IntToken(5), EqualsToken, GreaterThanToken, DoubleEqualsToken, IntToken(12355), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 244") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => ( < ) 12355 helloword / reserved1 5 == = ident1231asdad >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, LeftParenToken, LessThanToken, RightParenToken, IntToken(12355), IdentifierToken("helloword"), DivideToken, IdentifierToken("reserved1"), IntToken(5), DoubleEqualsToken, EqualsToken, IdentifierToken("ident1231asdad"), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 245") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => ( 12355 helloword / ident1231asdad = > reserved1 5 < == )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, LeftParenToken, IntToken(12355), IdentifierToken("helloword"), DivideToken, IdentifierToken("ident1231asdad"), EqualsToken, GreaterThanToken, IdentifierToken("reserved1"), IntToken(5), LessThanToken, DoubleEqualsToken, RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 246") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => ( ident1231asdad 12355 > helloword = < ) 5 == reserved1 /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, LeftParenToken, IdentifierToken("ident1231asdad"), IntToken(12355), GreaterThanToken, IdentifierToken("helloword"), EqualsToken, LessThanToken, RightParenToken, IntToken(5), DoubleEqualsToken, IdentifierToken("reserved1"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 247") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => ) ( > = helloword 12355 < reserved1 == / ident1231asdad 5")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, RightParenToken, LeftParenToken, GreaterThanToken, EqualsToken, IdentifierToken("helloword"), IntToken(12355), LessThanToken, IdentifierToken("reserved1"), DoubleEqualsToken, DivideToken, IdentifierToken("ident1231asdad"), IntToken(5))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 248") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => ) < ( reserved1 helloword 12355 = ident1231asdad 5 > == /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, RightParenToken, LessThanToken, LeftParenToken, IdentifierToken("reserved1"), IdentifierToken("helloword"), IntToken(12355), EqualsToken, IdentifierToken("ident1231asdad"), IntToken(5), GreaterThanToken, DoubleEqualsToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 249") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => ) 12355 helloword = ident1231asdad 5 ( == > < reserved1 /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, RightParenToken, IntToken(12355), IdentifierToken("helloword"), EqualsToken, IdentifierToken("ident1231asdad"), IntToken(5), LeftParenToken, DoubleEqualsToken, GreaterThanToken, LessThanToken, IdentifierToken("reserved1"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 250") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => ) ident1231asdad 12355 reserved1 helloword 5 / < = ( > ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, RightParenToken, IdentifierToken("ident1231asdad"), IntToken(12355), IdentifierToken("reserved1"), IdentifierToken("helloword"), IntToken(5), DivideToken, LessThanToken, EqualsToken, LeftParenToken, GreaterThanToken, DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 251") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => = ( > < helloword ident1231asdad reserved1 5 12355 == ) /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, EqualsToken, LeftParenToken, GreaterThanToken, LessThanToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IntToken(5), IntToken(12355), DoubleEqualsToken, RightParenToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 252") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => = < ) / ( == helloword 12355 reserved1 5 ident1231asdad >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, EqualsToken, LessThanToken, RightParenToken, DivideToken, LeftParenToken, DoubleEqualsToken, IdentifierToken("helloword"), IntToken(12355), IdentifierToken("reserved1"), IntToken(5), IdentifierToken("ident1231asdad"), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 253") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => = 12355 helloword > / ) reserved1 5 == ( ident1231asdad <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, EqualsToken, IntToken(12355), IdentifierToken("helloword"), GreaterThanToken, DivideToken, RightParenToken, IdentifierToken("reserved1"), IntToken(5), DoubleEqualsToken, LeftParenToken, IdentifierToken("ident1231asdad"), LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 254") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => = ident1231asdad 5 ( / < 12355 helloword reserved1 > == )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, EqualsToken, IdentifierToken("ident1231asdad"), IntToken(5), LeftParenToken, DivideToken, LessThanToken, IntToken(12355), IdentifierToken("helloword"), IdentifierToken("reserved1"), GreaterThanToken, DoubleEqualsToken, RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 255") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => == ( > reserved1 / 12355 5 = ident1231asdad < helloword )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, DoubleEqualsToken, LeftParenToken, GreaterThanToken, IdentifierToken("reserved1"), DivideToken, IntToken(12355), IntToken(5), EqualsToken, IdentifierToken("ident1231asdad"), LessThanToken, IdentifierToken("helloword"), RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 256") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => == < ) = / reserved1 12355 ident1231asdad > ( helloword 5")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, DoubleEqualsToken, LessThanToken, RightParenToken, EqualsToken, DivideToken, IdentifierToken("reserved1"), IntToken(12355), IdentifierToken("ident1231asdad"), GreaterThanToken, LeftParenToken, IdentifierToken("helloword"), IntToken(5))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 257") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => == 12355 helloword reserved1 / 5 ) ident1231asdad > < = (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, DoubleEqualsToken, IntToken(12355), IdentifierToken("helloword"), IdentifierToken("reserved1"), DivideToken, IntToken(5), RightParenToken, IdentifierToken("ident1231asdad"), GreaterThanToken, LessThanToken, EqualsToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 258") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => == ident1231asdad 5 = / helloword ) < 12355 > reserved1 (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, DoubleEqualsToken, IdentifierToken("ident1231asdad"), IntToken(5), EqualsToken, DivideToken, IdentifierToken("helloword"), RightParenToken, LessThanToken, IntToken(12355), GreaterThanToken, IdentifierToken("reserved1"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 259") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => < ( > helloword ) / = reserved1 12355 == ident1231asdad 5")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, LessThanToken, LeftParenToken, GreaterThanToken, IdentifierToken("helloword"), RightParenToken, DivideToken, EqualsToken, IdentifierToken("reserved1"), IntToken(12355), DoubleEqualsToken, IdentifierToken("ident1231asdad"), IntToken(5))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 260") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => < == ) 12355 ( / helloword ident1231asdad reserved1 5 > =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, LessThanToken, DoubleEqualsToken, RightParenToken, IntToken(12355), LeftParenToken, DivideToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IntToken(5), GreaterThanToken, EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 261") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => < 5 / ( = == helloword > reserved1 12355 ident1231asdad )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, LessThanToken, IntToken(5), DivideToken, LeftParenToken, EqualsToken, DoubleEqualsToken, IdentifierToken("helloword"), GreaterThanToken, IdentifierToken("reserved1"), IntToken(12355), IdentifierToken("ident1231asdad"), RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 262") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => < ident1231asdad 5 > ( = reserved1 12355 ) / helloword ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, LessThanToken, IdentifierToken("ident1231asdad"), IntToken(5), GreaterThanToken, LeftParenToken, EqualsToken, IdentifierToken("reserved1"), IntToken(12355), RightParenToken, DivideToken, IdentifierToken("helloword"), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 263") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => > ( 12355 ) = 5 reserved1 helloword ident1231asdad < == /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, GreaterThanToken, LeftParenToken, IntToken(12355), RightParenToken, EqualsToken, IntToken(5), IdentifierToken("reserved1"), IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), LessThanToken, DoubleEqualsToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 264") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => > == ) reserved1 ( 5 12355 = ident1231asdad < helloword /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, GreaterThanToken, DoubleEqualsToken, RightParenToken, IdentifierToken("reserved1"), LeftParenToken, IntToken(5), IntToken(12355), EqualsToken, IdentifierToken("ident1231asdad"), LessThanToken, IdentifierToken("helloword"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 265") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => > 5 / = ) ident1231asdad < reserved1 == ( helloword 12355")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, GreaterThanToken, IntToken(5), DivideToken, EqualsToken, RightParenToken, IdentifierToken("ident1231asdad"), LessThanToken, IdentifierToken("reserved1"), DoubleEqualsToken, LeftParenToken, IdentifierToken("helloword"), IntToken(12355))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 266") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => > ident1231asdad 5 reserved1 ( helloword ) 12355 < == = /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, GreaterThanToken, IdentifierToken("ident1231asdad"), IntToken(5), IdentifierToken("reserved1"), LeftParenToken, IdentifierToken("helloword"), RightParenToken, IntToken(12355), LessThanToken, DoubleEqualsToken, EqualsToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 267") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => 12355 ( > == = / < 5 ident1231asdad reserved1 helloword )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, IntToken(12355), LeftParenToken, GreaterThanToken, DoubleEqualsToken, EqualsToken, DivideToken, LessThanToken, IntToken(5), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IdentifierToken("helloword"), RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 268") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => 12355 == ) helloword = ( / reserved1 > < ident1231asdad 5")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, IntToken(12355), DoubleEqualsToken, RightParenToken, IdentifierToken("helloword"), EqualsToken, LeftParenToken, DivideToken, IdentifierToken("reserved1"), GreaterThanToken, LessThanToken, IdentifierToken("ident1231asdad"), IntToken(5))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 269") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => 12355 5 / < = ) helloword ident1231asdad reserved1 > == (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, IntToken(12355), IntToken(5), DivideToken, LessThanToken, EqualsToken, RightParenToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), GreaterThanToken, DoubleEqualsToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 270") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => 12355 ident1231asdad reserved1 / = == helloword ) > < 5 (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, IntToken(12355), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), DivideToken, EqualsToken, DoubleEqualsToken, IdentifierToken("helloword"), RightParenToken, GreaterThanToken, LessThanToken, IntToken(5), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 271") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => 5 ( > 12355 = < ident1231asdad reserved1 ) / helloword ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, IntToken(5), LeftParenToken, GreaterThanToken, IntToken(12355), EqualsToken, LessThanToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), RightParenToken, DivideToken, IdentifierToken("helloword"), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 272") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => 5 == = ( < reserved1 12355 helloword ident1231asdad > ) /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, IntToken(5), DoubleEqualsToken, EqualsToken, LeftParenToken, LessThanToken, IdentifierToken("reserved1"), IntToken(12355), IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), GreaterThanToken, RightParenToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 273") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => 5 12355 / reserved1 = ident1231asdad < ) > == helloword (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, IntToken(5), IntToken(12355), DivideToken, IdentifierToken("reserved1"), EqualsToken, IdentifierToken("ident1231asdad"), LessThanToken, RightParenToken, GreaterThanToken, DoubleEqualsToken, IdentifierToken("helloword"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 274") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => 5 ident1231asdad reserved1 ) = helloword == > ( / 12355 <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, IntToken(5), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), RightParenToken, EqualsToken, IdentifierToken("helloword"), DoubleEqualsToken, GreaterThanToken, LeftParenToken, DivideToken, IntToken(12355), LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 275") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => reserved1 ( > ident1231asdad == / = helloword 5 12355 < )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, IdentifierToken("reserved1"), LeftParenToken, GreaterThanToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken, DivideToken, EqualsToken, IdentifierToken("helloword"), IntToken(5), IntToken(12355), LessThanToken, RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 276") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => reserved1 == = < > ( ) 12355 ident1231asdad 5 helloword /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, IdentifierToken("reserved1"), DoubleEqualsToken, EqualsToken, LessThanToken, GreaterThanToken, LeftParenToken, RightParenToken, IntToken(12355), IdentifierToken("ident1231asdad"), IntToken(5), IdentifierToken("helloword"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 277") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => reserved1 12355 / helloword == = ( 5 < ) ident1231asdad >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, IdentifierToken("reserved1"), IntToken(12355), DivideToken, IdentifierToken("helloword"), DoubleEqualsToken, EqualsToken, LeftParenToken, IntToken(5), LessThanToken, RightParenToken, IdentifierToken("ident1231asdad"), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 278") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => reserved1 ident1231asdad 5 == = ) helloword 12355 > < ( /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, IdentifierToken("reserved1"), IdentifierToken("ident1231asdad"), IntToken(5), DoubleEqualsToken, EqualsToken, RightParenToken, IdentifierToken("helloword"), IntToken(12355), GreaterThanToken, LessThanToken, LeftParenToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 279") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => ident1231asdad ( 12355 / < > helloword = 5 == reserved1 )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, IdentifierToken("ident1231asdad"), LeftParenToken, IntToken(12355), DivideToken, LessThanToken, GreaterThanToken, IdentifierToken("helloword"), EqualsToken, IntToken(5), DoubleEqualsToken, IdentifierToken("reserved1"), RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 280") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => ident1231asdad == = 12355 < 5 reserved1 > ( / helloword )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken, EqualsToken, IntToken(12355), LessThanToken, IntToken(5), IdentifierToken("reserved1"), GreaterThanToken, LeftParenToken, DivideToken, IdentifierToken("helloword"), RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 281") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => ident1231asdad 12355 ( ) < reserved1 > helloword 5 == = /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, IdentifierToken("ident1231asdad"), IntToken(12355), LeftParenToken, RightParenToken, LessThanToken, IdentifierToken("reserved1"), GreaterThanToken, IdentifierToken("helloword"), IntToken(5), DoubleEqualsToken, EqualsToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 282") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => ident1231asdad reserved1 5 > = helloword == ( < ) 12355 /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IntToken(5), GreaterThanToken, EqualsToken, IdentifierToken("helloword"), DoubleEqualsToken, LeftParenToken, LessThanToken, RightParenToken, IntToken(12355), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 283") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => helloword ( 12355 = > / < reserved1 == ) ident1231asdad 5")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, IdentifierToken("helloword"), LeftParenToken, IntToken(12355), EqualsToken, GreaterThanToken, DivideToken, LessThanToken, IdentifierToken("reserved1"), DoubleEqualsToken, RightParenToken, IdentifierToken("ident1231asdad"), IntToken(5))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 284") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => helloword == = reserved1 > ( ) ident1231asdad 5 12355 < /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, IdentifierToken("helloword"), DoubleEqualsToken, EqualsToken, IdentifierToken("reserved1"), GreaterThanToken, LeftParenToken, RightParenToken, IdentifierToken("ident1231asdad"), IntToken(5), IntToken(12355), LessThanToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 285") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => helloword 12355 ( == > = ) < reserved1 5 ident1231asdad /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, IdentifierToken("helloword"), IntToken(12355), LeftParenToken, DoubleEqualsToken, GreaterThanToken, EqualsToken, RightParenToken, LessThanToken, IdentifierToken("reserved1"), IntToken(5), IdentifierToken("ident1231asdad"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 286") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * => helloword reserved1 5 ident1231asdad == = / > ) ( 12355 <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, ArrowToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), IntToken(5), IdentifierToken("ident1231asdad"), DoubleEqualsToken, EqualsToken, DivideToken, GreaterThanToken, RightParenToken, LeftParenToken, IntToken(12355), LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 287") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < / ) 5 > 12355 => helloword ident1231asdad reserved1 == = (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, DivideToken, RightParenToken, IntToken(5), GreaterThanToken, IntToken(12355), ArrowToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), DoubleEqualsToken, EqualsToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 288") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < / => > ( 5 reserved1 helloword = 12355 == ident1231asdad )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, DivideToken, ArrowToken, GreaterThanToken, LeftParenToken, IntToken(5), IdentifierToken("reserved1"), IdentifierToken("helloword"), EqualsToken, IntToken(12355), DoubleEqualsToken, IdentifierToken("ident1231asdad"), RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 289") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < / 5 ) 12355 > ident1231asdad reserved1 => = ( helloword ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, DivideToken, IntToken(5), RightParenToken, IntToken(12355), GreaterThanToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), ArrowToken, EqualsToken, LeftParenToken, IdentifierToken("helloword"), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 290") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < / ident1231asdad helloword ) > reserved1 => 5 12355 == = (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, DivideToken, IdentifierToken("ident1231asdad"), IdentifierToken("helloword"), RightParenToken, GreaterThanToken, IdentifierToken("reserved1"), ArrowToken, IntToken(5), IntToken(12355), DoubleEqualsToken, EqualsToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 291") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < ( ) 5 reserved1 12355 / > == ident1231asdad => helloword =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, LeftParenToken, RightParenToken, IntToken(5), IdentifierToken("reserved1"), IntToken(12355), DivideToken, GreaterThanToken, DoubleEqualsToken, IdentifierToken("ident1231asdad"), ArrowToken, IdentifierToken("helloword"), EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 292") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < ( => > = reserved1 ) 12355 ident1231asdad == / helloword 5")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, LeftParenToken, ArrowToken, GreaterThanToken, EqualsToken, IdentifierToken("reserved1"), RightParenToken, IntToken(12355), IdentifierToken("ident1231asdad"), DoubleEqualsToken, DivideToken, IdentifierToken("helloword"), IntToken(5))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 293") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < ( 5 ) ident1231asdad 12355 == = helloword reserved1 > => /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, LeftParenToken, IntToken(5), RightParenToken, IdentifierToken("ident1231asdad"), IntToken(12355), DoubleEqualsToken, EqualsToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), GreaterThanToken, ArrowToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 294") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < ( ident1231asdad helloword == 12355 => ) = 5 > reserved1 /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, LeftParenToken, IdentifierToken("ident1231asdad"), IdentifierToken("helloword"), DoubleEqualsToken, IntToken(12355), ArrowToken, RightParenToken, EqualsToken, IntToken(5), GreaterThanToken, IdentifierToken("reserved1"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 295") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < ) ( 5 helloword 12355 > / reserved1 == = ident1231asdad =>")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, RightParenToken, LeftParenToken, IntToken(5), IdentifierToken("helloword"), IntToken(12355), GreaterThanToken, DivideToken, IdentifierToken("reserved1"), DoubleEqualsToken, EqualsToken, IdentifierToken("ident1231asdad"), ArrowToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 296") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < ) => > 12355 reserved1 5 helloword ident1231asdad == = ( /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, RightParenToken, ArrowToken, GreaterThanToken, IntToken(12355), IdentifierToken("reserved1"), IntToken(5), IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), DoubleEqualsToken, EqualsToken, LeftParenToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 297") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < ) 5 = / reserved1 ident1231asdad helloword == > => 12355 (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, RightParenToken, IntToken(5), EqualsToken, DivideToken, IdentifierToken("reserved1"), IdentifierToken("ident1231asdad"), IdentifierToken("helloword"), DoubleEqualsToken, GreaterThanToken, ArrowToken, IntToken(12355), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 298") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < ) ident1231asdad helloword > 12355 reserved1 => == ( / 5 =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, RightParenToken, IdentifierToken("ident1231asdad"), IdentifierToken("helloword"), GreaterThanToken, IntToken(12355), IdentifierToken("reserved1"), ArrowToken, DoubleEqualsToken, LeftParenToken, DivideToken, IntToken(5), EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 299") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < = ( reserved1 ) ident1231asdad / 12355 helloword 5 > => ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, EqualsToken, LeftParenToken, IdentifierToken("reserved1"), RightParenToken, IdentifierToken("ident1231asdad"), DivideToken, IntToken(12355), IdentifierToken("helloword"), IntToken(5), GreaterThanToken, ArrowToken, DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 300") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < = => > reserved1 ident1231asdad ( 12355 ) 5 == helloword /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, EqualsToken, ArrowToken, GreaterThanToken, IdentifierToken("reserved1"), IdentifierToken("ident1231asdad"), LeftParenToken, IntToken(12355), RightParenToken, IntToken(5), DoubleEqualsToken, IdentifierToken("helloword"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 301") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < = 5 ) == ident1231asdad => > reserved1 ( / helloword 12355")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, EqualsToken, IntToken(5), RightParenToken, DoubleEqualsToken, IdentifierToken("ident1231asdad"), ArrowToken, GreaterThanToken, IdentifierToken("reserved1"), LeftParenToken, DivideToken, IdentifierToken("helloword"), IntToken(12355))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 302") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < = ident1231asdad helloword 5 12355 == ( reserved1 > => ) /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, EqualsToken, IdentifierToken("ident1231asdad"), IdentifierToken("helloword"), IntToken(5), IntToken(12355), DoubleEqualsToken, LeftParenToken, IdentifierToken("reserved1"), GreaterThanToken, ArrowToken, RightParenToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 303") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < == ( reserved1 => ident1231asdad 12355 ) = 5 > helloword /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, DoubleEqualsToken, LeftParenToken, IdentifierToken("reserved1"), ArrowToken, IdentifierToken("ident1231asdad"), IntToken(12355), RightParenToken, EqualsToken, IntToken(5), GreaterThanToken, IdentifierToken("helloword"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 304") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < == => > helloword reserved1 5 / 12355 ) ( ident1231asdad =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, DoubleEqualsToken, ArrowToken, GreaterThanToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), IntToken(5), DivideToken, IntToken(12355), RightParenToken, LeftParenToken, IdentifierToken("ident1231asdad"), EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 305") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < == 5 ) > ident1231asdad reserved1 helloword 12355 => = ( /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, DoubleEqualsToken, IntToken(5), RightParenToken, GreaterThanToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IdentifierToken("helloword"), IntToken(12355), ArrowToken, EqualsToken, LeftParenToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 306") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < == helloword / ( reserved1 ident1231asdad 5 = > => 12355 )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, DoubleEqualsToken, IdentifierToken("helloword"), DivideToken, LeftParenToken, IdentifierToken("reserved1"), IdentifierToken("ident1231asdad"), IntToken(5), EqualsToken, GreaterThanToken, ArrowToken, IntToken(12355), RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 307") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < => ( reserved1 12355 helloword / 5 > = ) ident1231asdad ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, ArrowToken, LeftParenToken, IdentifierToken("reserved1"), IntToken(12355), IdentifierToken("helloword"), DivideToken, IntToken(5), GreaterThanToken, EqualsToken, RightParenToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 308") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < => == 12355 ( helloword ) 5 ident1231asdad reserved1 > = /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, ArrowToken, DoubleEqualsToken, IntToken(12355), LeftParenToken, IdentifierToken("helloword"), RightParenToken, IntToken(5), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), GreaterThanToken, EqualsToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 309") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < => 5 ) reserved1 helloword = > ( 12355 == ident1231asdad /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, ArrowToken, IntToken(5), RightParenToken, IdentifierToken("reserved1"), IdentifierToken("helloword"), EqualsToken, GreaterThanToken, LeftParenToken, IntToken(12355), DoubleEqualsToken, IdentifierToken("ident1231asdad"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 310") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < => helloword / = ident1231asdad > == 5 ) ( reserved1 12355")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, ArrowToken, IdentifierToken("helloword"), DivideToken, EqualsToken, IdentifierToken("ident1231asdad"), GreaterThanToken, DoubleEqualsToken, IntToken(5), RightParenToken, LeftParenToken, IdentifierToken("reserved1"), IntToken(12355))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 311") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < > ( reserved1 ident1231asdad helloword => ) 5 12355 == = /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, GreaterThanToken, LeftParenToken, IdentifierToken("reserved1"), IdentifierToken("ident1231asdad"), IdentifierToken("helloword"), ArrowToken, RightParenToken, IntToken(5), IntToken(12355), DoubleEqualsToken, EqualsToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 312") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < > == 12355 = helloword reserved1 ( ) 5 => ident1231asdad /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, GreaterThanToken, DoubleEqualsToken, IntToken(12355), EqualsToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), LeftParenToken, RightParenToken, IntToken(5), ArrowToken, IdentifierToken("ident1231asdad"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 313") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < > 5 ) helloword ident1231asdad reserved1 / => = ( 12355 ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, GreaterThanToken, IntToken(5), RightParenToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), DivideToken, ArrowToken, EqualsToken, LeftParenToken, IntToken(12355), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 314") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < > helloword / => ident1231asdad reserved1 5 12355 == = ) (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, GreaterThanToken, IdentifierToken("helloword"), DivideToken, ArrowToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IntToken(5), IntToken(12355), DoubleEqualsToken, EqualsToken, RightParenToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 315") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < 12355 ( ident1231asdad ) / = helloword => 5 > reserved1 ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, IntToken(12355), LeftParenToken, IdentifierToken("ident1231asdad"), RightParenToken, DivideToken, EqualsToken, IdentifierToken("helloword"), ArrowToken, IntToken(5), GreaterThanToken, IdentifierToken("reserved1"), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 316") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < 12355 == > reserved1 / ) ident1231asdad 5 = ( helloword =>")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, IntToken(12355), DoubleEqualsToken, GreaterThanToken, IdentifierToken("reserved1"), DivideToken, RightParenToken, IdentifierToken("ident1231asdad"), IntToken(5), EqualsToken, LeftParenToken, IdentifierToken("helloword"), ArrowToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 317") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < 12355 5 = ) / => reserved1 helloword ident1231asdad > == (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, IntToken(12355), IntToken(5), EqualsToken, RightParenToken, DivideToken, ArrowToken, IdentifierToken("reserved1"), IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), GreaterThanToken, DoubleEqualsToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 318") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < 12355 helloword / reserved1 ( => > = 5 == ident1231asdad )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, IntToken(12355), IdentifierToken("helloword"), DivideToken, IdentifierToken("reserved1"), LeftParenToken, ArrowToken, GreaterThanToken, EqualsToken, IntToken(5), DoubleEqualsToken, IdentifierToken("ident1231asdad"), RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 319") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < 5 ( ident1231asdad == / 12355 => reserved1 = ) helloword >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, IntToken(5), LeftParenToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken, DivideToken, IntToken(12355), ArrowToken, IdentifierToken("reserved1"), EqualsToken, RightParenToken, IdentifierToken("helloword"), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 320") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < 5 == > helloword / reserved1 ) ident1231asdad 12355 => = (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, IntToken(5), DoubleEqualsToken, GreaterThanToken, IdentifierToken("helloword"), DivideToken, IdentifierToken("reserved1"), RightParenToken, IdentifierToken("ident1231asdad"), IntToken(12355), ArrowToken, EqualsToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 321") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < 5 12355 = => / helloword ) == reserved1 > ident1231asdad (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, IntToken(5), IntToken(12355), EqualsToken, ArrowToken, DivideToken, IdentifierToken("helloword"), RightParenToken, DoubleEqualsToken, IdentifierToken("reserved1"), GreaterThanToken, IdentifierToken("ident1231asdad"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 322") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < 5 helloword ( / = ) == reserved1 > => ident1231asdad 12355")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, IntToken(5), IdentifierToken("helloword"), LeftParenToken, DivideToken, EqualsToken, RightParenToken, DoubleEqualsToken, IdentifierToken("reserved1"), GreaterThanToken, ArrowToken, IdentifierToken("ident1231asdad"), IntToken(12355))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 323") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < reserved1 ( ident1231asdad > ) / helloword 5 12355 => == =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, IdentifierToken("reserved1"), LeftParenToken, IdentifierToken("ident1231asdad"), GreaterThanToken, RightParenToken, DivideToken, IdentifierToken("helloword"), IntToken(5), IntToken(12355), ArrowToken, DoubleEqualsToken, EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 324") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < reserved1 == 12355 ( ) = helloword => 5 > ident1231asdad /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, IdentifierToken("reserved1"), DoubleEqualsToken, IntToken(12355), LeftParenToken, RightParenToken, EqualsToken, IdentifierToken("helloword"), ArrowToken, IntToken(5), GreaterThanToken, IdentifierToken("ident1231asdad"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 325") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < reserved1 12355 = 5 ( == ident1231asdad > ) / helloword =>")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, IdentifierToken("reserved1"), IntToken(12355), EqualsToken, IntToken(5), LeftParenToken, DoubleEqualsToken, IdentifierToken("ident1231asdad"), GreaterThanToken, RightParenToken, DivideToken, IdentifierToken("helloword"), ArrowToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 326") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < reserved1 helloword ( = ) > 12355 ident1231asdad 5 => == /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, IdentifierToken("reserved1"), IdentifierToken("helloword"), LeftParenToken, EqualsToken, RightParenToken, GreaterThanToken, IntToken(12355), IdentifierToken("ident1231asdad"), IntToken(5), ArrowToken, DoubleEqualsToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 327") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < ident1231asdad ( reserved1 5 ) > => = 12355 == helloword /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, IdentifierToken("ident1231asdad"), LeftParenToken, IdentifierToken("reserved1"), IntToken(5), RightParenToken, GreaterThanToken, ArrowToken, EqualsToken, IntToken(12355), DoubleEqualsToken, IdentifierToken("helloword"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 328") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < ident1231asdad == 12355 = ( reserved1 => 5 ) / helloword >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken, IntToken(12355), EqualsToken, LeftParenToken, IdentifierToken("reserved1"), ArrowToken, IntToken(5), RightParenToken, DivideToken, IdentifierToken("helloword"), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 329") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < ident1231asdad 12355 = helloword ( reserved1 ) 5 > => == /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, IdentifierToken("ident1231asdad"), IntToken(12355), EqualsToken, IdentifierToken("helloword"), LeftParenToken, IdentifierToken("reserved1"), RightParenToken, IntToken(5), GreaterThanToken, ArrowToken, DoubleEqualsToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 330") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < ident1231asdad helloword ( => = / == > 5 12355 reserved1 )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, IdentifierToken("ident1231asdad"), IdentifierToken("helloword"), LeftParenToken, ArrowToken, EqualsToken, DivideToken, DoubleEqualsToken, GreaterThanToken, IntToken(5), IntToken(12355), IdentifierToken("reserved1"), RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 331") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < helloword ( ident1231asdad / == = ) 5 > => reserved1 12355")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, IdentifierToken("helloword"), LeftParenToken, IdentifierToken("ident1231asdad"), DivideToken, DoubleEqualsToken, EqualsToken, RightParenToken, IntToken(5), GreaterThanToken, ArrowToken, IdentifierToken("reserved1"), IntToken(12355))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 332") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < helloword == 12355 > ) ( ident1231asdad reserved1 5 => = /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, IdentifierToken("helloword"), DoubleEqualsToken, IntToken(12355), GreaterThanToken, RightParenToken, LeftParenToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IntToken(5), ArrowToken, EqualsToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 333") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < helloword 12355 == ( = => ident1231asdad ) 5 > reserved1 /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, IdentifierToken("helloword"), IntToken(12355), DoubleEqualsToken, LeftParenToken, EqualsToken, ArrowToken, IdentifierToken("ident1231asdad"), RightParenToken, IntToken(5), GreaterThanToken, IdentifierToken("reserved1"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 334") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * < helloword ident1231asdad ( 12355 = => 5 > ) / reserved1 ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, LessThanToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), LeftParenToken, IntToken(12355), EqualsToken, ArrowToken, IntToken(5), GreaterThanToken, RightParenToken, DivideToken, IdentifierToken("reserved1"), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 335") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > / ) helloword == => 5 12355 ident1231asdad reserved1 < = (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, DivideToken, RightParenToken, IdentifierToken("helloword"), DoubleEqualsToken, ArrowToken, IntToken(5), IntToken(12355), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), LessThanToken, EqualsToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 336") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > / => 5 ident1231asdad = reserved1 < ) 12355 == helloword (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, DivideToken, ArrowToken, IntToken(5), IdentifierToken("ident1231asdad"), EqualsToken, IdentifierToken("reserved1"), LessThanToken, RightParenToken, IntToken(12355), DoubleEqualsToken, IdentifierToken("helloword"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 337") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > / 5 => == = helloword < reserved1 ) ( ident1231asdad 12355")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, DivideToken, IntToken(5), ArrowToken, DoubleEqualsToken, EqualsToken, IdentifierToken("helloword"), LessThanToken, IdentifierToken("reserved1"), RightParenToken, LeftParenToken, IdentifierToken("ident1231asdad"), IntToken(12355))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 338") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > / helloword ) ident1231asdad => ( == reserved1 5 12355 < =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, DivideToken, IdentifierToken("helloword"), RightParenToken, IdentifierToken("ident1231asdad"), ArrowToken, LeftParenToken, DoubleEqualsToken, IdentifierToken("reserved1"), IntToken(5), IntToken(12355), LessThanToken, EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 339") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > ( ) helloword < => = == 12355 reserved1 5 ident1231asdad /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, LeftParenToken, RightParenToken, IdentifierToken("helloword"), LessThanToken, ArrowToken, EqualsToken, DoubleEqualsToken, IntToken(12355), IdentifierToken("reserved1"), IntToken(5), IdentifierToken("ident1231asdad"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 340") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > ( => reserved1 / < == ) ident1231asdad 12355 = helloword 5")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, LeftParenToken, ArrowToken, IdentifierToken("reserved1"), DivideToken, LessThanToken, DoubleEqualsToken, RightParenToken, IdentifierToken("ident1231asdad"), IntToken(12355), EqualsToken, IdentifierToken("helloword"), IntToken(5))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 341") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > ( 5 => 12355 == = helloword ident1231asdad reserved1 < ) /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, LeftParenToken, IntToken(5), ArrowToken, IntToken(12355), DoubleEqualsToken, EqualsToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), LessThanToken, RightParenToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 342") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > ( helloword = ) < 12355 ident1231asdad == 5 => reserved1 /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, LeftParenToken, IdentifierToken("helloword"), EqualsToken, RightParenToken, LessThanToken, IntToken(12355), IdentifierToken("ident1231asdad"), DoubleEqualsToken, IntToken(5), ArrowToken, IdentifierToken("reserved1"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 343") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > ) ( helloword 5 => 12355 reserved1 < = / ident1231asdad ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, RightParenToken, LeftParenToken, IdentifierToken("helloword"), IntToken(5), ArrowToken, IntToken(12355), IdentifierToken("reserved1"), LessThanToken, EqualsToken, DivideToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 344") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > ) => reserved1 = < ident1231asdad 12355 helloword 5 == ( /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, RightParenToken, ArrowToken, IdentifierToken("reserved1"), EqualsToken, LessThanToken, IdentifierToken("ident1231asdad"), IntToken(12355), IdentifierToken("helloword"), IntToken(5), DoubleEqualsToken, LeftParenToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 345") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > ) 5 => ident1231asdad == helloword < ( 12355 = reserved1 /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, RightParenToken, IntToken(5), ArrowToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken, IdentifierToken("helloword"), LessThanToken, LeftParenToken, IntToken(12355), EqualsToken, IdentifierToken("reserved1"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 346") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > ) helloword = => 12355 / < reserved1 == ( ident1231asdad 5")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, RightParenToken, IdentifierToken("helloword"), EqualsToken, ArrowToken, IntToken(12355), DivideToken, LessThanToken, IdentifierToken("reserved1"), DoubleEqualsToken, LeftParenToken, IdentifierToken("ident1231asdad"), IntToken(5))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 347") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > = ( helloword ident1231asdad < ) == reserved1 5 12355 => /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, EqualsToken, LeftParenToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), LessThanToken, RightParenToken, DoubleEqualsToken, IdentifierToken("reserved1"), IntToken(5), IntToken(12355), ArrowToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 348") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > = => reserved1 < 12355 ) ( == ident1231asdad 5 helloword /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, EqualsToken, ArrowToken, IdentifierToken("reserved1"), LessThanToken, IntToken(12355), RightParenToken, LeftParenToken, DoubleEqualsToken, IdentifierToken("ident1231asdad"), IntToken(5), IdentifierToken("helloword"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 349") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > = 5 < / 12355 => ( ident1231asdad == ) helloword reserved1")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, EqualsToken, IntToken(5), LessThanToken, DivideToken, IntToken(12355), ArrowToken, LeftParenToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken, RightParenToken, IdentifierToken("helloword"), IdentifierToken("reserved1"))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 350") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > = helloword ) 12355 < => ident1231asdad reserved1 5 == ( /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, EqualsToken, IdentifierToken("helloword"), RightParenToken, IntToken(12355), LessThanToken, ArrowToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IntToken(5), DoubleEqualsToken, LeftParenToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 351") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > == ) / = 5 reserved1 helloword => 12355 < ident1231asdad (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, DoubleEqualsToken, RightParenToken, DivideToken, EqualsToken, IntToken(5), IdentifierToken("reserved1"), IdentifierToken("helloword"), ArrowToken, IntToken(12355), LessThanToken, IdentifierToken("ident1231asdad"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 352") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > == => reserved1 5 < ident1231asdad 12355 = ( / helloword )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, DoubleEqualsToken, ArrowToken, IdentifierToken("reserved1"), IntToken(5), LessThanToken, IdentifierToken("ident1231asdad"), IntToken(12355), EqualsToken, LeftParenToken, DivideToken, IdentifierToken("helloword"), RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 353") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > == 5 < ) 12355 helloword => ident1231asdad reserved1 = ( /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, DoubleEqualsToken, IntToken(5), LessThanToken, RightParenToken, IntToken(12355), IdentifierToken("helloword"), ArrowToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), EqualsToken, LeftParenToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 354") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > == helloword ) reserved1 12355 / < = 5 => ident1231asdad (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, DoubleEqualsToken, IdentifierToken("helloword"), RightParenToken, IdentifierToken("reserved1"), IntToken(12355), DivideToken, LessThanToken, EqualsToken, IntToken(5), ArrowToken, IdentifierToken("ident1231asdad"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 355") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > => ) / < reserved1 = 12355 ident1231asdad == ( helloword 5")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, ArrowToken, RightParenToken, DivideToken, LessThanToken, IdentifierToken("reserved1"), EqualsToken, IntToken(12355), IdentifierToken("ident1231asdad"), DoubleEqualsToken, LeftParenToken, IdentifierToken("helloword"), IntToken(5))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 356") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > => == reserved1 helloword 12355 ) ( ident1231asdad 5 < = /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, ArrowToken, DoubleEqualsToken, IdentifierToken("reserved1"), IdentifierToken("helloword"), IntToken(12355), RightParenToken, LeftParenToken, IdentifierToken("ident1231asdad"), IntToken(5), LessThanToken, EqualsToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 357") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > => 5 < == reserved1 = ( ) ident1231asdad 12355 helloword /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, ArrowToken, IntToken(5), LessThanToken, DoubleEqualsToken, IdentifierToken("reserved1"), EqualsToken, LeftParenToken, RightParenToken, IdentifierToken("ident1231asdad"), IntToken(12355), IdentifierToken("helloword"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 358") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > => helloword = / 5 12355 ( reserved1 == ) ident1231asdad <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, ArrowToken, IdentifierToken("helloword"), EqualsToken, DivideToken, IntToken(5), IntToken(12355), LeftParenToken, IdentifierToken("reserved1"), DoubleEqualsToken, RightParenToken, IdentifierToken("ident1231asdad"), LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 359") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > < ) / 5 reserved1 12355 helloword ident1231asdad => == = (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, LessThanToken, RightParenToken, DivideToken, IntToken(5), IdentifierToken("reserved1"), IntToken(12355), IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), ArrowToken, DoubleEqualsToken, EqualsToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 360") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > < == ident1231asdad ( 5 reserved1 helloword ) => = 12355 /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, LessThanToken, DoubleEqualsToken, IdentifierToken("ident1231asdad"), LeftParenToken, IntToken(5), IdentifierToken("reserved1"), IdentifierToken("helloword"), RightParenToken, ArrowToken, EqualsToken, IntToken(12355), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 361") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > < 5 => reserved1 12355 helloword == = ( / ident1231asdad )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, LessThanToken, IntToken(5), ArrowToken, IdentifierToken("reserved1"), IntToken(12355), IdentifierToken("helloword"), DoubleEqualsToken, EqualsToken, LeftParenToken, DivideToken, IdentifierToken("ident1231asdad"), RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 362") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > < helloword = ) reserved1 / 12355 ident1231asdad 5 => == (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, LessThanToken, IdentifierToken("helloword"), EqualsToken, RightParenToken, IdentifierToken("reserved1"), DivideToken, IntToken(12355), IdentifierToken("ident1231asdad"), IntToken(5), ArrowToken, DoubleEqualsToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 363") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > 12355 ) / ident1231asdad reserved1 = < == 5 => helloword (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, IntToken(12355), RightParenToken, DivideToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), EqualsToken, LessThanToken, DoubleEqualsToken, IntToken(5), ArrowToken, IdentifierToken("helloword"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 364") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > 12355 == ident1231asdad = reserved1 ) => 5 ( / helloword <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, IntToken(12355), DoubleEqualsToken, IdentifierToken("ident1231asdad"), EqualsToken, IdentifierToken("reserved1"), RightParenToken, ArrowToken, IntToken(5), LeftParenToken, DivideToken, IdentifierToken("helloword"), LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 365") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > 12355 5 => helloword reserved1 = ( ident1231asdad < == ) /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, IntToken(12355), IntToken(5), ArrowToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), EqualsToken, LeftParenToken, IdentifierToken("ident1231asdad"), LessThanToken, DoubleEqualsToken, RightParenToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 366") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > 12355 helloword = => reserved1 < ( ) 5 == ident1231asdad /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, IntToken(12355), IdentifierToken("helloword"), EqualsToken, ArrowToken, IdentifierToken("reserved1"), LessThanToken, LeftParenToken, RightParenToken, IntToken(5), DoubleEqualsToken, IdentifierToken("ident1231asdad"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 367") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > 5 ) ( / ident1231asdad reserved1 = 12355 => == helloword <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, IntToken(5), RightParenToken, LeftParenToken, DivideToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), EqualsToken, IntToken(12355), ArrowToken, DoubleEqualsToken, IdentifierToken("helloword"), LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 368") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > 5 == ident1231asdad < reserved1 12355 helloword => = ) ( /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, IntToken(5), DoubleEqualsToken, IdentifierToken("ident1231asdad"), LessThanToken, IdentifierToken("reserved1"), IntToken(12355), IdentifierToken("helloword"), ArrowToken, EqualsToken, RightParenToken, LeftParenToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 369") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > 5 12355 < ( ident1231asdad helloword reserved1 ) == = => /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, IntToken(5), IntToken(12355), LessThanToken, LeftParenToken, IdentifierToken("ident1231asdad"), IdentifierToken("helloword"), IdentifierToken("reserved1"), RightParenToken, DoubleEqualsToken, EqualsToken, ArrowToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 370") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > 5 helloword = 12355 ident1231asdad / < => ) ( reserved1 ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, IntToken(5), IdentifierToken("helloword"), EqualsToken, IntToken(12355), IdentifierToken("ident1231asdad"), DivideToken, LessThanToken, ArrowToken, RightParenToken, LeftParenToken, IdentifierToken("reserved1"), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 371") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > reserved1 ) ( == helloword = 12355 ident1231asdad 5 < => /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, IdentifierToken("reserved1"), RightParenToken, LeftParenToken, DoubleEqualsToken, IdentifierToken("helloword"), EqualsToken, IntToken(12355), IdentifierToken("ident1231asdad"), IntToken(5), LessThanToken, ArrowToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 372") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > reserved1 == ident1231asdad 5 helloword ) => ( < = 12355 /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, IdentifierToken("reserved1"), DoubleEqualsToken, IdentifierToken("ident1231asdad"), IntToken(5), IdentifierToken("helloword"), RightParenToken, ArrowToken, LeftParenToken, LessThanToken, EqualsToken, IntToken(12355), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 373") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > reserved1 12355 < = helloword == ) 5 ( / ident1231asdad =>")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, IdentifierToken("reserved1"), IntToken(12355), LessThanToken, EqualsToken, IdentifierToken("helloword"), DoubleEqualsToken, RightParenToken, IntToken(5), LeftParenToken, DivideToken, IdentifierToken("ident1231asdad"), ArrowToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 374") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > reserved1 helloword = ident1231asdad 5 => ( 12355 < == ) /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, IdentifierToken("reserved1"), IdentifierToken("helloword"), EqualsToken, IdentifierToken("ident1231asdad"), IntToken(5), ArrowToken, LeftParenToken, IntToken(12355), LessThanToken, DoubleEqualsToken, RightParenToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 375") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > ident1231asdad ) ( < helloword 5 = == 12355 => reserved1 /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, IdentifierToken("ident1231asdad"), RightParenToken, LeftParenToken, LessThanToken, IdentifierToken("helloword"), IntToken(5), EqualsToken, DoubleEqualsToken, IntToken(12355), ArrowToken, IdentifierToken("reserved1"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 376") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > ident1231asdad == helloword / reserved1 5 ( < = ) 12355 =>")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken, IdentifierToken("helloword"), DivideToken, IdentifierToken("reserved1"), IntToken(5), LeftParenToken, LessThanToken, EqualsToken, RightParenToken, IntToken(12355), ArrowToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 377") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > ident1231asdad 12355 < => helloword reserved1 5 == = ) ( /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, IdentifierToken("ident1231asdad"), IntToken(12355), LessThanToken, ArrowToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), IntToken(5), DoubleEqualsToken, EqualsToken, RightParenToken, LeftParenToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 378") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > ident1231asdad helloword == ) / ( reserved1 => 12355 < 5 =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, IdentifierToken("ident1231asdad"), IdentifierToken("helloword"), DoubleEqualsToken, RightParenToken, DivideToken, LeftParenToken, IdentifierToken("reserved1"), ArrowToken, IntToken(12355), LessThanToken, IntToken(5), EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 379") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > helloword ) ( reserved1 / == 5 12355 => = ident1231asdad <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, IdentifierToken("helloword"), RightParenToken, LeftParenToken, IdentifierToken("reserved1"), DivideToken, DoubleEqualsToken, IntToken(5), IntToken(12355), ArrowToken, EqualsToken, IdentifierToken("ident1231asdad"), LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 380") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > helloword == ident1231asdad = / => 12355 reserved1 5 < ) (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, IdentifierToken("helloword"), DoubleEqualsToken, IdentifierToken("ident1231asdad"), EqualsToken, DivideToken, ArrowToken, IntToken(12355), IdentifierToken("reserved1"), IntToken(5), LessThanToken, RightParenToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 381") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > helloword 12355 < ident1231asdad / == => ) 5 = reserved1 (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, IdentifierToken("helloword"), IntToken(12355), LessThanToken, IdentifierToken("ident1231asdad"), DivideToken, DoubleEqualsToken, ArrowToken, RightParenToken, IntToken(5), EqualsToken, IdentifierToken("reserved1"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 382") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * > helloword ident1231asdad == => / 12355 = 5 ) ( reserved1 <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, GreaterThanToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), DoubleEqualsToken, ArrowToken, DivideToken, IntToken(12355), EqualsToken, IntToken(5), RightParenToken, LeftParenToken, IdentifierToken("reserved1"), LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 383") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 / = == ( ) ident1231asdad < helloword reserved1 5 > =>")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), DivideToken, EqualsToken, DoubleEqualsToken, LeftParenToken, RightParenToken, IdentifierToken("ident1231asdad"), LessThanToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), IntToken(5), GreaterThanToken, ArrowToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 384") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 / => helloword > ( ident1231asdad = == 5 < reserved1 )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), DivideToken, ArrowToken, IdentifierToken("helloword"), GreaterThanToken, LeftParenToken, IdentifierToken("ident1231asdad"), EqualsToken, DoubleEqualsToken, IntToken(5), LessThanToken, IdentifierToken("reserved1"), RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 385") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 / 5 reserved1 ) = ( == ident1231asdad < => helloword >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), DivideToken, IntToken(5), IdentifierToken("reserved1"), RightParenToken, EqualsToken, LeftParenToken, DoubleEqualsToken, IdentifierToken("ident1231asdad"), LessThanToken, ArrowToken, IdentifierToken("helloword"), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 386") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 / helloword => 5 ) ( ident1231asdad reserved1 > < == =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), DivideToken, IdentifierToken("helloword"), ArrowToken, IntToken(5), RightParenToken, LeftParenToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), GreaterThanToken, LessThanToken, DoubleEqualsToken, EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 387") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 ( = == => ) < helloword > reserved1 5 ident1231asdad /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), LeftParenToken, EqualsToken, DoubleEqualsToken, ArrowToken, RightParenToken, LessThanToken, IdentifierToken("helloword"), GreaterThanToken, IdentifierToken("reserved1"), IntToken(5), IdentifierToken("ident1231asdad"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 388") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 ( => helloword reserved1 ) == 5 > = / ident1231asdad <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), LeftParenToken, ArrowToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), RightParenToken, DoubleEqualsToken, IntToken(5), GreaterThanToken, EqualsToken, DivideToken, IdentifierToken("ident1231asdad"), LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 389") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 ( 5 reserved1 == ) < > helloword ident1231asdad => = /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), LeftParenToken, IntToken(5), IdentifierToken("reserved1"), DoubleEqualsToken, RightParenToken, LessThanToken, GreaterThanToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), ArrowToken, EqualsToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 390") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 ( helloword => ident1231asdad ) > < = 5 == reserved1 /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), LeftParenToken, IdentifierToken("helloword"), ArrowToken, IdentifierToken("ident1231asdad"), RightParenToken, GreaterThanToken, LessThanToken, EqualsToken, IntToken(5), DoubleEqualsToken, IdentifierToken("reserved1"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 391") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 ) = == > ( ident1231asdad < reserved1 => / helloword 5")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), RightParenToken, EqualsToken, DoubleEqualsToken, GreaterThanToken, LeftParenToken, IdentifierToken("ident1231asdad"), LessThanToken, IdentifierToken("reserved1"), ArrowToken, DivideToken, IdentifierToken("helloword"), IntToken(5))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 392") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 ) < / ( == helloword => ident1231asdad reserved1 5 > =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), RightParenToken, LessThanToken, DivideToken, LeftParenToken, DoubleEqualsToken, IdentifierToken("helloword"), ArrowToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IntToken(5), GreaterThanToken, EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 393") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 ) 5 reserved1 < = / == => ident1231asdad > helloword (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), RightParenToken, IntToken(5), IdentifierToken("reserved1"), LessThanToken, EqualsToken, DivideToken, DoubleEqualsToken, ArrowToken, IdentifierToken("ident1231asdad"), GreaterThanToken, IdentifierToken("helloword"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 394") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 ) helloword < ( == = / reserved1 > => ident1231asdad 5")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), RightParenToken, IdentifierToken("helloword"), LessThanToken, LeftParenToken, DoubleEqualsToken, EqualsToken, DivideToken, IdentifierToken("reserved1"), GreaterThanToken, ArrowToken, IdentifierToken("ident1231asdad"), IntToken(5))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 395") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 = ) == reserved1 => ( helloword ident1231asdad 5 > < /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), EqualsToken, RightParenToken, DoubleEqualsToken, IdentifierToken("reserved1"), ArrowToken, LeftParenToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), IntToken(5), GreaterThanToken, LessThanToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 396") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 = < / == => > helloword ) reserved1 5 ident1231asdad (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), EqualsToken, LessThanToken, DivideToken, DoubleEqualsToken, ArrowToken, GreaterThanToken, IdentifierToken("helloword"), RightParenToken, IdentifierToken("reserved1"), IntToken(5), IdentifierToken("ident1231asdad"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 397") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 = 5 reserved1 ident1231asdad ) => > < ( / helloword ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), EqualsToken, IntToken(5), IdentifierToken("reserved1"), IdentifierToken("ident1231asdad"), RightParenToken, ArrowToken, GreaterThanToken, LessThanToken, LeftParenToken, DivideToken, IdentifierToken("helloword"), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 398") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 = helloword < == ) 5 > ident1231asdad reserved1 => ( /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), EqualsToken, IdentifierToken("helloword"), LessThanToken, DoubleEqualsToken, RightParenToken, IntToken(5), GreaterThanToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), ArrowToken, LeftParenToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 399") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 == ) = helloword => reserved1 > ( 5 < ident1231asdad /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), DoubleEqualsToken, RightParenToken, EqualsToken, IdentifierToken("helloword"), ArrowToken, IdentifierToken("reserved1"), GreaterThanToken, LeftParenToken, IntToken(5), LessThanToken, IdentifierToken("ident1231asdad"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 400") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 == < / > = helloword => reserved1 ) ( ident1231asdad 5")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), DoubleEqualsToken, LessThanToken, DivideToken, GreaterThanToken, EqualsToken, IdentifierToken("helloword"), ArrowToken, IdentifierToken("reserved1"), RightParenToken, LeftParenToken, IdentifierToken("ident1231asdad"), IntToken(5))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 401") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 == 5 ident1231asdad / => ( = helloword reserved1 > < )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), DoubleEqualsToken, IntToken(5), IdentifierToken("ident1231asdad"), DivideToken, ArrowToken, LeftParenToken, EqualsToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), GreaterThanToken, LessThanToken, RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 402") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 == helloword < > = ( ) => reserved1 5 ident1231asdad /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), DoubleEqualsToken, IdentifierToken("helloword"), LessThanToken, GreaterThanToken, EqualsToken, LeftParenToken, RightParenToken, ArrowToken, IdentifierToken("reserved1"), IntToken(5), IdentifierToken("ident1231asdad"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 403") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 => ) == ( > < / ident1231asdad 5 = helloword reserved1")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), ArrowToken, RightParenToken, DoubleEqualsToken, LeftParenToken, GreaterThanToken, LessThanToken, DivideToken, IdentifierToken("ident1231asdad"), IntToken(5), EqualsToken, IdentifierToken("helloword"), IdentifierToken("reserved1"))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 404") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 => < / reserved1 == = helloword ident1231asdad 5 > ) (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), ArrowToken, LessThanToken, DivideToken, IdentifierToken("reserved1"), DoubleEqualsToken, EqualsToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), IntToken(5), GreaterThanToken, RightParenToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 405") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 => 5 ident1231asdad ) == < helloword ( > = reserved1 /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), ArrowToken, IntToken(5), IdentifierToken("ident1231asdad"), RightParenToken, DoubleEqualsToken, LessThanToken, IdentifierToken("helloword"), LeftParenToken, GreaterThanToken, EqualsToken, IdentifierToken("reserved1"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 406") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 => helloword < reserved1 = > 5 == ( / ident1231asdad )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), ArrowToken, IdentifierToken("helloword"), LessThanToken, IdentifierToken("reserved1"), EqualsToken, GreaterThanToken, IntToken(5), DoubleEqualsToken, LeftParenToken, DivideToken, IdentifierToken("ident1231asdad"), RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 407") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 < ) == => > ident1231asdad 5 helloword reserved1 = ( /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), LessThanToken, RightParenToken, DoubleEqualsToken, ArrowToken, GreaterThanToken, IdentifierToken("ident1231asdad"), IntToken(5), IdentifierToken("helloword"), IdentifierToken("reserved1"), EqualsToken, LeftParenToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 408") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 < => / helloword == ident1231asdad > ) 5 = reserved1 (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), LessThanToken, ArrowToken, DivideToken, IdentifierToken("helloword"), DoubleEqualsToken, IdentifierToken("ident1231asdad"), GreaterThanToken, RightParenToken, IntToken(5), EqualsToken, IdentifierToken("reserved1"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 409") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 < 5 ident1231asdad == => / = reserved1 ) ( helloword >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), LessThanToken, IntToken(5), IdentifierToken("ident1231asdad"), DoubleEqualsToken, ArrowToken, DivideToken, EqualsToken, IdentifierToken("reserved1"), RightParenToken, LeftParenToken, IdentifierToken("helloword"), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 410") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 < helloword > / => ) = ident1231asdad reserved1 5 == (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), LessThanToken, IdentifierToken("helloword"), GreaterThanToken, DivideToken, ArrowToken, RightParenToken, EqualsToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IntToken(5), DoubleEqualsToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 411") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 > ) == 5 < = ( => ident1231asdad reserved1 helloword /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), GreaterThanToken, RightParenToken, DoubleEqualsToken, IntToken(5), LessThanToken, EqualsToken, LeftParenToken, ArrowToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IdentifierToken("helloword"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 412") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 > => ( ) 5 < / ident1231asdad == = helloword reserved1")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), GreaterThanToken, ArrowToken, LeftParenToken, RightParenToken, IntToken(5), LessThanToken, DivideToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken, EqualsToken, IdentifierToken("helloword"), IdentifierToken("reserved1"))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 413") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 > 5 ident1231asdad < == = helloword reserved1 => ) ( /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), GreaterThanToken, IntToken(5), IdentifierToken("ident1231asdad"), LessThanToken, DoubleEqualsToken, EqualsToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), ArrowToken, RightParenToken, LeftParenToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 414") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 > helloword < ) => 5 ident1231asdad ( == = reserved1 /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), GreaterThanToken, IdentifierToken("helloword"), LessThanToken, RightParenToken, ArrowToken, IntToken(5), IdentifierToken("ident1231asdad"), LeftParenToken, DoubleEqualsToken, EqualsToken, IdentifierToken("reserved1"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 415") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 5 ) == ident1231asdad < reserved1 > => ( / helloword =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), IntToken(5), RightParenToken, DoubleEqualsToken, IdentifierToken("ident1231asdad"), LessThanToken, IdentifierToken("reserved1"), GreaterThanToken, ArrowToken, LeftParenToken, DivideToken, IdentifierToken("helloword"), EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 416") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 5 => ( == > helloword < ident1231asdad reserved1 = ) /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), IntToken(5), ArrowToken, LeftParenToken, DoubleEqualsToken, GreaterThanToken, IdentifierToken("helloword"), LessThanToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), EqualsToken, RightParenToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 417") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 5 > ident1231asdad helloword => / == ) < = reserved1 (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), IntToken(5), GreaterThanToken, IdentifierToken("ident1231asdad"), IdentifierToken("helloword"), ArrowToken, DivideToken, DoubleEqualsToken, RightParenToken, LessThanToken, EqualsToken, IdentifierToken("reserved1"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 418") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 5 helloword < == > ( = reserved1 ) / ident1231asdad =>")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), IntToken(5), IdentifierToken("helloword"), LessThanToken, DoubleEqualsToken, GreaterThanToken, LeftParenToken, EqualsToken, IdentifierToken("reserved1"), RightParenToken, DivideToken, IdentifierToken("ident1231asdad"), ArrowToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 419") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 reserved1 ) => / 5 == = helloword ident1231asdad > < (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), IdentifierToken("reserved1"), RightParenToken, ArrowToken, DivideToken, IntToken(5), DoubleEqualsToken, EqualsToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), GreaterThanToken, LessThanToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 420") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 reserved1 => ( > 5 == ) = ident1231asdad < helloword /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), IdentifierToken("reserved1"), ArrowToken, LeftParenToken, GreaterThanToken, IntToken(5), DoubleEqualsToken, RightParenToken, EqualsToken, IdentifierToken("ident1231asdad"), LessThanToken, IdentifierToken("helloword"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 421") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 reserved1 > helloword ( < => / 5 = ) ident1231asdad ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), IdentifierToken("reserved1"), GreaterThanToken, IdentifierToken("helloword"), LeftParenToken, LessThanToken, ArrowToken, DivideToken, IntToken(5), EqualsToken, RightParenToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 422") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 reserved1 helloword < > => == ident1231asdad 5 = ) ( /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), IdentifierToken("reserved1"), IdentifierToken("helloword"), LessThanToken, GreaterThanToken, ArrowToken, DoubleEqualsToken, IdentifierToken("ident1231asdad"), IntToken(5), EqualsToken, RightParenToken, LeftParenToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 423") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 ident1231asdad ) => = 5 reserved1 helloword ( < == > /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), IdentifierToken("ident1231asdad"), RightParenToken, ArrowToken, EqualsToken, IntToken(5), IdentifierToken("reserved1"), IdentifierToken("helloword"), LeftParenToken, LessThanToken, DoubleEqualsToken, GreaterThanToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 424") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 ident1231asdad => ( reserved1 > helloword < == ) / 5 =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), IdentifierToken("ident1231asdad"), ArrowToken, LeftParenToken, IdentifierToken("reserved1"), GreaterThanToken, IdentifierToken("helloword"), LessThanToken, DoubleEqualsToken, RightParenToken, DivideToken, IntToken(5), EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 425") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 ident1231asdad > helloword = 5 / => reserved1 < == ) (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), IdentifierToken("ident1231asdad"), GreaterThanToken, IdentifierToken("helloword"), EqualsToken, IntToken(5), DivideToken, ArrowToken, IdentifierToken("reserved1"), LessThanToken, DoubleEqualsToken, RightParenToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 426") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 ident1231asdad helloword < reserved1 > ( == ) => = 5 /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), IdentifierToken("ident1231asdad"), IdentifierToken("helloword"), LessThanToken, IdentifierToken("reserved1"), GreaterThanToken, LeftParenToken, DoubleEqualsToken, RightParenToken, ArrowToken, EqualsToken, IntToken(5), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 427") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 helloword ) => < reserved1 = == 5 ( / ident1231asdad >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), IdentifierToken("helloword"), RightParenToken, ArrowToken, LessThanToken, IdentifierToken("reserved1"), EqualsToken, DoubleEqualsToken, IntToken(5), LeftParenToken, DivideToken, IdentifierToken("ident1231asdad"), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 428") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 helloword => ) / reserved1 < = ident1231asdad 5 > == (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), IdentifierToken("helloword"), ArrowToken, RightParenToken, DivideToken, IdentifierToken("reserved1"), LessThanToken, EqualsToken, IdentifierToken("ident1231asdad"), IntToken(5), GreaterThanToken, DoubleEqualsToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 429") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 helloword > ident1231asdad => 5 == ( ) < = reserved1 /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), IdentifierToken("helloword"), GreaterThanToken, IdentifierToken("ident1231asdad"), ArrowToken, IntToken(5), DoubleEqualsToken, LeftParenToken, RightParenToken, LessThanToken, EqualsToken, IdentifierToken("reserved1"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 430") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 12355 helloword ident1231asdad > ( 5 < / => = ) reserved1 ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(12355), IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), GreaterThanToken, LeftParenToken, IntToken(5), LessThanToken, DivideToken, ArrowToken, EqualsToken, RightParenToken, IdentifierToken("reserved1"), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 431") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 / = < reserved1 ident1231asdad 12355 helloword > => == ) (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), DivideToken, EqualsToken, LessThanToken, IdentifierToken("reserved1"), IdentifierToken("ident1231asdad"), IntToken(12355), IdentifierToken("helloword"), GreaterThanToken, ArrowToken, DoubleEqualsToken, RightParenToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 432") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 / < = == ident1231asdad helloword reserved1 ) > => 12355 (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), DivideToken, LessThanToken, EqualsToken, DoubleEqualsToken, IdentifierToken("ident1231asdad"), IdentifierToken("helloword"), IdentifierToken("reserved1"), RightParenToken, GreaterThanToken, ArrowToken, IntToken(12355), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 433") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 / 12355 helloword reserved1 ident1231asdad ( < => = ) > ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), DivideToken, IntToken(12355), IdentifierToken("helloword"), IdentifierToken("reserved1"), IdentifierToken("ident1231asdad"), LeftParenToken, LessThanToken, ArrowToken, EqualsToken, RightParenToken, GreaterThanToken, DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 434") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 / helloword 12355 == ident1231asdad ) < reserved1 > => = (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), DivideToken, IdentifierToken("helloword"), IntToken(12355), DoubleEqualsToken, IdentifierToken("ident1231asdad"), RightParenToken, LessThanToken, IdentifierToken("reserved1"), GreaterThanToken, ArrowToken, EqualsToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 435") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 ( = < helloword ident1231asdad == > ) 12355 => reserved1 /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), LeftParenToken, EqualsToken, LessThanToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), DoubleEqualsToken, GreaterThanToken, RightParenToken, IntToken(12355), ArrowToken, IdentifierToken("reserved1"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 436") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 ( < = > helloword => == reserved1 ) / ident1231asdad 12355")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), LeftParenToken, LessThanToken, EqualsToken, GreaterThanToken, IdentifierToken("helloword"), ArrowToken, DoubleEqualsToken, IdentifierToken("reserved1"), RightParenToken, DivideToken, IdentifierToken("ident1231asdad"), IntToken(12355))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 437") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 ( reserved1 / ) helloword > == ident1231asdad 12355 < => =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), LeftParenToken, IdentifierToken("reserved1"), DivideToken, RightParenToken, IdentifierToken("helloword"), GreaterThanToken, DoubleEqualsToken, IdentifierToken("ident1231asdad"), IntToken(12355), LessThanToken, ArrowToken, EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 438") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 ( helloword 12355 < ident1231asdad > ) = => == reserved1 /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), LeftParenToken, IdentifierToken("helloword"), IntToken(12355), LessThanToken, IdentifierToken("ident1231asdad"), GreaterThanToken, RightParenToken, EqualsToken, ArrowToken, DoubleEqualsToken, IdentifierToken("reserved1"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 439") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 ) = > ( helloword ident1231asdad / 12355 => == reserved1 <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), RightParenToken, EqualsToken, GreaterThanToken, LeftParenToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), DivideToken, IntToken(12355), ArrowToken, DoubleEqualsToken, IdentifierToken("reserved1"), LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 440") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 ) < = reserved1 helloword ident1231asdad 12355 > => == ( /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), RightParenToken, LessThanToken, EqualsToken, IdentifierToken("reserved1"), IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), IntToken(12355), GreaterThanToken, ArrowToken, DoubleEqualsToken, LeftParenToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 441") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 ) reserved1 / => ( = helloword < 12355 > ident1231asdad ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), RightParenToken, IdentifierToken("reserved1"), DivideToken, ArrowToken, LeftParenToken, EqualsToken, IdentifierToken("helloword"), LessThanToken, IntToken(12355), GreaterThanToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 442") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 ) helloword 12355 ident1231asdad / = > < == ( reserved1 =>")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), RightParenToken, IdentifierToken("helloword"), IntToken(12355), IdentifierToken("ident1231asdad"), DivideToken, EqualsToken, GreaterThanToken, LessThanToken, DoubleEqualsToken, LeftParenToken, IdentifierToken("reserved1"), ArrowToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 443") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 = ) > < / => reserved1 helloword ident1231asdad 12355 == (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), EqualsToken, RightParenToken, GreaterThanToken, LessThanToken, DivideToken, ArrowToken, IdentifierToken("reserved1"), IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), IntToken(12355), DoubleEqualsToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 444") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 = < == / ( 12355 reserved1 => ident1231asdad > helloword )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), EqualsToken, LessThanToken, DoubleEqualsToken, DivideToken, LeftParenToken, IntToken(12355), IdentifierToken("reserved1"), ArrowToken, IdentifierToken("ident1231asdad"), GreaterThanToken, IdentifierToken("helloword"), RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 445") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 = reserved1 / > ( 12355 => ident1231asdad == ) helloword <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), EqualsToken, IdentifierToken("reserved1"), DivideToken, GreaterThanToken, LeftParenToken, IntToken(12355), ArrowToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken, RightParenToken, IdentifierToken("helloword"), LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 446") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 = helloword reserved1 ( / 12355 == ident1231asdad > < => )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), EqualsToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), LeftParenToken, DivideToken, IntToken(12355), DoubleEqualsToken, IdentifierToken("ident1231asdad"), GreaterThanToken, LessThanToken, ArrowToken, RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 447") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 == ) > reserved1 / helloword = => 12355 < ident1231asdad (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), DoubleEqualsToken, RightParenToken, GreaterThanToken, IdentifierToken("reserved1"), DivideToken, IdentifierToken("helloword"), EqualsToken, ArrowToken, IntToken(12355), LessThanToken, IdentifierToken("ident1231asdad"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 448") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 == < = ) ( / => ident1231asdad 12355 > helloword reserved1")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), DoubleEqualsToken, LessThanToken, EqualsToken, RightParenToken, LeftParenToken, DivideToken, ArrowToken, IdentifierToken("ident1231asdad"), IntToken(12355), GreaterThanToken, IdentifierToken("helloword"), IdentifierToken("reserved1"))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 449") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 == reserved1 / ident1231asdad ) ( helloword 12355 > < => =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), DoubleEqualsToken, IdentifierToken("reserved1"), DivideToken, IdentifierToken("ident1231asdad"), RightParenToken, LeftParenToken, IdentifierToken("helloword"), IntToken(12355), GreaterThanToken, LessThanToken, ArrowToken, EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 450") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 == helloword reserved1 = ( ) ident1231asdad => > < 12355 /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), DoubleEqualsToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), EqualsToken, LeftParenToken, RightParenToken, IdentifierToken("ident1231asdad"), ArrowToken, GreaterThanToken, LessThanToken, IntToken(12355), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 451") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 => ) > helloword ( == reserved1 12355 = / ident1231asdad <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), ArrowToken, RightParenToken, GreaterThanToken, IdentifierToken("helloword"), LeftParenToken, DoubleEqualsToken, IdentifierToken("reserved1"), IntToken(12355), EqualsToken, DivideToken, IdentifierToken("ident1231asdad"), LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 452") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 => < = > ( 12355 reserved1 helloword ident1231asdad == ) /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), ArrowToken, LessThanToken, EqualsToken, GreaterThanToken, LeftParenToken, IntToken(12355), IdentifierToken("reserved1"), IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), DoubleEqualsToken, RightParenToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 453") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 => reserved1 ( / = 12355 > == ident1231asdad < helloword )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), ArrowToken, IdentifierToken("reserved1"), LeftParenToken, DivideToken, EqualsToken, IntToken(12355), GreaterThanToken, DoubleEqualsToken, IdentifierToken("ident1231asdad"), LessThanToken, IdentifierToken("helloword"), RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 454") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 => helloword reserved1 < ( 12355 = > ) / ident1231asdad ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), ArrowToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), LessThanToken, LeftParenToken, IntToken(12355), EqualsToken, GreaterThanToken, RightParenToken, DivideToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 455") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 < ) 12355 ( = helloword == ident1231asdad reserved1 > => /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), LessThanToken, RightParenToken, IntToken(12355), LeftParenToken, EqualsToken, IdentifierToken("helloword"), DoubleEqualsToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), GreaterThanToken, ArrowToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 456") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 < => = reserved1 ) / == > ident1231asdad 12355 helloword (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), LessThanToken, ArrowToken, EqualsToken, IdentifierToken("reserved1"), RightParenToken, DivideToken, DoubleEqualsToken, GreaterThanToken, IdentifierToken("ident1231asdad"), IntToken(12355), IdentifierToken("helloword"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 457") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 < reserved1 ( = == ) / ident1231asdad > => helloword 12355")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), LessThanToken, IdentifierToken("reserved1"), LeftParenToken, EqualsToken, DoubleEqualsToken, RightParenToken, DivideToken, IdentifierToken("ident1231asdad"), GreaterThanToken, ArrowToken, IdentifierToken("helloword"), IntToken(12355))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 458") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 < helloword reserved1 12355 ) ( ident1231asdad > => == = /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), LessThanToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), IntToken(12355), RightParenToken, LeftParenToken, IdentifierToken("ident1231asdad"), GreaterThanToken, ArrowToken, DoubleEqualsToken, EqualsToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 459") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 > ) 12355 == = => helloword ( reserved1 < ident1231asdad /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), GreaterThanToken, RightParenToken, IntToken(12355), DoubleEqualsToken, EqualsToken, ArrowToken, IdentifierToken("helloword"), LeftParenToken, IdentifierToken("reserved1"), LessThanToken, IdentifierToken("ident1231asdad"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 460") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 > => = helloword ) < reserved1 12355 ( / ident1231asdad ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), GreaterThanToken, ArrowToken, EqualsToken, IdentifierToken("helloword"), RightParenToken, LessThanToken, IdentifierToken("reserved1"), IntToken(12355), LeftParenToken, DivideToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 461") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 > reserved1 ( => = 12355 < helloword ident1231asdad == ) /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), GreaterThanToken, IdentifierToken("reserved1"), LeftParenToken, ArrowToken, EqualsToken, IntToken(12355), LessThanToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), DoubleEqualsToken, RightParenToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 462") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 > helloword ident1231asdad / = 12355 => ) < == reserved1 (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), GreaterThanToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), DivideToken, EqualsToken, IntToken(12355), ArrowToken, RightParenToken, LessThanToken, DoubleEqualsToken, IdentifierToken("reserved1"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 463") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 12355 ) > < = helloword == reserved1 ( / ident1231asdad =>")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), IntToken(12355), RightParenToken, GreaterThanToken, LessThanToken, EqualsToken, IdentifierToken("helloword"), DoubleEqualsToken, IdentifierToken("reserved1"), LeftParenToken, DivideToken, IdentifierToken("ident1231asdad"), ArrowToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 464") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 12355 => == ( < / = helloword ident1231asdad reserved1 > )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), IntToken(12355), ArrowToken, DoubleEqualsToken, LeftParenToken, LessThanToken, DivideToken, EqualsToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), GreaterThanToken, RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 465") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 12355 reserved1 ( > == ) = => ident1231asdad < helloword /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), IntToken(12355), IdentifierToken("reserved1"), LeftParenToken, GreaterThanToken, DoubleEqualsToken, RightParenToken, EqualsToken, ArrowToken, IdentifierToken("ident1231asdad"), LessThanToken, IdentifierToken("helloword"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 466") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 12355 helloword ident1231asdad ) == = / > => ( reserved1 <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), IntToken(12355), IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), RightParenToken, DoubleEqualsToken, EqualsToken, DivideToken, GreaterThanToken, ArrowToken, LeftParenToken, IdentifierToken("reserved1"), LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 467") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 reserved1 ) > ident1231asdad == = helloword 12355 < => ( /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), IdentifierToken("reserved1"), RightParenToken, GreaterThanToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken, EqualsToken, IdentifierToken("helloword"), IntToken(12355), LessThanToken, ArrowToken, LeftParenToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 468") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 reserved1 => == = < > helloword ( 12355 ) ident1231asdad /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), IdentifierToken("reserved1"), ArrowToken, DoubleEqualsToken, EqualsToken, LessThanToken, GreaterThanToken, IdentifierToken("helloword"), LeftParenToken, IntToken(12355), RightParenToken, IdentifierToken("ident1231asdad"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 469") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 reserved1 12355 ( helloword == < > => ) / ident1231asdad =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), IdentifierToken("reserved1"), IntToken(12355), LeftParenToken, IdentifierToken("helloword"), DoubleEqualsToken, LessThanToken, GreaterThanToken, ArrowToken, RightParenToken, DivideToken, IdentifierToken("ident1231asdad"), EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 470") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 reserved1 helloword ident1231asdad == = > => 12355 < ) ( /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), IdentifierToken("reserved1"), IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), DoubleEqualsToken, EqualsToken, GreaterThanToken, ArrowToken, IntToken(12355), LessThanToken, RightParenToken, LeftParenToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 471") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 ident1231asdad ) 12355 / => helloword < = > == reserved1 (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), IdentifierToken("ident1231asdad"), RightParenToken, IntToken(12355), DivideToken, ArrowToken, IdentifierToken("helloword"), LessThanToken, EqualsToken, GreaterThanToken, DoubleEqualsToken, IdentifierToken("reserved1"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 472") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 ident1231asdad => == > < / = reserved1 ) ( helloword 12355")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), IdentifierToken("ident1231asdad"), ArrowToken, DoubleEqualsToken, GreaterThanToken, LessThanToken, DivideToken, EqualsToken, IdentifierToken("reserved1"), RightParenToken, LeftParenToken, IdentifierToken("helloword"), IntToken(12355))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 473") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 ident1231asdad 12355 ) ( < = == helloword reserved1 > => /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), IdentifierToken("ident1231asdad"), IntToken(12355), RightParenToken, LeftParenToken, LessThanToken, EqualsToken, DoubleEqualsToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), GreaterThanToken, ArrowToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 474") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 ident1231asdad helloword reserved1 < == ) ( = > => 12355 /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), IdentifierToken("ident1231asdad"), IdentifierToken("helloword"), IdentifierToken("reserved1"), LessThanToken, DoubleEqualsToken, RightParenToken, LeftParenToken, EqualsToken, GreaterThanToken, ArrowToken, IntToken(12355), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 475") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 helloword ) 12355 = < => / reserved1 == ( ident1231asdad >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), IdentifierToken("helloword"), RightParenToken, IntToken(12355), EqualsToken, LessThanToken, ArrowToken, DivideToken, IdentifierToken("reserved1"), DoubleEqualsToken, LeftParenToken, IdentifierToken("ident1231asdad"), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 476") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 helloword => == reserved1 < = ident1231asdad 12355 > ) ( /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), IdentifierToken("helloword"), ArrowToken, DoubleEqualsToken, IdentifierToken("reserved1"), LessThanToken, EqualsToken, IdentifierToken("ident1231asdad"), IntToken(12355), GreaterThanToken, RightParenToken, LeftParenToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 477") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 helloword 12355 ) == < > ident1231asdad ( => = reserved1 /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), IdentifierToken("helloword"), IntToken(12355), RightParenToken, DoubleEqualsToken, LessThanToken, GreaterThanToken, IdentifierToken("ident1231asdad"), LeftParenToken, ArrowToken, EqualsToken, IdentifierToken("reserved1"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 478") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * 5 helloword ident1231asdad reserved1 12355 == < => = ( / > )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IntToken(5), IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IntToken(12355), DoubleEqualsToken, LessThanToken, ArrowToken, EqualsToken, LeftParenToken, DivideToken, GreaterThanToken, RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 479") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * reserved1 / = 5 < > helloword => ident1231asdad 12355 == ) (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("reserved1"), DivideToken, EqualsToken, IntToken(5), LessThanToken, GreaterThanToken, IdentifierToken("helloword"), ArrowToken, IdentifierToken("ident1231asdad"), IntToken(12355), DoubleEqualsToken, RightParenToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 480") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * reserved1 / < > ( 5 ) 12355 == ident1231asdad => helloword =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("reserved1"), DivideToken, LessThanToken, GreaterThanToken, LeftParenToken, IntToken(5), RightParenToken, IntToken(12355), DoubleEqualsToken, IdentifierToken("ident1231asdad"), ArrowToken, IdentifierToken("helloword"), EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 481") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * reserved1 / 5 = > 12355 ) => ident1231asdad == ( helloword <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("reserved1"), DivideToken, IntToken(5), EqualsToken, GreaterThanToken, IntToken(12355), RightParenToken, ArrowToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken, LeftParenToken, IdentifierToken("helloword"), LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 482") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * reserved1 ( / ) == 5 < => helloword ident1231asdad 12355 > =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("reserved1"), LeftParenToken, DivideToken, RightParenToken, DoubleEqualsToken, IntToken(5), LessThanToken, ArrowToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), IntToken(12355), GreaterThanToken, EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 483") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * reserved1 ( = 5 12355 > => ) == ident1231asdad < helloword /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("reserved1"), LeftParenToken, EqualsToken, IntToken(5), IntToken(12355), GreaterThanToken, ArrowToken, RightParenToken, DoubleEqualsToken, IdentifierToken("ident1231asdad"), LessThanToken, IdentifierToken("helloword"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 484") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * reserved1 ( < > = 5 12355 / ident1231asdad == ) helloword =>")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("reserved1"), LeftParenToken, LessThanToken, GreaterThanToken, EqualsToken, IntToken(5), IntToken(12355), DivideToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken, RightParenToken, IdentifierToken("helloword"), ArrowToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 485") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * reserved1 ( 5 = ident1231asdad > < helloword 12355 => == ) /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("reserved1"), LeftParenToken, IntToken(5), EqualsToken, IdentifierToken("ident1231asdad"), GreaterThanToken, LessThanToken, IdentifierToken("helloword"), IntToken(12355), ArrowToken, DoubleEqualsToken, RightParenToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 486") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * reserved1 ) / ( < 5 ident1231asdad helloword == > => 12355 =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("reserved1"), RightParenToken, DivideToken, LeftParenToken, LessThanToken, IntToken(5), IdentifierToken("ident1231asdad"), IdentifierToken("helloword"), DoubleEqualsToken, GreaterThanToken, ArrowToken, IntToken(12355), EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 487") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * reserved1 ) = 5 helloword > ident1231asdad < => ( / 12355 ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("reserved1"), RightParenToken, EqualsToken, IntToken(5), IdentifierToken("helloword"), GreaterThanToken, IdentifierToken("ident1231asdad"), LessThanToken, ArrowToken, LeftParenToken, DivideToken, IntToken(12355), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 488") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * reserved1 ) < > => ident1231asdad / 12355 helloword 5 == = (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("reserved1"), RightParenToken, LessThanToken, GreaterThanToken, ArrowToken, IdentifierToken("ident1231asdad"), DivideToken, IntToken(12355), IdentifierToken("helloword"), IntToken(5), DoubleEqualsToken, EqualsToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 489") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * reserved1 ) 5 == / ident1231asdad = > => 12355 < helloword (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("reserved1"), RightParenToken, IntToken(5), DoubleEqualsToken, DivideToken, IdentifierToken("ident1231asdad"), EqualsToken, GreaterThanToken, ArrowToken, IntToken(12355), LessThanToken, IdentifierToken("helloword"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 490") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * reserved1 = / ( 12355 ident1231asdad => < 5 == ) helloword >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("reserved1"), EqualsToken, DivideToken, LeftParenToken, IntToken(12355), IdentifierToken("ident1231asdad"), ArrowToken, LessThanToken, IntToken(5), DoubleEqualsToken, RightParenToken, IdentifierToken("helloword"), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 491") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * reserved1 = ) ident1231asdad ( 5 < == helloword 12355 > => /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("reserved1"), EqualsToken, RightParenToken, IdentifierToken("ident1231asdad"), LeftParenToken, IntToken(5), LessThanToken, DoubleEqualsToken, IdentifierToken("helloword"), IntToken(12355), GreaterThanToken, ArrowToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 492") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * reserved1 = < > 5 ident1231asdad => ( ) 12355 == helloword /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("reserved1"), EqualsToken, LessThanToken, GreaterThanToken, IntToken(5), IdentifierToken("ident1231asdad"), ArrowToken, LeftParenToken, RightParenToken, IntToken(12355), DoubleEqualsToken, IdentifierToken("helloword"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 493") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * reserved1 = 5 == ) ident1231asdad 12355 / > => ( helloword <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("reserved1"), EqualsToken, IntToken(5), DoubleEqualsToken, RightParenToken, IdentifierToken("ident1231asdad"), IntToken(12355), DivideToken, GreaterThanToken, ArrowToken, LeftParenToken, IdentifierToken("helloword"), LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 494") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * reserved1 == / ( ident1231asdad 5 12355 helloword > < => = )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("reserved1"), DoubleEqualsToken, DivideToken, LeftParenToken, IdentifierToken("ident1231asdad"), IntToken(5), IntToken(12355), IdentifierToken("helloword"), GreaterThanToken, LessThanToken, ArrowToken, EqualsToken, RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 495") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * reserved1 == ) ident1231asdad => 5 helloword 12355 ( < = > /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("reserved1"), DoubleEqualsToken, RightParenToken, IdentifierToken("ident1231asdad"), ArrowToken, IntToken(5), IdentifierToken("helloword"), IntToken(12355), LeftParenToken, LessThanToken, EqualsToken, GreaterThanToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 496") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * reserved1 == < > helloword ident1231asdad / 12355 => ) ( 5 =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("reserved1"), DoubleEqualsToken, LessThanToken, GreaterThanToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), DivideToken, IntToken(12355), ArrowToken, RightParenToken, LeftParenToken, IntToken(5), EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 497") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * reserved1 == 5 = < helloword ( > ident1231asdad 12355 => ) /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("reserved1"), DoubleEqualsToken, IntToken(5), EqualsToken, LessThanToken, IdentifierToken("helloword"), LeftParenToken, GreaterThanToken, IdentifierToken("ident1231asdad"), IntToken(12355), ArrowToken, RightParenToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 498") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * reserved1 => / ) ( helloword < 12355 == 5 > ident1231asdad =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("reserved1"), ArrowToken, DivideToken, RightParenToken, LeftParenToken, IdentifierToken("helloword"), LessThanToken, IntToken(12355), DoubleEqualsToken, IntToken(5), GreaterThanToken, IdentifierToken("ident1231asdad"), EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 499") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * reserved1 => ) ident1231asdad > helloword == = 12355 ( / 5 <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("reserved1"), ArrowToken, RightParenToken, IdentifierToken("ident1231asdad"), GreaterThanToken, IdentifierToken("helloword"), DoubleEqualsToken, EqualsToken, IntToken(12355), LeftParenToken, DivideToken, IntToken(5), LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 500") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * reserved1 => < 12355 ( helloword > ) ident1231asdad 5 == = /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("reserved1"), ArrowToken, LessThanToken, IntToken(12355), LeftParenToken, IdentifierToken("helloword"), GreaterThanToken, RightParenToken, IdentifierToken("ident1231asdad"), IntToken(5), DoubleEqualsToken, EqualsToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 501") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * reserved1 => 5 = 12355 helloword > ( ) < == ident1231asdad /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("reserved1"), ArrowToken, IntToken(5), EqualsToken, IntToken(12355), IdentifierToken("helloword"), GreaterThanToken, LeftParenToken, RightParenToken, LessThanToken, DoubleEqualsToken, IdentifierToken("ident1231asdad"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 502") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * reserved1 < / ) == helloword ident1231asdad ( 12355 => = 5 >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("reserved1"), LessThanToken, DivideToken, RightParenToken, DoubleEqualsToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), LeftParenToken, IntToken(12355), ArrowToken, EqualsToken, IntToken(5), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 503") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * reserved1 < ) ident1231asdad 5 helloword 12355 > => == = ( /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("reserved1"), LessThanToken, RightParenToken, IdentifierToken("ident1231asdad"), IntToken(5), IdentifierToken("helloword"), IntToken(12355), GreaterThanToken, ArrowToken, DoubleEqualsToken, EqualsToken, LeftParenToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 504") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * reserved1 < => 12355 == / ( helloword = 5 > ident1231asdad )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("reserved1"), LessThanToken, ArrowToken, IntToken(12355), DoubleEqualsToken, DivideToken, LeftParenToken, IdentifierToken("helloword"), EqualsToken, IntToken(5), GreaterThanToken, IdentifierToken("ident1231asdad"), RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 505") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * reserved1 < 5 == / ( = ident1231asdad 12355 => ) helloword >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("reserved1"), LessThanToken, IntToken(5), DoubleEqualsToken, DivideToken, LeftParenToken, EqualsToken, IdentifierToken("ident1231asdad"), IntToken(12355), ArrowToken, RightParenToken, IdentifierToken("helloword"), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 506") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * reserved1 > / ) 12355 ( => 5 helloword ident1231asdad < == =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("reserved1"), GreaterThanToken, DivideToken, RightParenToken, IntToken(12355), LeftParenToken, ArrowToken, IntToken(5), IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), LessThanToken, DoubleEqualsToken, EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 507") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * reserved1 > ) helloword ( / < 12355 == 5 => ident1231asdad =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("reserved1"), GreaterThanToken, RightParenToken, IdentifierToken("helloword"), LeftParenToken, DivideToken, LessThanToken, IntToken(12355), DoubleEqualsToken, IntToken(5), ArrowToken, IdentifierToken("ident1231asdad"), EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 508") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * reserved1 > => 12355 5 / < = ident1231asdad ) ( helloword ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("reserved1"), GreaterThanToken, ArrowToken, IntToken(12355), IntToken(5), DivideToken, LessThanToken, EqualsToken, IdentifierToken("ident1231asdad"), RightParenToken, LeftParenToken, IdentifierToken("helloword"), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 509") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * reserved1 > 5 == ) / ident1231asdad = helloword 12355 < => (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("reserved1"), GreaterThanToken, IntToken(5), DoubleEqualsToken, RightParenToken, DivideToken, IdentifierToken("ident1231asdad"), EqualsToken, IdentifierToken("helloword"), IntToken(12355), LessThanToken, ArrowToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 510") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * reserved1 12355 / ) ident1231asdad ( helloword == => > < 5 =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("reserved1"), IntToken(12355), DivideToken, RightParenToken, IdentifierToken("ident1231asdad"), LeftParenToken, IdentifierToken("helloword"), DoubleEqualsToken, ArrowToken, GreaterThanToken, LessThanToken, IntToken(5), EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 511") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * reserved1 12355 ) helloword == ( / = 5 < => ident1231asdad >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("reserved1"), IntToken(12355), RightParenToken, IdentifierToken("helloword"), DoubleEqualsToken, LeftParenToken, DivideToken, EqualsToken, IntToken(5), LessThanToken, ArrowToken, IdentifierToken("ident1231asdad"), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 512") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * reserved1 12355 => > helloword ( / ident1231asdad 5 < == = )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("reserved1"), IntToken(12355), ArrowToken, GreaterThanToken, IdentifierToken("helloword"), LeftParenToken, DivideToken, IdentifierToken("ident1231asdad"), IntToken(5), LessThanToken, DoubleEqualsToken, EqualsToken, RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 513") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * reserved1 12355 5 == => ( ) helloword = > < ident1231asdad /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("reserved1"), IntToken(12355), IntToken(5), DoubleEqualsToken, ArrowToken, LeftParenToken, RightParenToken, IdentifierToken("helloword"), EqualsToken, GreaterThanToken, LessThanToken, IdentifierToken("ident1231asdad"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 514") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * reserved1 5 / = ( == < ident1231asdad 12355 => ) helloword >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("reserved1"), IntToken(5), DivideToken, EqualsToken, LeftParenToken, DoubleEqualsToken, LessThanToken, IdentifierToken("ident1231asdad"), IntToken(12355), ArrowToken, RightParenToken, IdentifierToken("helloword"), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 515") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * reserved1 5 ) helloword < ( => > ident1231asdad 12355 == = /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("reserved1"), IntToken(5), RightParenToken, IdentifierToken("helloword"), LessThanToken, LeftParenToken, ArrowToken, GreaterThanToken, IdentifierToken("ident1231asdad"), IntToken(12355), DoubleEqualsToken, EqualsToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 516") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * reserved1 5 => 12355 ( ) > < = ident1231asdad == helloword /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("reserved1"), IntToken(5), ArrowToken, IntToken(12355), LeftParenToken, RightParenToken, GreaterThanToken, LessThanToken, EqualsToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken, IdentifierToken("helloword"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 517") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * reserved1 5 12355 == > ( ident1231asdad = < ) / helloword =>")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("reserved1"), IntToken(5), IntToken(12355), DoubleEqualsToken, GreaterThanToken, LeftParenToken, IdentifierToken("ident1231asdad"), EqualsToken, LessThanToken, RightParenToken, DivideToken, IdentifierToken("helloword"), ArrowToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 518") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * reserved1 ident1231asdad / = == ) helloword => 5 12355 > < (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("reserved1"), IdentifierToken("ident1231asdad"), DivideToken, EqualsToken, DoubleEqualsToken, RightParenToken, IdentifierToken("helloword"), ArrowToken, IntToken(5), IntToken(12355), GreaterThanToken, LessThanToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 519") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * reserved1 ident1231asdad ) helloword 12355 = / == => > < 5 (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("reserved1"), IdentifierToken("ident1231asdad"), RightParenToken, IdentifierToken("helloword"), IntToken(12355), EqualsToken, DivideToken, DoubleEqualsToken, ArrowToken, GreaterThanToken, LessThanToken, IntToken(5), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 520") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * reserved1 ident1231asdad => 12355 = ) ( / 5 < == helloword >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("reserved1"), IdentifierToken("ident1231asdad"), ArrowToken, IntToken(12355), EqualsToken, RightParenToken, LeftParenToken, DivideToken, IntToken(5), LessThanToken, DoubleEqualsToken, IdentifierToken("helloword"), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 521") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * reserved1 ident1231asdad 12355 == helloword ) ( 5 > < => = /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("reserved1"), IdentifierToken("ident1231asdad"), IntToken(12355), DoubleEqualsToken, IdentifierToken("helloword"), RightParenToken, LeftParenToken, IntToken(5), GreaterThanToken, LessThanToken, ArrowToken, EqualsToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 522") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * reserved1 helloword / = < == => ident1231asdad ) 12355 > 5 (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("reserved1"), IdentifierToken("helloword"), DivideToken, EqualsToken, LessThanToken, DoubleEqualsToken, ArrowToken, IdentifierToken("ident1231asdad"), RightParenToken, IntToken(12355), GreaterThanToken, IntToken(5), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 523") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * reserved1 helloword = / ( => > 5 12355 == ) ident1231asdad <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("reserved1"), IdentifierToken("helloword"), EqualsToken, DivideToken, LeftParenToken, ArrowToken, GreaterThanToken, IntToken(5), IntToken(12355), DoubleEqualsToken, RightParenToken, IdentifierToken("ident1231asdad"), LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 524") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * reserved1 helloword => 12355 < ) > == ident1231asdad 5 = ( /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("reserved1"), IdentifierToken("helloword"), ArrowToken, IntToken(12355), LessThanToken, RightParenToken, GreaterThanToken, DoubleEqualsToken, IdentifierToken("ident1231asdad"), IntToken(5), EqualsToken, LeftParenToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 525") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * reserved1 helloword 12355 => ( = 5 < ) > == ident1231asdad /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("reserved1"), IdentifierToken("helloword"), IntToken(12355), ArrowToken, LeftParenToken, EqualsToken, IntToken(5), LessThanToken, RightParenToken, GreaterThanToken, DoubleEqualsToken, IdentifierToken("ident1231asdad"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 526") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad / ( == 5 => helloword < 12355 = ) reserved1 >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), DivideToken, LeftParenToken, DoubleEqualsToken, IntToken(5), ArrowToken, IdentifierToken("helloword"), LessThanToken, IntToken(12355), EqualsToken, RightParenToken, IdentifierToken("reserved1"), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 527") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad / == ( => > ) < helloword reserved1 5 12355 =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), DivideToken, DoubleEqualsToken, LeftParenToken, ArrowToken, GreaterThanToken, RightParenToken, LessThanToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), IntToken(5), IntToken(12355), EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 528") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad / < 5 reserved1 == ) = => 12355 > helloword (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), DivideToken, LessThanToken, IntToken(5), IdentifierToken("reserved1"), DoubleEqualsToken, RightParenToken, EqualsToken, ArrowToken, IntToken(12355), GreaterThanToken, IdentifierToken("helloword"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 529") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad / 5 < == => = ( reserved1 > ) helloword 12355")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), DivideToken, IntToken(5), LessThanToken, DoubleEqualsToken, ArrowToken, EqualsToken, LeftParenToken, IdentifierToken("reserved1"), GreaterThanToken, RightParenToken, IdentifierToken("helloword"), IntToken(12355))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 530") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad ( / == helloword < => reserved1 5 12355 > = )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), LeftParenToken, DivideToken, DoubleEqualsToken, IdentifierToken("helloword"), LessThanToken, ArrowToken, IdentifierToken("reserved1"), IntToken(5), IntToken(12355), GreaterThanToken, EqualsToken, RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 531") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad ( == / > < 12355 helloword = 5 => reserved1 )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), LeftParenToken, DoubleEqualsToken, DivideToken, GreaterThanToken, LessThanToken, IntToken(12355), IdentifierToken("helloword"), EqualsToken, IntToken(5), ArrowToken, IdentifierToken("reserved1"), RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 532") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad ( < reserved1 / => 12355 5 > = ) helloword ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), LeftParenToken, LessThanToken, IdentifierToken("reserved1"), DivideToken, ArrowToken, IntToken(12355), IntToken(5), GreaterThanToken, EqualsToken, RightParenToken, IdentifierToken("helloword"), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 533") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad ( 5 < > == reserved1 => helloword 12355 = ) /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), LeftParenToken, IntToken(5), LessThanToken, GreaterThanToken, DoubleEqualsToken, IdentifierToken("reserved1"), ArrowToken, IdentifierToken("helloword"), IntToken(12355), EqualsToken, RightParenToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 534") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad ) / => = > helloword 12355 == 5 < reserved1 (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), RightParenToken, DivideToken, ArrowToken, EqualsToken, GreaterThanToken, IdentifierToken("helloword"), IntToken(12355), DoubleEqualsToken, IntToken(5), LessThanToken, IdentifierToken("reserved1"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 535") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad ) == / 5 > ( < reserved1 => = helloword 12355")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), RightParenToken, DoubleEqualsToken, DivideToken, IntToken(5), GreaterThanToken, LeftParenToken, LessThanToken, IdentifierToken("reserved1"), ArrowToken, EqualsToken, IdentifierToken("helloword"), IntToken(12355))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 536") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad ) < reserved1 = > ( == helloword 5 12355 => /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), RightParenToken, LessThanToken, IdentifierToken("reserved1"), EqualsToken, GreaterThanToken, LeftParenToken, DoubleEqualsToken, IdentifierToken("helloword"), IntToken(5), IntToken(12355), ArrowToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 537") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad ) 5 < reserved1 => = ( == 12355 > helloword /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), RightParenToken, IntToken(5), LessThanToken, IdentifierToken("reserved1"), ArrowToken, EqualsToken, LeftParenToken, DoubleEqualsToken, IntToken(12355), GreaterThanToken, IdentifierToken("helloword"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 538") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad = / => < 12355 > ( reserved1 == ) helloword 5")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), EqualsToken, DivideToken, ArrowToken, LessThanToken, IntToken(12355), GreaterThanToken, LeftParenToken, IdentifierToken("reserved1"), DoubleEqualsToken, RightParenToken, IdentifierToken("helloword"), IntToken(5))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 539") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad = == / helloword > < reserved1 5 12355 => ) (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), EqualsToken, DoubleEqualsToken, DivideToken, IdentifierToken("helloword"), GreaterThanToken, LessThanToken, IdentifierToken("reserved1"), IntToken(5), IntToken(12355), ArrowToken, RightParenToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 540") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad = < reserved1 => > 12355 helloword ( == ) 5 /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), EqualsToken, LessThanToken, IdentifierToken("reserved1"), ArrowToken, GreaterThanToken, IntToken(12355), IdentifierToken("helloword"), LeftParenToken, DoubleEqualsToken, RightParenToken, IntToken(5), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 541") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad = 5 > / < reserved1 12355 => ) ( helloword ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), EqualsToken, IntToken(5), GreaterThanToken, DivideToken, LessThanToken, IdentifierToken("reserved1"), IntToken(12355), ArrowToken, RightParenToken, LeftParenToken, IdentifierToken("helloword"), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 542") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad == / => 12355 > helloword < reserved1 5 = ) (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken, DivideToken, ArrowToken, IntToken(12355), GreaterThanToken, IdentifierToken("helloword"), LessThanToken, IdentifierToken("reserved1"), IntToken(5), EqualsToken, RightParenToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 543") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad == = ( ) 5 / 12355 < reserved1 > helloword =>")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken, EqualsToken, LeftParenToken, RightParenToken, IntToken(5), DivideToken, IntToken(12355), LessThanToken, IdentifierToken("reserved1"), GreaterThanToken, IdentifierToken("helloword"), ArrowToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 544") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad == < reserved1 12355 > ( = 5 ) / helloword =>")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken, LessThanToken, IdentifierToken("reserved1"), IntToken(12355), GreaterThanToken, LeftParenToken, EqualsToken, IntToken(5), RightParenToken, DivideToken, IdentifierToken("helloword"), ArrowToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 545") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad == 5 > ) 12355 = ( helloword reserved1 < => /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken, IntToken(5), GreaterThanToken, RightParenToken, IntToken(12355), EqualsToken, LeftParenToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), LessThanToken, ArrowToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 546") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad => / == reserved1 12355 < ) = 5 > helloword (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), ArrowToken, DivideToken, DoubleEqualsToken, IdentifierToken("reserved1"), IntToken(12355), LessThanToken, RightParenToken, EqualsToken, IntToken(5), GreaterThanToken, IdentifierToken("helloword"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 547") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad => = ( < 5 12355 / reserved1 == ) helloword >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), ArrowToken, EqualsToken, LeftParenToken, LessThanToken, IntToken(5), IntToken(12355), DivideToken, IdentifierToken("reserved1"), DoubleEqualsToken, RightParenToken, IdentifierToken("helloword"), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 548") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad => < reserved1 helloword > == 5 12355 = ) ( /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), ArrowToken, LessThanToken, IdentifierToken("reserved1"), IdentifierToken("helloword"), GreaterThanToken, DoubleEqualsToken, IntToken(5), IntToken(12355), EqualsToken, RightParenToken, LeftParenToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 549") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad => 5 > == 12355 reserved1 helloword ( = ) < /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), ArrowToken, IntToken(5), GreaterThanToken, DoubleEqualsToken, IntToken(12355), IdentifierToken("reserved1"), IdentifierToken("helloword"), LeftParenToken, EqualsToken, RightParenToken, LessThanToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 550") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad < / => ( 5 helloword 12355 > = ) reserved1 ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), LessThanToken, DivideToken, ArrowToken, LeftParenToken, IntToken(5), IdentifierToken("helloword"), IntToken(12355), GreaterThanToken, EqualsToken, RightParenToken, IdentifierToken("reserved1"), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 551") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad < = ( 12355 reserved1 / > helloword 5 => == )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), LessThanToken, EqualsToken, LeftParenToken, IntToken(12355), IdentifierToken("reserved1"), DivideToken, GreaterThanToken, IdentifierToken("helloword"), IntToken(5), ArrowToken, DoubleEqualsToken, RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 552") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad < => helloword ( 5 ) > = 12355 == reserved1 /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), LessThanToken, ArrowToken, IdentifierToken("helloword"), LeftParenToken, IntToken(5), RightParenToken, GreaterThanToken, EqualsToken, IntToken(12355), DoubleEqualsToken, IdentifierToken("reserved1"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 553") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad < 5 > 12355 reserved1 ) = => ( / helloword ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), LessThanToken, IntToken(5), GreaterThanToken, IntToken(12355), IdentifierToken("reserved1"), RightParenToken, EqualsToken, ArrowToken, LeftParenToken, DivideToken, IdentifierToken("helloword"), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 554") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad > / => = reserved1 < ) helloword 5 12355 == (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), GreaterThanToken, DivideToken, ArrowToken, EqualsToken, IdentifierToken("reserved1"), LessThanToken, RightParenToken, IdentifierToken("helloword"), IntToken(5), IntToken(12355), DoubleEqualsToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 555") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad > = ( reserved1 5 < ) == 12355 => helloword /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), GreaterThanToken, EqualsToken, LeftParenToken, IdentifierToken("reserved1"), IntToken(5), LessThanToken, RightParenToken, DoubleEqualsToken, IntToken(12355), ArrowToken, IdentifierToken("helloword"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 556") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad > => helloword = 5 12355 / < ) ( reserved1 ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), GreaterThanToken, ArrowToken, IdentifierToken("helloword"), EqualsToken, IntToken(5), IntToken(12355), DivideToken, LessThanToken, RightParenToken, LeftParenToken, IdentifierToken("reserved1"), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 557") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad > 5 < helloword 12355 => reserved1 == = ) ( /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), GreaterThanToken, IntToken(5), LessThanToken, IdentifierToken("helloword"), IntToken(12355), ArrowToken, IdentifierToken("reserved1"), DoubleEqualsToken, EqualsToken, RightParenToken, LeftParenToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 558") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad 12355 / => < reserved1 helloword 5 ) == = > (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), IntToken(12355), DivideToken, ArrowToken, LessThanToken, IdentifierToken("reserved1"), IdentifierToken("helloword"), IntToken(5), RightParenToken, DoubleEqualsToken, EqualsToken, GreaterThanToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 559") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad 12355 = ) / helloword ( 5 > => == reserved1 <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), IntToken(12355), EqualsToken, RightParenToken, DivideToken, IdentifierToken("helloword"), LeftParenToken, IntToken(5), GreaterThanToken, ArrowToken, DoubleEqualsToken, IdentifierToken("reserved1"), LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 560") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad 12355 => helloword < reserved1 ( == 5 > = ) /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), IntToken(12355), ArrowToken, IdentifierToken("helloword"), LessThanToken, IdentifierToken("reserved1"), LeftParenToken, DoubleEqualsToken, IntToken(5), GreaterThanToken, EqualsToken, RightParenToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 561") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad 12355 5 > ( helloword = => ) < == reserved1 /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), IntToken(12355), IntToken(5), GreaterThanToken, LeftParenToken, IdentifierToken("helloword"), EqualsToken, ArrowToken, RightParenToken, LessThanToken, DoubleEqualsToken, IdentifierToken("reserved1"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 562") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad 5 / => 12355 helloword == = > ) ( reserved1 <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), IntToken(5), DivideToken, ArrowToken, IntToken(12355), IdentifierToken("helloword"), DoubleEqualsToken, EqualsToken, GreaterThanToken, RightParenToken, LeftParenToken, IdentifierToken("reserved1"), LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 563") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad 5 = ) == helloword > ( reserved1 12355 < => /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), IntToken(5), EqualsToken, RightParenToken, DoubleEqualsToken, IdentifierToken("helloword"), GreaterThanToken, LeftParenToken, IdentifierToken("reserved1"), IntToken(12355), LessThanToken, ArrowToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 564") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad 5 => helloword 12355 reserved1 < ( ) == = > /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), IntToken(5), ArrowToken, IdentifierToken("helloword"), IntToken(12355), IdentifierToken("reserved1"), LessThanToken, LeftParenToken, RightParenToken, DoubleEqualsToken, EqualsToken, GreaterThanToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 565") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad 5 12355 > = helloword reserved1 / => ) ( < ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), IntToken(5), IntToken(12355), GreaterThanToken, EqualsToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), DivideToken, ArrowToken, RightParenToken, LeftParenToken, LessThanToken, DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 566") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad reserved1 / => helloword 5 12355 > < == = ) (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), DivideToken, ArrowToken, IdentifierToken("helloword"), IntToken(5), IntToken(12355), GreaterThanToken, LessThanToken, DoubleEqualsToken, EqualsToken, RightParenToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 567") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad reserved1 = ) > / ( helloword => 12355 < 5 ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), EqualsToken, RightParenToken, GreaterThanToken, DivideToken, LeftParenToken, IdentifierToken("helloword"), ArrowToken, IntToken(12355), LessThanToken, IntToken(5), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 568") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad reserved1 < / ) ( == 5 12355 => = helloword >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), LessThanToken, DivideToken, RightParenToken, LeftParenToken, DoubleEqualsToken, IntToken(5), IntToken(12355), ArrowToken, EqualsToken, IdentifierToken("helloword"), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 569") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad reserved1 12355 > < / = => helloword 5 == ) (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IntToken(12355), GreaterThanToken, LessThanToken, DivideToken, EqualsToken, ArrowToken, IdentifierToken("helloword"), IntToken(5), DoubleEqualsToken, RightParenToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 570") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad helloword / < = ( > 12355 == 5 => reserved1 )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), IdentifierToken("helloword"), DivideToken, LessThanToken, EqualsToken, LeftParenToken, GreaterThanToken, IntToken(12355), DoubleEqualsToken, IntToken(5), ArrowToken, IdentifierToken("reserved1"), RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 571") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad helloword = ) 5 / > => 12355 == ( reserved1 <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), IdentifierToken("helloword"), EqualsToken, RightParenToken, IntToken(5), DivideToken, GreaterThanToken, ArrowToken, IntToken(12355), DoubleEqualsToken, LeftParenToken, IdentifierToken("reserved1"), LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 572") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad helloword < / == ( 5 = reserved1 12355 > => )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), IdentifierToken("helloword"), LessThanToken, DivideToken, DoubleEqualsToken, LeftParenToken, IntToken(5), EqualsToken, IdentifierToken("reserved1"), IntToken(12355), GreaterThanToken, ArrowToken, RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 573") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * ident1231asdad helloword 12355 > reserved1 / 5 ) = => == < (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("ident1231asdad"), IdentifierToken("helloword"), IntToken(12355), GreaterThanToken, IdentifierToken("reserved1"), DivideToken, IntToken(5), RightParenToken, EqualsToken, ArrowToken, DoubleEqualsToken, LessThanToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 574") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword / ( > < = ) == reserved1 12355 => ident1231asdad 5")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), DivideToken, LeftParenToken, GreaterThanToken, LessThanToken, EqualsToken, RightParenToken, DoubleEqualsToken, IdentifierToken("reserved1"), IntToken(12355), ArrowToken, IdentifierToken("ident1231asdad"), IntToken(5))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 575") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword / == => ( = ) ident1231asdad reserved1 5 12355 > <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), DivideToken, DoubleEqualsToken, ArrowToken, LeftParenToken, EqualsToken, RightParenToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IntToken(5), IntToken(12355), GreaterThanToken, LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 576") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword / > ( 12355 = == ident1231asdad => 5 < reserved1 )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), DivideToken, GreaterThanToken, LeftParenToken, IntToken(12355), EqualsToken, DoubleEqualsToken, IdentifierToken("ident1231asdad"), ArrowToken, IntToken(5), LessThanToken, IdentifierToken("reserved1"), RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 577") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword / 5 reserved1 ) = => 12355 > == ( ident1231asdad <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), DivideToken, IntToken(5), IdentifierToken("reserved1"), RightParenToken, EqualsToken, ArrowToken, IntToken(12355), GreaterThanToken, DoubleEqualsToken, LeftParenToken, IdentifierToken("ident1231asdad"), LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 578") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword ( / > 5 = < 12355 ident1231asdad reserved1 => == )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), LeftParenToken, DivideToken, GreaterThanToken, IntToken(5), EqualsToken, LessThanToken, IntToken(12355), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), ArrowToken, DoubleEqualsToken, RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 579") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword ( == => = ) 5 12355 < reserved1 > ident1231asdad /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), LeftParenToken, DoubleEqualsToken, ArrowToken, EqualsToken, RightParenToken, IntToken(5), IntToken(12355), LessThanToken, IdentifierToken("reserved1"), GreaterThanToken, IdentifierToken("ident1231asdad"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 580") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword ( > / reserved1 = 5 => 12355 == ) ident1231asdad <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), LeftParenToken, GreaterThanToken, DivideToken, IdentifierToken("reserved1"), EqualsToken, IntToken(5), ArrowToken, IntToken(12355), DoubleEqualsToken, RightParenToken, IdentifierToken("ident1231asdad"), LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 581") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword ( 5 reserved1 == ) ident1231asdad = 12355 > < => /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), LeftParenToken, IntToken(5), IdentifierToken("reserved1"), DoubleEqualsToken, RightParenToken, IdentifierToken("ident1231asdad"), EqualsToken, IntToken(12355), GreaterThanToken, LessThanToken, ArrowToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 582") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword ) / > ident1231asdad == ( => < 5 12355 reserved1 =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), RightParenToken, DivideToken, GreaterThanToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken, LeftParenToken, ArrowToken, LessThanToken, IntToken(5), IntToken(12355), IdentifierToken("reserved1"), EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 583") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword ) == => > = ( / reserved1 12355 < ident1231asdad 5")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), RightParenToken, DoubleEqualsToken, ArrowToken, GreaterThanToken, EqualsToken, LeftParenToken, DivideToken, IdentifierToken("reserved1"), IntToken(12355), LessThanToken, IdentifierToken("ident1231asdad"), IntToken(5))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 584") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword ) > ( / => == ident1231asdad reserved1 5 12355 < =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), RightParenToken, GreaterThanToken, LeftParenToken, DivideToken, ArrowToken, DoubleEqualsToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IntToken(5), IntToken(12355), LessThanToken, EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 585") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword ) 5 reserved1 < = == ident1231asdad ( > => 12355 /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), RightParenToken, IntToken(5), IdentifierToken("reserved1"), LessThanToken, EqualsToken, DoubleEqualsToken, IdentifierToken("ident1231asdad"), LeftParenToken, GreaterThanToken, ArrowToken, IntToken(12355), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 586") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword = / 12355 ) => > reserved1 5 == ( ident1231asdad <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), EqualsToken, DivideToken, IntToken(12355), RightParenToken, ArrowToken, GreaterThanToken, IdentifierToken("reserved1"), IntToken(5), DoubleEqualsToken, LeftParenToken, IdentifierToken("ident1231asdad"), LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 587") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword = == => 5 ) 12355 > ident1231asdad reserved1 < ( /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), EqualsToken, DoubleEqualsToken, ArrowToken, IntToken(5), RightParenToken, IntToken(12355), GreaterThanToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), LessThanToken, LeftParenToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 588") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword = > ( == => reserved1 12355 ) 5 < ident1231asdad /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), EqualsToken, GreaterThanToken, LeftParenToken, DoubleEqualsToken, ArrowToken, IdentifierToken("reserved1"), IntToken(12355), RightParenToken, IntToken(5), LessThanToken, IdentifierToken("ident1231asdad"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 589") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword = 5 reserved1 12355 ) ident1231asdad == < ( / > =>")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), EqualsToken, IntToken(5), IdentifierToken("reserved1"), IntToken(12355), RightParenToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken, LessThanToken, LeftParenToken, DivideToken, GreaterThanToken, ArrowToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 590") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword == / 12355 => < ( = ident1231asdad reserved1 5 > )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), DoubleEqualsToken, DivideToken, IntToken(12355), ArrowToken, LessThanToken, LeftParenToken, EqualsToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IntToken(5), GreaterThanToken, RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 591") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword == = => ident1231asdad < ( ) > 5 12355 reserved1 /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), DoubleEqualsToken, EqualsToken, ArrowToken, IdentifierToken("ident1231asdad"), LessThanToken, LeftParenToken, RightParenToken, GreaterThanToken, IntToken(5), IntToken(12355), IdentifierToken("reserved1"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 592") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword == > ( < => = / reserved1 12355 ) ident1231asdad 5")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), DoubleEqualsToken, GreaterThanToken, LeftParenToken, LessThanToken, ArrowToken, EqualsToken, DivideToken, IdentifierToken("reserved1"), IntToken(12355), RightParenToken, IdentifierToken("ident1231asdad"), IntToken(5))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 593") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword == 5 ident1231asdad / => = reserved1 12355 > < ) (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), DoubleEqualsToken, IntToken(5), IdentifierToken("ident1231asdad"), DivideToken, ArrowToken, EqualsToken, IdentifierToken("reserved1"), IntToken(12355), GreaterThanToken, LessThanToken, RightParenToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 594") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword => / 12355 > == < ident1231asdad ) 5 = reserved1 (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), ArrowToken, DivideToken, IntToken(12355), GreaterThanToken, DoubleEqualsToken, LessThanToken, IdentifierToken("ident1231asdad"), RightParenToken, IntToken(5), EqualsToken, IdentifierToken("reserved1"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 595") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword => = < ( > 5 reserved1 12355 ) / ident1231asdad ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), ArrowToken, EqualsToken, LessThanToken, LeftParenToken, GreaterThanToken, IntToken(5), IdentifierToken("reserved1"), IntToken(12355), RightParenToken, DivideToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 596") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword => > ( 5 == reserved1 < ident1231asdad 12355 = ) /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), ArrowToken, GreaterThanToken, LeftParenToken, IntToken(5), DoubleEqualsToken, IdentifierToken("reserved1"), LessThanToken, IdentifierToken("ident1231asdad"), IntToken(12355), EqualsToken, RightParenToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 597") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword => 5 ident1231asdad ) == reserved1 < ( > = 12355 /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), ArrowToken, IntToken(5), IdentifierToken("ident1231asdad"), RightParenToken, DoubleEqualsToken, IdentifierToken("reserved1"), LessThanToken, LeftParenToken, GreaterThanToken, EqualsToken, IntToken(12355), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 598") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword < / 12355 reserved1 => ( == 5 = ) ident1231asdad >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), LessThanToken, DivideToken, IntToken(12355), IdentifierToken("reserved1"), ArrowToken, LeftParenToken, DoubleEqualsToken, IntToken(5), EqualsToken, RightParenToken, IdentifierToken("ident1231asdad"), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 599") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword < = => == 12355 ( ) ident1231asdad reserved1 5 > /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), LessThanToken, EqualsToken, ArrowToken, DoubleEqualsToken, IntToken(12355), LeftParenToken, RightParenToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IntToken(5), GreaterThanToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 600") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword < > ( ident1231asdad => = ) == 5 12355 reserved1 /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), LessThanToken, GreaterThanToken, LeftParenToken, IdentifierToken("ident1231asdad"), ArrowToken, EqualsToken, RightParenToken, DoubleEqualsToken, IntToken(5), IntToken(12355), IdentifierToken("reserved1"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 601") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword < 5 ident1231asdad == => = / 12355 ) ( reserved1 >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), LessThanToken, IntToken(5), IdentifierToken("ident1231asdad"), DoubleEqualsToken, ArrowToken, EqualsToken, DivideToken, IntToken(12355), RightParenToken, LeftParenToken, IdentifierToken("reserved1"), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 602") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword > / 5 ( < => ident1231asdad reserved1 12355 == = )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), GreaterThanToken, DivideToken, IntToken(5), LeftParenToken, LessThanToken, ArrowToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IntToken(12355), DoubleEqualsToken, EqualsToken, RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 603") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword > = => 12355 < 5 ident1231asdad ( == ) reserved1 /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), GreaterThanToken, EqualsToken, ArrowToken, IntToken(12355), LessThanToken, IntToken(5), IdentifierToken("ident1231asdad"), LeftParenToken, DoubleEqualsToken, RightParenToken, IdentifierToken("reserved1"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 604") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword > < ) ( 12355 reserved1 5 => = / ident1231asdad ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), GreaterThanToken, LessThanToken, RightParenToken, LeftParenToken, IntToken(12355), IdentifierToken("reserved1"), IntToken(5), ArrowToken, EqualsToken, DivideToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 605") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword > 5 ident1231asdad < == reserved1 = 12355 => ) ( /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), GreaterThanToken, IntToken(5), IdentifierToken("ident1231asdad"), LessThanToken, DoubleEqualsToken, IdentifierToken("reserved1"), EqualsToken, IntToken(12355), ArrowToken, RightParenToken, LeftParenToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 606") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword 12355 / 5 = > ( < == reserved1 => ident1231asdad )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), IntToken(12355), DivideToken, IntToken(5), EqualsToken, GreaterThanToken, LeftParenToken, LessThanToken, DoubleEqualsToken, IdentifierToken("reserved1"), ArrowToken, IdentifierToken("ident1231asdad"), RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 607") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword 12355 = => reserved1 > ( == 5 ) / ident1231asdad <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), IntToken(12355), EqualsToken, ArrowToken, IdentifierToken("reserved1"), GreaterThanToken, LeftParenToken, DoubleEqualsToken, IntToken(5), RightParenToken, DivideToken, IdentifierToken("ident1231asdad"), LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 608") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword 12355 < ) == 5 = ( ident1231asdad reserved1 > => /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), IntToken(12355), LessThanToken, RightParenToken, DoubleEqualsToken, IntToken(5), EqualsToken, LeftParenToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), GreaterThanToken, ArrowToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 609") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword 12355 5 ident1231asdad reserved1 => = ( ) < == > /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), IntToken(12355), IntToken(5), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), ArrowToken, EqualsToken, LeftParenToken, RightParenToken, LessThanToken, DoubleEqualsToken, GreaterThanToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 610") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword 5 / 12355 => > < ( reserved1 = ) ident1231asdad ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), IntToken(5), DivideToken, IntToken(12355), ArrowToken, GreaterThanToken, LessThanToken, LeftParenToken, IdentifierToken("reserved1"), EqualsToken, RightParenToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 611") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword 5 = < / 12355 > ident1231asdad reserved1 => == ) (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), IntToken(5), EqualsToken, LessThanToken, DivideToken, IntToken(12355), GreaterThanToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), ArrowToken, DoubleEqualsToken, RightParenToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 612") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword 5 < ) > 12355 reserved1 ident1231asdad ( == = => /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), IntToken(5), LessThanToken, RightParenToken, GreaterThanToken, IntToken(12355), IdentifierToken("reserved1"), IdentifierToken("ident1231asdad"), LeftParenToken, DoubleEqualsToken, EqualsToken, ArrowToken, DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 613") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword 5 reserved1 / ) > ident1231asdad < => = ( 12355 ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), IntToken(5), IdentifierToken("reserved1"), DivideToken, RightParenToken, GreaterThanToken, IdentifierToken("ident1231asdad"), LessThanToken, ArrowToken, EqualsToken, LeftParenToken, IntToken(12355), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 614") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword reserved1 / 12355 > 5 ( => ident1231asdad < == = )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), DivideToken, IntToken(12355), GreaterThanToken, IntToken(5), LeftParenToken, ArrowToken, IdentifierToken("ident1231asdad"), LessThanToken, DoubleEqualsToken, EqualsToken, RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 615") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword reserved1 = < ) 5 ( > == 12355 => ident1231asdad /")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), EqualsToken, LessThanToken, RightParenToken, IntToken(5), LeftParenToken, GreaterThanToken, DoubleEqualsToken, IntToken(12355), ArrowToken, IdentifierToken("ident1231asdad"), DivideToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 616") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword reserved1 < ) 5 12355 = == > ( / ident1231asdad =>")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), LessThanToken, RightParenToken, IntToken(5), IntToken(12355), EqualsToken, DoubleEqualsToken, GreaterThanToken, LeftParenToken, DivideToken, IdentifierToken("ident1231asdad"), ArrowToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 617") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword reserved1 5 / == 12355 => ) ident1231asdad > < = (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), IntToken(5), DivideToken, DoubleEqualsToken, IntToken(12355), ArrowToken, RightParenToken, IdentifierToken("ident1231asdad"), GreaterThanToken, LessThanToken, EqualsToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 618") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword ident1231asdad / 12355 reserved1 > => ) = < == 5 (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), DivideToken, IntToken(12355), IdentifierToken("reserved1"), GreaterThanToken, ArrowToken, RightParenToken, EqualsToken, LessThanToken, DoubleEqualsToken, IntToken(5), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 619") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword ident1231asdad = < => 5 12355 / > ) ( reserved1 ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), EqualsToken, LessThanToken, ArrowToken, IntToken(5), IntToken(12355), DivideToken, GreaterThanToken, RightParenToken, LeftParenToken, IdentifierToken("reserved1"), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 620") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword ident1231asdad < = / 5 12355 reserved1 > => == ) (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), LessThanToken, EqualsToken, DivideToken, IntToken(5), IntToken(12355), IdentifierToken("reserved1"), GreaterThanToken, ArrowToken, DoubleEqualsToken, RightParenToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 621") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - * helloword ident1231asdad 5 / < 12355 reserved1 > ) == = => (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, MultiplyToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), IntToken(5), DivideToken, LessThanToken, IntToken(12355), IdentifierToken("reserved1"), GreaterThanToken, RightParenToken, DoubleEqualsToken, EqualsToken, ArrowToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 622") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * ( ) ident1231asdad == helloword = 5 12355 < => reserved1 >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, LeftParenToken, RightParenToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken, IdentifierToken("helloword"), EqualsToken, IntToken(5), IntToken(12355), LessThanToken, ArrowToken, IdentifierToken("reserved1"), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 623") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * ( => 12355 reserved1 helloword = > ident1231asdad 5 < == )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, LeftParenToken, ArrowToken, IntToken(12355), IdentifierToken("reserved1"), IdentifierToken("helloword"), EqualsToken, GreaterThanToken, IdentifierToken("ident1231asdad"), IntToken(5), LessThanToken, DoubleEqualsToken, RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 624") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * ( 12355 => == helloword < 5 = reserved1 > ident1231asdad )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, LeftParenToken, IntToken(12355), ArrowToken, DoubleEqualsToken, IdentifierToken("helloword"), LessThanToken, IntToken(5), EqualsToken, IdentifierToken("reserved1"), GreaterThanToken, IdentifierToken("ident1231asdad"), RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 625") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * ( ident1231asdad ) reserved1 helloword < => 12355 == = 5 >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, LeftParenToken, IdentifierToken("ident1231asdad"), RightParenToken, IdentifierToken("reserved1"), IdentifierToken("helloword"), LessThanToken, ArrowToken, IntToken(12355), DoubleEqualsToken, EqualsToken, IntToken(5), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 626") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * ) ( ident1231asdad < helloword 12355 == reserved1 5 > => =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, RightParenToken, LeftParenToken, IdentifierToken("ident1231asdad"), LessThanToken, IdentifierToken("helloword"), IntToken(12355), DoubleEqualsToken, IdentifierToken("reserved1"), IntToken(5), GreaterThanToken, ArrowToken, EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 627") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * ) => 12355 helloword ident1231asdad 5 = == > < reserved1 (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, RightParenToken, ArrowToken, IntToken(12355), IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), IntToken(5), EqualsToken, DoubleEqualsToken, GreaterThanToken, LessThanToken, IdentifierToken("reserved1"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 628") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * ) 12355 => > helloword ident1231asdad ( 5 == = reserved1 <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, RightParenToken, IntToken(12355), ArrowToken, GreaterThanToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), LeftParenToken, IntToken(5), DoubleEqualsToken, EqualsToken, IdentifierToken("reserved1"), LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 629") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * ) ident1231asdad = ( helloword reserved1 5 12355 > < => ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, RightParenToken, IdentifierToken("ident1231asdad"), EqualsToken, LeftParenToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), IntToken(5), IntToken(12355), GreaterThanToken, LessThanToken, ArrowToken, DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 630") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * = ( ident1231asdad 5 ) == helloword < 12355 > reserved1 =>")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, EqualsToken, LeftParenToken, IdentifierToken("ident1231asdad"), IntToken(5), RightParenToken, DoubleEqualsToken, IdentifierToken("helloword"), LessThanToken, IntToken(12355), GreaterThanToken, IdentifierToken("reserved1"), ArrowToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 631") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * = => 5 == ( < ident1231asdad reserved1 > ) helloword 12355")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, EqualsToken, ArrowToken, IntToken(5), DoubleEqualsToken, LeftParenToken, LessThanToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), GreaterThanToken, RightParenToken, IdentifierToken("helloword"), IntToken(12355))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 632") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * = 12355 => ident1231asdad ( < 5 helloword reserved1 > == )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, EqualsToken, IntToken(12355), ArrowToken, IdentifierToken("ident1231asdad"), LeftParenToken, LessThanToken, IntToken(5), IdentifierToken("helloword"), IdentifierToken("reserved1"), GreaterThanToken, DoubleEqualsToken, RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 633") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * = ident1231asdad ) < ( 12355 5 => reserved1 > helloword ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, EqualsToken, IdentifierToken("ident1231asdad"), RightParenToken, LessThanToken, LeftParenToken, IntToken(12355), IntToken(5), ArrowToken, IdentifierToken("reserved1"), GreaterThanToken, IdentifierToken("helloword"), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 634") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * == ( ident1231asdad helloword ) 12355 < 5 => = reserved1 >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, DoubleEqualsToken, LeftParenToken, IdentifierToken("ident1231asdad"), IdentifierToken("helloword"), RightParenToken, IntToken(12355), LessThanToken, IntToken(5), ArrowToken, EqualsToken, IdentifierToken("reserved1"), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 635") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * == => 5 > ( ident1231asdad = helloword reserved1 12355 < )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, DoubleEqualsToken, ArrowToken, IntToken(5), GreaterThanToken, LeftParenToken, IdentifierToken("ident1231asdad"), EqualsToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), IntToken(12355), LessThanToken, RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 636") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * == 12355 < ( ) helloword => > reserved1 5 ident1231asdad =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, DoubleEqualsToken, IntToken(12355), LessThanToken, LeftParenToken, RightParenToken, IdentifierToken("helloword"), ArrowToken, GreaterThanToken, IdentifierToken("reserved1"), IntToken(5), IdentifierToken("ident1231asdad"), EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 637") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * == ident1231asdad ) 12355 = ( => reserved1 > < helloword 5")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, DoubleEqualsToken, IdentifierToken("ident1231asdad"), RightParenToken, IntToken(12355), EqualsToken, LeftParenToken, ArrowToken, IdentifierToken("reserved1"), GreaterThanToken, LessThanToken, IdentifierToken("helloword"), IntToken(5))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 638") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * => ( helloword = == ) ident1231asdad reserved1 5 12355 > <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, ArrowToken, LeftParenToken, IdentifierToken("helloword"), EqualsToken, DoubleEqualsToken, RightParenToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IntToken(5), IntToken(12355), GreaterThanToken, LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 639") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * => == 5 reserved1 ) = helloword < 12355 > ident1231asdad (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, ArrowToken, DoubleEqualsToken, IntToken(5), IdentifierToken("reserved1"), RightParenToken, EqualsToken, IdentifierToken("helloword"), LessThanToken, IntToken(12355), GreaterThanToken, IdentifierToken("ident1231asdad"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 640") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * => 12355 < = ) > ident1231asdad reserved1 == ( helloword 5")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, ArrowToken, IntToken(12355), LessThanToken, EqualsToken, RightParenToken, GreaterThanToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), DoubleEqualsToken, LeftParenToken, IdentifierToken("helloword"), IntToken(5))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 641") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * => ident1231asdad ) reserved1 = > 12355 helloword 5 < == (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, ArrowToken, IdentifierToken("ident1231asdad"), RightParenToken, IdentifierToken("reserved1"), EqualsToken, GreaterThanToken, IntToken(12355), IdentifierToken("helloword"), IntToken(5), LessThanToken, DoubleEqualsToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 642") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * < ( helloword => = 5 12355 == reserved1 > ident1231asdad )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, LessThanToken, LeftParenToken, IdentifierToken("helloword"), ArrowToken, EqualsToken, IntToken(5), IntToken(12355), DoubleEqualsToken, IdentifierToken("reserved1"), GreaterThanToken, IdentifierToken("ident1231asdad"), RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 643") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * < == 5 helloword ) reserved1 => 12355 = ( ident1231asdad >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, LessThanToken, DoubleEqualsToken, IntToken(5), IdentifierToken("helloword"), RightParenToken, IdentifierToken("reserved1"), ArrowToken, IntToken(12355), EqualsToken, LeftParenToken, IdentifierToken("ident1231asdad"), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 644") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * < 12355 => > ) helloword = ident1231asdad reserved1 5 == (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, LessThanToken, IntToken(12355), ArrowToken, GreaterThanToken, RightParenToken, IdentifierToken("helloword"), EqualsToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IntToken(5), DoubleEqualsToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 645") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * < ident1231asdad = ( => ) > 12355 reserved1 5 helloword ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, LessThanToken, IdentifierToken("ident1231asdad"), EqualsToken, LeftParenToken, ArrowToken, RightParenToken, GreaterThanToken, IntToken(12355), IdentifierToken("reserved1"), IntToken(5), IdentifierToken("helloword"), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 646") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * > ( helloword 12355 == = ) reserved1 < => ident1231asdad 5")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, GreaterThanToken, LeftParenToken, IdentifierToken("helloword"), IntToken(12355), DoubleEqualsToken, EqualsToken, RightParenToken, IdentifierToken("reserved1"), LessThanToken, ArrowToken, IdentifierToken("ident1231asdad"), IntToken(5))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 647") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * > == reserved1 ) => = helloword ident1231asdad 5 12355 < (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, GreaterThanToken, DoubleEqualsToken, IdentifierToken("reserved1"), RightParenToken, ArrowToken, EqualsToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), IntToken(5), IntToken(12355), LessThanToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 648") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * > 12355 => reserved1 = == helloword ) 5 < ident1231asdad (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, GreaterThanToken, IntToken(12355), ArrowToken, IdentifierToken("reserved1"), EqualsToken, DoubleEqualsToken, IdentifierToken("helloword"), RightParenToken, IntToken(5), LessThanToken, IdentifierToken("ident1231asdad"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 649") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * > ident1231asdad = == => 12355 reserved1 5 ) ( helloword <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, GreaterThanToken, IdentifierToken("ident1231asdad"), EqualsToken, DoubleEqualsToken, ArrowToken, IntToken(12355), IdentifierToken("reserved1"), IntToken(5), RightParenToken, LeftParenToken, IdentifierToken("helloword"), LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 650") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * 12355 ( helloword reserved1 == > < ident1231asdad 5 => = )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, IntToken(12355), LeftParenToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), DoubleEqualsToken, GreaterThanToken, LessThanToken, IdentifierToken("ident1231asdad"), IntToken(5), ArrowToken, EqualsToken, RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 651") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * 12355 == reserved1 => = ident1231asdad > ) 5 < helloword (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, IntToken(12355), DoubleEqualsToken, IdentifierToken("reserved1"), ArrowToken, EqualsToken, IdentifierToken("ident1231asdad"), GreaterThanToken, RightParenToken, IntToken(5), LessThanToken, IdentifierToken("helloword"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 652") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * 12355 > => helloword = ident1231asdad == 5 ) ( reserved1 <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, IntToken(12355), GreaterThanToken, ArrowToken, IdentifierToken("helloword"), EqualsToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken, IntToken(5), RightParenToken, LeftParenToken, IdentifierToken("reserved1"), LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 653") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * 12355 ident1231asdad = < => ( == helloword reserved1 5 > )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, IntToken(12355), IdentifierToken("ident1231asdad"), EqualsToken, LessThanToken, ArrowToken, LeftParenToken, DoubleEqualsToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), IntToken(5), GreaterThanToken, RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 654") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * 5 ) ( = > => < 12355 ident1231asdad reserved1 helloword ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, IntToken(5), RightParenToken, LeftParenToken, EqualsToken, GreaterThanToken, ArrowToken, LessThanToken, IntToken(12355), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IdentifierToken("helloword"), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 655") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * 5 == reserved1 > => = ( ident1231asdad < ) helloword 12355")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, IntToken(5), DoubleEqualsToken, IdentifierToken("reserved1"), GreaterThanToken, ArrowToken, EqualsToken, LeftParenToken, IdentifierToken("ident1231asdad"), LessThanToken, RightParenToken, IdentifierToken("helloword"), IntToken(12355))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 656") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * 5 > < ) => == helloword ident1231asdad reserved1 12355 = (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, IntToken(5), GreaterThanToken, LessThanToken, RightParenToken, ArrowToken, DoubleEqualsToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IntToken(12355), EqualsToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 657") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * 5 ident1231asdad = 12355 => < helloword ) > == reserved1 (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, IntToken(5), IdentifierToken("ident1231asdad"), EqualsToken, IntToken(12355), ArrowToken, LessThanToken, IdentifierToken("helloword"), RightParenToken, GreaterThanToken, DoubleEqualsToken, IdentifierToken("reserved1"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 658") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * reserved1 ) ( => > 5 ident1231asdad 12355 == = helloword <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, IdentifierToken("reserved1"), RightParenToken, LeftParenToken, ArrowToken, GreaterThanToken, IntToken(5), IdentifierToken("ident1231asdad"), IntToken(12355), DoubleEqualsToken, EqualsToken, IdentifierToken("helloword"), LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 659") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * reserved1 == 5 ident1231asdad => 12355 < helloword > = ) (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, IdentifierToken("reserved1"), DoubleEqualsToken, IntToken(5), IdentifierToken("ident1231asdad"), ArrowToken, IntToken(12355), LessThanToken, IdentifierToken("helloword"), GreaterThanToken, EqualsToken, RightParenToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 660") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * reserved1 > < == => helloword 12355 ) 5 = ident1231asdad (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, IdentifierToken("reserved1"), GreaterThanToken, LessThanToken, DoubleEqualsToken, ArrowToken, IdentifierToken("helloword"), IntToken(12355), RightParenToken, IntToken(5), EqualsToken, IdentifierToken("ident1231asdad"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 661") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * reserved1 ident1231asdad = helloword < ( => 12355 == ) 5 >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, IdentifierToken("reserved1"), IdentifierToken("ident1231asdad"), EqualsToken, IdentifierToken("helloword"), LessThanToken, LeftParenToken, ArrowToken, IntToken(12355), DoubleEqualsToken, RightParenToken, IntToken(5), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 662") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * ident1231asdad ) ( > 12355 == => helloword reserved1 5 < =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, IdentifierToken("ident1231asdad"), RightParenToken, LeftParenToken, GreaterThanToken, IntToken(12355), DoubleEqualsToken, ArrowToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), IntToken(5), LessThanToken, EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 663") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * ident1231asdad == reserved1 ( > => = < 5 12355 helloword )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken, IdentifierToken("reserved1"), LeftParenToken, GreaterThanToken, ArrowToken, EqualsToken, LessThanToken, IntToken(5), IntToken(12355), IdentifierToken("helloword"), RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 664") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * ident1231asdad > < 12355 => == ( reserved1 = ) helloword 5")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, IdentifierToken("ident1231asdad"), GreaterThanToken, LessThanToken, IntToken(12355), ArrowToken, DoubleEqualsToken, LeftParenToken, IdentifierToken("reserved1"), EqualsToken, RightParenToken, IdentifierToken("helloword"), IntToken(5))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 665") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * ident1231asdad reserved1 == ) > < helloword 5 12355 => = (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), DoubleEqualsToken, RightParenToken, GreaterThanToken, LessThanToken, IdentifierToken("helloword"), IntToken(5), IntToken(12355), ArrowToken, EqualsToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 666") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * helloword ) ( 5 > 12355 ident1231asdad == < => reserved1 =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, IdentifierToken("helloword"), RightParenToken, LeftParenToken, IntToken(5), GreaterThanToken, IntToken(12355), IdentifierToken("ident1231asdad"), DoubleEqualsToken, LessThanToken, ArrowToken, IdentifierToken("reserved1"), EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 667") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * helloword == reserved1 = > 5 12355 < ) ( ident1231asdad =>")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, IdentifierToken("helloword"), DoubleEqualsToken, IdentifierToken("reserved1"), EqualsToken, GreaterThanToken, IntToken(5), IntToken(12355), LessThanToken, RightParenToken, LeftParenToken, IdentifierToken("ident1231asdad"), ArrowToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 668") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * helloword > < reserved1 => ident1231asdad == 5 12355 = ) (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, IdentifierToken("helloword"), GreaterThanToken, LessThanToken, IdentifierToken("reserved1"), ArrowToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken, IntToken(5), IntToken(12355), EqualsToken, RightParenToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 669") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / * helloword reserved1 == => 12355 ( > = 5 < ident1231asdad )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, MultiplyToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), DoubleEqualsToken, ArrowToken, IntToken(12355), LeftParenToken, GreaterThanToken, EqualsToken, IntToken(5), LessThanToken, IdentifierToken("ident1231asdad"), RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 670") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( * = ) helloword 5 => > reserved1 < == ident1231asdad 12355")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, MultiplyToken, EqualsToken, RightParenToken, IdentifierToken("helloword"), IntToken(5), ArrowToken, GreaterThanToken, IdentifierToken("reserved1"), LessThanToken, DoubleEqualsToken, IdentifierToken("ident1231asdad"), IntToken(12355))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 671") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( * => ident1231asdad > 5 == = helloword reserved1 12355 < )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, MultiplyToken, ArrowToken, IdentifierToken("ident1231asdad"), GreaterThanToken, IntToken(5), DoubleEqualsToken, EqualsToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), IntToken(12355), LessThanToken, RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 672") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( * 12355 5 ) reserved1 < == => ident1231asdad > helloword =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, MultiplyToken, IntToken(12355), IntToken(5), RightParenToken, IdentifierToken("reserved1"), LessThanToken, DoubleEqualsToken, ArrowToken, IdentifierToken("ident1231asdad"), GreaterThanToken, IdentifierToken("helloword"), EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 673") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( * ident1231asdad => 12355 5 > ) reserved1 == = helloword <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, MultiplyToken, IdentifierToken("ident1231asdad"), ArrowToken, IntToken(12355), IntToken(5), GreaterThanToken, RightParenToken, IdentifierToken("reserved1"), DoubleEqualsToken, EqualsToken, IdentifierToken("helloword"), LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 674") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( ) = == => reserved1 5 helloword ident1231asdad 12355 > < *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, RightParenToken, EqualsToken, DoubleEqualsToken, ArrowToken, IdentifierToken("reserved1"), IntToken(5), IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), IntToken(12355), GreaterThanToken, LessThanToken, MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 675") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( ) => ident1231asdad 5 12355 reserved1 helloword = < == > *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, RightParenToken, ArrowToken, IdentifierToken("ident1231asdad"), IntToken(5), IntToken(12355), IdentifierToken("reserved1"), IdentifierToken("helloword"), EqualsToken, LessThanToken, DoubleEqualsToken, GreaterThanToken, MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 676") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( ) 12355 5 == reserved1 helloword > < = * ident1231asdad =>")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, RightParenToken, IntToken(12355), IntToken(5), DoubleEqualsToken, IdentifierToken("reserved1"), IdentifierToken("helloword"), GreaterThanToken, LessThanToken, EqualsToken, MultiplyToken, IdentifierToken("ident1231asdad"), ArrowToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 677") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( ) ident1231asdad => reserved1 5 * > helloword 12355 < == =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, RightParenToken, IdentifierToken("ident1231asdad"), ArrowToken, IdentifierToken("reserved1"), IntToken(5), MultiplyToken, GreaterThanToken, IdentifierToken("helloword"), IntToken(12355), LessThanToken, DoubleEqualsToken, EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 678") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( = ) == > ident1231asdad => 5 < reserved1 12355 helloword *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, EqualsToken, RightParenToken, DoubleEqualsToken, GreaterThanToken, IdentifierToken("ident1231asdad"), ArrowToken, IntToken(5), LessThanToken, IdentifierToken("reserved1"), IntToken(12355), IdentifierToken("helloword"), MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 679") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( = => ident1231asdad helloword 5 == < 12355 ) * reserved1 >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, EqualsToken, ArrowToken, IdentifierToken("ident1231asdad"), IdentifierToken("helloword"), IntToken(5), DoubleEqualsToken, LessThanToken, IntToken(12355), RightParenToken, MultiplyToken, IdentifierToken("reserved1"), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 680") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( = 12355 5 < ident1231asdad => ) helloword reserved1 > == *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, EqualsToken, IntToken(12355), IntToken(5), LessThanToken, IdentifierToken("ident1231asdad"), ArrowToken, RightParenToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), GreaterThanToken, DoubleEqualsToken, MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 681") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( = ident1231asdad < * reserved1 12355 == => 5 > helloword )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, EqualsToken, IdentifierToken("ident1231asdad"), LessThanToken, MultiplyToken, IdentifierToken("reserved1"), IntToken(12355), DoubleEqualsToken, ArrowToken, IntToken(5), GreaterThanToken, IdentifierToken("helloword"), RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 682") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( == ) = 5 ident1231asdad reserved1 * 12355 < => helloword >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, DoubleEqualsToken, RightParenToken, EqualsToken, IntToken(5), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), MultiplyToken, IntToken(12355), LessThanToken, ArrowToken, IdentifierToken("helloword"), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 683") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( == => helloword ) reserved1 5 ident1231asdad 12355 > < = *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, DoubleEqualsToken, ArrowToken, IdentifierToken("helloword"), RightParenToken, IdentifierToken("reserved1"), IntToken(5), IdentifierToken("ident1231asdad"), IntToken(12355), GreaterThanToken, LessThanToken, EqualsToken, MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 684") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( == 12355 5 reserved1 ident1231asdad helloword > ) => = < *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, DoubleEqualsToken, IntToken(12355), IntToken(5), IdentifierToken("reserved1"), IdentifierToken("ident1231asdad"), IdentifierToken("helloword"), GreaterThanToken, RightParenToken, ArrowToken, EqualsToken, LessThanToken, MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 685") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( == ident1231asdad < = helloword * 5 12355 => ) reserved1 >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, DoubleEqualsToken, IdentifierToken("ident1231asdad"), LessThanToken, EqualsToken, IdentifierToken("helloword"), MultiplyToken, IntToken(5), IntToken(12355), ArrowToken, RightParenToken, IdentifierToken("reserved1"), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 686") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( => ) = ident1231asdad helloword == 12355 reserved1 5 > < *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, ArrowToken, RightParenToken, EqualsToken, IdentifierToken("ident1231asdad"), IdentifierToken("helloword"), DoubleEqualsToken, IntToken(12355), IdentifierToken("reserved1"), IntToken(5), GreaterThanToken, LessThanToken, MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 687") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( => == helloword < ident1231asdad = 12355 ) 5 > reserved1 *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, ArrowToken, DoubleEqualsToken, IdentifierToken("helloword"), LessThanToken, IdentifierToken("ident1231asdad"), EqualsToken, IntToken(12355), RightParenToken, IntToken(5), GreaterThanToken, IdentifierToken("reserved1"), MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 688") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( => 12355 5 helloword ident1231asdad == = > ) * reserved1 <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, ArrowToken, IntToken(12355), IntToken(5), IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), DoubleEqualsToken, EqualsToken, GreaterThanToken, RightParenToken, MultiplyToken, IdentifierToken("reserved1"), LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 689") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( => ident1231asdad < > helloword 12355 ) reserved1 5 == = *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, ArrowToken, IdentifierToken("ident1231asdad"), LessThanToken, GreaterThanToken, IdentifierToken("helloword"), IntToken(12355), RightParenToken, IdentifierToken("reserved1"), IntToken(5), DoubleEqualsToken, EqualsToken, MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 690") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( < ) == * helloword reserved1 => > 5 12355 ident1231asdad =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, LessThanToken, RightParenToken, DoubleEqualsToken, MultiplyToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), ArrowToken, GreaterThanToken, IntToken(5), IntToken(12355), IdentifierToken("ident1231asdad"), EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 691") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( < == helloword 12355 ident1231asdad reserved1 * > = ) 5 =>")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, LessThanToken, DoubleEqualsToken, IdentifierToken("helloword"), IntToken(12355), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), MultiplyToken, GreaterThanToken, EqualsToken, RightParenToken, IntToken(5), ArrowToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 692") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( < 12355 reserved1 ) helloword ident1231asdad 5 > => == = *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, LessThanToken, IntToken(12355), IdentifierToken("reserved1"), RightParenToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), IntToken(5), GreaterThanToken, ArrowToken, DoubleEqualsToken, EqualsToken, MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 693") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( < ident1231asdad => reserved1 * ) helloword == 12355 > 5 =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, LessThanToken, IdentifierToken("ident1231asdad"), ArrowToken, IdentifierToken("reserved1"), MultiplyToken, RightParenToken, IdentifierToken("helloword"), DoubleEqualsToken, IntToken(12355), GreaterThanToken, IntToken(5), EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 694") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( > ) == < * => ident1231asdad reserved1 12355 = helloword 5")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, GreaterThanToken, RightParenToken, DoubleEqualsToken, LessThanToken, MultiplyToken, ArrowToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IntToken(12355), EqualsToken, IdentifierToken("helloword"), IntToken(5))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 695") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( > == helloword ident1231asdad * => 12355 reserved1 5 < = )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, GreaterThanToken, DoubleEqualsToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), MultiplyToken, ArrowToken, IntToken(12355), IdentifierToken("reserved1"), IntToken(5), LessThanToken, EqualsToken, RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 696") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( > 12355 reserved1 => * < 5 = ident1231asdad == helloword )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, GreaterThanToken, IntToken(12355), IdentifierToken("reserved1"), ArrowToken, MultiplyToken, LessThanToken, IntToken(5), EqualsToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken, IdentifierToken("helloword"), RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 697") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( > ident1231asdad < * ) 5 => reserved1 == = helloword 12355")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, GreaterThanToken, IdentifierToken("ident1231asdad"), LessThanToken, MultiplyToken, RightParenToken, IntToken(5), ArrowToken, IdentifierToken("reserved1"), DoubleEqualsToken, EqualsToken, IdentifierToken("helloword"), IntToken(12355))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 698") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( 12355 ) == 5 * ident1231asdad => helloword reserved1 > < =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, IntToken(12355), RightParenToken, DoubleEqualsToken, IntToken(5), MultiplyToken, IdentifierToken("ident1231asdad"), ArrowToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), GreaterThanToken, LessThanToken, EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 699") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( 12355 => * = ) helloword < > reserved1 5 ident1231asdad ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, IntToken(12355), ArrowToken, MultiplyToken, EqualsToken, RightParenToken, IdentifierToken("helloword"), LessThanToken, GreaterThanToken, IdentifierToken("reserved1"), IntToken(5), IdentifierToken("ident1231asdad"), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 700") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( 12355 > reserved1 5 ) * = ident1231asdad => == helloword <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, IntToken(12355), GreaterThanToken, IdentifierToken("reserved1"), IntToken(5), RightParenToken, MultiplyToken, EqualsToken, IdentifierToken("ident1231asdad"), ArrowToken, DoubleEqualsToken, IdentifierToken("helloword"), LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 701") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( 12355 ident1231asdad < = ) * helloword reserved1 5 > => ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, IntToken(12355), IdentifierToken("ident1231asdad"), LessThanToken, EqualsToken, RightParenToken, MultiplyToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), IntToken(5), GreaterThanToken, ArrowToken, DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 702") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( 5 ) == ident1231asdad = => helloword < 12355 > reserved1 *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, IntToken(5), RightParenToken, DoubleEqualsToken, IdentifierToken("ident1231asdad"), EqualsToken, ArrowToken, IdentifierToken("helloword"), LessThanToken, IntToken(12355), GreaterThanToken, IdentifierToken("reserved1"), MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 703") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( 5 => * < = > ident1231asdad reserved1 == ) helloword 12355")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, IntToken(5), ArrowToken, MultiplyToken, LessThanToken, EqualsToken, GreaterThanToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), DoubleEqualsToken, RightParenToken, IdentifierToken("helloword"), IntToken(12355))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 704") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( 5 > reserved1 helloword ) => < ident1231asdad 12355 == = *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, IntToken(5), GreaterThanToken, IdentifierToken("reserved1"), IdentifierToken("helloword"), RightParenToken, ArrowToken, LessThanToken, IdentifierToken("ident1231asdad"), IntToken(12355), DoubleEqualsToken, EqualsToken, MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 705") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( 5 ident1231asdad < => ) 12355 > = reserved1 == helloword *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, IntToken(5), IdentifierToken("ident1231asdad"), LessThanToken, ArrowToken, RightParenToken, IntToken(12355), GreaterThanToken, EqualsToken, IdentifierToken("reserved1"), DoubleEqualsToken, IdentifierToken("helloword"), MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 706") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( reserved1 ) => * == ident1231asdad > 5 < = helloword 12355")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, IdentifierToken("reserved1"), RightParenToken, ArrowToken, MultiplyToken, DoubleEqualsToken, IdentifierToken("ident1231asdad"), GreaterThanToken, IntToken(5), LessThanToken, EqualsToken, IdentifierToken("helloword"), IntToken(12355))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 707") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( reserved1 => * 12355 = helloword == ident1231asdad 5 > < )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, IdentifierToken("reserved1"), ArrowToken, MultiplyToken, IntToken(12355), EqualsToken, IdentifierToken("helloword"), DoubleEqualsToken, IdentifierToken("ident1231asdad"), IntToken(5), GreaterThanToken, LessThanToken, RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 708") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( reserved1 > ident1231asdad ) == * => < 5 12355 helloword =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, IdentifierToken("reserved1"), GreaterThanToken, IdentifierToken("ident1231asdad"), RightParenToken, DoubleEqualsToken, MultiplyToken, ArrowToken, LessThanToken, IntToken(5), IntToken(12355), IdentifierToken("helloword"), EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 709") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( reserved1 ident1231asdad < 12355 = ) * 5 => == helloword >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, IdentifierToken("reserved1"), IdentifierToken("ident1231asdad"), LessThanToken, IntToken(12355), EqualsToken, RightParenToken, MultiplyToken, IntToken(5), ArrowToken, DoubleEqualsToken, IdentifierToken("helloword"), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 710") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( ident1231asdad ) => == < = helloword reserved1 5 12355 > *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, IdentifierToken("ident1231asdad"), RightParenToken, ArrowToken, DoubleEqualsToken, LessThanToken, EqualsToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), IntToken(5), IntToken(12355), GreaterThanToken, MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 711") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( ident1231asdad => * reserved1 == < helloword = 12355 > 5 )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, IdentifierToken("ident1231asdad"), ArrowToken, MultiplyToken, IdentifierToken("reserved1"), DoubleEqualsToken, LessThanToken, IdentifierToken("helloword"), EqualsToken, IntToken(12355), GreaterThanToken, IntToken(5), RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 712") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( ident1231asdad > reserved1 == = < 5 12355 ) * helloword =>")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, IdentifierToken("ident1231asdad"), GreaterThanToken, IdentifierToken("reserved1"), DoubleEqualsToken, EqualsToken, LessThanToken, IntToken(5), IntToken(12355), RightParenToken, MultiplyToken, IdentifierToken("helloword"), ArrowToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 713") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( ident1231asdad reserved1 < helloword = > => 5 12355 == ) *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), LessThanToken, IdentifierToken("helloword"), EqualsToken, GreaterThanToken, ArrowToken, IntToken(5), IntToken(12355), DoubleEqualsToken, RightParenToken, MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 714") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( helloword ) => > == reserved1 12355 = 5 < ident1231asdad *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, IdentifierToken("helloword"), RightParenToken, ArrowToken, GreaterThanToken, DoubleEqualsToken, IdentifierToken("reserved1"), IntToken(12355), EqualsToken, IntToken(5), LessThanToken, IdentifierToken("ident1231asdad"), MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 715") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( helloword => ) * < ident1231asdad > 5 == = reserved1 12355")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, IdentifierToken("helloword"), ArrowToken, RightParenToken, MultiplyToken, LessThanToken, IdentifierToken("ident1231asdad"), GreaterThanToken, IntToken(5), DoubleEqualsToken, EqualsToken, IdentifierToken("reserved1"), IntToken(12355))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 716") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( helloword > reserved1 < == * = ident1231asdad 5 12355 => )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, IdentifierToken("helloword"), GreaterThanToken, IdentifierToken("reserved1"), LessThanToken, DoubleEqualsToken, MultiplyToken, EqualsToken, IdentifierToken("ident1231asdad"), IntToken(5), IntToken(12355), ArrowToken, RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 717") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ( helloword reserved1 > ) => = == < 5 12355 ident1231asdad *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, LeftParenToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), GreaterThanToken, RightParenToken, ArrowToken, EqualsToken, DoubleEqualsToken, LessThanToken, IntToken(5), IntToken(12355), IdentifierToken("ident1231asdad"), MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 718") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) * = < reserved1 > => ( ident1231asdad 12355 == helloword 5")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, MultiplyToken, EqualsToken, LessThanToken, IdentifierToken("reserved1"), GreaterThanToken, ArrowToken, LeftParenToken, IdentifierToken("ident1231asdad"), IntToken(12355), DoubleEqualsToken, IdentifierToken("helloword"), IntToken(5))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 719") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) * < = => 12355 > helloword ident1231asdad reserved1 5 == (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, MultiplyToken, LessThanToken, EqualsToken, ArrowToken, IntToken(12355), GreaterThanToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IntToken(5), DoubleEqualsToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 720") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) * 12355 ident1231asdad reserved1 => < helloword = > == 5 (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, MultiplyToken, IntToken(12355), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), ArrowToken, LessThanToken, IdentifierToken("helloword"), EqualsToken, GreaterThanToken, DoubleEqualsToken, IntToken(5), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 721") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) * ident1231asdad 12355 => < 5 reserved1 > = ( helloword ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, MultiplyToken, IdentifierToken("ident1231asdad"), IntToken(12355), ArrowToken, LessThanToken, IntToken(5), IdentifierToken("reserved1"), GreaterThanToken, EqualsToken, LeftParenToken, IdentifierToken("helloword"), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 722") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) ( = < helloword > reserved1 12355 ident1231asdad 5 => == *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, LeftParenToken, EqualsToken, LessThanToken, IdentifierToken("helloword"), GreaterThanToken, IdentifierToken("reserved1"), IntToken(12355), IdentifierToken("ident1231asdad"), IntToken(5), ArrowToken, DoubleEqualsToken, MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 723") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) ( < = 12355 > helloword 5 == reserved1 => ident1231asdad *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, LeftParenToken, LessThanToken, EqualsToken, IntToken(12355), GreaterThanToken, IdentifierToken("helloword"), IntToken(5), DoubleEqualsToken, IdentifierToken("reserved1"), ArrowToken, IdentifierToken("ident1231asdad"), MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 724") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) ( 12355 helloword * > = < reserved1 => == ident1231asdad 5")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, LeftParenToken, IntToken(12355), IdentifierToken("helloword"), MultiplyToken, GreaterThanToken, EqualsToken, LessThanToken, IdentifierToken("reserved1"), ArrowToken, DoubleEqualsToken, IdentifierToken("ident1231asdad"), IntToken(5))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 725") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) ( ident1231asdad 12355 > < = == helloword reserved1 5 => *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, LeftParenToken, IdentifierToken("ident1231asdad"), IntToken(12355), GreaterThanToken, LessThanToken, EqualsToken, DoubleEqualsToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), IntToken(5), ArrowToken, MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 726") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) = ( > == 5 < => 12355 ident1231asdad reserved1 helloword *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, EqualsToken, LeftParenToken, GreaterThanToken, DoubleEqualsToken, IntToken(5), LessThanToken, ArrowToken, IntToken(12355), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IdentifierToken("helloword"), MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 727") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) = < ( reserved1 12355 > * ident1231asdad => == helloword 5")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, EqualsToken, LessThanToken, LeftParenToken, IdentifierToken("reserved1"), IntToken(12355), GreaterThanToken, MultiplyToken, IdentifierToken("ident1231asdad"), ArrowToken, DoubleEqualsToken, IdentifierToken("helloword"), IntToken(5))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 728") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) = 12355 helloword == > < ident1231asdad reserved1 5 => ( *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, EqualsToken, IntToken(12355), IdentifierToken("helloword"), DoubleEqualsToken, GreaterThanToken, LessThanToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IntToken(5), ArrowToken, LeftParenToken, MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 729") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) = ident1231asdad 12355 reserved1 < > helloword ( => == 5 *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, EqualsToken, IdentifierToken("ident1231asdad"), IntToken(12355), IdentifierToken("reserved1"), LessThanToken, GreaterThanToken, IdentifierToken("helloword"), LeftParenToken, ArrowToken, DoubleEqualsToken, IntToken(5), MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 730") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) == ( > < 5 ident1231asdad reserved1 12355 = * helloword =>")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, DoubleEqualsToken, LeftParenToken, GreaterThanToken, LessThanToken, IntToken(5), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IntToken(12355), EqualsToken, MultiplyToken, IdentifierToken("helloword"), ArrowToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 731") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) == < ( helloword 12355 ident1231asdad > reserved1 5 => = *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, DoubleEqualsToken, LessThanToken, LeftParenToken, IdentifierToken("helloword"), IntToken(12355), IdentifierToken("ident1231asdad"), GreaterThanToken, IdentifierToken("reserved1"), IntToken(5), ArrowToken, EqualsToken, MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 732") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) == 12355 helloword < 5 * > = reserved1 => ident1231asdad (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, DoubleEqualsToken, IntToken(12355), IdentifierToken("helloword"), LessThanToken, IntToken(5), MultiplyToken, GreaterThanToken, EqualsToken, IdentifierToken("reserved1"), ArrowToken, IdentifierToken("ident1231asdad"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 733") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) == ident1231asdad 5 * 12355 = < reserved1 => ( helloword >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, DoubleEqualsToken, IdentifierToken("ident1231asdad"), IntToken(5), MultiplyToken, IntToken(12355), EqualsToken, LessThanToken, IdentifierToken("reserved1"), ArrowToken, LeftParenToken, IdentifierToken("helloword"), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 734") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) => ( > 5 reserved1 == = helloword ident1231asdad 12355 < *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, ArrowToken, LeftParenToken, GreaterThanToken, IntToken(5), IdentifierToken("reserved1"), DoubleEqualsToken, EqualsToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), IntToken(12355), LessThanToken, MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 735") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) => < = ( reserved1 12355 == > ident1231asdad 5 helloword *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, ArrowToken, LessThanToken, EqualsToken, LeftParenToken, IdentifierToken("reserved1"), IntToken(12355), DoubleEqualsToken, GreaterThanToken, IdentifierToken("ident1231asdad"), IntToken(5), IdentifierToken("helloword"), MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 736") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) => 12355 helloword 5 > < * reserved1 = ( ident1231asdad ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, ArrowToken, IntToken(12355), IdentifierToken("helloword"), IntToken(5), GreaterThanToken, LessThanToken, MultiplyToken, IdentifierToken("reserved1"), EqualsToken, LeftParenToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 737") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) => ident1231asdad 5 = 12355 > helloword reserved1 < == ( *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, ArrowToken, IdentifierToken("ident1231asdad"), IntToken(5), EqualsToken, IntToken(12355), GreaterThanToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), LessThanToken, DoubleEqualsToken, LeftParenToken, MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 738") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) < ( > ident1231asdad 5 reserved1 helloword = => == 12355 *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, LessThanToken, LeftParenToken, GreaterThanToken, IdentifierToken("ident1231asdad"), IntToken(5), IdentifierToken("reserved1"), IdentifierToken("helloword"), EqualsToken, ArrowToken, DoubleEqualsToken, IntToken(12355), MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 739") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) < => = > reserved1 helloword 5 12355 ( * ident1231asdad ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, LessThanToken, ArrowToken, EqualsToken, GreaterThanToken, IdentifierToken("reserved1"), IdentifierToken("helloword"), IntToken(5), IntToken(12355), LeftParenToken, MultiplyToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 740") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) < 12355 helloword ident1231asdad 5 * => reserved1 > == = (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, LessThanToken, IntToken(12355), IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), IntToken(5), MultiplyToken, ArrowToken, IdentifierToken("reserved1"), GreaterThanToken, DoubleEqualsToken, EqualsToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 741") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) < ident1231asdad 5 => reserved1 ( > = 12355 == helloword *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, LessThanToken, IdentifierToken("ident1231asdad"), IntToken(5), ArrowToken, IdentifierToken("reserved1"), LeftParenToken, GreaterThanToken, EqualsToken, IntToken(12355), DoubleEqualsToken, IdentifierToken("helloword"), MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 742") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) > ( 12355 * ident1231asdad => < reserved1 == = helloword 5")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, GreaterThanToken, LeftParenToken, IntToken(12355), MultiplyToken, IdentifierToken("ident1231asdad"), ArrowToken, LessThanToken, IdentifierToken("reserved1"), DoubleEqualsToken, EqualsToken, IdentifierToken("helloword"), IntToken(5))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 743") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) > => = 5 ident1231asdad < ( helloword reserved1 12355 == *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, GreaterThanToken, ArrowToken, EqualsToken, IntToken(5), IdentifierToken("ident1231asdad"), LessThanToken, LeftParenToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), IntToken(12355), DoubleEqualsToken, MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 744") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) > 5 * = ident1231asdad 12355 == => reserved1 < helloword (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, GreaterThanToken, IntToken(5), MultiplyToken, EqualsToken, IdentifierToken("ident1231asdad"), IntToken(12355), DoubleEqualsToken, ArrowToken, IdentifierToken("reserved1"), LessThanToken, IdentifierToken("helloword"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 745") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) > ident1231asdad 5 12355 reserved1 < * => = ( helloword ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, GreaterThanToken, IdentifierToken("ident1231asdad"), IntToken(5), IntToken(12355), IdentifierToken("reserved1"), LessThanToken, MultiplyToken, ArrowToken, EqualsToken, LeftParenToken, IdentifierToken("helloword"), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 746") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) 12355 ( > == ident1231asdad reserved1 helloword 5 < => = *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, IntToken(12355), LeftParenToken, GreaterThanToken, DoubleEqualsToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IdentifierToken("helloword"), IntToken(5), LessThanToken, ArrowToken, EqualsToken, MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 747") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) 12355 => = ident1231asdad reserved1 helloword 5 ( < == > *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, IntToken(12355), ArrowToken, EqualsToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IdentifierToken("helloword"), IntToken(5), LeftParenToken, LessThanToken, DoubleEqualsToken, GreaterThanToken, MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 748") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) 12355 5 * => helloword ( reserved1 > == = ident1231asdad <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, IntToken(12355), IntToken(5), MultiplyToken, ArrowToken, IdentifierToken("helloword"), LeftParenToken, IdentifierToken("reserved1"), GreaterThanToken, DoubleEqualsToken, EqualsToken, IdentifierToken("ident1231asdad"), LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 749") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) 12355 ident1231asdad 5 helloword reserved1 ( => > < == = *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, IntToken(12355), IdentifierToken("ident1231asdad"), IntToken(5), IdentifierToken("helloword"), IdentifierToken("reserved1"), LeftParenToken, ArrowToken, GreaterThanToken, LessThanToken, DoubleEqualsToken, EqualsToken, MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 750") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) 5 ( > < helloword == 12355 = reserved1 => ident1231asdad *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, IntToken(5), LeftParenToken, GreaterThanToken, LessThanToken, IdentifierToken("helloword"), DoubleEqualsToken, IntToken(12355), EqualsToken, IdentifierToken("reserved1"), ArrowToken, IdentifierToken("ident1231asdad"), MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 751") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) 5 => == * helloword > < reserved1 = ( ident1231asdad 12355")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, IntToken(5), ArrowToken, DoubleEqualsToken, MultiplyToken, IdentifierToken("helloword"), GreaterThanToken, LessThanToken, IdentifierToken("reserved1"), EqualsToken, LeftParenToken, IdentifierToken("ident1231asdad"), IntToken(12355))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 752") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) 5 12355 * > helloword < = ident1231asdad reserved1 => == (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, IntToken(5), IntToken(12355), MultiplyToken, GreaterThanToken, IdentifierToken("helloword"), LessThanToken, EqualsToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), ArrowToken, DoubleEqualsToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 753") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) 5 ident1231asdad reserved1 ( helloword > = == < => 12355 *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, IntToken(5), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), LeftParenToken, IdentifierToken("helloword"), GreaterThanToken, EqualsToken, DoubleEqualsToken, LessThanToken, ArrowToken, IntToken(12355), MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 754") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) reserved1 ( > 5 helloword ident1231asdad * < == = 12355 =>")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, IdentifierToken("reserved1"), LeftParenToken, GreaterThanToken, IntToken(5), IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), MultiplyToken, LessThanToken, DoubleEqualsToken, EqualsToken, IntToken(12355), ArrowToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 755") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) reserved1 => == = helloword ident1231asdad 5 12355 > < ( *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, IdentifierToken("reserved1"), ArrowToken, DoubleEqualsToken, EqualsToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), IntToken(5), IntToken(12355), GreaterThanToken, LessThanToken, LeftParenToken, MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 756") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) reserved1 12355 * helloword ( = ident1231asdad => > < 5 ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, IdentifierToken("reserved1"), IntToken(12355), MultiplyToken, IdentifierToken("helloword"), LeftParenToken, EqualsToken, IdentifierToken("ident1231asdad"), ArrowToken, GreaterThanToken, LessThanToken, IntToken(5), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 757") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) reserved1 ident1231asdad 5 => * = 12355 > == ( helloword <")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, IdentifierToken("reserved1"), IdentifierToken("ident1231asdad"), IntToken(5), ArrowToken, MultiplyToken, EqualsToken, IntToken(12355), GreaterThanToken, DoubleEqualsToken, LeftParenToken, IdentifierToken("helloword"), LessThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 758") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) ident1231asdad ( 12355 * = < 5 helloword reserved1 > => ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, IdentifierToken("ident1231asdad"), LeftParenToken, IntToken(12355), MultiplyToken, EqualsToken, LessThanToken, IntToken(5), IdentifierToken("helloword"), IdentifierToken("reserved1"), GreaterThanToken, ArrowToken, DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 759") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) ident1231asdad => == 12355 * > 5 = reserved1 < helloword (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, IdentifierToken("ident1231asdad"), ArrowToken, DoubleEqualsToken, IntToken(12355), MultiplyToken, GreaterThanToken, IntToken(5), EqualsToken, IdentifierToken("reserved1"), LessThanToken, IdentifierToken("helloword"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 760") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) ident1231asdad 12355 ( = * 5 < reserved1 => == helloword >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, IdentifierToken("ident1231asdad"), IntToken(12355), LeftParenToken, EqualsToken, MultiplyToken, IntToken(5), LessThanToken, IdentifierToken("reserved1"), ArrowToken, DoubleEqualsToken, IdentifierToken("helloword"), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 761") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) ident1231asdad reserved1 5 > * 12355 = helloword < => == (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IntToken(5), GreaterThanToken, MultiplyToken, IntToken(12355), EqualsToken, IdentifierToken("helloword"), LessThanToken, ArrowToken, DoubleEqualsToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 762") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) helloword ( 12355 == * ident1231asdad => < 5 > reserved1 =")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, IdentifierToken("helloword"), LeftParenToken, IntToken(12355), DoubleEqualsToken, MultiplyToken, IdentifierToken("ident1231asdad"), ArrowToken, LessThanToken, IntToken(5), GreaterThanToken, IdentifierToken("reserved1"), EqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 763") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) helloword => == reserved1 ( * = 5 > < ident1231asdad 12355")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, IdentifierToken("helloword"), ArrowToken, DoubleEqualsToken, IdentifierToken("reserved1"), LeftParenToken, MultiplyToken, EqualsToken, IntToken(5), GreaterThanToken, LessThanToken, IdentifierToken("ident1231asdad"), IntToken(12355))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 764") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) helloword 12355 ( => = * ident1231asdad reserved1 5 > < ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, IdentifierToken("helloword"), IntToken(12355), LeftParenToken, ArrowToken, EqualsToken, MultiplyToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IntToken(5), GreaterThanToken, LessThanToken, DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 765") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / ) helloword reserved1 5 ident1231asdad ( = 12355 == < => > *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, RightParenToken, IdentifierToken("helloword"), IdentifierToken("reserved1"), IntToken(5), IdentifierToken("ident1231asdad"), LeftParenToken, EqualsToken, IntToken(12355), DoubleEqualsToken, LessThanToken, ArrowToken, GreaterThanToken, MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 766") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / = * ) 5 > == < ident1231asdad reserved1 => ( helloword 12355")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, EqualsToken, MultiplyToken, RightParenToken, IntToken(5), GreaterThanToken, DoubleEqualsToken, LessThanToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), ArrowToken, LeftParenToken, IdentifierToken("helloword"), IntToken(12355))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 767") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / = * < > ( == 5 reserved1 helloword ident1231asdad 12355 => )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, EqualsToken, MultiplyToken, LessThanToken, GreaterThanToken, LeftParenToken, DoubleEqualsToken, IntToken(5), IdentifierToken("reserved1"), IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), IntToken(12355), ArrowToken, RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 768") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / = * 5 ) 12355 == reserved1 > => ident1231asdad < helloword (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, EqualsToken, MultiplyToken, IntToken(5), RightParenToken, IntToken(12355), DoubleEqualsToken, IdentifierToken("reserved1"), GreaterThanToken, ArrowToken, IdentifierToken("ident1231asdad"), LessThanToken, IdentifierToken("helloword"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 769") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / = * ident1231asdad helloword ) == 5 < 12355 => ( reserved1 >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, EqualsToken, MultiplyToken, IdentifierToken("ident1231asdad"), IdentifierToken("helloword"), RightParenToken, DoubleEqualsToken, IntToken(5), LessThanToken, IntToken(12355), ArrowToken, LeftParenToken, IdentifierToken("reserved1"), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 770") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / = ( ) 5 reserved1 == helloword => ident1231asdad 12355 > < *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, EqualsToken, LeftParenToken, RightParenToken, IntToken(5), IdentifierToken("reserved1"), DoubleEqualsToken, IdentifierToken("helloword"), ArrowToken, IdentifierToken("ident1231asdad"), IntToken(12355), GreaterThanToken, LessThanToken, MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 771") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / = ( < > == => * 12355 5 ident1231asdad reserved1 helloword )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, EqualsToken, LeftParenToken, LessThanToken, GreaterThanToken, DoubleEqualsToken, ArrowToken, MultiplyToken, IntToken(12355), IntToken(5), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IdentifierToken("helloword"), RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 772") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / = ( 5 ) ident1231asdad => == * reserved1 > < helloword 12355")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, EqualsToken, LeftParenToken, IntToken(5), RightParenToken, IdentifierToken("ident1231asdad"), ArrowToken, DoubleEqualsToken, MultiplyToken, IdentifierToken("reserved1"), GreaterThanToken, LessThanToken, IdentifierToken("helloword"), IntToken(12355))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 773") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / = ( ident1231asdad helloword => == ) reserved1 5 12355 > < *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, EqualsToken, LeftParenToken, IdentifierToken("ident1231asdad"), IdentifierToken("helloword"), ArrowToken, DoubleEqualsToken, RightParenToken, IdentifierToken("reserved1"), IntToken(5), IntToken(12355), GreaterThanToken, LessThanToken, MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 774") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / = ) ( 5 helloword => < ident1231asdad == 12355 > reserved1 *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, EqualsToken, RightParenToken, LeftParenToken, IntToken(5), IdentifierToken("helloword"), ArrowToken, LessThanToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken, IntToken(12355), GreaterThanToken, IdentifierToken("reserved1"), MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 775") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / = ) < > 12355 == 5 ident1231asdad reserved1 ( * helloword =>")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, EqualsToken, RightParenToken, LessThanToken, GreaterThanToken, IntToken(12355), DoubleEqualsToken, IntToken(5), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), LeftParenToken, MultiplyToken, IdentifierToken("helloword"), ArrowToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 776") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / = ) 5 == * < reserved1 12355 helloword ident1231asdad > => (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, EqualsToken, RightParenToken, IntToken(5), DoubleEqualsToken, MultiplyToken, LessThanToken, IdentifierToken("reserved1"), IntToken(12355), IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), GreaterThanToken, ArrowToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 777") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / = ) ident1231asdad helloword > == 5 < ( 12355 => reserved1 *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, EqualsToken, RightParenToken, IdentifierToken("ident1231asdad"), IdentifierToken("helloword"), GreaterThanToken, DoubleEqualsToken, IntToken(5), LessThanToken, LeftParenToken, IntToken(12355), ArrowToken, IdentifierToken("reserved1"), MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 778") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / = == ( reserved1 ) < helloword > 5 => * ident1231asdad 12355")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, EqualsToken, DoubleEqualsToken, LeftParenToken, IdentifierToken("reserved1"), RightParenToken, LessThanToken, IdentifierToken("helloword"), GreaterThanToken, IntToken(5), ArrowToken, MultiplyToken, IdentifierToken("ident1231asdad"), IntToken(12355))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 779") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / = == < > reserved1 => * ) helloword ident1231asdad 5 12355 (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, EqualsToken, DoubleEqualsToken, LessThanToken, GreaterThanToken, IdentifierToken("reserved1"), ArrowToken, MultiplyToken, RightParenToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), IntToken(5), IntToken(12355), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 780") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / = == 5 ) => > ( < 12355 ident1231asdad reserved1 helloword *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, EqualsToken, DoubleEqualsToken, IntToken(5), RightParenToken, ArrowToken, GreaterThanToken, LeftParenToken, LessThanToken, IntToken(12355), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), IdentifierToken("helloword"), MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 781") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / = == ident1231asdad helloword 5 => ) * 12355 < ( reserved1 >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, EqualsToken, DoubleEqualsToken, IdentifierToken("ident1231asdad"), IdentifierToken("helloword"), IntToken(5), ArrowToken, RightParenToken, MultiplyToken, IntToken(12355), LessThanToken, LeftParenToken, IdentifierToken("reserved1"), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 782") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / = => ( reserved1 < > == helloword ident1231asdad 5 12355 ) *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, EqualsToken, ArrowToken, LeftParenToken, IdentifierToken("reserved1"), LessThanToken, GreaterThanToken, DoubleEqualsToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), IntToken(5), IntToken(12355), RightParenToken, MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 783") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / = => < > helloword == 12355 ident1231asdad ( 5 ) reserved1 *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, EqualsToken, ArrowToken, LessThanToken, GreaterThanToken, IdentifierToken("helloword"), DoubleEqualsToken, IntToken(12355), IdentifierToken("ident1231asdad"), LeftParenToken, IntToken(5), RightParenToken, IdentifierToken("reserved1"), MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 784") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / = => 5 ) > < reserved1 ident1231asdad 12355 ( * helloword ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, EqualsToken, ArrowToken, IntToken(5), RightParenToken, GreaterThanToken, LessThanToken, IdentifierToken("reserved1"), IdentifierToken("ident1231asdad"), IntToken(12355), LeftParenToken, MultiplyToken, IdentifierToken("helloword"), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 785") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / = => helloword * ( > reserved1 12355 ident1231asdad 5 < == )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, EqualsToken, ArrowToken, IdentifierToken("helloword"), MultiplyToken, LeftParenToken, GreaterThanToken, IdentifierToken("reserved1"), IntToken(12355), IdentifierToken("ident1231asdad"), IntToken(5), LessThanToken, DoubleEqualsToken, RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 786") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / = < ( reserved1 12355 => helloword > ) 5 == ident1231asdad *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, EqualsToken, LessThanToken, LeftParenToken, IdentifierToken("reserved1"), IntToken(12355), ArrowToken, IdentifierToken("helloword"), GreaterThanToken, RightParenToken, IntToken(5), DoubleEqualsToken, IdentifierToken("ident1231asdad"), MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 787") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / = < => 12355 ( 5 * > ident1231asdad == ) helloword reserved1")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, EqualsToken, LessThanToken, ArrowToken, IntToken(12355), LeftParenToken, IntToken(5), MultiplyToken, GreaterThanToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken, RightParenToken, IdentifierToken("helloword"), IdentifierToken("reserved1"))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 788") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / = < 5 ) reserved1 > ( == helloword ident1231asdad 12355 => *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, EqualsToken, LessThanToken, IntToken(5), RightParenToken, IdentifierToken("reserved1"), GreaterThanToken, LeftParenToken, DoubleEqualsToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), IntToken(12355), ArrowToken, MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 789") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / = < helloword * == 12355 => ) > reserved1 5 ident1231asdad (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, EqualsToken, LessThanToken, IdentifierToken("helloword"), MultiplyToken, DoubleEqualsToken, IntToken(12355), ArrowToken, RightParenToken, GreaterThanToken, IdentifierToken("reserved1"), IntToken(5), IdentifierToken("ident1231asdad"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 790") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / = > ( reserved1 ident1231asdad < => * 5 == ) helloword 12355")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, EqualsToken, GreaterThanToken, LeftParenToken, IdentifierToken("reserved1"), IdentifierToken("ident1231asdad"), LessThanToken, ArrowToken, MultiplyToken, IntToken(5), DoubleEqualsToken, RightParenToken, IdentifierToken("helloword"), IntToken(12355))
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 791") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / = > => 12355 == 5 < helloword ident1231asdad reserved1 ) ( *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, EqualsToken, GreaterThanToken, ArrowToken, IntToken(12355), DoubleEqualsToken, IntToken(5), LessThanToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), RightParenToken, LeftParenToken, MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 792") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / = > 5 ) helloword < 12355 ident1231asdad ( => == reserved1 *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, EqualsToken, GreaterThanToken, IntToken(5), RightParenToken, IdentifierToken("helloword"), LessThanToken, IntToken(12355), IdentifierToken("ident1231asdad"), LeftParenToken, ArrowToken, DoubleEqualsToken, IdentifierToken("reserved1"), MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 793") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / = > helloword * < 12355 reserved1 5 => ) ( ident1231asdad ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, EqualsToken, GreaterThanToken, IdentifierToken("helloword"), MultiplyToken, LessThanToken, IntToken(12355), IdentifierToken("reserved1"), IntToken(5), ArrowToken, RightParenToken, LeftParenToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 794") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / = 12355 ( ident1231asdad * > helloword < reserved1 5 => == )")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, EqualsToken, IntToken(12355), LeftParenToken, IdentifierToken("ident1231asdad"), MultiplyToken, GreaterThanToken, IdentifierToken("helloword"), LessThanToken, IdentifierToken("reserved1"), IntToken(5), ArrowToken, DoubleEqualsToken, RightParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 795") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / = 12355 => > 5 reserved1 * < ) ident1231asdad == helloword (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, EqualsToken, IntToken(12355), ArrowToken, GreaterThanToken, IntToken(5), IdentifierToken("reserved1"), MultiplyToken, LessThanToken, RightParenToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken, IdentifierToken("helloword"), LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 796") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / = 12355 5 == ( reserved1 ) < ident1231asdad => * helloword >")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, EqualsToken, IntToken(12355), IntToken(5), DoubleEqualsToken, LeftParenToken, IdentifierToken("reserved1"), RightParenToken, LessThanToken, IdentifierToken("ident1231asdad"), ArrowToken, MultiplyToken, IdentifierToken("helloword"), GreaterThanToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 797") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / = 12355 helloword * 5 > == ) ident1231asdad reserved1 < => (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, EqualsToken, IntToken(12355), IdentifierToken("helloword"), MultiplyToken, IntToken(5), GreaterThanToken, DoubleEqualsToken, RightParenToken, IdentifierToken("ident1231asdad"), IdentifierToken("reserved1"), LessThanToken, ArrowToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 798") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / = 5 ( ident1231asdad == 12355 < ) => reserved1 > helloword *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, EqualsToken, IntToken(5), LeftParenToken, IdentifierToken("ident1231asdad"), DoubleEqualsToken, IntToken(12355), LessThanToken, RightParenToken, ArrowToken, IdentifierToken("reserved1"), GreaterThanToken, IdentifierToken("helloword"), MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 799") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / = 5 => > ident1231asdad 12355 < * reserved1 ) ( helloword ==")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, EqualsToken, IntToken(5), ArrowToken, GreaterThanToken, IdentifierToken("ident1231asdad"), IntToken(12355), LessThanToken, MultiplyToken, IdentifierToken("reserved1"), RightParenToken, LeftParenToken, IdentifierToken("helloword"), DoubleEqualsToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 800") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / = 5 12355 == => reserved1 > helloword ident1231asdad < ) ( *")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, EqualsToken, IntToken(5), IntToken(12355), DoubleEqualsToken, ArrowToken, IdentifierToken("reserved1"), GreaterThanToken, IdentifierToken("helloword"), IdentifierToken("ident1231asdad"), LessThanToken, RightParenToken, LeftParenToken, MultiplyToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Mixed Tokens 801") {
        val obtained = TokenizerTools.Tokenizer.tokenize("assert choice return block def var + - / = 5 helloword * ident1231asdad > 12355 reserved1 ) => == < (")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, EqualsToken, IntToken(5), IdentifierToken("helloword"), MultiplyToken, IdentifierToken("ident1231asdad"), GreaterThanToken, IntToken(12355), IdentifierToken("reserved1"), RightParenToken, ArrowToken, DoubleEqualsToken, LessThanToken, LeftParenToken)
        assertEquals(obtained, expected)
    }


    test("Tokenizer should throw TokenizationException for non Tokenizable inputs") {
        val invalidInput = "hello 124.23 return invalidToken =1"
        val exception = interceptMessage[TokenizationException]("Unrecognized token") {
            Tokenizer.tokenize(invalidInput)
        }
    }

    test("Tokenizer should throw TokenizationException for numbers directly followed by charecters") {
        val invalidInput = "hello 12423 retur2n=1invalidToken =1"
        val exception = interceptMessage[TokenizationException]("Numbers cannot directly be followed by Charecters") {
            Tokenizer.tokenize(invalidInput)
        }
    }

    test("Testing Tokenization of Mixed Tokens with mixed spaces") {
        val obtained = TokenizerTools.Tokenizer.tokenize("     assert    choice   return block          def var+-       / =     5              helloword*ident1231asdad      > 12355      reserved1)  =>  ==   <    (                                  ")
        val expected = List(AssertToken, ChoiceToken, ReturnToken, BlockToken, FuncDefToken, VarDefToken, PlusToken, MinusToken, DivideToken, EqualsToken, IntToken(5), IdentifierToken("helloword"), MultiplyToken, IdentifierToken("ident1231asdad"), GreaterThanToken, IntToken(12355), IdentifierToken("reserved1"), RightParenToken, ArrowToken, DoubleEqualsToken, LessThanToken, LeftParenToken)
        assertEquals(obtained, expected)
    }

    test("Testing Tokenization of Empty Input") {
        val obtained = TokenizerTools.Tokenizer.tokenize("")
        val expected = List()
        assertEquals(obtained, expected)
    }
    
    test("Testing Tokenization of just Spaces") {
        val obtained = TokenizerTools.Tokenizer.tokenize("                                          ")
        val expected = List()
        assertEquals(obtained, expected)
    }
}