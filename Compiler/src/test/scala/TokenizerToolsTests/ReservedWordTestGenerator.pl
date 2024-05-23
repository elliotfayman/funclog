% Define a comprehensive list of different token types
tokens([
    reserved('assert'), reserved('choice'), reserved('return'),  reserved('block'),  reserved('def'),  reserved('var'), % Reserved Words
    op('+'), op('-'), op('*'), op('/'), op('('), op(')'), op('='), op('=='), op('=>'), op('<'), op('>'), % Operators
    intLit(12355), intLit(5), % Integer Literals
    identifier('reserved1'), identifier('ident1231asdad'), identifier('helloword') % Identifiers
]).

% Initialize a counter for generating test numbers
:- dynamic test_counter/1.
test_counter(0).

% Increment the test counter
increment_test_counter :-
    test_counter(Count),
    NewCount is Count + 1,
    retract(test_counter(Count)),
    assert(test_counter(NewCount)).

% Helper to write out the Scala test function
write_test_function(Stream, Permutation, Count) :-
    maplist(token_to_string, Permutation, TokenStrings),
    atomics_to_string(TokenStrings, " ", TokenizeInput),
    maplist(token_to_scala_representation, Permutation, ScalaTokens),
    atomic_list_concat(ScalaTokens, ', ', ScalaTokensString),
    format(Stream, 'test("Testing Tokenization of Mixed Tokens ~d") {~n', [Count]),
    format(Stream, '    val obtained = TokenizerTools.Tokenizer.tokenize("~w")~n', [TokenizeInput]),
    format(Stream, '    val expected = List(~w)~n', [ScalaTokensString]),
    format(Stream, '    assertEquals(obtained, expected)~n', []),
    format(Stream, '}~n~n', []).

% Convert a token to its string representation for tokenize input
token_to_string(reserved(Word), Word).
token_to_string(op(Op), Op).
token_to_string(intLit(Int), String) :- number_string(Int, String).
token_to_string(identifier(Id), Id).

% Map tokens to their Scala token representation
token_to_scala_representation(reserved(Word), ScalaReserved) :- reserved_to_scala(Word, ScalaReserved).
token_to_scala_representation(op(Op), ScalaOp) :- op_to_scala(Op, ScalaOp).
token_to_scala_representation(intLit(Int), ScalaIntLit) :- format(atom(ScalaIntLit), 'IntToken(~d)', [Int]).
token_to_scala_representation(identifier(Id), ScalaId) :- format(atom(ScalaId), 'IdentifierToken("~w")', [Id]).

% Helper to map reserved words to their Scala token representation
reserved_to_scala('assert', 'AssertToken').
reserved_to_scala('choice', 'ChoiceToken').
reserved_to_scala('return', 'ReturnToken').
reserved_to_scala('block', 'BlockToken').
reserved_to_scala('def', 'FuncDefToken').
reserved_to_scala('var', 'VarDefToken').

% Helper to map operators to their Scala token representation
op_to_scala('+', 'PlusToken').
op_to_scala('-', 'MinusToken').
op_to_scala('*', 'MultiplyToken').
op_to_scala('/', 'DivideToken').
op_to_scala('(', 'LeftParenToken').
op_to_scala(')', 'RightParenToken').
op_to_scala('=', 'EqualsToken').
op_to_scala('==', 'DoubleEqualsToken').
op_to_scala('=>', 'ArrowToken').
op_to_scala('<', 'LessThanToken').
op_to_scala('>', 'GreaterThanToken').

% Adjusted generate_all_tests to handle on-the-fly generation and selective writing
generate_all_tests :-
    retractall(test_counter(_)),
    assert(test_counter(0)),
    open('generated_tests.scala', write, Stream),
    tokens(Tokens),
    SkipInterval is 10000000, % Define how often to write a test case
    findnsols(SkipInterval, Permutation, permutation(Tokens, Permutation), Permutations), % Find the first SkipInterval permutations
    length(Permutations, Length),
    (Length < SkipInterval -> true; write_nth_permutation(Stream, Permutations, SkipInterval)),
    close(Stream).

write_nth_permutation(Stream, Permutations, SkipInterval) :-
    nth1(SkipInterval, Permutations, SelectedPermutation),
    test_counter(Count),
    increment_test_counter,
    write_test_function(Stream, SelectedPermutation, Count),
    fail. % Fail to backtrack and continue finding the next set of permutations

