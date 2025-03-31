import gleeunit/should
import monkey/lexer
import monkey/token

pub fn next_token_test() {
  let input =
    "let five = 5;
let ten = 10;

let add = fn(x, y) {
    x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
  return true;
} else {
  return false;
}

10 == 10;
10 != 9;
\"foobar\"
\"foo bar\"
[]
{\"foo\": \"bar\"}
macro(x, y) { x + y; };
"

  let test_tokens = [
    token.let_,
    token.Identifier("five"),
    token.assign,
    token.Integer("5"),
    token.semicolon,
    token.let_,
    token.Identifier("ten"),
    token.assign,
    token.Integer("10"),
    token.semicolon,
    token.let_,
    token.Identifier("add"),
    token.assign,
    token.function,
    token.l_paren,
    token.Identifier("x"),
    token.comma,
    token.Identifier("y"),
    token.r_paren,
    token.l_brace,
    token.Identifier("x"),
    token.plus,
    token.Identifier("y"),
    token.semicolon,
    token.r_brace,
    token.semicolon,
    token.let_,
    token.Identifier("result"),
    token.assign,
    token.Identifier("add"),
    token.l_paren,
    token.Identifier("five"),
    token.comma,
    token.Identifier("ten"),
    token.r_paren,
    token.semicolon,
    token.bang,
    token.minus,
    token.slash,
    token.asterisk,
    token.Integer("5"),
    token.semicolon,
    token.Integer("5"),
    token.lesser_than,
    token.Integer("10"),
    token.greater_than,
    token.Integer("5"),
    token.semicolon,
    token.if_,
    token.l_paren,
    token.Integer("5"),
    token.lesser_than,
    token.Integer("10"),
    token.r_paren,
    token.l_brace,
    token.return,
    token.true,
    token.semicolon,
    token.r_brace,
    token.else_,
    token.l_brace,
    token.return,
    token.false,
    token.semicolon,
    token.r_brace,
    token.Integer("10"),
    token.equals,
    token.Integer("10"),
    token.semicolon,
    token.Integer("10"),
    token.not_equals,
    token.Integer("9"),
    token.semicolon,
    token.String("foobar"),
    token.String("foo bar"),
    token.l_bracket,
    token.r_bracket,
    token.l_brace,
    token.String("foo"),
    token.colon,
    token.String("bar"),
    token.r_brace,
    token.macro_,
    token.l_paren,
    token.Identifier("x"),
    token.comma,
    token.Identifier("y"),
    token.r_paren,
    token.l_brace,
    token.Identifier("x"),
    token.plus,
    token.Identifier("y"),
    token.semicolon,
    token.r_brace,
    token.semicolon,
    token.eof,
  ]

  lexer.lex(input)
  |> should.be_ok
  |> should.equal(test_tokens)
}
