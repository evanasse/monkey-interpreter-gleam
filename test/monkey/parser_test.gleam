import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import monkey/ast
import monkey/lexer
import monkey/parser
import monkey/token

pub fn let_statement_test() {
  let input =
    "
let x = 5;
let y = true;
let foobar = y;
"

  let assert Ok(tokens) = lexer.lex(input)

  let assert Ok(program) = parser.parse(tokens)

  let expected_program =
    ast.Program(token.eof, [
      ast.LetStatement(
        token: token.let_,
        name: ast.Identifier(token.Identifier("x"), "x"),
        value: ast.IntegerLiteral(token: token.Integer("5"), value: 5),
      ),
      ast.LetStatement(
        token: token.let_,
        name: ast.Identifier(token.Identifier("y"), "y"),
        value: ast.Boolean(token: token.true, value: True),
      ),
      ast.LetStatement(
        token: token.let_,
        name: ast.Identifier(token.Identifier("foobar"), "foobar"),
        value: ast.Identifier(token: token.Identifier("y"), value: "y"),
      ),
    ])

  program
  |> should.equal(expected_program)
}

pub fn return_statement_test() {
  let input =
    "
return 5;
return true;
return y;
"

  let assert Ok(tokens) = lexer.lex(input)

  let assert Ok(program) = parser.parse(tokens)

  let expected_program =
    ast.Program(token.eof, [
      ast.ReturnStatement(
        token: token.return,
        return_value: ast.IntegerLiteral(token: token.Integer("5"), value: 5),
      ),
      ast.ReturnStatement(
        token: token.return,
        return_value: ast.Boolean(token: token.true, value: True),
      ),
      ast.ReturnStatement(
        token: token.return,
        return_value: ast.Identifier(token: token.Identifier("y"), value: "y"),
      ),
    ])

  program
  |> should.equal(expected_program)
}

pub fn identifier_expression_test() {
  let input =
    "
foobar;
foobar
"

  let assert Ok(tokens) = lexer.lex(input)

  let assert Ok(program) = parser.parse(tokens)

  let expected_program =
    ast.Program(token.eof, [
      ast.ExpressionStatement(
        token: token.Identifier("foobar"),
        expression: ast.Identifier(token.Identifier("foobar"), "foobar"),
      ),
      ast.ExpressionStatement(
        token: token.Identifier("foobar"),
        expression: ast.Identifier(token.Identifier("foobar"), "foobar"),
      ),
    ])

  program
  |> should.equal(expected_program)
}

pub fn integer_literal_expression_test() {
  let input = "5"
  let assert Ok(tokens) = lexer.lex(input)

  let assert Ok(program) = parser.parse(tokens)

  let expected_program =
    ast.Program(token.eof, [
      ast.ExpressionStatement(
        token: token.Integer("5"),
        expression: ast.IntegerLiteral(token.Integer("5"), 5),
      ),
    ])

  program
  |> should.equal(expected_program)
}

pub fn string_literal_expression_test() {
  let input = "\"hello world\";"
  let assert Ok(tokens) = lexer.lex(input)

  let assert Ok(program) = parser.parse(tokens)

  let expected_program =
    ast.Program(token.eof, [
      ast.ExpressionStatement(
        token: token.String("hello world"),
        expression: ast.StringLiteral(
          token.String("hello world"),
          "hello world",
        ),
      ),
    ])

  program
  |> should.equal(expected_program)
}

pub fn prefix_expression_test() {
  let input =
    "
!5;
-15;
!true;
!false;
"

  let assert Ok(tokens) = lexer.lex(input)

  let assert Ok(program) = parser.parse(tokens)

  let expected_program =
    ast.Program(token.eof, [
      ast.ExpressionStatement(
        token: token.bang,
        expression: ast.PrefixExpression(
          token: token.bang,
          operator: token.bang.literal,
          right: ast.IntegerLiteral(token.Integer("5"), 5),
        ),
      ),
      ast.ExpressionStatement(
        token: token.minus,
        expression: ast.PrefixExpression(
          token: token.minus,
          operator: token.minus.literal,
          right: ast.IntegerLiteral(token.Integer("15"), 15),
        ),
      ),
      ast.ExpressionStatement(
        token: token.bang,
        expression: ast.PrefixExpression(
          token: token.bang,
          operator: token.bang.literal,
          right: ast.Boolean(token.true, True),
        ),
      ),
      ast.ExpressionStatement(
        token: token.bang,
        expression: ast.PrefixExpression(
          token: token.bang,
          operator: token.bang.literal,
          right: ast.Boolean(token.false, False),
        ),
      ),
    ])

  program
  |> should.equal(expected_program)
}

pub fn infix_expression_test() {
  let input =
    "
5 + 5;
5 - 5;
5 * 5;
5 / 5;
5 > 5;
5 < 5;
5 == 5;
5 != 5;
true == true;
true != false;
false == false;
"

  let assert Ok(tokens) = lexer.lex(input)

  let assert Ok(program) = parser.parse(tokens)

  let expected_program =
    ast.Program(token.eof, [
      ast.ExpressionStatement(
        token: token.plus,
        expression: ast.InfixExpression(
          token: token.plus,
          left: ast.IntegerLiteral(token.Integer("5"), 5),
          operator: token.plus.literal,
          right: ast.IntegerLiteral(token.Integer("5"), 5),
        ),
      ),
      ast.ExpressionStatement(
        token: token.minus,
        expression: ast.InfixExpression(
          token: token.minus,
          left: ast.IntegerLiteral(token.Integer("5"), 5),
          operator: token.minus.literal,
          right: ast.IntegerLiteral(token.Integer("5"), 5),
        ),
      ),
      ast.ExpressionStatement(
        token: token.asterisk,
        expression: ast.InfixExpression(
          token: token.asterisk,
          left: ast.IntegerLiteral(token.Integer("5"), 5),
          operator: token.asterisk.literal,
          right: ast.IntegerLiteral(token.Integer("5"), 5),
        ),
      ),
      ast.ExpressionStatement(
        token: token.slash,
        expression: ast.InfixExpression(
          token: token.slash,
          left: ast.IntegerLiteral(token.Integer("5"), 5),
          operator: token.slash.literal,
          right: ast.IntegerLiteral(token.Integer("5"), 5),
        ),
      ),
      ast.ExpressionStatement(
        token: token.greater_than,
        expression: ast.InfixExpression(
          token: token.greater_than,
          left: ast.IntegerLiteral(token.Integer("5"), 5),
          operator: token.greater_than.literal,
          right: ast.IntegerLiteral(token.Integer("5"), 5),
        ),
      ),
      ast.ExpressionStatement(
        token: token.lesser_than,
        expression: ast.InfixExpression(
          token: token.lesser_than,
          left: ast.IntegerLiteral(token.Integer("5"), 5),
          operator: token.lesser_than.literal,
          right: ast.IntegerLiteral(token.Integer("5"), 5),
        ),
      ),
      ast.ExpressionStatement(
        token: token.equals,
        expression: ast.InfixExpression(
          token: token.equals,
          left: ast.IntegerLiteral(token.Integer("5"), 5),
          operator: token.equals.literal,
          right: ast.IntegerLiteral(token.Integer("5"), 5),
        ),
      ),
      ast.ExpressionStatement(
        token: token.not_equals,
        expression: ast.InfixExpression(
          token: token.not_equals,
          left: ast.IntegerLiteral(token.Integer("5"), 5),
          operator: token.not_equals.literal,
          right: ast.IntegerLiteral(token.Integer("5"), 5),
        ),
      ),
      ast.ExpressionStatement(
        token: token.equals,
        expression: ast.InfixExpression(
          token: token.equals,
          left: ast.Boolean(token.true, True),
          operator: token.equals.literal,
          right: ast.Boolean(token.true, True),
        ),
      ),
      ast.ExpressionStatement(
        token: token.not_equals,
        expression: ast.InfixExpression(
          token: token.not_equals,
          left: ast.Boolean(token.true, True),
          operator: token.not_equals.literal,
          right: ast.Boolean(token.false, False),
        ),
      ),
      ast.ExpressionStatement(
        token: token.equals,
        expression: ast.InfixExpression(
          token: token.equals,
          left: ast.Boolean(token.false, False),
          operator: token.equals.literal,
          right: ast.Boolean(token.false, False),
        ),
      ),
    ])

  program
  |> should.equal(expected_program)
}

pub fn operator_precedence_parsing_test() {
  let tests = [
    #("-a * b", "((-a) * b)"),
    #("!-a", "(!(-a))"),
    #("a + b + c", "((a + b) + c)"),
    #("a + b - c", "((a + b) - c)"),
    #("a * b * c", "((a * b) * c)"),
    #("a * b / c", "((a * b) / c)"),
    #("a + b / c", "(a + (b / c))"),
    #("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
    #("3 + 4; -5 * 5", "(3 + 4)\n((-5) * 5)"),
    #("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
    #("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
    #("3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))"),
    #("true", "true"),
    #("false", "false"),
    #("3 > 5 == false", "((3 > 5) == false)"),
    #("3 < 5 == true", "((3 < 5) == true)"),
    #("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
    #("(5 + 5) * 2", "((5 + 5) * 2)"),
    #("2 / (5 + 5)", "(2 / (5 + 5))"),
    #("-(5 + 5)", "(-(5 + 5))"),
    #("!(true == true)", "(!(true == true))"),
    #("a + add(b * c) + d", "((a + add((b * c))) + d)"),
    #(
      "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
      "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
    ),
    #("add(a + b + c * d / f + g)", "add((((a + b) + ((c * d) / f)) + g))"),
    #("a * [1, 2, 3, 4][b * c] * d", "((a * ([1, 2, 3, 4][(b * c)])) * d)"),
    #(
      "add(a * b[2], b[1], 2 * [1, 2][1])",
      "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
    ),
  ]

  tests
  |> list.each(fn(t) {
    let assert Ok(tokens) = lexer.lex(t.0)

    let assert Ok(program) = parser.parse(tokens)

    ast.node_to_string(program)
    |> should.equal(t.1)
  })
}

pub fn boolean_literal_test() {
  let input =
    "
true;
false;
"

  let assert Ok(tokens) = lexer.lex(input)

  let assert Ok(program) = parser.parse(tokens)

  let expected_program =
    ast.Program(token.eof, [
      ast.ExpressionStatement(
        token: token.true,
        expression: ast.Boolean(token.true, True),
      ),
      ast.ExpressionStatement(
        token: token.false,
        expression: ast.Boolean(token.false, False),
      ),
    ])

  program
  |> should.equal(expected_program)
}

pub fn if_expression_test() {
  let input =
    "
if (x < y) { x };
"

  let assert Ok(tokens) = lexer.lex(input)

  let assert Ok(program) = parser.parse(tokens)

  let expected_program =
    ast.Program(token.eof, [
      ast.ExpressionStatement(
        token: token.if_,
        expression: ast.IfExpression(
          token: token.if_,
          condition: ast.InfixExpression(
            token.lesser_than,
            ast.Identifier(token.Identifier("x"), "x"),
            token.lesser_than.literal,
            ast.Identifier(token.Identifier("y"), "y"),
          ),
          consequence: ast.BlockStatement(token: token.l_brace, statements: [
            ast.ExpressionStatement(
              token: token.Identifier("x"),
              expression: ast.Identifier(token.Identifier("x"), "x"),
            ),
          ]),
          alternative: None,
        ),
      ),
    ])

  program
  |> should.equal(expected_program)
}

pub fn if_else_expression_test() {
  let input =
    "
if (x < y) { x } else { y };
"

  let assert Ok(tokens) = lexer.lex(input)

  let assert Ok(program) = parser.parse(tokens)

  let expected_program =
    ast.Program(token.eof, [
      ast.ExpressionStatement(
        token: token.if_,
        expression: ast.IfExpression(
          token: token.if_,
          condition: ast.InfixExpression(
            token.lesser_than,
            ast.Identifier(token.Identifier("x"), "x"),
            token.lesser_than.literal,
            ast.Identifier(token.Identifier("y"), "y"),
          ),
          consequence: ast.BlockStatement(token: token.l_brace, statements: [
            ast.ExpressionStatement(
              token: token.Identifier("x"),
              expression: ast.Identifier(token.Identifier("x"), "x"),
            ),
          ]),
          alternative: Some(
            ast.BlockStatement(token: token.l_brace, statements: [
              ast.ExpressionStatement(
                token: token.Identifier("y"),
                expression: ast.Identifier(token.Identifier("y"), "y"),
              ),
            ]),
          ),
        ),
      ),
    ])

  program
  |> should.equal(expected_program)
}

pub fn function_literal_test() {
  let input =
    "
fn() { x + y; };
fn(x) { x + y; };
fn(x, y) { x + y; };
"

  let assert Ok(tokens) = lexer.lex(input)

  let assert Ok(program) = parser.parse(tokens)

  let expected_program =
    ast.Program(token.eof, [
      ast.ExpressionStatement(
        token: token.function,
        expression: ast.FunctionLiteral(
          token: token.function,
          parameters: [],
          body: ast.BlockStatement(token: token.l_brace, statements: [
            ast.ExpressionStatement(
              token: token.plus,
              expression: ast.InfixExpression(
                token: token.plus,
                left: ast.Identifier(token.Identifier("x"), "x"),
                operator: token.plus.literal,
                right: ast.Identifier(token.Identifier("y"), "y"),
              ),
            ),
          ]),
        ),
      ),
      ast.ExpressionStatement(
        token: token.function,
        expression: ast.FunctionLiteral(
          token: token.function,
          parameters: [ast.Identifier(token.Identifier("x"), "x")],
          body: ast.BlockStatement(token: token.l_brace, statements: [
            ast.ExpressionStatement(
              token: token.plus,
              expression: ast.InfixExpression(
                token: token.plus,
                left: ast.Identifier(token.Identifier("x"), "x"),
                operator: token.plus.literal,
                right: ast.Identifier(token.Identifier("y"), "y"),
              ),
            ),
          ]),
        ),
      ),
      ast.ExpressionStatement(
        token: token.function,
        expression: ast.FunctionLiteral(
          token: token.function,
          parameters: [
            ast.Identifier(token.Identifier("x"), "x"),
            ast.Identifier(token.Identifier("y"), "y"),
          ],
          body: ast.BlockStatement(token: token.l_brace, statements: [
            ast.ExpressionStatement(
              token: token.plus,
              expression: ast.InfixExpression(
                token: token.plus,
                left: ast.Identifier(token.Identifier("x"), "x"),
                operator: token.plus.literal,
                right: ast.Identifier(token.Identifier("y"), "y"),
              ),
            ),
          ]),
        ),
      ),
    ])

  program
  |> should.equal(expected_program)
}

pub fn call_expression_test() {
  let input =
    "
add(1, 2 * 3, 4 + 5);
"

  let assert Ok(tokens) = lexer.lex(input)

  let assert Ok(program) = parser.parse(tokens)

  let expected_program =
    ast.Program(token.eof, [
      ast.ExpressionStatement(
        token: token.l_paren,
        expression: ast.CallExpression(
          token: token.l_paren,
          function: ast.Identifier(token: token.Identifier("add"), value: "add"),
          arguments: [
            ast.IntegerLiteral(token: token.Integer("1"), value: 1),
            ast.InfixExpression(
              token: token.asterisk,
              left: ast.IntegerLiteral(token: token.Integer("2"), value: 2),
              operator: token.asterisk.literal,
              right: ast.IntegerLiteral(token: token.Integer("3"), value: 3),
            ),
            ast.InfixExpression(
              token: token.plus,
              left: ast.IntegerLiteral(token: token.Integer("4"), value: 4),
              operator: token.plus.literal,
              right: ast.IntegerLiteral(token: token.Integer("5"), value: 5),
            ),
          ],
        ),
      ),
    ])

  program
  |> should.equal(expected_program)
}

pub fn parse_array_literal_test() {
  let input =
    "
[1, 2 * 3, 4 + 5];
"

  let assert Ok(tokens) = lexer.lex(input)

  let assert Ok(program) = parser.parse(tokens)

  let expected_program =
    ast.Program(token.eof, [
      ast.ExpressionStatement(
        token: token.l_bracket,
        expression: ast.ArrayLiteral(token: token.l_bracket, elements: [
          ast.IntegerLiteral(token: token.Integer("1"), value: 1),
          ast.InfixExpression(
            token: token.asterisk,
            left: ast.IntegerLiteral(token: token.Integer("2"), value: 2),
            operator: token.asterisk.literal,
            right: ast.IntegerLiteral(token: token.Integer("3"), value: 3),
          ),
          ast.InfixExpression(
            token: token.plus,
            left: ast.IntegerLiteral(token: token.Integer("4"), value: 4),
            operator: token.plus.literal,
            right: ast.IntegerLiteral(token: token.Integer("5"), value: 5),
          ),
        ]),
      ),
    ])

  program
  |> should.equal(expected_program)
}

pub fn parse_index_expression_test() {
  let input =
    "
myArray[1 + 1]
"

  let assert Ok(tokens) = lexer.lex(input)

  let assert Ok(program) = parser.parse(tokens)

  let expected_program =
    ast.Program(token.eof, [
      ast.ExpressionStatement(
        token: token.l_bracket,
        expression: ast.IndexExpression(
          token: token.l_bracket,
          left: ast.Identifier(token.Identifier("myArray"), "myArray"),
          index: ast.InfixExpression(
            token: token.plus,
            left: ast.IntegerLiteral(token: token.Integer("1"), value: 1),
            operator: token.plus.literal,
            right: ast.IntegerLiteral(token: token.Integer("1"), value: 1),
          ),
        ),
      ),
    ])

  program
  |> should.equal(expected_program)
}

pub fn parse_hash_literal_string_keys_test() {
  let input =
    "
{\"one\": 1, \"two\": 2, \"three\": 3}
"

  let assert Ok(tokens) = lexer.lex(input)

  let assert Ok(program) = parser.parse(tokens)

  let expected_program =
    ast.Program(token.eof, [
      ast.ExpressionStatement(
        token: token.l_brace,
        expression: ast.HashLiteral(token: token.l_brace, pairs: [
          #(
            ast.StringLiteral(token: token.String("one"), value: "one"),
            ast.IntegerLiteral(token: token.Integer("1"), value: 1),
          ),
          #(
            ast.StringLiteral(token: token.String("two"), value: "two"),
            ast.IntegerLiteral(token: token.Integer("2"), value: 2),
          ),
          #(
            ast.StringLiteral(token: token.String("three"), value: "three"),
            ast.IntegerLiteral(token: token.Integer("3"), value: 3),
          ),
        ]),
      ),
    ])

  program
  |> should.equal(expected_program)
}

pub fn parse_hash_literal_integer_keys_test() {
  let input =
    "
{1: 1, 2: 2, 3: 3}
"

  let assert Ok(tokens) = lexer.lex(input)

  let assert Ok(program) = parser.parse(tokens)

  let expected_program =
    ast.Program(token.eof, [
      ast.ExpressionStatement(
        token: token.l_brace,
        expression: ast.HashLiteral(token: token.l_brace, pairs: [
          #(
            ast.IntegerLiteral(token: token.Integer("1"), value: 1),
            ast.IntegerLiteral(token: token.Integer("1"), value: 1),
          ),
          #(
            ast.IntegerLiteral(token: token.Integer("2"), value: 2),
            ast.IntegerLiteral(token: token.Integer("2"), value: 2),
          ),
          #(
            ast.IntegerLiteral(token: token.Integer("3"), value: 3),
            ast.IntegerLiteral(token: token.Integer("3"), value: 3),
          ),
        ]),
      ),
    ])

  program
  |> should.equal(expected_program)
}

pub fn parse_hash_literal_boolean_keys_test() {
  let input =
    "
{true: 1, false: 2}
"

  let assert Ok(tokens) = lexer.lex(input)

  let assert Ok(program) = parser.parse(tokens)

  let expected_program =
    ast.Program(token.eof, [
      ast.ExpressionStatement(
        token: token.l_brace,
        expression: ast.HashLiteral(token: token.l_brace, pairs: [
          #(
            ast.Boolean(token: token.true, value: True),
            ast.IntegerLiteral(token: token.Integer("1"), value: 1),
          ),
          #(
            ast.Boolean(token: token.false, value: False),
            ast.IntegerLiteral(token: token.Integer("2"), value: 2),
          ),
        ]),
      ),
    ])

  program
  |> should.equal(expected_program)
}

pub fn parse_empty_hash_literal_test() {
  let input =
    "
{}
"

  let assert Ok(tokens) = lexer.lex(input)

  let assert Ok(program) = parser.parse(tokens)

  let expected_program =
    ast.Program(token.eof, [
      ast.ExpressionStatement(
        token: token.l_brace,
        expression: ast.HashLiteral(token: token.l_brace, pairs: []),
      ),
    ])

  program
  |> should.equal(expected_program)
}

pub fn parse_hash_literal_with_expressions_test() {
  let input =
    "
{\"one\": 0 + 1, \"two\": 10 - 8, \"three\": 15 / 5}
"

  let assert Ok(tokens) = lexer.lex(input)

  let assert Ok(program) = parser.parse(tokens)

  let expected_program =
    ast.Program(token.eof, [
      ast.ExpressionStatement(
        token: token.l_brace,
        expression: ast.HashLiteral(token: token.l_brace, pairs: [
          #(
            ast.StringLiteral(token: token.String("one"), value: "one"),
            ast.InfixExpression(
              token: token.plus,
              left: ast.IntegerLiteral(token: token.Integer("0"), value: 0),
              operator: token.plus.literal,
              right: ast.IntegerLiteral(token: token.Integer("1"), value: 1),
            ),
          ),
          #(
            ast.StringLiteral(token: token.String("two"), value: "two"),
            ast.InfixExpression(
              token: token.minus,
              left: ast.IntegerLiteral(token: token.Integer("10"), value: 10),
              operator: token.minus.literal,
              right: ast.IntegerLiteral(token: token.Integer("8"), value: 8),
            ),
          ),
          #(
            ast.StringLiteral(token: token.String("three"), value: "three"),
            ast.InfixExpression(
              token: token.slash,
              left: ast.IntegerLiteral(token: token.Integer("15"), value: 15),
              operator: token.slash.literal,
              right: ast.IntegerLiteral(token: token.Integer("5"), value: 5),
            ),
          ),
        ]),
      ),
    ])

  program
  |> should.equal(expected_program)
}
