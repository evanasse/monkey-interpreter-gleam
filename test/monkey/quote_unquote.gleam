import gleam/list
import gleeunit/should
import monkey/ast
import monkey/eval.{type EvalError}
import monkey/lexer
import monkey/object.{type Environment, type Object}
import monkey/parser
import monkey/token

fn eval_program(input: String) -> Result(#(Object, Environment), EvalError) {
  let assert Ok(tokens) = lexer.lex(input)
  let assert Ok(program) = parser.parse(tokens)
  eval.eval_program(program, object.new_env())
}

fn test_pair(t: #(String, Result(Object, EvalError))) {
  let expected_result = t.1
  let evaluated_program = eval_program(t.0)
  case evaluated_program {
    Ok(#(object, _env)) -> Ok(object) |> should.equal(expected_result)
    Error(e) -> Error(e) |> should.equal(expected_result)
  }
}

pub fn quote_test() {
  let tests = [
    #(
      "quote(5)",
      Ok(
        object.Quote(node: ast.IntegerLiteral(
          token: token.Integer("5"),
          value: 5,
        )),
      ),
    ),
    #(
      "quote(5 + 8)",
      Ok(
        object.Quote(node: ast.InfixExpression(
          token: token.plus,
          left: ast.IntegerLiteral(token: token.Integer("5"), value: 5),
          operator: token.plus.literal,
          right: ast.IntegerLiteral(token: token.Integer("8"), value: 8),
        )),
      ),
    ),
    #(
      "quote(foobar)",
      Ok(
        object.Quote(node: ast.Identifier(
          token: token.Identifier("foobar"),
          value: "foobar",
        )),
      ),
    ),
    #(
      "quote(foobar + barfoo)",
      Ok(
        object.Quote(node: ast.InfixExpression(
          token: token.plus,
          left: ast.Identifier(
            token: token.Identifier("foobar"),
            value: "foobar",
          ),
          operator: token.plus.literal,
          right: ast.Identifier(
            token: token.Identifier("barfoo"),
            value: "barfoo",
          ),
        )),
      ),
    ),
  ]

  tests
  |> list.each(test_pair)
}

pub fn quote_unquote_test() {
  let tests = [
    #(
      "quote(unquote(4))",
      Ok(
        object.Quote(node: ast.IntegerLiteral(
          token: token.Integer("4"),
          value: 4,
        )),
      ),
    ),
    #(
      "quote(unquote(4 + 4))",
      Ok(
        object.Quote(node: ast.IntegerLiteral(
          token: token.Integer("8"),
          value: 8,
        )),
      ),
    ),
    #(
      "quote(8 + unquote(4 + 4))",
      Ok(
        object.Quote(node: ast.InfixExpression(
          token: token.plus,
          left: ast.IntegerLiteral(token: token.Integer("8"), value: 8),
          operator: token.plus.literal,
          right: ast.IntegerLiteral(token: token.Integer("8"), value: 8),
        )),
      ),
    ),
    #(
      "quote(unquote(4 + 4) + 8)",
      Ok(
        object.Quote(node: ast.InfixExpression(
          token: token.plus,
          left: ast.IntegerLiteral(token: token.Integer("8"), value: 8),
          operator: token.plus.literal,
          right: ast.IntegerLiteral(token: token.Integer("8"), value: 8),
        )),
      ),
    ),
    #(
      "let foobar = 8; quote(foobar)",
      Ok(
        object.Quote(node: ast.Identifier(
          token: token.Identifier("foobar"),
          value: "foobar",
        )),
      ),
    ),
    #(
      "let foobar = 8; quote(unquote(foobar))",
      Ok(
        object.Quote(node: ast.IntegerLiteral(
          token: token.Integer("8"),
          value: 8,
        )),
      ),
    ),
    #(
      "quote(unquote(true))",
      Ok(object.Quote(node: ast.BooleanLiteral(token: token.true, value: True))),
    ),
    #(
      "quote(unquote(true == false))",
      Ok(
        object.Quote(node: ast.BooleanLiteral(token: token.true, value: False)),
      ),
    ),
    #(
      "quote(unquote(quote(4 + 4)))",
      Ok(
        object.Quote(node: ast.InfixExpression(
          token: token.plus,
          left: ast.IntegerLiteral(token: token.Integer("4"), value: 4),
          operator: token.plus.literal,
          right: ast.IntegerLiteral(token: token.Integer("4"), value: 4),
        )),
      ),
    ),
    #(
      "let quoted_infix_expression = quote(4 + 4); quote(unquote(4 + 4) + unquote(quoted_infix_expression))",
      Ok(
        object.Quote(node: ast.InfixExpression(
          token: token.plus,
          left: ast.IntegerLiteral(token: token.Integer("8"), value: 8),
          operator: token.plus.literal,
          right: ast.InfixExpression(
            token: token.plus,
            left: ast.IntegerLiteral(token: token.Integer("4"), value: 4),
            operator: token.plus.literal,
            right: ast.IntegerLiteral(token: token.Integer("4"), value: 4),
          ),
        )),
      ),
    ),
  ]

  tests
  |> list.each(test_pair)
}
