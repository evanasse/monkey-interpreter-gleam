import gleam/dict
import gleam/list
import gleeunit/should
import monkey/ast
import monkey/lexer
import monkey/object.{type Environment, type EvalError, type Object, eval}
import monkey/parser
import monkey/token

fn eval_program(input: String) -> Result(#(Object, Environment), EvalError) {
  let assert Ok(tokens) = lexer.lex(input)
  let assert Ok(program) = parser.parse(tokens)
  eval(program, object.new_env())
}

fn test_pair(t: #(String, Result(Object, EvalError))) {
  let assert Ok(expected_object) = t.1
  let assert Ok(evaluated_tuple) = eval_program(t.0)
  evaluated_tuple.0 |> should.equal(expected_object)
}

pub fn eval_integer_expression_test() {
  let tests = [
    #("5", Ok(object.Integer(5))),
    #("10", Ok(object.Integer(10))),
    #("-5", Ok(object.Integer(-5))),
    #("-10", Ok(object.Integer(-10))),
    #("5 + 5 + 5 + 5 - 10", Ok(object.Integer(10))),
    #("2 * 2 * 2 * 2 * 2", Ok(object.Integer(32))),
    #("-50 + 100 + -50", Ok(object.Integer(0))),
    #("5 * 2 + 10", Ok(object.Integer(20))),
    #("5 + 2 * 10", Ok(object.Integer(25))),
    #("20 + 2 * -10", Ok(object.Integer(0))),
    #("50 / 2 * 2 + 10", Ok(object.Integer(60))),
    #("2 * (5 + 10)", Ok(object.Integer(30))),
    #("3 * 3 * 3 + 10", Ok(object.Integer(37))),
    #("3 * (3 * 3) + 10", Ok(object.Integer(37))),
    #("(5 + 10 * 2 + 15 / 3) * 2 + -10", Ok(object.Integer(50))),
  ]

  tests
  |> list.each(test_pair)
}

pub fn eval_boolean_expression_test() {
  let tests = [
    #("true", Ok(object.Boolean(True))),
    #("false", Ok(object.Boolean(False))),
    #("1 < 2", Ok(object.Boolean(True))),
    #("1 > 2", Ok(object.Boolean(False))),
    #("1 < 1", Ok(object.Boolean(False))),
    #("1 > 1", Ok(object.Boolean(False))),
    #("1 == 1", Ok(object.Boolean(True))),
    #("1 != 1", Ok(object.Boolean(False))),
    #("1 == 2", Ok(object.Boolean(False))),
    #("1 != 2", Ok(object.Boolean(True))),
    #("true == true", Ok(object.Boolean(True))),
    #("false == false", Ok(object.Boolean(True))),
    #("true == false", Ok(object.Boolean(False))),
    #("true != false", Ok(object.Boolean(True))),
    #("false != false", Ok(object.Boolean(False))),
    #("(1 < 2) == true", Ok(object.Boolean(True))),
    #("(1 < 2) == false", Ok(object.Boolean(False))),
    #("(1 > 2) == true", Ok(object.Boolean(False))),
    #("(1 > 2) == false", Ok(object.Boolean(True))),
  ]

  tests
  |> list.each(test_pair)
}

pub fn bang_operator_test() {
  let tests = [
    #("!true", Ok(object.Boolean(False))),
    #("!false", Ok(object.Boolean(True))),
    #("!5", Ok(object.Boolean(False))),
    #("!!true", Ok(object.Boolean(True))),
    #("!!false", Ok(object.Boolean(False))),
    #("!!5", Ok(object.Boolean(True))),
  ]

  tests
  |> list.each(test_pair)
}

pub fn if_else_expression_test() {
  let tests = [
    #("if (true) { 10 }", Ok(object.Integer(10))),
    #("if (false) { 10 }", Ok(object.null)),
    #("if (1) { 10 }", Ok(object.Integer(10))),
    #("if (1 < 2) { 10 }", Ok(object.Integer(10))),
    #("if (1 > 2) { 10 }", Ok(object.null)),
    #("if (1 > 2) { 10 } else { 20 }", Ok(object.Integer(20))),
    #("if (1 < 2) { 10 } else { 20 }", Ok(object.Integer(10))),
  ]

  tests
  |> list.each(test_pair)
}

pub fn return_statement_test() {
  let tests = [
    #("return 10;", Ok(object.Integer(10))),
    #("return 10; 9;", Ok(object.Integer(10))),
    #("return 2 * 5; 9;", Ok(object.Integer(10))),
    #("9; return 2 * 5; 9;", Ok(object.Integer(10))),
    #(
      "if (10 > 1) {
        if (10 > 1) {
          return 10;
        }
        return 1;
      }",
      Ok(object.Integer(10)),
    ),
  ]

  tests
  |> list.each(test_pair)
}

pub fn error_handling_test() {
  let tests = [
    #(
      "5 + true;",
      Ok(object.ErrorObj("addition not supported: INTEGER + BOOLEAN")),
    ),
    #(
      "5 + true; 5;",
      Ok(object.ErrorObj("addition not supported: INTEGER + BOOLEAN")),
    ),
    #("-true", Ok(object.ErrorObj("unknown operator: -BOOLEAN"))),
    #(
      "true + false;",
      Ok(object.ErrorObj("addition not supported: BOOLEAN + BOOLEAN")),
    ),
    #(
      "5; true + false; 5",
      Ok(object.ErrorObj("addition not supported: BOOLEAN + BOOLEAN")),
    ),
    #(
      "if (10 > 1) { true + false; }",
      Ok(object.ErrorObj("addition not supported: BOOLEAN + BOOLEAN")),
    ),
    #(
      "if (10 > 1) {
        if (10 > 1) {
          return true + false;
        }
        return 1;
      }",
      Ok(object.ErrorObj("addition not supported: BOOLEAN + BOOLEAN")),
    ),
    #("foobar", Ok(object.ErrorObj("identifier not found: foobar"))),
  ]

  tests
  |> list.each(test_pair)
}

pub fn let_statement_test() {
  let tests = [
    #("let a = 5; a;", Ok(object.Integer(5))),
    #("let a = 5 * 5; a;", Ok(object.Integer(25))),
    #("let a = 5; let b = a; a;", Ok(object.Integer(5))),
    #("let a = 5; let b = a; let c = a + b + 5; c;", Ok(object.Integer(15))),
  ]

  tests
  |> list.each(test_pair)
}

pub fn function_object_test() {
  let tests = [
    #(
      "fn(x) { x + 2; };",
      Ok(object.Function(
        parameters: [ast.Identifier(token: token.Identifier("x"), value: "x")],
        body: ast.BlockStatement(token: token.l_brace, statements: [
          ast.ExpressionStatement(
            token: token.plus,
            expression: ast.InfixExpression(
              token: token.plus,
              left: ast.Identifier(token: token.Identifier("x"), value: "x"),
              operator: token.plus.literal,
              right: ast.IntegerLiteral(token: token.Integer("2"), value: 2),
            ),
          ),
        ]),
        env: object.new_env(),
      )),
    ),
  ]

  tests
  |> list.each(test_pair)
}

pub fn function_application_test() {
  let tests = [
    #("let identity = fn(x) { x; }; identity(5);", Ok(object.Integer(5))),
    #("let identity = fn(x) { return x; }; identity(5);", Ok(object.Integer(5))),
    #("let double = fn(x) { x * 2; }; double(5);", Ok(object.Integer(10))),
    #("let add = fn(x, y) { x + y; }; add(5, 5);", Ok(object.Integer(10))),
    #(
      "let add = fn(x, y) { x + y; }; add(5, add(5, 5));",
      Ok(object.Integer(15)),
    ),
    #("fn(x) { x; }(5)", Ok(object.Integer(5))),
    #("fn() { 5; }()", Ok(object.Integer(5))),
    #(
      "let newAdder = fn(x) { fn(y) { x + y }; }; let addTwo = newAdder(2); addTwo(2);",
      Ok(object.Integer(4)),
    ),
  ]

  tests
  |> list.each(test_pair)
}
