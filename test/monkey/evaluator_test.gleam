import gleam/list
import gleeunit/should
import monkey/ast
import monkey/eval.{type EvalError, eval}
import monkey/lexer
import monkey/object.{type Environment, type Object}
import monkey/parser
import monkey/token

fn eval_program(input: String) -> Result(#(Object, Environment), EvalError) {
  let assert Ok(tokens) = lexer.lex(input)
  let assert Ok(program) = parser.parse(tokens)
  eval(program, object.new_env())
}

fn test_pair(t: #(String, Result(Object, EvalError))) {
  let expected_result = t.1
  let evaluated_program = eval_program(t.0)
  case evaluated_program {
    Ok(#(object, _env)) -> Ok(object) |> should.equal(expected_result)
    Error(e) -> Error(e) |> should.equal(expected_result)
  }
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

pub fn eval_string_expression_test() {
  let tests = [#("\"Hello World!\"", Ok(object.String("Hello World!")))]

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
    #("5 + true;", Error(eval.AdditionNotSupported("INTEGER", "BOOLEAN"))),
    #("5 + true; 5;", Error(eval.AdditionNotSupported("INTEGER", "BOOLEAN"))),
    #("-true", Error(eval.UnknownPrefixOperator("-", "BOOLEAN"))),
    #("true + false;", Error(eval.AdditionNotSupported("BOOLEAN", "BOOLEAN"))),
    #(
      "5; true + false; 5",
      Error(eval.AdditionNotSupported("BOOLEAN", "BOOLEAN")),
    ),
    #(
      "if (10 > 1) { true + false; }",
      Error(eval.AdditionNotSupported("BOOLEAN", "BOOLEAN")),
    ),
    #(
      "\"Hello\" - \"World!\"",
      Error(eval.SubstractionNotSupported("STRING", "STRING")),
    ),
    #(
      "if (10 > 1) {
        if (10 > 1) {
          return true + false;
        }
        return 1;
      }",
      Error(eval.AdditionNotSupported("BOOLEAN", "BOOLEAN")),
    ),
    #("foobar", Error(eval.IdentifierNotFound("foobar"))),
    #(
      "{\"name\": \"Monkey\"}[fn(x){ x }];",
      Error(eval.TypeNotSupportedAsHashKey("FUNCTION")),
    ),
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

pub fn string_concatenation_test() {
  let tests = [
    #("\"Hello\" + \" \" + \"World!\"", Ok(object.String("Hello World!"))),
  ]

  tests
  |> list.each(test_pair)
}

pub fn builtin_functions_test() {
  let tests = [
    #("len(\"\")", Ok(object.Integer(0))),
    #("len(\"four\")", Ok(object.Integer(4))),
    #("len(\"hello world\")", Ok(object.Integer(11))),
    #("len([1, 2, 3])", Ok(object.Integer(3))),
    #("len(1)", Error(eval.ArgumentNotSupported("len", "INTEGER"))),
    #(
      "len(\"one\", \"two\")",
      Error(eval.WrongNumberOfArguments(expected: 1, got: 2)),
    ),
    #("first([1,2,3])", Ok(object.Integer(1))),
    #("first([\"1\", 2, 3])", Ok(object.String("1"))),
    #("first([])", Error(eval.ListIsEmpty)),
    #("first(1)", Error(eval.ArgumentNotSupported("first", "INTEGER"))),
    #(
      "first([1,2,3],[4,5,6])",
      Error(eval.WrongNumberOfArguments(expected: 1, got: 2)),
    ),
    #("last([1,2,3])", Ok(object.Integer(3))),
    #("last([\"1\", 2, 3])", Ok(object.Integer(3))),
    #("last([])", Error(eval.ListIsEmpty)),
    #("last(1)", Error(eval.ArgumentNotSupported("last", "INTEGER"))),
    #(
      "last([1,2,3],[4,5,6])",
      Error(eval.WrongNumberOfArguments(expected: 1, got: 2)),
    ),
    #("rest([1,2,3])", Ok(object.Array([object.Integer(2), object.Integer(3)]))),
    #(
      "rest([\"1\", \"2\", 3])",
      Ok(object.Array([object.String("2"), object.Integer(3)])),
    ),
    #("rest([])", Error(eval.ListIsEmpty)),
    #("rest(1)", Error(eval.ArgumentNotSupported("rest", "INTEGER"))),
    #(
      "rest([1,2,3],[4,5,6])",
      Error(eval.WrongNumberOfArguments(expected: 1, got: 2)),
    ),
    #(
      "push([1,2,3], 4)",
      Ok(
        object.Array([
          object.Integer(1),
          object.Integer(2),
          object.Integer(3),
          object.Integer(4),
        ]),
      ),
    ),
    #(
      "push([\"1\", \"2\", 3], \"4\")",
      Ok(
        object.Array([
          object.String("1"),
          object.String("2"),
          object.Integer(3),
          object.String("4"),
        ]),
      ),
    ),
    #("push([], 1)", Ok(object.Array([object.Integer(1)]))),
    #(
      "push([], [1,2,3])",
      Ok(
        object.Array([
          object.Array([object.Integer(1), object.Integer(2), object.Integer(3)]),
        ]),
      ),
    ),
    #("push(1, 1)", Error(eval.ArgumentNotSupported("push", "INTEGER"))),
    #("push([])", Error(eval.WrongNumberOfArguments(expected: 2, got: 1))),
    #(
      "push([1,2,3], 2, 3)",
      Error(eval.WrongNumberOfArguments(expected: 2, got: 3)),
    ),
  ]

  tests
  |> list.each(test_pair)
}

pub fn array_literal_test() {
  let tests = [
    #(
      "[1, 2 * 2, 3 + 3]",
      Ok(
        object.Array(elements: [
          object.Integer(1),
          object.Integer(4),
          object.Integer(6),
        ]),
      ),
    ),
  ]

  tests
  |> list.each(test_pair)
}

pub fn array_index_expressions_test() {
  let tests = [
    #("[1, 2, 3][0]", Ok(object.Integer(1))),
    #("[1, 2, 3][1]", Ok(object.Integer(2))),
    #("[1, 2, 3][2]", Ok(object.Integer(3))),
    #("let i = 0; [1][i];", Ok(object.Integer(1))),
    #("[1, 2, 3][1 + 1];", Ok(object.Integer(3))),
    #("let myArray = [1, 2, 3]; myArray[2];", Ok(object.Integer(3))),
    #(
      "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
      Ok(object.Integer(6)),
    ),
    #(
      "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i];",
      Ok(object.Integer(2)),
    ),
    #("[1, 2, 3][3]", Ok(object.null)),
    #("[1, 2, 3][-1]", Ok(object.null)),
  ]

  tests
  |> list.each(test_pair)
}

pub fn hash_literals_test() {
  let tests = [
    #(
      "let two = \"two\"; {\"one\": 10 - 9, two: 1 + 1, \"thr\" + \"ee\": 6 / 2, 4: 4, true: 5, false: 6}",
      Ok(
        object.Hash(pairs: [
          #(
            object.HashKey(key_type: object.string_obj, value: "one"),
            object.HashPair(key: object.String("one"), value: object.Integer(1)),
          ),
          #(
            object.HashKey(key_type: object.string_obj, value: "two"),
            object.HashPair(key: object.String("two"), value: object.Integer(2)),
          ),
          #(
            object.HashKey(key_type: object.string_obj, value: "three"),
            object.HashPair(
              key: object.String("three"),
              value: object.Integer(3),
            ),
          ),
          #(
            object.HashKey(key_type: object.integer_obj, value: "4"),
            object.HashPair(key: object.Integer(4), value: object.Integer(4)),
          ),
          #(
            object.HashKey(key_type: object.boolean_obj, value: "true"),
            object.HashPair(key: object.Boolean(True), value: object.Integer(5)),
          ),
          #(
            object.HashKey(key_type: object.boolean_obj, value: "false"),
            object.HashPair(
              key: object.Boolean(False),
              value: object.Integer(6),
            ),
          ),
        ]),
      ),
    ),
  ]

  tests
  |> list.each(test_pair)
}

pub fn hash_index_expression_test() {
  let tests = [
    #("{\"foo\": 5}[\"foo\"]", Ok(object.Integer(5))),
    #("{\"foo\": 5}[\"bar\"]", Error(eval.KeyNotFound("bar"))),
    #("let key = \"foo\"; {\"foo\": 5}[key]", Ok(object.Integer(5))),
    #("{}[\"foo\"]", Error(eval.KeyNotFound("foo"))),
    #("{5: 5}[5]", Ok(object.Integer(5))),
    #("{true: 5}[true]", Ok(object.Integer(5))),
    #("{false: 5}[false]", Ok(object.Integer(5))),
  ]

  tests
  |> list.each(test_pair)
}
