import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import monkey/token.{type Token}

pub type Expression {
  Identifier(token: Token, value: String)
  IntegerLiteral(token: Token, value: Int)
  StringLiteral(token: Token, value: String)
  ArrayLiteral(token: Token, elements: List(Expression))
  BooleanLiteral(token: Token, value: Bool)
  PrefixExpression(token: Token, operator: String, right: Expression)
  InfixExpression(
    token: Token,
    left: Expression,
    operator: String,
    right: Expression,
  )
  IfExpression(
    token: Token,
    condition: Expression,
    consequence: Statement,
    alternative: Option(Statement),
  )
  FunctionLiteral(token: Token, parameters: List(Expression), body: Statement)
  CallExpression(
    token: Token,
    function: Expression,
    arguments: List(Expression),
  )
  IndexExpression(token: Token, left: Expression, index: Expression)
  HashLiteral(token: Token, pairs: List(#(Expression, Expression)))
}

pub fn expression_to_string(expression: Expression) -> String {
  case expression {
    Identifier(_, value) -> value
    IntegerLiteral(token, _) -> token.literal
    StringLiteral(token, _) -> token.literal
    BooleanLiteral(token, _) -> token.literal
    PrefixExpression(_, operator, right) ->
      prefix_expression_to_string(operator, right)
    InfixExpression(_, left, operator, right) ->
      infix_expression_to_string(left, operator, right)
    IfExpression(_, condition, consequence, alternative) ->
      if_expression_to_string(condition, consequence, alternative)
    FunctionLiteral(_, parameters, body) ->
      function_literal_to_string(parameters, body)
    CallExpression(_, function, arguments) ->
      call_expression_to_string(function, arguments)
    ArrayLiteral(_, elements) -> array_literal_to_string(elements)
    IndexExpression(_, left, index) -> index_expression_to_string(left, index)
    HashLiteral(_, pairs) -> hash_literal_to_string(pairs)
  }
}

pub type Statement {
  LetStatement(token: Token, name: Expression, value: Expression)
  ReturnStatement(token: Token, return_value: Expression)
  ExpressionStatement(token: Token, expression: Expression)
  BlockStatement(token: Token, statements: List(Statement))
}

pub fn statement_to_string(statement: Statement) -> String {
  case statement {
    LetStatement(_, name, value) -> let_statement_to_string(name, value)
    ReturnStatement(_, return_value) -> return_statement_to_string(return_value)
    ExpressionStatement(_, expression) ->
      expression_statement_to_string(expression)
    BlockStatement(_, statements) ->
      "{"
      <> statements |> list.map(statement_to_string) |> string.join("\n")
      <> "}"
  }
}

pub type Program {
  Program(token: Token, statements: List(Statement))
}

pub fn program_to_string(program: Program) -> String {
  program.statements
  |> list.map(statement_to_string)
  |> string.join("\n")
}

fn let_statement_to_string(name: Expression, value: Expression) -> String {
  "let"
  <> " "
  <> expression_to_string(name)
  <> " = "
  <> expression_to_string(value)
  <> ";"
}

fn return_statement_to_string(return_value: Expression) -> String {
  "return" <> expression_to_string(return_value) <> ";"
}

fn expression_statement_to_string(expression: Expression) -> String {
  expression_to_string(expression)
}

fn prefix_expression_to_string(operator: String, right: Expression) -> String {
  "(" <> operator <> expression_to_string(right) <> ")"
}

fn infix_expression_to_string(
  left: Expression,
  operator: String,
  right: Expression,
) -> String {
  "("
  <> expression_to_string(left)
  <> " "
  <> operator
  <> " "
  <> expression_to_string(right)
  <> ")"
}

fn if_expression_to_string(
  condition: Expression,
  consequence: Statement,
  alternative: Option(Statement),
) -> String {
  "if"
  <> " "
  <> expression_to_string(condition)
  <> " "
  <> statement_to_string(consequence)
  <> case alternative {
    Some(alt) -> " else" <> " " <> statement_to_string(alt)
    None -> ""
  }
}

fn function_literal_to_string(
  parameters: List(Expression),
  body: Statement,
) -> String {
  "fn("
  <> parameters |> list.map(expression_to_string) |> string.join(", ")
  <> ")"
  <> statement_to_string(body)
}

fn call_expression_to_string(
  function: Expression,
  arguments: List(Expression),
) -> String {
  expression_to_string(function)
  <> "("
  <> arguments |> list.map(expression_to_string) |> string.join(", ")
  <> ")"
}

fn array_literal_to_string(elements: List(Expression)) -> String {
  "[" <> elements |> list.map(expression_to_string) |> string.join(", ") <> "]"
}

fn index_expression_to_string(left: Expression, index: Expression) -> String {
  "("
  <> expression_to_string(left)
  <> "["
  <> expression_to_string(index)
  <> "])"
}

fn hash_literal_to_string(pairs: List(#(Expression, Expression))) -> String {
  "{"
  <> pairs
  |> list.map(fn(pair) {
    expression_to_string(pair.0) <> ": " <> expression_to_string(pair.1)
  })
  |> string.join(", ")
  <> "}"
}
