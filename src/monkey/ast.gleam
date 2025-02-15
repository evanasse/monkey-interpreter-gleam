import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import monkey/token.{type Token}

pub type Node(t) {
  Program(token: Token, statements: List(Node(Statement)))
  LetStatement(token: Token, name: Node(Expression), value: Node(Expression))
  ReturnStatement(token: Token, return_value: Node(Expression))
  ExpressionStatement(token: Token, expression: Node(Expression))
  Identifier(token: Token, value: String)
  IntegerLiteral(token: Token, value: Int)
  Boolean(token: Token, value: Bool)
  PrefixExpression(token: Token, operator: String, right: Node(Expression))
  InfixExpression(
    token: Token,
    left: Node(Expression),
    operator: String,
    right: Node(Expression),
  )
  IfExpression(
    token: Token,
    condition: Node(Expression),
    consequence: Node(Statement),
    alternative: Option(Node(Statement)),
  )
  BlockStatement(token: Token, statements: List(Node(Statement)))
  FunctionLiteral(
    token: Token,
    parameters: List(Node(Expression)),
    body: Node(Statement),
  )
  CallExpression(
    token: Token,
    function: Node(Expression),
    arguments: List(Node(Expression)),
  )
}

pub type Expression

pub type Statement

pub type Program

pub fn node_to_string(node: Node(t)) -> String {
  case node {
    Program(_, statements) -> program_to_string(statements)
    LetStatement(_, name, value) -> let_statement_to_string(name, value)
    ReturnStatement(_, return_value) -> return_statement_to_string(return_value)
    ExpressionStatement(_, expression) ->
      expression_statement_to_string(expression)
    Identifier(_, value) -> value
    IntegerLiteral(token, _) -> token.literal
    Boolean(token, _) -> token.literal
    PrefixExpression(_, operator, right) ->
      prefix_expression_to_string(operator, right)
    InfixExpression(_, left, operator, right) ->
      infix_expression_to_string(left, operator, right)
    IfExpression(_, condition, consequence, alternative) ->
      if_expression_to_string(condition, consequence, alternative)
    BlockStatement(_, statements) ->
      "{"
      <> statements |> list.map(statement_to_string) |> string.join("\n")
      <> "}"
    FunctionLiteral(_, parameters, body) ->
      function_literal_to_string(parameters, body)
    CallExpression(_, function, arguments) ->
      call_expression_to_string(function, arguments)
  }
}

fn program_to_string(statements: List(Node(Statement))) -> String {
  statements
  |> list.map(node_to_string)
  |> string.join("\n")
}

pub fn expression_to_string(expression: Node(Expression)) -> String {
  node_to_string(expression)
}

fn statement_to_string(expression: Node(Statement)) -> String {
  node_to_string(expression)
}

fn let_statement_to_string(
  name: Node(Expression),
  value: Node(Expression),
) -> String {
  "let"
  <> " "
  <> expression_to_string(name)
  <> " = "
  <> expression_to_string(value)
  <> ";"
}

fn return_statement_to_string(return_value: Node(Expression)) -> String {
  "return" <> expression_to_string(return_value) <> ";"
}

fn expression_statement_to_string(expression: Node(Expression)) -> String {
  expression_to_string(expression)
}

fn prefix_expression_to_string(
  operator: String,
  right: Node(Expression),
) -> String {
  "(" <> operator <> expression_to_string(right) <> ")"
}

fn infix_expression_to_string(
  left: Node(Expression),
  operator: String,
  right: Node(Expression),
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
  condition: Node(Expression),
  consequence: Node(Statement),
  alternative: Option(Node(Statement)),
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
  parameters: List(Node(Expression)),
  body: Node(Statement),
) -> String {
  "fn("
  <> parameters |> list.map(expression_to_string) |> string.join(", ")
  <> ")"
  <> statement_to_string(body)
}

fn call_expression_to_string(
  function: Node(Expression),
  arguments: List(Node(Expression)),
) -> String {
  expression_to_string(function)
  <> "("
  <> arguments |> list.map(expression_to_string) |> string.join(", ")
  <> ")"
}
