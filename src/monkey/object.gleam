import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/list.{Continue, Stop}
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import monkey/ast

pub type EvalError {
  ExpectedAProgramNode
  UnexpectedNode
  UnexpectedOperator(expected_one_of: List(String), got: String)
  UnexpectedObject
}

pub type ObjectType =
  String

pub const null_obj: ObjectType = "NULL"

pub const integer_obj: ObjectType = "INTEGER"

pub const boolean_obj: ObjectType = "BOOLEAN"

pub const return_value_obj: ObjectType = "RETURN_VALUE"

pub const error_obj: ObjectType = "ERROR"

pub const function_obj: ObjectType = "FUNCTION"

pub type Object {
  Null
  Integer(value: Int)
  Boolean(value: Bool)
  ReturnValue(value: Object)
  ErrorObj(message: String)
  Function(
    parameters: List(ast.Node(ast.Expression)),
    body: ast.Node(ast.Statement),
    env: Environment,
  )
}

pub type Environment {
  Environment(store: Dict(String, Object), outer: Option(Environment))
}

pub fn new_env() -> Environment {
  Environment(dict.new(), None)
}

pub fn new_enclosed_env(outer: Environment) -> Environment {
  Environment(dict.new(), Some(outer))
}

pub fn get(name name: String, from env: Environment) -> Object {
  case env.store |> dict.get(name) {
    Ok(o) -> o
    Error(_) -> {
      case env.outer {
        Some(outer) -> get(name, outer)
        None -> ErrorObj("identifier not found: " <> name)
      }
    }
  }
}

pub fn set(
  name name: String,
  to value: Object,
  in env: Environment,
) -> Environment {
  Environment(dict.insert(env.store, name, value), env.outer)
}

pub const null = Null

pub const true = Boolean(True)

pub const false = Boolean(False)

pub fn get_type(object: Object) -> ObjectType {
  case object {
    Null -> null_obj
    Integer(_) -> integer_obj
    Boolean(_) -> boolean_obj
    ReturnValue(_) -> return_value_obj
    ErrorObj(_) -> return_value_obj
    Function(_, _, _) -> function_obj
  }
}

pub fn inspect(object: Object) -> String {
  case object {
    Null -> "null"
    Integer(_) -> int.to_string(object.value)
    Boolean(_) -> bool.to_string(object.value)
    ReturnValue(_) -> inspect(object.value)
    ErrorObj(message) -> "ERROR: " <> message
    Function(parameters, body, _) ->
      "fn("
      <> parameters |> list.map(ast.expression_to_string) |> string.join(", ")
      <> ")"
      <> ast.node_to_string(body)
  }
}

pub fn eval(
  node: ast.Node(t),
  env: Environment,
) -> Result(#(Object, Environment), EvalError) {
  case node {
    ast.Program(_, statements) -> eval_program(statements, env)
    ast.ExpressionStatement(_, expression) -> eval_expression(expression, env)
    ast.ReturnStatement(_, expression) -> eval_return_statement(expression, env)
    ast.BlockStatement(_, statements) -> eval_block_statement(statements, env)
    ast.LetStatement(_, name, value) -> eval_let_statement(name, value, env)
    ast.PrefixExpression(_, operator, right) ->
      eval_prefix_expression(operator, right, env)
    ast.InfixExpression(_, left, operator, right) ->
      eval_infix_expression(left, operator, right, env)
    ast.IfExpression(_, condition, consequence, alternative) ->
      eval_if_expression(condition, consequence, alternative, env)
    ast.IntegerLiteral(_, value) -> Ok(#(Integer(value), env))
    ast.Boolean(_, value) -> native_bool_to_boolean_object(value, env)
    ast.Identifier(_, name) -> eval_identifier(name, env)
    ast.FunctionLiteral(_, parameters, body) ->
      eval_function(parameters, body, env)
    ast.CallExpression(_, function, arguments) ->
      eval_call_expression(function, arguments, env)
  }
}

fn eval_expression(
  expression: ast.Node(ast.Expression),
  env: Environment,
) -> Result(#(Object, Environment), EvalError) {
  eval(expression, env)
}

fn eval_statement(
  statement: ast.Node(ast.Statement),
  env: Environment,
) -> Result(#(Object, Environment), EvalError) {
  eval(statement, env)
}

fn eval_program(
  statements: List(ast.Node(ast.Statement)),
  env: Environment,
) -> Result(#(Object, Environment), EvalError) {
  statements
  |> list.fold_until(Ok(#(null, env)), fn(acc, statement) {
    case acc {
      Ok(#(_, env)) -> {
        case eval_statement(statement, env) {
          Ok(#(object, env)) -> {
            case object {
              ReturnValue(return_value) -> Stop(Ok(#(return_value, env)))
              ErrorObj(_) -> Stop(Ok(#(object, env)))
              _ -> Continue(Ok(#(object, env)))
            }
          }
          Error(e) -> Stop(Error(e))
        }
      }
      Error(e) -> Stop(Error(e))
    }
  })
}

fn eval_prefix_expression(
  operator: String,
  right: ast.Node(ast.Expression),
  env: Environment,
) -> Result(#(Object, Environment), EvalError) {
  use #(right, env) <- result.try(eval_expression(right, env))

  case right {
    ErrorObj(_) -> Ok(#(right, env))
    _ -> {
      case operator {
        "!" -> eval_bang_operator(right, env)
        "-" -> eval_minus_prefix_operator(right, env)
        _ ->
          Ok(#(
            ErrorObj("unknown operator: " <> operator <> inspect(right)),
            env,
          ))
      }
    }
  }
}

fn eval_infix_expression(
  left: ast.Node(ast.Expression),
  operator: String,
  right: ast.Node(ast.Expression),
  env: Environment,
) -> Result(#(Object, Environment), EvalError) {
  use #(left, env) <- result.try(eval_expression(left, env))

  case left {
    ErrorObj(_) -> Ok(#(left, env))
    _ -> {
      use #(right, env) <- result.try(eval_expression(right, env))

      case right {
        ErrorObj(_) -> Ok(#(right, env))
        _ -> {
          case operator {
            "+" -> eval_addition(left, right, env)
            "-" -> eval_substraction(left, right, env)
            "*" -> eval_multiplication(left, right, env)
            "/" -> eval_division(left, right, env)
            "==" -> eval_equals(left, right, env)
            "!=" -> eval_not_equals(left, right, env)
            "<" -> eval_lesser_than(left, right, env)
            ">" -> eval_greater_than(left, right, env)
            _ ->
              Ok(#(
                ErrorObj(
                  "unknown operator: "
                  <> inspect(left)
                  <> " "
                  <> operator
                  <> " "
                  <> inspect(right),
                ),
                env,
              ))
          }
        }
      }
    }
  }
}

fn eval_block_statement(
  statements: List(ast.Node(ast.Statement)),
  env: Environment,
) -> Result(#(Object, Environment), EvalError) {
  statements
  |> list.fold_until(Ok(#(null, env)), fn(acc, statement) {
    case acc {
      Ok(#(_, env)) -> {
        case eval_statement(statement, env) {
          Ok(#(object, env)) -> {
            case object {
              ReturnValue(_) | ErrorObj(_) -> Stop(Ok(#(object, env)))
              _ -> Continue(Ok(#(object, env)))
            }
          }
          Error(e) -> Stop(Error(e))
        }
      }
      Error(e) -> Stop(Error(e))
    }
  })
}

fn eval_return_statement(
  expression: ast.Node(ast.Expression),
  env: Environment,
) -> Result(#(Object, Environment), EvalError) {
  use #(return_value, env) <- result.try(eval_expression(expression, env))

  case return_value {
    ErrorObj(_) -> Ok(#(return_value, env))
    _ -> Ok(#(ReturnValue(return_value), env))
  }
}

fn eval_call_expression(
  function: ast.Node(ast.Expression),
  arguments: List(ast.Node(ast.Expression)),
  env: Environment,
) -> Result(#(Object, Environment), EvalError) {
  use #(function, env) <- result.try(eval_expression(function, env))

  case function {
    ErrorObj(_) -> Ok(#(function, env))
    Function(_, body, _function_env) -> {
      let args =
        arguments
        |> list.fold_until([], fn(acc, argument) {
          case eval_expression(argument, env) {
            Ok(#(arg, _env)) -> {
              case arg {
                ErrorObj(_) -> Stop([arg])
                _ -> Continue([arg, ..acc])
              }
            }
            _ -> Stop(acc)
          }
        })
        |> list.reverse

      case args {
        [x] ->
          case x {
            ErrorObj(_) -> Ok(#(x, env))
            _ -> {
              use extended_env <- result.try(extend_function_env(function, args))
              use #(evaluated, _) <- result.try(eval(body, extended_env))
              Ok(#(unwrap_return_value(evaluated), env))
            }
          }
        _ -> {
          use extended_env <- result.try(extend_function_env(function, args))
          use #(evaluated, _) <- result.try(eval(body, extended_env))
          Ok(#(unwrap_return_value(evaluated), env))
        }
      }
    }
    _ -> Error(UnexpectedObject)
  }
}

fn extend_function_env(
  function: Object,
  args: List(Object),
) -> Result(Environment, EvalError) {
  case function {
    Function(parameters, _, env) -> {
      let env = new_enclosed_env(env)

      let extended_env =
        parameters
        |> list.zip(args)
        |> list.fold(env, fn(acc, pair) {
          let assert ast.Identifier(_, value) = pair.0
          set(value, pair.1, acc)
        })

      Ok(extended_env)
    }
    _ -> Error(UnexpectedObject)
  }
}

fn unwrap_return_value(obj: Object) -> Object {
  case obj {
    ReturnValue(o) -> o
    _ -> obj
  }
}

fn eval_if_expression(
  condition: ast.Node(ast.Expression),
  consequence: ast.Node(ast.Statement),
  alternative: Option(ast.Node(ast.Statement)),
  env: Environment,
) -> Result(#(Object, Environment), EvalError) {
  use #(condition, env) <- result.try(eval_expression(condition, env))

  case condition {
    ErrorObj(_) -> Ok(#(condition, env))
    _ -> {
      case condition {
        Null | Boolean(_) if condition == false -> {
          case alternative {
            Some(block_statement) -> eval(block_statement, env)
            None -> Ok(#(null, env))
          }
        }
        _ -> {
          eval(consequence, env)
        }
      }
    }
  }
}

fn eval_identifier(
  name: String,
  env: Environment,
) -> Result(#(Object, Environment), EvalError) {
  Ok(#(get(name, env), env))
}

fn eval_function(
  parameters: List(ast.Node(ast.Expression)),
  body: ast.Node(ast.Statement),
  env: Environment,
) -> Result(#(Object, Environment), EvalError) {
  Ok(#(Function(parameters, body, env), env))
}

fn eval_let_statement(
  name: ast.Node(ast.Expression),
  value: ast.Node(ast.Expression),
  env: Environment,
) -> Result(#(Object, Environment), EvalError) {
  use #(value, env) <- result.try(eval_expression(value, env))
  case value {
    ErrorObj(_) -> Ok(#(value, env))
    _ -> {
      case name {
        ast.Identifier(_, name) -> {
          Ok(#(null, set(name, value, env)))
        }
        _ -> Error(UnexpectedNode)
      }
    }
  }
}

fn eval_bang_operator(
  right: Object,
  env: Environment,
) -> Result(#(Object, Environment), EvalError) {
  case right {
    Boolean(_) if right == true -> Ok(#(false, env))
    Boolean(_) if right == false -> Ok(#(true, env))
    Boolean(_) if right == null -> Ok(#(false, env))
    _ -> Ok(#(false, env))
  }
}

fn eval_minus_prefix_operator(
  right: Object,
  env: Environment,
) -> Result(#(Object, Environment), EvalError) {
  case right {
    Integer(value) -> Ok(#(Integer(-value), env))
    _ -> Ok(#(ErrorObj("unknown operator: -" <> get_type(right)), env))
  }
}

fn eval_addition(
  left: Object,
  right: Object,
  env: Environment,
) -> Result(#(Object, Environment), EvalError) {
  case left, right {
    Integer(l), Integer(r) -> Ok(#(Integer(l + r), env))
    _, _ ->
      Ok(#(
        ErrorObj(
          "addition not supported: "
          <> get_type(left)
          <> " + "
          <> get_type(right),
        ),
        env,
      ))
  }
}

fn eval_substraction(
  left: Object,
  right: Object,
  env: Environment,
) -> Result(#(Object, Environment), EvalError) {
  case left, right {
    Integer(l), Integer(r) -> Ok(#(Integer(l - r), env))
    _, _ ->
      Ok(#(
        ErrorObj(
          "substraction not supported: "
          <> get_type(left)
          <> " - "
          <> get_type(right),
        ),
        env,
      ))
  }
}

fn eval_multiplication(
  left: Object,
  right: Object,
  env: Environment,
) -> Result(#(Object, Environment), EvalError) {
  case left, right {
    Integer(l), Integer(r) -> Ok(#(Integer(l * r), env))
    _, _ ->
      Ok(#(
        ErrorObj(
          "multiplication not supported: "
          <> get_type(left)
          <> " * "
          <> get_type(right),
        ),
        env,
      ))
  }
}

fn eval_division(
  left: Object,
  right: Object,
  env: Environment,
) -> Result(#(Object, Environment), EvalError) {
  case left, right {
    Integer(l), Integer(r) -> Ok(#(Integer(l / r), env))
    _, _ ->
      Ok(#(
        ErrorObj(
          "division not supported: "
          <> get_type(left)
          <> " / "
          <> get_type(right),
        ),
        env,
      ))
  }
}

fn eval_equals(
  left: Object,
  right: Object,
  env: Environment,
) -> Result(#(Object, Environment), EvalError) {
  case left, right {
    Integer(l), Integer(r) -> native_bool_to_boolean_object(l == r, env)
    Boolean(l), Boolean(r) -> native_bool_to_boolean_object(l == r, env)
    _, _ ->
      Ok(#(
        ErrorObj(
          "equals not supported: "
          <> get_type(left)
          <> " == "
          <> get_type(right),
        ),
        env,
      ))
  }
}

fn eval_not_equals(
  left: Object,
  right: Object,
  env: Environment,
) -> Result(#(Object, Environment), EvalError) {
  case left, right {
    Integer(l), Integer(r) -> native_bool_to_boolean_object(l != r, env)
    Boolean(l), Boolean(r) -> native_bool_to_boolean_object(l != r, env)
    _, _ ->
      Ok(#(
        ErrorObj(
          "not equals not supported: "
          <> get_type(left)
          <> " != "
          <> get_type(right),
        ),
        env,
      ))
  }
}

fn eval_lesser_than(
  left: Object,
  right: Object,
  env: Environment,
) -> Result(#(Object, Environment), EvalError) {
  case left, right {
    Integer(l), Integer(r) -> native_bool_to_boolean_object(l < r, env)
    _, _ ->
      Ok(#(
        ErrorObj(
          "lesser than not supported: "
          <> get_type(left)
          <> " < "
          <> get_type(right),
        ),
        env,
      ))
  }
}

fn eval_greater_than(
  left: Object,
  right: Object,
  env: Environment,
) -> Result(#(Object, Environment), EvalError) {
  case left, right {
    Integer(l), Integer(r) -> native_bool_to_boolean_object(l > r, env)
    _, _ ->
      Ok(#(
        ErrorObj(
          "greater than not supported: "
          <> get_type(left)
          <> " > "
          <> get_type(right),
        ),
        env,
      ))
  }
}

fn native_bool_to_boolean_object(
  input: Bool,
  env: Environment,
) -> Result(#(Object, Environment), EvalError) {
  case input {
    True -> Ok(#(true, env))
    False -> Ok(#(false, env))
  }
}
