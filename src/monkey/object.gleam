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
  UnexpectedHashKeyType
  CouldNotEvaluateHashPair(details: String)
}

pub type ObjectType =
  String

pub const null_obj: ObjectType = "NULL"

pub const integer_obj: ObjectType = "INTEGER"

pub const string_obj: ObjectType = "STRING"

pub const boolean_obj: ObjectType = "BOOLEAN"

pub const return_value_obj: ObjectType = "RETURN_VALUE"

pub const error_obj: ObjectType = "ERROR"

pub const function_obj: ObjectType = "FUNCTION"

pub const builtin_obj: ObjectType = "BUILTIN"

pub const array_obj: ObjectType = "ARRAY"

pub const hash_obj: ObjectType = "HASH"

pub type Object {
  Null
  Integer(value: Int)
  String(value: String)
  Boolean(value: Bool)
  ReturnValue(value: Object)
  ErrorObj(message: String)
  Function(
    parameters: List(ast.Node(ast.Expression)),
    body: ast.Node(ast.Statement),
    env: Environment,
  )
  BuiltinFunction(func: fn(List(Object)) -> Object)
  Array(elements: List(Object))
  Hash(pairs: List(#(HashKey, HashPair)))
}

pub type HashKey {
  HashKey(key_type: ObjectType, value: String)
}

pub type HashPair {
  HashPair(key: Object, value: Object)
}

pub fn hash_key(object: Object) -> Result(HashKey, EvalError) {
  case object {
    Boolean(value) -> {
      case value {
        True -> Ok(HashKey(boolean_obj, "true"))
        False -> Ok(HashKey(boolean_obj, "false"))
      }
    }
    Integer(value) -> Ok(HashKey(integer_obj, int.to_string(value)))
    String(value) -> Ok(HashKey(string_obj, value))
    _ -> Error(UnexpectedObject)
  }
}

fn builtin_function_len(arguments: List(Object)) -> Object {
  case arguments {
    [x] ->
      case x {
        String(s) -> Integer(string.length(s))
        Array(elements) -> Integer(list.length(elements))
        _ -> ErrorObj("Argument to 'len' not supported, got " <> get_type(x))
      }
    [] | [_, ..] ->
      ErrorObj(
        "Wrong number of arguments. got: "
        <> int.to_string(list.length(arguments))
        <> ", expected: 1",
      )
  }
}

fn builtin_function_first(arguments: List(Object)) -> Object {
  case arguments {
    [x] -> {
      case x {
        Array(elements) -> {
          case elements |> list.first {
            Ok(e) -> e
            Error(_) -> ErrorObj("List is empty.")
          }
        }
        _ -> ErrorObj("Argument to 'first' not supported, got " <> get_type(x))
      }
    }
    [] | [_, ..] ->
      ErrorObj(
        "Wrong number of arguments. got: "
        <> int.to_string(list.length(arguments))
        <> ", expected: 1",
      )
  }
}

fn builtin_function_last(arguments: List(Object)) -> Object {
  case arguments {
    [x] -> {
      case x {
        Array(elements) -> {
          case elements |> list.last {
            Ok(e) -> e
            Error(_) -> ErrorObj("List is empty.")
          }
        }
        _ -> ErrorObj("Argument to 'last' not supported, got " <> get_type(x))
      }
    }
    [] | [_, ..] ->
      ErrorObj(
        "Wrong number of arguments. got: "
        <> int.to_string(list.length(arguments))
        <> ", expected: 1",
      )
  }
}

fn builtin_function_rest(arguments: List(Object)) -> Object {
  case arguments {
    [x] -> {
      case x {
        Array(elements) -> {
          case elements |> list.rest {
            Ok(e) -> Array(e)
            Error(_) -> ErrorObj("List is empty.")
          }
        }
        _ -> ErrorObj("Argument to 'rest' not supported, got " <> get_type(x))
      }
    }
    [] | [_, ..] ->
      ErrorObj(
        "Wrong number of arguments. got: "
        <> int.to_string(list.length(arguments))
        <> ", expected: 1",
      )
  }
}

fn builtin_function_push(arguments: List(Object)) -> Object {
  case arguments {
    [array, object] -> {
      case array {
        Array(elements) -> Array(elements |> list.append([object]))
        _ ->
          ErrorObj("Argument to 'push' not supported, got " <> get_type(array))
      }
    }
    [] | [_] | [_, _, ..] ->
      ErrorObj(
        "Wrong number of arguments. got: "
        <> int.to_string(list.length(arguments))
        <> ", expected: 2",
      )
  }
}

fn builtin_function_puts(arguments: List(Object)) -> Object {
  arguments
  |> list.each(fn(arg) { io.println(inspect(arg)) })

  null
}

pub fn builtin_functions() -> Dict(String, Object) {
  let builtin_functions = [
    #("len", BuiltinFunction(builtin_function_len)),
    #("first", BuiltinFunction(builtin_function_first)),
    #("last", BuiltinFunction(builtin_function_last)),
    #("rest", BuiltinFunction(builtin_function_rest)),
    #("push", BuiltinFunction(builtin_function_push)),
    #("puts", BuiltinFunction(builtin_function_puts)),
  ]

  dict.from_list(builtin_functions)
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
        None -> {
          case dict.get(builtin_functions(), name) {
            Ok(function) -> function
            Error(_) -> ErrorObj("identifier not found: " <> name)
          }
        }
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
    String(_) -> string_obj
    Boolean(_) -> boolean_obj
    ReturnValue(_) -> return_value_obj
    ErrorObj(_) -> return_value_obj
    Function(_, _, _) -> function_obj
    BuiltinFunction(_) -> builtin_obj
    Array(_) -> array_obj
    Hash(_) -> hash_obj
  }
}

pub fn inspect(object: Object) -> String {
  case object {
    Null -> "null"
    Integer(_) -> int.to_string(object.value)
    String(_) -> object.value
    Boolean(_) -> bool.to_string(object.value)
    ReturnValue(_) -> inspect(object.value)
    ErrorObj(message) -> "ERROR: " <> message
    Function(parameters, body, _) ->
      "fn("
      <> parameters |> list.map(ast.expression_to_string) |> string.join(", ")
      <> ")"
      <> ast.node_to_string(body)
    BuiltinFunction(_) -> "builtin_function"
    Array(elements) ->
      "[" <> elements |> list.map(inspect) |> string.join(", ") <> "]"
    Hash(pairs) ->
      "{"
      <> pairs
      |> list.map(fn(pair) {
        inspect({ pair.1 }.key) <> ": " <> inspect({ pair.1 }.value)
      })
      |> string.join(", ")
      <> "}"
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
    ast.StringLiteral(_, value) -> Ok(#(String(value), env))
    ast.Boolean(_, value) -> native_bool_to_boolean_object(value, env)
    ast.Identifier(_, name) -> eval_identifier(name, env)
    ast.FunctionLiteral(_, parameters, body) ->
      eval_function(parameters, body, env)
    ast.CallExpression(_, function, arguments) ->
      eval_call_expression(function, arguments, env)
    ast.ArrayLiteral(_, elements) -> eval_array_literal(elements, env)
    ast.IndexExpression(_, left, index) ->
      eval_index_expression(left, index, env)
    ast.HashLiteral(_, pairs) -> eval_hash_literal(pairs, env)
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

fn eval_expression_list(
  expressions: List(ast.Node(ast.Expression)),
  env: Environment,
) -> List(Object) {
  expressions
  |> list.fold_until([], fn(acc, expression) {
    case eval_expression(expression, env) {
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
      let args = eval_expression_list(arguments, env)
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
    BuiltinFunction(func) -> {
      let args = eval_expression_list(arguments, env)
      case args {
        [x] ->
          case x {
            ErrorObj(_) -> Ok(#(x, env))
            _ -> {
              Ok(#(func(args), env))
            }
          }
        _ -> {
          Ok(#(func(args), env))
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
    String(l), String(r) -> Ok(#(String(l <> r), env))
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

fn eval_array_literal(
  elements: List(ast.Node(ast.Expression)),
  env: Environment,
) -> Result(#(Object, Environment), EvalError) {
  let elements = eval_expression_list(elements, env)
  case elements {
    [x] ->
      case x {
        ErrorObj(_) -> Ok(#(x, env))
        _ -> {
          Ok(#(Array(elements), env))
        }
      }
    _ -> {
      Ok(#(Array(elements), env))
    }
  }
}

fn eval_index_expression(
  left: ast.Node(ast.Expression),
  index: ast.Node(ast.Expression),
  env: Environment,
) -> Result(#(Object, Environment), EvalError) {
  use #(left, env) <- result.try(eval_expression(left, env))
  use #(index, env) <- result.try(eval_expression(index, env))

  case left {
    Array(elements) -> {
      case index {
        Integer(i) -> {
          case i < list.length(elements) {
            True if i < 0 -> Ok(#(null, env))
            True -> {
              let element = elements |> list.take(i + 1) |> list.last
              case element {
                Ok(e) -> Ok(#(e, env))
                Error(_) -> {
                  Ok(#(ErrorObj("Impossible to get here."), env))
                }
              }
            }
            False -> Ok(#(null, env))
          }
        }
        _ ->
          Ok(#(ErrorObj(get_type(index) <> " cannot be used as an index."), env))
      }
    }
    Hash(pairs) -> {
      case hash_key(index) {
        Ok(hashkey) -> {
          case dict.get(dict.from_list(pairs), hashkey) {
            Ok(hash_pair) -> Ok(#(hash_pair.value, env))
            Error(_) -> Ok(#(ErrorObj("Key not found."), env))
          }
        }
        Error(_) ->
          Ok(#(
            ErrorObj(get_type(index) <> " cannot be used as a hash key."),
            env,
          ))
      }
    }
    _ -> Ok(#(ErrorObj(get_type(left) <> " is not subscriptable"), env))
  }
}

fn eval_hash_literal(
  pairs: List(#(ast.Node(ast.Expression), ast.Node(ast.Expression))),
  env: Environment,
) -> Result(#(Object, Environment), EvalError) {
  let #(evaluated_pairs, env) =
    pairs
    |> list.fold_until(#([], env), fn(acc, pair) {
      case eval_hash_pair(pair, env) {
        Ok(#(p, env)) -> Continue(#([#(p.0, p.1), ..acc.0], env))
        Error(_) -> Stop(#([], env))
      }
    })

  case pairs {
    [] -> Ok(#(Hash([]), env))
    _ ->
      case evaluated_pairs {
        [] -> Ok(#(ErrorObj("Error evaluating hash literal"), env))
        _ -> Ok(#(Hash(list.reverse(evaluated_pairs)), env))
      }
  }
}

fn eval_hash_pair(
  pair: #(ast.Node(ast.Expression), ast.Node(ast.Expression)),
  env: Environment,
) -> Result(#(#(HashKey, HashPair), Environment), EvalError) {
  use #(key, env) <- result.try(eval_expression(pair.0, env))
  case key {
    ErrorObj(_) -> Error(CouldNotEvaluateHashPair("Error evaluating key"))
    String(_) | Integer(_) | Boolean(_) -> {
      use #(value, env) <- result.try(eval_expression(pair.1, env))
      case value {
        ErrorObj(_) -> Error(CouldNotEvaluateHashPair("Error evaluating value"))
        _ -> {
          case hash_key(key) {
            Error(e) -> Error(e)
            Ok(hk) -> Ok(#(#(hk, HashPair(key, value)), env))
          }
        }
      }
    }
    _ -> Error(UnexpectedHashKeyType)
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
