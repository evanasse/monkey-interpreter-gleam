import gleam/dict
import gleam/int
import gleam/io
import gleam/list.{Continue, Stop}
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import monkey/ast
import monkey/object.{
  type Environment, type HashKey, type HashPair, type Object, Array, Boolean,
  BuiltinFunction, Function, Hash, HashPair, Integer, Null, ReturnValue, String,
}
import monkey/token

pub type EvalError {
  AdditionNotSupported(left: String, right: String)
  ArgumentNotSupported(function_name: String, argument_type: String)
  CouldNotEvaluateHashPair(details: String)
  DivisionNotSupported(left: String, right: String)
  EqualsNotSupported(left: String, right: String)
  ExpectedAProgramNode
  GreaterThanNotSupported(left: String, right: String)
  IdentifierNotFound(identifier: String)
  Impossible
  IndexingNotSupported(left: String)
  KeyNotFound(key: String)
  LesserThanNotSupported(left: String, right: String)
  ListIsEmpty
  MultiplicationNotSupported(left: String, right: String)
  NotEqualsNotSupported(left: String, right: String)
  SubstractionNotSupported(left: String, right: String)
  TypeNotSupportedAsHashKey(object_type: String)
  TypeNotSupportedAsIndex(object_type: String)
  UnexpectedHashKeyType
  UnexpectedNode
  UnexpectedObject
  UnexpectedOperator(expected_one_of: List(String), got: String)
  UnknownInfixOperator(left: String, operator: String, right: String)
  UnknownPrefixOperator(operator: String, right: String)
  WrongNumberOfArguments(expected: Int, got: Int)
}

pub fn to_string(error: EvalError) -> String {
  case error {
    AdditionNotSupported(left, right) ->
      operation_not_supported("Addition", left, right)
    ArgumentNotSupported(function_name, argument_type) ->
      "Argument of type "
      <> argument_type
      <> " not supported for '"
      <> function_name
      <> "'"
    CouldNotEvaluateHashPair(details) -> "Could evaluate hash pair: " <> details
    DivisionNotSupported(left, right) ->
      operation_not_supported("Division", left, right)
    EqualsNotSupported(left, right) ->
      operation_not_supported("Equality comparison", left, right)
    ExpectedAProgramNode -> "Expected a program node"
    GreaterThanNotSupported(left, right) ->
      operation_not_supported("Greater-than comparison", left, right)
    IdentifierNotFound(identifier) -> "Identifier not found: " <> identifier
    Impossible -> "Impossible to get here."
    IndexingNotSupported(left) -> "Indexing not supported for " <> left
    KeyNotFound(key) -> "Key not found: " <> key
    LesserThanNotSupported(left, right) ->
      operation_not_supported("Lesser-than comparison", left, right)
    ListIsEmpty -> "List is empty"
    MultiplicationNotSupported(left, right) ->
      operation_not_supported("Multiplication", left, right)
    NotEqualsNotSupported(left, right) ->
      operation_not_supported("Non-equality comparison", left, right)
    SubstractionNotSupported(left, right) ->
      operation_not_supported("Substraction", left, right)
    TypeNotSupportedAsHashKey(object_type) ->
      "Type not supported as hash key: " <> object_type
    TypeNotSupportedAsIndex(object_type) ->
      "Type not supported as index: " <> object_type
    UnexpectedHashKeyType -> "Unexpected hash key type"
    UnexpectedNode -> "Unexpected node"
    UnexpectedObject -> "Unexpected object"
    UnexpectedOperator(expected_one_of, got) ->
      "Unexpected operator. Expected one of "
      <> expected_one_of |> string.join(", ")
      <> ". Got "
      <> got
    UnknownInfixOperator(left, operator, right) ->
      "Unknown infix operator '"
      <> operator
      <> "' between "
      <> left
      <> " and "
      <> right
    UnknownPrefixOperator(operator, right) ->
      "Unknown operator '" <> operator <> "' in front of " <> right
    WrongNumberOfArguments(expected, got) ->
      "Wrong number of arguments. Expected: "
      <> int.to_string(expected)
      <> ". Got: "
      <> int.to_string(got)
  }
}

fn operation_not_supported(
  operation: String,
  left: String,
  right: String,
) -> String {
  operation <> " not supported between " <> left <> " and " <> right
}

fn eval_expression(
  expression: ast.Expression,
  env: Environment,
) -> Result(#(Object, Environment), EvalError) {
  case expression {
    ast.PrefixExpression(_, operator, right) ->
      eval_prefix_expression(operator, right, env)
    ast.InfixExpression(_, left, operator, right) ->
      eval_infix_expression(left, operator, right, env)
    ast.IfExpression(_, condition, consequence, alternative) ->
      eval_if_expression(condition, consequence, alternative, env)
    ast.IntegerLiteral(_, value) -> Ok(#(Integer(value), env))
    ast.StringLiteral(_, value) -> Ok(#(String(value), env))
    ast.BooleanLiteral(_, value) -> native_bool_to_boolean_object(value, env)
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

fn eval_statement(
  statement: ast.Statement,
  env: Environment,
) -> Result(#(Object, Environment), EvalError) {
  case statement {
    ast.ExpressionStatement(_, expression) -> eval_expression(expression, env)
    ast.ReturnStatement(_, expression) -> eval_return_statement(expression, env)
    ast.BlockStatement(_, statements) -> eval_block_statement(statements, env)
    ast.LetStatement(_, name, value) -> eval_let_statement(name, value, env)
  }
}

pub fn eval_program(
  program: ast.Program,
  env: Environment,
) -> Result(#(Object, Environment), EvalError) {
  program.statements
  |> list.fold_until(Ok(#(object.null, env)), fn(acc, statement) {
    case acc {
      Ok(#(_, env)) -> {
        case eval_statement(statement, env) {
          Ok(#(object, env)) -> {
            case object {
              ReturnValue(return_value) -> Stop(Ok(#(return_value, env)))
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
  right: ast.Expression,
  env: Environment,
) -> Result(#(Object, Environment), EvalError) {
  use #(right, env) <- result.try(eval_expression(right, env))

  case operator {
    "!" -> eval_bang_operator(right, env)
    "-" -> eval_minus_prefix_operator(right, env)
    _ ->
      Error(UnknownPrefixOperator(
        operator: operator,
        right: object.inspect(right),
      ))
  }
}

fn eval_infix_expression(
  left: ast.Expression,
  operator: String,
  right: ast.Expression,
  env: Environment,
) -> Result(#(Object, Environment), EvalError) {
  use #(left, env) <- result.try(eval_expression(left, env))
  use #(right, env) <- result.try(eval_expression(right, env))

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
      Error(UnknownInfixOperator(
        left: object.inspect(left),
        operator: operator,
        right: object.inspect(right),
      ))
  }
}

fn eval_block_statement(
  statements: List(ast.Statement),
  env: Environment,
) -> Result(#(Object, Environment), EvalError) {
  statements
  |> list.fold_until(Ok(#(object.null, env)), fn(acc, statement) {
    case acc {
      Ok(#(_, env)) -> {
        case eval_statement(statement, env) {
          Ok(#(object, env)) -> {
            case object {
              ReturnValue(_) -> Stop(Ok(#(object, env)))
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
  expression: ast.Expression,
  env: Environment,
) -> Result(#(Object, Environment), EvalError) {
  use #(return_value, env) <- result.try(eval_expression(expression, env))

  Ok(#(ReturnValue(return_value), env))
}

fn eval_expression_list(
  expressions: List(ast.Expression),
  env: Environment,
) -> Result(List(Object), EvalError) {
  let list_eval =
    expressions
    |> list.fold_until(Ok([]), fn(acc, expression) {
      let acc = result.unwrap(acc, [])
      case eval_expression(expression, env) {
        Ok(#(arg, _env)) -> Continue(Ok([arg, ..acc]))
        Error(e) -> Stop(Error(e))
      }
    })

  case list_eval {
    Ok(list) -> Ok(list.reverse(list))
    Error(e) -> Error(e)
  }
}

fn eval_call_expression(
  function: ast.Expression,
  arguments: List(ast.Expression),
  env: Environment,
) -> Result(#(Object, Environment), EvalError) {
  case function.token.literal == "quote" {
    True -> {
      case arguments |> list.first {
        Ok(x) -> Ok(#(quote(x, env), env))
        Error(_) ->
          Error(WrongNumberOfArguments(expected: 1, got: list.length(arguments)))
      }
    }
    False -> {
      use #(function_object, env) <- result.try(eval_expression(function, env))

      case function_object {
        Function(_, body, _function_env) -> {
          use args <- result.try(eval_expression_list(arguments, env))
          use extended_env <- result.try(extend_function_env(
            function_object,
            args,
          ))
          use #(evaluated, _) <- result.try(eval_statement(body, extended_env))
          Ok(#(unwrap_return_value(evaluated), env))
        }

        BuiltinFunction(func) -> {
          use args <- result.try(eval_expression_list(arguments, env))
          case func(args) {
            Ok(outcome) -> Ok(#(outcome, env))
            Error(e) -> Error(builtin_to_eval_error(e))
          }
        }
        _ -> Error(UnexpectedObject)
      }
    }
  }
}

fn builtin_to_eval_error(error: object.BuiltinFunctionError) -> EvalError {
  case error {
    object.ArgumentNotSupported(function_name, argument_type) ->
      ArgumentNotSupported(function_name, argument_type)
    object.ListIsEmpty -> ListIsEmpty
    object.WrongNumberOfArguments(expected, got) ->
      WrongNumberOfArguments(expected, got)
  }
}

fn extend_function_env(
  function: Object,
  args: List(Object),
) -> Result(Environment, EvalError) {
  case function {
    Function(parameters, _, env) -> {
      let env = object.new_enclosed_env(env)

      let extended_env =
        parameters
        |> list.zip(args)
        |> list.fold(env, fn(acc, pair) {
          let assert ast.Identifier(_, value) = pair.0
          object.set(value, pair.1, acc)
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
  condition: ast.Expression,
  consequence: ast.Statement,
  alternative: Option(ast.Statement),
  env: Environment,
) -> Result(#(Object, Environment), EvalError) {
  use #(condition, env) <- result.try(eval_expression(condition, env))

  case condition {
    Null | Boolean(_) if condition == object.false -> {
      case alternative {
        Some(block_statement) -> eval_statement(block_statement, env)
        None -> Ok(#(object.null, env))
      }
    }
    _ -> {
      eval_statement(consequence, env)
    }
  }
}

fn eval_identifier(
  name: String,
  env: Environment,
) -> Result(#(Object, Environment), EvalError) {
  case object.get(name, env) {
    Ok(o) -> Ok(#(o, env))
    Error(e) ->
      case e {
        object.IdentifierNotFound(identifier) ->
          Error(IdentifierNotFound(identifier))
      }
  }
}

fn eval_function(
  parameters: List(ast.Expression),
  body: ast.Statement,
  env: Environment,
) -> Result(#(Object, Environment), EvalError) {
  Ok(#(Function(parameters, body, env), env))
}

fn eval_let_statement(
  name: ast.Expression,
  value: ast.Expression,
  env: Environment,
) -> Result(#(Object, Environment), EvalError) {
  use #(value, env) <- result.try(eval_expression(value, env))

  case name {
    ast.Identifier(_, name) -> {
      Ok(#(object.null, object.set(name, value, env)))
    }
    _ -> Error(UnexpectedNode)
  }
}

fn eval_bang_operator(
  right: Object,
  env: Environment,
) -> Result(#(Object, Environment), EvalError) {
  case right {
    Boolean(_) if right == object.true -> Ok(#(object.false, env))
    Boolean(_) if right == object.false -> Ok(#(object.true, env))
    Boolean(_) if right == object.null -> Ok(#(object.false, env))
    _ -> Ok(#(object.false, env))
  }
}

fn eval_minus_prefix_operator(
  right: Object,
  env: Environment,
) -> Result(#(Object, Environment), EvalError) {
  case right {
    Integer(value) -> Ok(#(Integer(-value), env))
    _ ->
      Error(UnknownPrefixOperator(operator: "-", right: object.get_type(right)))
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
    _, _ -> {
      Error(AdditionNotSupported(
        left: object.get_type(left),
        right: object.get_type(right),
      ))
    }
  }
}

fn eval_substraction(
  left: Object,
  right: Object,
  env: Environment,
) -> Result(#(Object, Environment), EvalError) {
  case left, right {
    Integer(l), Integer(r) -> Ok(#(Integer(l - r), env))
    _, _ -> {
      Error(SubstractionNotSupported(
        left: object.get_type(left),
        right: object.get_type(right),
      ))
    }
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
      Error(MultiplicationNotSupported(
        left: object.get_type(left),
        right: object.get_type(right),
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
      Error(DivisionNotSupported(
        left: object.get_type(left),
        right: object.get_type(right),
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
      Error(EqualsNotSupported(
        left: object.get_type(left),
        right: object.get_type(right),
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
      Error(NotEqualsNotSupported(
        left: object.get_type(left),
        right: object.get_type(right),
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
      Error(LesserThanNotSupported(
        left: object.get_type(left),
        right: object.get_type(right),
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
      Error(GreaterThanNotSupported(
        left: object.get_type(left),
        right: object.get_type(right),
      ))
  }
}

fn eval_array_literal(
  elements: List(ast.Expression),
  env: Environment,
) -> Result(#(Object, Environment), EvalError) {
  use elements <- result.try(eval_expression_list(elements, env))

  Ok(#(Array(elements), env))
}

fn eval_index_expression(
  left: ast.Expression,
  index: ast.Expression,
  env: Environment,
) -> Result(#(Object, Environment), EvalError) {
  use #(left, env) <- result.try(eval_expression(left, env))
  use #(index, env) <- result.try(eval_expression(index, env))

  case left {
    Array(elements) -> {
      eval_array_index(elements, index, env)
    }
    Hash(pairs) -> {
      eval_hash_index(pairs, index, env)
    }
    _ -> Error(IndexingNotSupported(left: object.get_type(left)))
  }
}

fn eval_array_index(
  elements: List(Object),
  index: Object,
  env: Environment,
) -> Result(#(Object, Environment), EvalError) {
  case index {
    Integer(i) -> {
      case i < list.length(elements) {
        True if i < 0 -> Ok(#(object.null, env))
        True -> {
          let element = elements |> list.take(i + 1) |> list.last
          case element {
            Ok(e) -> Ok(#(e, env))
            Error(_) -> Error(Impossible)
          }
        }
        False -> Ok(#(object.null, env))
      }
    }
    _ -> Error(TypeNotSupportedAsIndex(object_type: object.get_type(index)))
  }
}

fn eval_hash_index(
  pairs: List(#(HashKey, HashPair)),
  index: Object,
  env: Environment,
) -> Result(#(Object, Environment), EvalError) {
  case object.hash_key(index) {
    Ok(hashkey) -> {
      case dict.get(dict.from_list(pairs), hashkey) {
        Ok(hash_pair) -> Ok(#(hash_pair.value, env))
        Error(_) -> Error(KeyNotFound(object.inspect(index)))
      }
    }
    Error(_) ->
      Error(TypeNotSupportedAsHashKey(object_type: object.get_type(index)))
  }
}

fn eval_hash_literal(
  pairs: List(#(ast.Expression, ast.Expression)),
  env: Environment,
) -> Result(#(Object, Environment), EvalError) {
  let evaluated_pairs =
    pairs
    |> list.try_fold(#([], env), fn(acc, pair) {
      case eval_hash_pair(pair, env) {
        Ok(#(pair, env)) -> Ok(#([#(pair.0, pair.1), ..acc.0], env))
        Error(e) -> Error(e)
      }
    })

  case evaluated_pairs {
    Ok(#(pairs, env)) -> Ok(#(Hash(list.reverse(pairs)), env))
    Error(e) -> Error(e)
  }
}

fn eval_hash_pair(
  pair: #(ast.Expression, ast.Expression),
  env: Environment,
) -> Result(#(#(HashKey, HashPair), Environment), EvalError) {
  use #(key, env) <- result.try(eval_expression(pair.0, env))
  case key {
    String(_) | Integer(_) | Boolean(_) -> {
      use #(value, env) <- result.try(eval_expression(pair.1, env))
      case object.hash_key(key) {
        Ok(hash_key) -> Ok(#(#(hash_key, HashPair(key, value)), env))
        Error(e) -> {
          case e {
            object.TypeNotSupportedAsHashKey(object_type) ->
              Error(TypeNotSupportedAsHashKey(object_type))
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
    True -> Ok(#(object.true, env))
    False -> Ok(#(object.false, env))
  }
}

fn quote(node: ast.Expression, env: object.Environment) -> object.Object {
  let node = eval_unquote_calls(node, env)
  object.Quote(node: node)
}

fn eval_unquote_calls(
  quoted_node: ast.Expression,
  env: object.Environment,
) -> ast.Expression {
  ast.modify_expression(quoted_node, fn(node: ast.Expression) -> ast.Expression {
    case node {
      ast.CallExpression(
        _,
        ast.Identifier(token: token.Identifier("unquote"), value: "unquote"),
        [arg],
      ) -> {
        case eval_expression(arg, env) {
          Ok(#(obj, _env)) -> convert_object_to_ast_node(obj)
          _ -> node
        }
      }
      _ -> node
    }
  })
}

fn convert_object_to_ast_node(obj: object.Object) -> ast.Expression {
  case obj {
    object.Integer(value) ->
      ast.IntegerLiteral(token: token.Integer(int.to_string(value)), value:)
    object.Boolean(value) -> {
      case value {
        True -> ast.BooleanLiteral(token: token.true, value:)
        False -> ast.BooleanLiteral(token: token.true, value:)
      }
    }
    object.Quote(node) -> node
    // TODO: should not return a dummy integerliteral
    _ -> ast.IntegerLiteral(token: token.Integer(int.to_string(0)), value: 0)
  }
}
