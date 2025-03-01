import gleam/bool
import gleam/dict.{type Dict}
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import monkey/ast

pub type ObjectType =
  String

pub const null_obj: ObjectType = "NULL"

pub const integer_obj: ObjectType = "INTEGER"

pub const string_obj: ObjectType = "STRING"

pub const boolean_obj: ObjectType = "BOOLEAN"

pub const return_value_obj: ObjectType = "RETURN_VALUE"

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
  Function(
    parameters: List(ast.Node(ast.Expression)),
    body: ast.Node(ast.Statement),
    env: Environment,
  )
  BuiltinFunction(
    func: fn(List(Object)) -> Result(Object, BuiltinFunctionError),
  )
  Array(elements: List(Object))
  Hash(pairs: List(#(HashKey, HashPair)))
}

pub type BuiltinFunctionError {
  ArgumentNotSupported(function_name: String, argument_type: String)
  ListIsEmpty
  WrongNumberOfArguments(expected: Int, got: Int)
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

pub type HashKey {
  HashKey(key_type: ObjectType, value: String)
}

pub type HashKeyError {
  TypeNotSupportedAsHashKey(object_type: ObjectType)
}

pub type HashPair {
  HashPair(key: Object, value: Object)
}

pub fn hash_key(object: Object) -> Result(HashKey, HashKeyError) {
  case object {
    Boolean(value) -> {
      case value {
        True -> Ok(HashKey(boolean_obj, "true"))
        False -> Ok(HashKey(boolean_obj, "false"))
      }
    }
    Integer(value) -> Ok(HashKey(integer_obj, int.to_string(value)))
    String(value) -> Ok(HashKey(string_obj, value))
    _ -> Error(TypeNotSupportedAsHashKey(get_type(object)))
  }
}

fn builtin_function_len(
  arguments: List(Object),
) -> Result(Object, BuiltinFunctionError) {
  case arguments {
    [x] ->
      case x {
        String(s) -> Ok(Integer(string.length(s)))
        Array(elements) -> Ok(Integer(list.length(elements)))
        _ ->
          Error(ArgumentNotSupported(
            function_name: "len",
            argument_type: get_type(x),
          ))
      }
    [] | [_, ..] ->
      Error(WrongNumberOfArguments(expected: 1, got: list.length(arguments)))
  }
}

fn builtin_function_first(
  arguments: List(Object),
) -> Result(Object, BuiltinFunctionError) {
  case arguments {
    [x] -> {
      case x {
        Array(elements) -> {
          case elements |> list.first {
            Ok(e) -> Ok(e)
            Error(_) -> Error(ListIsEmpty)
          }
        }
        _ ->
          Error(ArgumentNotSupported(
            function_name: "first",
            argument_type: get_type(x),
          ))
      }
    }
    [] | [_, ..] ->
      Error(WrongNumberOfArguments(expected: 1, got: list.length(arguments)))
  }
}

fn builtin_function_last(
  arguments: List(Object),
) -> Result(Object, BuiltinFunctionError) {
  case arguments {
    [x] -> {
      case x {
        Array(elements) -> {
          case elements |> list.last {
            Ok(e) -> Ok(e)
            Error(_) -> Error(ListIsEmpty)
          }
        }
        _ ->
          Error(ArgumentNotSupported(
            function_name: "last",
            argument_type: get_type(x),
          ))
      }
    }
    [] | [_, ..] ->
      Error(WrongNumberOfArguments(expected: 1, got: list.length(arguments)))
  }
}

fn builtin_function_rest(
  arguments: List(Object),
) -> Result(Object, BuiltinFunctionError) {
  case arguments {
    [x] -> {
      case x {
        Array(elements) -> {
          case elements |> list.rest {
            Ok(e) -> Ok(Array(e))
            Error(_) -> Error(ListIsEmpty)
          }
        }
        _ ->
          Error(ArgumentNotSupported(
            function_name: "rest",
            argument_type: get_type(x),
          ))
      }
    }
    [] | [_, ..] ->
      Error(WrongNumberOfArguments(expected: 1, got: list.length(arguments)))
  }
}

fn builtin_function_push(
  arguments: List(Object),
) -> Result(Object, BuiltinFunctionError) {
  case arguments {
    [array, object] -> {
      case array {
        Array(elements) -> Ok(Array(elements |> list.append([object])))
        _ ->
          Error(ArgumentNotSupported(
            function_name: "push",
            argument_type: get_type(array),
          ))
      }
    }
    [] | [_] | [_, _, ..] ->
      Error(WrongNumberOfArguments(expected: 2, got: list.length(arguments)))
  }
}

fn builtin_function_puts(
  arguments: List(Object),
) -> Result(Object, BuiltinFunctionError) {
  arguments
  |> list.each(fn(arg) { io.println(inspect(arg)) })

  Ok(null)
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

pub type EnvironmentError {
  IdentifierNotFound(identifier: String)
}

pub fn new_env() -> Environment {
  Environment(dict.new(), None)
}

pub fn new_enclosed_env(outer: Environment) -> Environment {
  Environment(dict.new(), Some(outer))
}

pub fn get(
  name name: String,
  from env: Environment,
) -> Result(Object, EnvironmentError) {
  case env.store |> dict.get(name) {
    Ok(o) -> Ok(o)
    Error(_) -> {
      case env.outer {
        Some(outer) -> get(name, outer)
        None -> {
          case dict.get(builtin_functions(), name) {
            Ok(function) -> Ok(function)
            Error(_) -> Error(IdentifierNotFound(name))
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
