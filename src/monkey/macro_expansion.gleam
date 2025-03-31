import gleam/list
import gleam/result
import monkey/ast
import monkey/eval
import monkey/object
import monkey/token

pub type MacroExpansionError {
  ExpectedLetStatement(got: ast.Statement)
  ExpectedMacroLiteral(got: ast.Expression)
  ExpectedIdentifier(got: ast.Expression)
}

pub fn define_macros(
  program: ast.Program,
  env: object.Environment,
) -> Result(#(ast.Program, object.Environment), MacroExpansionError) {
  let filtered_program =
    program.statements
    |> list.try_fold(#([], env), fn(acc, statement) {
      case add_macro(statement, acc.1) {
        Ok(new_env) -> Ok(#(acc.0, new_env))
        Error(ExpectedMacroLiteral(_)) | Error(ExpectedLetStatement(_)) ->
          Ok(#([statement, ..acc.0], acc.1))
        Error(e) -> Error(e)
      }
    })

  case filtered_program {
    Ok(#(statements, env)) ->
      Ok(#(
        ast.Program(token: token.eof, statements: list.reverse(statements)),
        env,
      ))
    Error(e) -> Error(e)
  }
}

fn unpack_macro_definition(
  statement: ast.Statement,
) -> Result(#(String, List(ast.Expression), ast.Statement), MacroExpansionError) {
  case statement {
    ast.LetStatement(_, name, value) -> {
      case value {
        ast.MacroLiteral(_, parameters, body) ->
          case name {
            ast.Identifier(_, macro_name) -> Ok(#(macro_name, parameters, body))
            _ -> Error(ExpectedIdentifier(got: name))
          }
        _ -> Error(ExpectedMacroLiteral(got: value))
      }
    }
    _ -> Error(ExpectedLetStatement(got: statement))
  }
}

fn add_macro(
  statement: ast.Statement,
  env: object.Environment,
) -> Result(object.Environment, MacroExpansionError) {
  use #(name, parameters, body) <- result.try(unpack_macro_definition(statement))

  let macro_object = object.Macro(parameters, body, env)

  Ok(object.set(in: env, name: name, to: macro_object))
}

pub fn expand_macros(program: ast.Program, env: object.Environment) -> ast.Node {
  ast.modify(
    ast.ProgramNode(program),
    fn(node: ast.Expression) -> ast.Expression {
      case node {
        ast.CallExpression(_, ast.Identifier(_, identifier_value), arguments) ->
          case object.get(env, identifier_value) {
            Ok(object) ->
              case object {
                object.Macro(macro_parameters, macro_body, macro_env) -> {
                  let quoted_args =
                    arguments
                    |> list.map(fn(arg) { object.Quote(node: arg) })

                  let eval_env =
                    macro_parameters
                    |> list.map(fn(param) {
                      case param {
                        ast.Identifier(_, value) -> value
                        _ -> ""
                      }
                    })
                    |> list.zip(quoted_args)
                    |> list.fold(object.new_enclosed_env(macro_env), fn(acc, p) {
                      object.set(in: acc, name: p.0, to: p.1)
                    })

                  case eval.eval_statement(macro_body, eval_env) {
                    Ok(#(object, _)) ->
                      case object {
                        object.Quote(node) -> node
                        _ -> {
                          panic
                        }
                      }
                    _ -> node
                  }
                }
                _ -> node
              }
            _ -> node
          }
        _ -> node
      }
    },
  )
}
