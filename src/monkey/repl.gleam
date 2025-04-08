import gleam/erlang
import gleam/result
import monkey/ast
import monkey/eval
import monkey/lexer
import monkey/macro_expansion.{define_macros}
import monkey/object.{type Environment}
import monkey/parser
import monkey/parser_error
import monkey/token

const prompt = ">> "

pub type ReplError {
  ReadingError(erlang.GetLineError)
  LexingError(lexer.LexingError)
  ParsingError(parser_error.ParserError)
  EvalError(eval.EvalError)
  MacroExpansionError(macro_expansion.MacroExpansionError)
}

pub fn repl(
  main_env: Environment,
  macro_env: Environment,
) -> Result(#(String, Environment, Environment), ReplError) {
  use user_input <- result.try(read_line())
  use tokens <- result.try(lex(user_input))
  use program <- result.try(parse(tokens))
  use #(program, macro_env) <- result.try(expand_macros(program, macro_env))

  use #(object, main_env) <- result.try(eval(program, main_env))

  Ok(#(object.inspect(object), main_env, macro_env))
}

fn read_line() -> Result(String, ReplError) {
  case erlang.get_line(prompt) {
    Ok(input) -> Ok(input)
    Error(e) -> Error(ReadingError(e))
  }
}

fn lex(input: String) -> Result(List(token.Token), ReplError) {
  case lexer.lex(input) {
    Ok(tokens) -> Ok(tokens)
    Error(e) -> Error(LexingError(e))
  }
}

fn parse(tokens: List(token.Token)) -> Result(ast.Program, ReplError) {
  case parser.parse(tokens) {
    Ok(program) -> Ok(program)
    Error(e) -> Error(ParsingError(e))
  }
}

fn expand_macros(
  program: ast.Program,
  macro_env: Environment,
) -> Result(#(ast.Program, Environment), ReplError) {
  case define_macros(program, macro_env) {
    Ok(#(program, macro_env)) -> {
      let assert ast.ProgramNode(program) =
        macro_expansion.expand_macros(program, macro_env)

      Ok(#(program, macro_env))
    }
    Error(e) -> Error(MacroExpansionError(e))
  }
}

fn eval(
  program: ast.Program,
  env: Environment,
) -> Result(#(object.Object, Environment), ReplError) {
  case eval.eval_program(program, env) {
    Ok(#(object, env)) -> Ok(#(object, env))
    Error(e) -> Error(EvalError(e))
  }
}
