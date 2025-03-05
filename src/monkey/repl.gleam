import gleam/erlang
import gleam/result
import monkey/ast
import monkey/eval
import monkey/lexer
import monkey/object.{type Environment}
import monkey/parser
import monkey/token

const prompt = ">> "

pub type ReplError {
  ReadingError(erlang.GetLineError)
  LexingError(lexer.LexingError)
  ParsingError(parser.ParserError)
  EvalError(eval.EvalError)
}

pub fn repl(env: Environment) -> Result(#(String, Environment), ReplError) {
  use user_input <- result.try(read_line())
  use tokens <- result.try(lex(user_input))
  use program <- result.try(parse(tokens))
  use #(object, env) <- result.try(eval(program, env))

  Ok(#(object.inspect(object), env))
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

fn eval(
  program: ast.Program,
  env: Environment,
) -> Result(#(object.Object, Environment), ReplError) {
  case eval.eval_program(program, env) {
    Ok(#(object, env)) -> Ok(#(object, env))
    Error(e) -> Error(EvalError(e))
  }
}
