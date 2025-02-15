import gleam/erlang
import gleam/result
import monkey/ast
import monkey/lexer
import monkey/object.{type Environment}
import monkey/parser
import monkey/token

const prompt = ">> "

pub type ReplError {
  ReadingError(erlang.GetLineError)
  LexingError(lexer.LexingError)
  ParsingError(parser.ParserError)
  EvalError(object.EvalError)
}

pub fn repl(env: Environment) -> Result(String, ReplError) {
  use user_input <- result.try(read_line())
  use tokens <- result.try(lex(user_input))
  use program <- result.try(parse(tokens))
  use object <- result.try(eval(program, env))

  Ok(object.inspect(object))
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

fn parse(tokens: List(token.Token)) -> Result(ast.Node(ast.Program), ReplError) {
  case parser.parse(tokens) {
    Ok(program) -> Ok(program)
    Error(e) -> Error(ParsingError(e))
  }
}

fn eval(
  program: ast.Node(ast.Program),
  env: Environment,
) -> Result(object.Object, ReplError) {
  case object.eval(program, env) {
    Ok(#(object, _)) -> Ok(object)
    Error(e) -> Error(EvalError(e))
  }
}
