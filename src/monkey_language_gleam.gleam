import gleam/io
import monkey/object.{type Environment, new_env}
import monkey/repl

pub fn main() {
  main_loop(new_env())
}

fn main_loop(env: Environment) {
  case repl.repl(env) {
    Ok(#(output, env)) -> {
      io.println(output)
      main_loop(env)
    }
    Error(e) -> {
      case e {
        repl.ReadingError(_) -> Nil
        _ -> {
          io.debug(e)
          main_loop(env)
        }
      }
    }
  }
}
