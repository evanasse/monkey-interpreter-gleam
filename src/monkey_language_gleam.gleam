import gleam/io
import monkey/eval
import monkey/object.{type Environment, new_env}
import monkey/repl

pub fn main() {
  main_loop(main_env: new_env(), macro_env: new_env())
}

fn main_loop(main_env main_env: Environment, macro_env macro_env: Environment) {
  case repl.repl(main_env, macro_env) {
    Ok(#(output, main_env, macro_env)) -> {
      io.println(output)
      main_loop(main_env, macro_env)
    }
    Error(e) -> {
      case e {
        repl.ReadingError(_) -> Nil
        repl.EvalError(e) -> {
          eval.to_string(e) |> io.println
          main_loop(main_env, macro_env)
        }
        _ -> {
          io.debug(e)
          main_loop(main_env, macro_env)
        }
      }
    }
  }
}
