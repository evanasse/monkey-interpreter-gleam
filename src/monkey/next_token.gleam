import gleam/int
import gleam/list
import gleam/result
import monkey/parser_error.{type ParserError}
import monkey/token.{type Token}

pub fn get_next_token(
  tokens: List(Token),
) -> Result(#(Token, List(Token)), ParserError) {
  case tokens |> list.pop(fn(_) { True }) {
    Ok(#(token, remaining_tokens)) -> Ok(#(token, remaining_tokens))
    Error(_) -> Error(parser_error.ExpectedAToken)
  }
}

pub fn expect_next_token(
  tokens: List(Token),
  check_function: fn(Token, List(Token)) ->
    Result(#(Token, token_value, List(Token)), ParserError),
) -> Result(#(Token, token_value, List(Token)), ParserError) {
  use #(next_token, remaining_tokens) <- result.try(get_next_token(tokens))
  check_function(next_token, remaining_tokens)
}

pub fn is_identifier(
  next_token: Token,
  remaining_tokens: List(Token),
) -> Result(#(Token, String, List(Token)), ParserError) {
  case next_token {
    token.Identifier(_) ->
      Ok(#(next_token, next_token.literal, remaining_tokens))
    _ ->
      Error(parser_error.UnexpectedToken(
        expected_one_of: [token.Identifier("some_identifier")],
        got: next_token,
      ))
  }
}

pub fn is_integer(
  next_token: Token,
  remaining_tokens: List(Token),
) -> Result(#(Token, Int, List(Token)), ParserError) {
  case next_token {
    token.Integer(_) -> {
      case int.parse(next_token.literal) {
        Ok(i) -> Ok(#(next_token, i, remaining_tokens))
        Error(_) -> Error(parser_error.CannotParseInteger(next_token.literal))
      }
    }
    _ ->
      Error(parser_error.UnexpectedToken(
        expected_one_of: [token.Integer("some_integer")],
        got: next_token,
      ))
  }
}

pub fn is_string(
  next_token: Token,
  remaining_tokens: List(Token),
) -> Result(#(Token, String, List(Token)), ParserError) {
  case next_token {
    token.String(s) -> Ok(#(next_token, s, remaining_tokens))
    _ ->
      Error(parser_error.UnexpectedToken(
        expected_one_of: [token.String("some_string")],
        got: next_token,
      ))
  }
}

pub fn is_boolean(
  next_token: Token,
  remaining_tokens: List(Token),
) -> Result(#(Token, Bool, List(Token)), ParserError) {
  case next_token {
    token.TRUE(_) -> Ok(#(next_token, True, remaining_tokens))
    token.FALSE(_) -> Ok(#(next_token, False, remaining_tokens))
    _ ->
      Error(parser_error.UnexpectedToken(
        expected_one_of: [token.true, token.false],
        got: next_token,
      ))
  }
}

pub fn is_assign(
  next_token: Token,
  remaining_tokens: List(Token),
) -> Result(#(Token, String, List(Token)), ParserError) {
  case next_token {
    token.Assign(literal) -> Ok(#(next_token, literal, remaining_tokens))
    _ ->
      Error(parser_error.UnexpectedToken(
        expected_one_of: [token.assign],
        got: next_token,
      ))
  }
}

pub fn is_prefix_operator(
  next_token: Token,
  remaining_tokens: List(Token),
) -> Result(#(Token, String, List(Token)), ParserError) {
  case next_token {
    token.Bang(literal) | token.Minus(literal) ->
      Ok(#(next_token, literal, remaining_tokens))
    _ ->
      Error(parser_error.UnexpectedToken(
        expected_one_of: [token.bang, token.minus],
        got: next_token,
      ))
  }
}

pub fn is_infix_operator(
  next_token: Token,
  remaining_tokens: List(Token),
) -> Result(#(Token, String, List(Token)), ParserError) {
  case next_token {
    token.Plus(literal)
    | token.Minus(literal)
    | token.Asterisk(literal)
    | token.Slash(literal)
    | token.Equals(literal)
    | token.NotEquals(literal)
    | token.LesserThan(literal)
    | token.GreaterThan(literal) -> Ok(#(next_token, literal, remaining_tokens))
    _ ->
      Error(parser_error.UnexpectedToken(
        expected_one_of: [
          token.plus,
          token.minus,
          token.asterisk,
          token.slash,
          token.equals,
          token.not_equals,
          token.lesser_than,
          token.greater_than,
        ],
        got: next_token,
      ))
  }
}

pub fn is_if_token(
  next_token: Token,
  remaining_tokens: List(Token),
) -> Result(#(Token, String, List(Token)), ParserError) {
  case next_token {
    token.If(literal) -> Ok(#(next_token, literal, remaining_tokens))
    _ ->
      Error(parser_error.UnexpectedToken(
        expected_one_of: [token.if_],
        got: next_token,
      ))
  }
}

pub fn is_left_parenthesis(
  next_token: Token,
  remaining_tokens: List(Token),
) -> Result(#(Token, String, List(Token)), ParserError) {
  case next_token {
    token.LeftParenthesis(literal) ->
      Ok(#(next_token, literal, remaining_tokens))
    _ ->
      Error(parser_error.UnexpectedToken(
        expected_one_of: [token.l_paren],
        got: next_token,
      ))
  }
}

pub fn is_left_bracket(
  next_token: Token,
  remaining_tokens: List(Token),
) -> Result(#(Token, String, List(Token)), ParserError) {
  case next_token {
    token.LeftBracket(literal) -> Ok(#(next_token, literal, remaining_tokens))
    _ ->
      Error(parser_error.UnexpectedToken(
        expected_one_of: [token.l_bracket],
        got: next_token,
      ))
  }
}

pub fn is_right_bracket(
  next_token: Token,
  remaining_tokens: List(Token),
) -> Result(#(Token, String, List(Token)), ParserError) {
  case next_token {
    token.RightBracket(literal) -> Ok(#(next_token, literal, remaining_tokens))
    _ ->
      Error(parser_error.UnexpectedToken(
        expected_one_of: [token.r_bracket],
        got: next_token,
      ))
  }
}

pub fn is_left_brace(
  next_token: Token,
  remaining_tokens: List(Token),
) -> Result(#(Token, String, List(Token)), ParserError) {
  case next_token {
    token.LeftBrace(literal) -> Ok(#(next_token, literal, remaining_tokens))
    _ ->
      Error(parser_error.UnexpectedToken(
        expected_one_of: [token.l_brace],
        got: next_token,
      ))
  }
}

pub fn is_colon(
  next_token: Token,
  remaining_tokens: List(Token),
) -> Result(#(Token, String, List(Token)), ParserError) {
  case next_token {
    token.Colon(literal) -> Ok(#(next_token, literal, remaining_tokens))
    _ ->
      Error(parser_error.UnexpectedToken(
        expected_one_of: [token.colon],
        got: next_token,
      ))
  }
}

pub fn is_function_token(
  next_token: Token,
  remaining_tokens: List(Token),
) -> Result(#(Token, String, List(Token)), ParserError) {
  case next_token {
    token.Function(literal) -> Ok(#(next_token, literal, remaining_tokens))
    _ ->
      Error(parser_error.UnexpectedToken(
        expected_one_of: [token.function],
        got: next_token,
      ))
  }
}

pub fn is_macro_token(
  next_token: Token,
  remaining_tokens: List(Token),
) -> Result(#(Token, String, List(Token)), ParserError) {
  case next_token {
    token.Macro(literal) -> Ok(#(next_token, literal, remaining_tokens))
    _ ->
      Error(parser_error.UnexpectedToken(
        expected_one_of: [token.macro_],
        got: next_token,
      ))
  }
}

pub fn may_be_semicolon(tokens: List(Token)) -> List(Token) {
  case get_next_token(tokens) {
    Ok(#(next_token, remaining_tokens)) -> {
      case next_token {
        token.Semicolon(_) -> remaining_tokens
        _ -> [next_token, ..remaining_tokens]
      }
    }
    Error(_) -> tokens
  }
}
