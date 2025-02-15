import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleam/string
import monkey/token.{type Token}

type LexingState {
  ReadingIdentifier
  ReadingNumber
  SkippingWhitespace
}

pub type LexingError {
  WrongCharType(expected: CharType, got: CharType)
  IllegalChar(char: String, tokens: List(Token))
}

pub fn lex(input: String) -> Result(List(Token), LexingError) {
  case lex_loop(input, [], "", SkippingWhitespace) {
    Ok(tokens) -> Ok(list.reverse(tokens))
    Error(e) -> Error(e)
  }
}

fn lex_loop(
  remaining_input: String,
  lexed_tokens: List(Token),
  current_word: String,
  lexing_state: LexingState,
) -> Result(List(Token), LexingError) {
  case string.pop_grapheme(remaining_input) {
    Error(_) -> {
      case current_word {
        "" -> Ok([token.eof, ..lexed_tokens])
        _ -> {
          case int.parse(current_word) {
            Ok(_) ->
              Ok([token.eof, token.Integer(current_word), ..lexed_tokens])
            Error(_) ->
              Ok([token.eof, lookup_ident(current_word), ..lexed_tokens])
          }
        }
      }
    }
    Ok(#(char, tail)) -> {
      case parse_char(char), lexing_state {
        Illegal(c), _ -> Error(IllegalChar(c, list.reverse(lexed_tokens)))
        Letter(_) as letter, state -> {
          lex_letter(letter, tail, lexed_tokens, current_word, state)
        }
        Digit(_) as digit, state ->
          lex_digit(digit, tail, lexed_tokens, current_word, state)
        Symbol(_) as symbol, _ ->
          lex_symbol(symbol, tail, lexed_tokens, current_word, lexing_state)
        Whitespace, _ ->
          lex_whitespace(tail, lexed_tokens, current_word, lexing_state)
      }
    }
  }
}

fn lex_letter(
  letter: CharType,
  remaining_input: String,
  lexed_tokens: List(Token),
  current_word: String,
  lexing_state: LexingState,
) -> Result(List(Token), LexingError) {
  case letter, lexing_state {
    Letter(l), ReadingIdentifier ->
      lex_loop(
        remaining_input,
        lexed_tokens,
        current_word <> l,
        ReadingIdentifier,
      )
    Letter(l), ReadingNumber -> Error(IllegalChar(l, lexed_tokens))
    Letter(l), SkippingWhitespace ->
      lex_loop(remaining_input, lexed_tokens, l, ReadingIdentifier)
    unexpected_type, _ ->
      Error(WrongCharType(expected: Letter("[a-zA-z]"), got: unexpected_type))
  }
}

fn lex_digit(
  digit: CharType,
  remaining_input: String,
  lexed_tokens: List(Token),
  current_word: String,
  lexing_state: LexingState,
) -> Result(List(Token), LexingError) {
  case digit, lexing_state {
    Digit(d), ReadingNumber ->
      lex_loop(remaining_input, lexed_tokens, current_word <> d, ReadingNumber)
    Digit(d), _ -> lex_loop(remaining_input, lexed_tokens, d, ReadingNumber)
    unexpected_type, _ ->
      Error(WrongCharType(expected: Digit("[0-9]"), got: unexpected_type))
  }
}

fn lex_symbol(
  symbol: CharType,
  remaining_input: String,
  lexed_tokens: List(Token),
  current_word: String,
  lexing_state: LexingState,
) -> Result(List(Token), LexingError) {
  let symbol_token = case symbol {
    Symbol("=") -> {
      case string.pop_grapheme(remaining_input) {
        Error(_) -> Ok(token.assign)
        Ok(#(char, _)) -> {
          case parse_char(char) {
            Symbol("=") -> Ok(token.equals)
            _ -> Ok(token.assign)
          }
        }
      }
    }
    Symbol(";") -> Ok(token.semicolon)
    Symbol("(") -> Ok(token.l_paren)
    Symbol(")") -> Ok(token.r_paren)
    Symbol(",") -> Ok(token.comma)
    Symbol("+") -> Ok(token.plus)
    Symbol("-") -> Ok(token.minus)
    Symbol("*") -> Ok(token.asterisk)
    Symbol("/") -> Ok(token.slash)
    Symbol("{") -> Ok(token.l_brace)
    Symbol("}") -> Ok(token.r_brace)
    Symbol("!") -> {
      case string.pop_grapheme(remaining_input) {
        Error(_) -> Ok(token.bang)
        Ok(#(char, _)) -> {
          case parse_char(char) {
            Symbol("=") -> Ok(token.not_equals)
            _ -> Ok(token.bang)
          }
        }
      }
    }
    Symbol("<") -> Ok(token.lesser_than)
    Symbol(">") -> Ok(token.greater_than)
    unexpected_type ->
      Error(WrongCharType(expected: Symbol("symbol"), got: unexpected_type))
  }

  let symbol_length = case symbol_token {
    Ok(st) -> string.length(st.literal)
    Error(_) -> 1
  }

  let remaining_input = case symbol_length {
    l if l > 1 -> string.drop_start(remaining_input, l)
    _ -> remaining_input
  }

  case lexing_state, symbol_token {
    SkippingWhitespace, Ok(token) ->
      lex_loop(remaining_input, [token, ..lexed_tokens], "", SkippingWhitespace)
    ReadingIdentifier, Ok(token) ->
      lex_loop(
        remaining_input,
        [token, lookup_ident(current_word), ..lexed_tokens],
        "",
        SkippingWhitespace,
      )
    ReadingNumber, Ok(token) ->
      lex_loop(
        remaining_input,
        [token, token.Integer(current_word), ..lexed_tokens],
        "",
        SkippingWhitespace,
      )
    _, Error(e) -> Error(e)
  }
}

fn lex_whitespace(
  remaining_input: String,
  lexed_tokens: List(Token),
  current_word: String,
  lexing_state: LexingState,
) -> Result(List(Token), LexingError) {
  case lexing_state {
    ReadingIdentifier -> {
      lex_loop(
        remaining_input,
        [lookup_ident(current_word), ..lexed_tokens],
        "",
        SkippingWhitespace,
      )
    }
    ReadingNumber -> {
      lex_loop(
        remaining_input,
        [token.Integer(current_word), ..lexed_tokens],
        "",
        SkippingWhitespace,
      )
    }
    SkippingWhitespace -> {
      case current_word |> string.trim {
        "" ->
          lex_loop(
            remaining_input,
            lexed_tokens,
            current_word,
            SkippingWhitespace,
          )
        _ ->
          lex_loop(
            remaining_input,
            [lookup_ident(current_word), ..lexed_tokens],
            current_word,
            SkippingWhitespace,
          )
      }
    }
  }
}

fn lookup_ident(identifier: String) -> Token {
  case dict.get(token.keywords(), identifier) {
    Ok(keyword_token) -> keyword_token
    Error(_) -> token.Identifier(identifier)
  }
}

pub type CharType {
  Letter(c: String)
  Digit(c: String)
  Symbol(c: String)
  Whitespace
  Illegal(c: String)
}

// Letters
const lower_a = 97

const lower_z = 122

const upper_a = 65

const upper_z = 90

// Digits
const zero = 48

const nine = 57

// Whitespaces
const space = 32

const tab = 9

const newline = 10

const carriage_return = 13

fn parse_char(char: String) -> CharType {
  case char_to_codepoint_value(char) {
    // Letters
    Ok(i) if lower_a <= i && i <= lower_z -> Letter(char)
    Ok(i) if upper_a <= i && i <= upper_z -> Letter(char)
    Ok(_) if char == "_" -> Letter(char)
    // Digits
    Ok(i) if zero <= i && i <= nine -> Digit(char)
    // Symbols
    Ok(_) if char == "=" -> Symbol(char)
    Ok(_) if char == ";" -> Symbol(char)
    Ok(_) if char == "(" -> Symbol(char)
    Ok(_) if char == ")" -> Symbol(char)
    Ok(_) if char == "," -> Symbol(char)
    Ok(_) if char == "+" -> Symbol(char)
    Ok(_) if char == "-" -> Symbol(char)
    Ok(_) if char == "*" -> Symbol(char)
    Ok(_) if char == "/" -> Symbol(char)
    Ok(_) if char == "{" -> Symbol(char)
    Ok(_) if char == "}" -> Symbol(char)
    Ok(_) if char == "!" -> Symbol(char)
    Ok(_) if char == ">" -> Symbol(char)
    Ok(_) if char == "<" -> Symbol(char)
    // Whitespace
    Ok(i) if i == space || i == tab || i == newline || i == carriage_return ->
      Whitespace
    _ -> Illegal(char)
  }
}

fn char_to_codepoint_value(char: String) -> Result(Int, String) {
  let str_length = string.length(char)
  case str_length {
    1 ->
      char
      |> string.to_utf_codepoints
      |> list.fold(0, fn(acc, cp) { acc + string.utf_codepoint_to_int(cp) })
      |> Ok
    _ ->
      Error(
        "'char' is not a grapheme. Expected string of length 1, got: "
        <> int.to_string(str_length),
      )
  }
}
