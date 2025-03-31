import gleam/dict.{type Dict}
import gleam/io

pub type Token {
  Illegal(literal: String)
  EOF(literal: String)

  // Identifiers + literals
  Identifier(literal: String)
  Integer(literal: String)
  String(literal: String)

  // Operators
  Assign(literal: String)
  Plus(literal: String)
  Minus(literal: String)
  Bang(literal: String)
  Asterisk(literal: String)
  Slash(literal: String)
  LesserThan(literal: String)
  GreaterThan(literal: String)
  Equals(literal: String)
  NotEquals(literal: String)

  // Delimiters
  Comma(literal: String)
  Semicolon(literal: String)
  Colon(literal: String)

  LeftParenthesis(literal: String)
  RightParenthesis(literal: String)
  LeftBrace(literal: String)
  RightBrace(literal: String)
  LeftBracket(literal: String)
  RightBracket(literal: String)
  DoubleQuote(literal: String)

  // Keywords
  Function(literal: String)
  Let(literal: String)
  TRUE(literal: String)
  FALSE(literal: String)
  If(literal: String)
  Else(literal: String)
  Return(literal: String)
  Macro(literal: String)
}

pub fn debug(token: Token) -> Token {
  case token {
    TRUE(_) -> {
      io.println("TRUE(\"true\")")
      token
    }
    FALSE(_) -> {
      io.println("FALSE(\"false\")")
      token
    }
    _ -> io.debug(token)
  }
}

pub const eof: Token = EOF("")

pub const assign: Token = Assign("=")

pub const plus: Token = Plus("+")

pub const minus: Token = Minus("-")

pub const bang: Token = Bang("!")

pub const asterisk: Token = Asterisk("*")

pub const slash: Token = Slash("/")

pub const lesser_than: Token = LesserThan("<")

pub const greater_than: Token = GreaterThan(">")

pub const equals: Token = Equals("==")

pub const not_equals: Token = NotEquals("!=")

pub const comma: Token = Comma(",")

pub const semicolon: Token = Semicolon(";")

pub const l_paren: Token = LeftParenthesis("(")

pub const r_paren: Token = RightParenthesis(")")

pub const l_brace: Token = LeftBrace("{")

pub const r_brace: Token = RightBrace("}")

pub const l_bracket: Token = LeftBracket("[")

pub const r_bracket: Token = RightBracket("]")

pub const double_quote: Token = DoubleQuote("\"")

pub const colon: Token = Colon(":")

pub const function: Token = Function("fn")

pub const let_: Token = Let("let")

pub const true: Token = TRUE("true")

pub const false: Token = FALSE("false")

pub const if_: Token = If("if")

pub const else_: Token = Else("else")

pub const return: Token = Return("return")

pub const macro_: Token = Macro("macro")

pub fn keywords() -> Dict(String, Token) {
  let keywords_list = [
    #(function.literal, function),
    #(let_.literal, let_),
    #(true.literal, true),
    #(false.literal, false),
    #(if_.literal, if_),
    #(else_.literal, else_),
    #(return.literal, return),
    #(macro_.literal, macro_),
  ]
  dict.from_list(keywords_list)
}
