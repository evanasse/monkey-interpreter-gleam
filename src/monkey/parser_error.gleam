import monkey/ast.{type Expression, type Statement}
import monkey/token.{type Token}

pub type ParserError {
  CannotParseInteger(literal: String)
  ExpectedAToken
  NoPrefixFunctionForToken(token: Token)
  NoInfixFunctionForToken(token: Token)
  NotAStatementStartToken(token: Token)
  UnexpectedExpressionNode(got: Expression)
  UnexpectedStatementNode(got: Statement)
  UnexpectedToken(expected_one_of: List(Token), got: Token)
}
