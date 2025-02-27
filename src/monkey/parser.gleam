import gleam/int
import gleam/io
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import monkey/ast.{
  type Expression, type Node, type Program, type Statement, Program,
}
import monkey/op_precedence.{type OpPrecedence}
import monkey/token.{type Token}

pub type ParserError {
  CannotParseInteger(literal: String)
  ExpectedAToken
  NoPrefixFunctionForToken(token: Token)
  NoInfixFunctionForToken(token: Token)
  NotAStatementStartToken(token: Token)
  UnexpectedExpressionNode(got: Node(Expression))
  UnexpectedStatementNode(got: Node(Statement))
  UnexpectedToken(expected_one_of: List(Token), got: Token)
}

type PrefixParseFunction =
  fn(List(Token), OpPrecedence) ->
    Result(#(Node(Expression), List(Token)), ParserError)

type InfixParseFunction =
  fn(List(Token), Node(Expression), OpPrecedence) ->
    Result(#(Node(Expression), List(Token)), ParserError)

pub fn parse(tokens: List(Token)) -> Result(Node(Program), ParserError) {
  parse_loop(tokens, [])
}

fn parse_loop(
  remaining_tokens: List(Token),
  program_statements: List(Node(Statement)),
) -> Result(Node(Program), ParserError) {
  case remaining_tokens |> next_token {
    Ok(#(next_token, remaining_tokens)) -> {
      case next_token {
        token.EOF(_) -> Ok(parsing_complete(program_statements))
        _ -> {
          use #(statement, remaining_tokens) <- result.try(
            parse_statement([next_token, ..remaining_tokens]),
          )
          parse_loop(remaining_tokens, [statement, ..program_statements])
        }
      }
    }
    Error(_) -> Ok(parsing_complete(program_statements))
  }
}

fn parse_statement(
  remaining_tokens: List(Token),
) -> Result(#(Node(Statement), List(Token)), ParserError) {
  use #(next_token, remaining_tokens) <- result.try(next_token(remaining_tokens))
  case next_token {
    token.Let(_) -> parse_let_statement(remaining_tokens)
    token.Return(_) -> parse_return_statement(remaining_tokens)
    _ -> parse_expression_statement([next_token, ..remaining_tokens])
  }
}

fn parse_let_statement(
  remaining_tokens: List(Token),
) -> Result(#(Node(Statement), List(Token)), ParserError) {
  use #(identifier_token, remaining_tokens) <- result.try(
    next_token_is_identifier(remaining_tokens),
  )
  use #(_, remaining_tokens) <- result.try(next_token_is_assign(
    remaining_tokens,
  ))

  use #(expression, remaining_tokens) <- result.try(parse_expression(
    remaining_tokens,
    op_precedence.lowest,
  ))

  let remaining_tokens = next_token_may_be_semicolon(remaining_tokens)

  Ok(#(
    ast.LetStatement(
      token: token.let_,
      name: ast.Identifier(
        token: identifier_token,
        value: identifier_token.literal,
      ),
      value: expression,
    ),
    remaining_tokens,
  ))
}

fn parse_return_statement(
  remaining_tokens: List(Token),
) -> Result(#(Node(Statement), List(Token)), ParserError) {
  use #(expression, remaining_tokens) <- result.try(parse_expression(
    remaining_tokens,
    op_precedence.lowest,
  ))

  let remaining_tokens = next_token_may_be_semicolon(remaining_tokens)

  Ok(#(
    ast.ReturnStatement(token: token.return, return_value: expression),
    remaining_tokens,
  ))
}

fn parse_expression_statement(
  remaining_tokens: List(Token),
) -> Result(#(Node(Statement), List(Token)), ParserError) {
  use #(expression, remaining_tokens) <- result.try(parse_expression(
    remaining_tokens,
    op_precedence.lowest,
  ))

  let remaining_tokens = next_token_may_be_semicolon(remaining_tokens)

  Ok(#(ast.ExpressionStatement(expression.token, expression), remaining_tokens))
}

fn parse_expression(
  remaining_tokens: List(Token),
  op_precedence: OpPrecedence,
) -> Result(#(Node(Expression), List(Token)), ParserError) {
  use parse_function <- result.try(next_token_has_prefix_function(
    remaining_tokens,
  ))

  use #(left_expression, remaining_tokens) <- result.try(parse_function(
    remaining_tokens,
    op_precedence,
  ))

  parse_infix_expression_loop(left_expression, op_precedence, remaining_tokens)
}

fn parse_infix_expression_loop(
  left_expression: Node(Expression),
  op_precedence: OpPrecedence,
  remaining_tokens: List(Token),
) -> Result(#(Node(Expression), List(Token)), ParserError) {
  use #(next_token, remaining_tokens) <- result.try(next_token(remaining_tokens))

  case next_token, op_precedence < token_precedence(next_token) {
    token.Semicolon(_), _ -> {
      let remaining_tokens = [next_token, ..remaining_tokens]
      Ok(#(left_expression, remaining_tokens))
    }
    _, False -> {
      let remaining_tokens = [next_token, ..remaining_tokens]
      Ok(#(left_expression, remaining_tokens))
    }
    _, _ -> {
      let remaining_tokens = [next_token, ..remaining_tokens]
      case next_token_has_infix_function([next_token, ..remaining_tokens]) {
        Ok(parse_function) -> {
          use #(left_expression, remaining_tokens) <- result.try(parse_function(
            remaining_tokens,
            left_expression,
            op_precedence,
          ))
          parse_infix_expression_loop(
            left_expression,
            op_precedence,
            remaining_tokens,
          )
        }
        _ -> Ok(#(left_expression, remaining_tokens))
      }
    }
  }
}

fn parse_identifier(
  remaining_tokens: List(Token),
  _op_precedence: OpPrecedence,
) -> Result(#(Node(Expression), List(Token)), ParserError) {
  use #(identifier_token, remaining_tokens) <- result.try(
    next_token_is_identifier(remaining_tokens),
  )
  Ok(#(
    ast.Identifier(identifier_token, identifier_token.literal),
    remaining_tokens,
  ))
}

fn parse_integer_literal(
  remaining_tokens: List(Token),
  _op_precedence: OpPrecedence,
) -> Result(#(Node(Expression), List(Token)), ParserError) {
  use #(integer_token, integer_value, remaining_tokens) <- result.try(
    next_token_is_integer(remaining_tokens),
  )
  Ok(#(ast.IntegerLiteral(integer_token, integer_value), remaining_tokens))
}

fn parse_string_literal(
  remaining_tokens: List(Token),
  _op_precedence: OpPrecedence,
) -> Result(#(Node(Expression), List(Token)), ParserError) {
  use #(string_token, string_value, remaining_tokens) <- result.try(
    next_token_is_string(remaining_tokens),
  )
  Ok(#(ast.StringLiteral(string_token, string_value), remaining_tokens))
}

fn parse_boolean(
  remaining_tokens: List(Token),
  _op_precedence: OpPrecedence,
) -> Result(#(Node(Expression), List(Token)), ParserError) {
  use #(boolean_token, boolean_value, remaining_tokens) <- result.try(
    next_token_is_boolean(remaining_tokens),
  )
  Ok(#(ast.Boolean(boolean_token, boolean_value), remaining_tokens))
}

fn parse_prefix_expression(
  remaining_tokens: List(Token),
  op_precedence: OpPrecedence,
) -> Result(#(Node(Expression), List(Token)), ParserError) {
  use #(operator_token, remaining_tokens) <- result.try(
    next_token_is_prefix_operator(remaining_tokens),
  )

  use #(right_expression, remaining_tokens) <- result.try(parse_expression(
    remaining_tokens,
    op_precedence.prefix,
  ))

  Ok(#(
    ast.PrefixExpression(
      operator_token,
      operator_token.literal,
      right_expression,
    ),
    remaining_tokens,
  ))
}

fn parse_infix_expression(
  remaining_tokens: List(Token),
  left_expression: Node(Expression),
  _op_precedence: OpPrecedence,
) -> Result(#(Node(Expression), List(Token)), ParserError) {
  use #(operator_token, remaining_tokens) <- result.try(
    next_token_is_infix_operator(remaining_tokens),
  )

  use #(right_expression, remaining_tokens) <- result.try(parse_expression(
    remaining_tokens,
    token_precedence(operator_token),
  ))

  Ok(#(
    ast.InfixExpression(
      operator_token,
      left_expression,
      operator_token.literal,
      right_expression,
    ),
    remaining_tokens,
  ))
}

fn parse_grouped_expression(
  remaining_tokens: List(Token),
  op_precedence: OpPrecedence,
) -> Result(#(Node(Expression), List(Token)), ParserError) {
  use #(_, remaining_tokens) <- result.try(next_token(remaining_tokens))
  use #(expression, remaining_tokens) <- result.try(parse_expression(
    remaining_tokens,
    op_precedence.lowest,
  ))
  use #(next_token, remaining_tokens) <- result.try(next_token(remaining_tokens))
  case next_token {
    token.RightParenthesis(_) -> Ok(#(expression, remaining_tokens))
    _ -> parse_expression([next_token, ..remaining_tokens], op_precedence)
  }
}

fn parse_if_expression(
  remaining_tokens: List(Token),
  op_precedence: OpPrecedence,
) -> Result(#(Node(Expression), List(Token)), ParserError) {
  use #(if_token, remaining_tokens) <- result.try(next_token_is_if_token(
    remaining_tokens,
  ))
  use #(left_parenthesis, remaining_tokens) <- result.try(
    next_token_is_left_parenthesis(remaining_tokens),
  )
  use #(condition_expression, remaining_tokens) <- result.try(parse_expression(
    [left_parenthesis, ..remaining_tokens],
    op_precedence.lowest,
  ))
  use #(consequence_block, remaining_tokens) <- result.try(
    parse_block_statement(remaining_tokens),
  )
  use #(alternative_block, remaining_tokens) <- result.try(
    parse_else_expression(remaining_tokens),
  )
  Ok(#(
    ast.IfExpression(
      if_token,
      condition_expression,
      consequence_block,
      alternative_block,
    ),
    remaining_tokens,
  ))
}

fn parse_block_statement(
  remaining_tokens: List(Token),
) -> Result(#(Node(Statement), List(Token)), ParserError) {
  use #(_, remaining_tokens) <- result.try(next_token_is_left_brace(
    remaining_tokens,
  ))
  parse_block_statement_loop(remaining_tokens, [])
}

fn parse_block_statement_loop(
  remaining_tokens: List(Token),
  statements: List(Node(Statement)),
) -> Result(#(Node(Statement), List(Token)), ParserError) {
  use #(next_token, remaining_tokens) <- result.try(next_token(remaining_tokens))
  case next_token {
    token.RightBrace(_) | token.EOF(_) ->
      Ok(#(
        ast.BlockStatement(token.l_brace, list.reverse(statements)),
        remaining_tokens,
      ))
    _ -> {
      use #(statement, remaining_tokens) <- result.try(
        parse_statement([next_token, ..remaining_tokens]),
      )
      parse_block_statement_loop(remaining_tokens, [statement, ..statements])
    }
  }
}

fn parse_function_literal(
  remaining_tokens: List(Token),
  _op_precedence: OpPrecedence,
) -> Result(#(Node(Expression), List(Token)), ParserError) {
  use #(function_token, remaining_tokens) <- result.try(
    next_token_is_function_token(remaining_tokens),
  )
  use #(parameters, remaining_tokens) <- result.try(parse_parameters(
    remaining_tokens,
  ))
  use #(function_body_block, remaining_tokens) <- result.try(
    parse_block_statement(remaining_tokens),
  )

  Ok(#(
    ast.FunctionLiteral(function_token, parameters, function_body_block),
    remaining_tokens,
  ))
}

fn parse_parameters(
  remaining_tokens: List(Token),
) -> Result(#(List(Node(Expression)), List(Token)), ParserError) {
  use #(_, remaining_tokens) <- result.try(next_token_is_left_parenthesis(
    remaining_tokens,
  ))
  parse_parameters_loop(remaining_tokens, [])
}

fn parse_parameters_loop(
  remaining_tokens: List(Token),
  parameters: List(Node(Expression)),
) -> Result(#(List(Node(Expression)), List(Token)), ParserError) {
  use #(next_token, remaining_tokens) <- result.try(next_token(remaining_tokens))
  case next_token {
    token.RightParenthesis(_) ->
      Ok(#(list.reverse(parameters), remaining_tokens))
    token.Comma(_) -> parse_parameters_loop(remaining_tokens, parameters)
    _ -> {
      use #(identifier, remaining_tokens) <- result.try(parse_identifier(
        [next_token, ..remaining_tokens],
        op_precedence.lowest,
      ))
      parse_parameters_loop(remaining_tokens, [identifier, ..parameters])
    }
  }
}

fn parse_array_literal(
  remaining_tokens: List(Token),
  _op_precedence: OpPrecedence,
) -> Result(#(Node(Expression), List(Token)), ParserError) {
  use #(left_bracket_token, remaining_tokens) <- result.try(
    next_token_is_left_bracket(remaining_tokens),
  )
  use #(elements, remaining_tokens) <- result.try(
    parse_array_elements([left_bracket_token, ..remaining_tokens]),
  )
  Ok(#(ast.ArrayLiteral(left_bracket_token, elements), remaining_tokens))
}

fn parse_array_elements(
  remaining_tokens: List(Token),
) -> Result(#(List(Node(Expression)), List(Token)), ParserError) {
  use #(_, remaining_tokens) <- result.try(next_token_is_left_bracket(
    remaining_tokens,
  ))
  parse_array_elements_loop(remaining_tokens, [])
}

fn parse_array_elements_loop(
  remaining_tokens: List(Token),
  elements: List(Node(Expression)),
) -> Result(#(List(Node(Expression)), List(Token)), ParserError) {
  use #(next_token, remaining_tokens) <- result.try(next_token(remaining_tokens))
  case next_token {
    token.RightBracket(_) -> Ok(#(list.reverse(elements), remaining_tokens))
    token.Comma(_) -> parse_array_elements_loop(remaining_tokens, elements)
    _ -> {
      use #(element, remaining_tokens) <- result.try(parse_expression(
        [next_token, ..remaining_tokens],
        op_precedence.lowest,
      ))
      parse_array_elements_loop(remaining_tokens, [element, ..elements])
    }
  }
}

fn parse_call_expression(
  remaining_tokens: List(Token),
  function: Node(Expression),
  _op_precedence: OpPrecedence,
) -> Result(#(Node(Expression), List(Token)), ParserError) {
  use #(left_parenthesis_token, remaining_tokens) <- result.try(
    next_token_is_left_parenthesis(remaining_tokens),
  )
  use #(arguments, remaining_tokens) <- result.try(
    parse_call_arguments([left_parenthesis_token, ..remaining_tokens]),
  )

  Ok(#(
    ast.CallExpression(left_parenthesis_token, function, arguments),
    remaining_tokens,
  ))
}

fn parse_call_arguments(
  remaining_tokens: List(Token),
) -> Result(#(List(Node(Expression)), List(Token)), ParserError) {
  use #(_, remaining_tokens) <- result.try(next_token_is_left_parenthesis(
    remaining_tokens,
  ))
  parse_call_arguments_loop(remaining_tokens, [])
}

fn parse_call_arguments_loop(
  remaining_tokens: List(Token),
  arguments: List(Node(Expression)),
) -> Result(#(List(Node(Expression)), List(Token)), ParserError) {
  use #(next_token, remaining_tokens) <- result.try(next_token(remaining_tokens))
  case next_token {
    token.RightParenthesis(_) ->
      Ok(#(list.reverse(arguments), remaining_tokens))
    token.Comma(_) -> parse_call_arguments_loop(remaining_tokens, arguments)
    _ -> {
      use #(argument, remaining_tokens) <- result.try(parse_expression(
        [next_token, ..remaining_tokens],
        op_precedence.lowest,
      ))
      parse_call_arguments_loop(remaining_tokens, [argument, ..arguments])
    }
  }
}

fn parse_index_expression(
  remaining_tokens: List(Token),
  left: Node(Expression),
  _op_precedence: OpPrecedence,
) -> Result(#(Node(Expression), List(Token)), ParserError) {
  use #(left_bracket_token, remaining_tokens) <- result.try(
    next_token_is_left_bracket(remaining_tokens),
  )
  use #(index, remaining_tokens) <- result.try(parse_expression(
    remaining_tokens,
    op_precedence.lowest,
  ))
  use #(_, remaining_tokens) <- result.try(next_token_is_right_bracket(
    remaining_tokens,
  ))

  Ok(#(ast.IndexExpression(left_bracket_token, left, index), remaining_tokens))
}

fn parse_hash_literal(
  remaining_tokens: List(Token),
  _op_precedence: OpPrecedence,
) -> Result(#(Node(Expression), List(Token)), ParserError) {
  use #(left_brace_token, remaining_tokens) <- result.try(
    next_token_is_left_brace(remaining_tokens),
  )
  use #(pairs, remaining_tokens) <- result.try(parse_hash_pairs(
    remaining_tokens,
  ))

  Ok(#(ast.HashLiteral(left_brace_token, pairs), remaining_tokens))
}

fn parse_hash_pairs(
  remaining_tokens: List(Token),
) -> Result(
  #(List(#(Node(Expression), Node(Expression))), List(Token)),
  ParserError,
) {
  parse_hash_pairs_loop(remaining_tokens, [])
}

fn parse_hash_pairs_loop(
  remaining_tokens: List(Token),
  pairs: List(#(Node(Expression), Node(Expression))),
) -> Result(
  #(List(#(Node(Expression), Node(Expression))), List(Token)),
  ParserError,
) {
  use #(next_token, remaining_tokens) <- result.try(next_token(remaining_tokens))
  case next_token {
    token.RightBrace(_) -> Ok(#(list.reverse(pairs), remaining_tokens))
    token.Comma(_) -> parse_hash_pairs_loop(remaining_tokens, pairs)
    _ -> {
      use #(pair, remaining_tokens) <- result.try(parse_hash_pair(
        [next_token, ..remaining_tokens],
        op_precedence.lowest,
      ))
      parse_hash_pairs_loop(remaining_tokens, [pair, ..pairs])
    }
  }
}

fn parse_hash_pair(
  remaining_tokens: List(Token),
  op_precedence: OpPrecedence,
) -> Result(#(#(Node(Expression), Node(Expression)), List(Token)), ParserError) {
  use #(key, remaining_tokens) <- result.try(parse_expression(
    remaining_tokens,
    op_precedence,
  ))
  use #(_colon_token, remaining_tokens) <- result.try(next_token_is_colon(
    remaining_tokens,
  ))
  use #(value, remaining_tokens) <- result.try(parse_expression(
    remaining_tokens,
    op_precedence,
  ))

  Ok(#(#(key, value), remaining_tokens))
}

fn next_token_has_prefix_function(
  tokens: List(Token),
) -> Result(PrefixParseFunction, ParserError) {
  use #(next_token, _) <- result.try(next_token(tokens))
  get_prefix_parse_functions(next_token)
}

fn next_token_has_infix_function(
  tokens: List(Token),
) -> Result(InfixParseFunction, ParserError) {
  use #(next_token, _) <- result.try(next_token(tokens))
  get_infix_parse_functions(next_token)
}

fn next_token_is_identifier(
  tokens: List(Token),
) -> Result(#(Token, List(Token)), ParserError) {
  case tokens |> next_token {
    Ok(#(next_token, remaining_tokens)) -> {
      case next_token {
        token.Identifier(_) -> Ok(#(next_token, remaining_tokens))
        _ ->
          Error(UnexpectedToken(
            [token.Identifier("some_identifier")],
            next_token,
          ))
      }
    }
    Error(_) -> Error(ExpectedAToken)
  }
}

fn next_token_is_integer(
  tokens: List(Token),
) -> Result(#(Token, Int, List(Token)), ParserError) {
  case tokens |> next_token {
    Ok(#(next_token, remaining_tokens)) -> {
      case next_token {
        token.Integer(_) -> {
          case int.parse(next_token.literal) {
            Ok(i) -> Ok(#(next_token, i, remaining_tokens))
            Error(_) -> Error(CannotParseInteger(next_token.literal))
          }
        }
        _ -> Error(UnexpectedToken([token.Integer("some_integer")], next_token))
      }
    }
    Error(_) -> Error(ExpectedAToken)
  }
}

fn next_token_is_string(
  tokens: List(Token),
) -> Result(#(Token, String, List(Token)), ParserError) {
  case tokens |> next_token {
    Ok(#(next_token, remaining_tokens)) -> {
      case next_token {
        token.String(s) -> Ok(#(next_token, s, remaining_tokens))
        _ -> Error(UnexpectedToken([token.String("some_string")], next_token))
      }
    }
    Error(_) -> Error(ExpectedAToken)
  }
}

fn next_token_is_boolean(
  tokens: List(Token),
) -> Result(#(Token, Bool, List(Token)), ParserError) {
  case tokens |> next_token {
    Ok(#(next_token, remaining_tokens)) -> {
      case next_token {
        token.TRUE(_) -> Ok(#(next_token, True, remaining_tokens))
        token.FALSE(_) -> Ok(#(next_token, False, remaining_tokens))
        _ -> Error(UnexpectedToken([token.true, token.false], next_token))
      }
    }
    Error(_) -> Error(ExpectedAToken)
  }
}

fn next_token_is_assign(
  tokens: List(Token),
) -> Result(#(Token, List(Token)), ParserError) {
  case tokens |> next_token {
    Ok(#(next_token, remaining_tokens)) -> {
      case next_token {
        token.Assign(_) -> Ok(#(next_token, remaining_tokens))
        _ -> Error(UnexpectedToken([token.assign], next_token))
      }
    }
    Error(_) -> Error(ExpectedAToken)
  }
}

fn next_token_is_prefix_operator(
  tokens: List(Token),
) -> Result(#(Token, List(Token)), ParserError) {
  case tokens |> next_token {
    Ok(#(next_token, remaining_tokens)) -> {
      case next_token {
        token.Bang(_) | token.Minus(_) -> Ok(#(next_token, remaining_tokens))
        _ -> Error(UnexpectedToken([token.bang, token.minus], next_token))
      }
    }
    Error(_) -> Error(ExpectedAToken)
  }
}

fn next_token_is_infix_operator(
  tokens: List(Token),
) -> Result(#(Token, List(Token)), ParserError) {
  case tokens |> next_token {
    Ok(#(next_token, remaining_tokens)) -> {
      case next_token {
        token.Plus(_)
        | token.Minus(_)
        | token.Asterisk(_)
        | token.Slash(_)
        | token.Equals(_)
        | token.NotEquals(_)
        | token.LesserThan(_)
        | token.GreaterThan(_) -> Ok(#(next_token, remaining_tokens))
        _ ->
          Error(UnexpectedToken(
            [
              token.plus,
              token.minus,
              token.asterisk,
              token.slash,
              token.equals,
              token.not_equals,
              token.lesser_than,
              token.greater_than,
            ],
            next_token,
          ))
      }
    }
    Error(_) -> Error(ExpectedAToken)
  }
}

fn next_token_is_if_token(
  tokens: List(Token),
) -> Result(#(Token, List(Token)), ParserError) {
  case tokens |> next_token {
    Ok(#(next_token, remaining_tokens)) -> {
      case next_token {
        token.If(_) -> Ok(#(next_token, remaining_tokens))
        _ -> Error(UnexpectedToken([token.if_], next_token))
      }
    }
    Error(_) -> Error(ExpectedAToken)
  }
}

fn parse_else_expression(
  tokens: List(Token),
) -> Result(#(Option(Node(Statement)), List(Token)), ParserError) {
  case tokens |> next_token {
    Ok(#(next_token, remaining_tokens)) -> {
      case next_token {
        token.Else(_) -> {
          use #(alternative_block, remaining_tokens) <- result.try(
            parse_block_statement(remaining_tokens),
          )
          Ok(#(Some(alternative_block), remaining_tokens))
        }
        _ -> Ok(#(None, [next_token, ..remaining_tokens]))
      }
    }
    Error(_) -> Error(ExpectedAToken)
  }
}

fn next_token_is_left_parenthesis(
  tokens: List(Token),
) -> Result(#(Token, List(Token)), ParserError) {
  case tokens |> next_token {
    Ok(#(next_token, remaining_tokens)) -> {
      case next_token {
        token.LeftParenthesis(_) -> Ok(#(next_token, remaining_tokens))
        _ -> Error(UnexpectedToken([token.l_paren], next_token))
      }
    }
    Error(_) -> Error(ExpectedAToken)
  }
}

fn next_token_is_left_bracket(
  tokens: List(Token),
) -> Result(#(Token, List(Token)), ParserError) {
  case tokens |> next_token {
    Ok(#(next_token, remaining_tokens)) -> {
      case next_token {
        token.LeftBracket(_) -> Ok(#(next_token, remaining_tokens))
        _ -> Error(UnexpectedToken([token.l_bracket], next_token))
      }
    }
    Error(_) -> Error(ExpectedAToken)
  }
}

fn next_token_is_right_bracket(
  tokens: List(Token),
) -> Result(#(Token, List(Token)), ParserError) {
  case tokens |> next_token {
    Ok(#(next_token, remaining_tokens)) -> {
      case next_token {
        token.RightBracket(_) -> Ok(#(next_token, remaining_tokens))
        _ -> Error(UnexpectedToken([token.r_bracket], next_token))
      }
    }
    Error(_) -> Error(ExpectedAToken)
  }
}

fn next_token_is_left_brace(
  tokens: List(Token),
) -> Result(#(Token, List(Token)), ParserError) {
  case tokens |> next_token {
    Ok(#(next_token, remaining_tokens)) -> {
      case next_token {
        token.LeftBrace(_) -> Ok(#(next_token, remaining_tokens))
        _ -> Error(UnexpectedToken([token.l_brace], next_token))
      }
    }
    Error(_) -> Error(ExpectedAToken)
  }
}

fn next_token_is_colon(
  tokens: List(Token),
) -> Result(#(Token, List(Token)), ParserError) {
  case tokens |> next_token {
    Ok(#(next_token, remaining_tokens)) -> {
      case next_token {
        token.Colon(_) -> Ok(#(next_token, remaining_tokens))
        _ -> Error(UnexpectedToken([token.colon], next_token))
      }
    }
    Error(_) -> Error(ExpectedAToken)
  }
}

fn next_token_is_function_token(
  tokens: List(Token),
) -> Result(#(Token, List(Token)), ParserError) {
  case tokens |> next_token {
    Ok(#(next_token, remaining_tokens)) -> {
      case next_token {
        token.Function(_) -> Ok(#(next_token, remaining_tokens))
        _ -> Error(UnexpectedToken([token.function], next_token))
      }
    }
    Error(_) -> Error(ExpectedAToken)
  }
}

fn next_token_may_be_semicolon(tokens: List(Token)) -> List(Token) {
  case tokens |> next_token {
    Ok(#(next_token, remaining_tokens)) -> {
      case next_token {
        token.Semicolon(_) -> remaining_tokens
        _ -> [next_token, ..remaining_tokens]
      }
    }
    Error(_) -> tokens
  }
}

fn next_token(tokens: List(Token)) -> Result(#(Token, List(Token)), ParserError) {
  case tokens |> list.pop(fn(_) { True }) {
    Ok(#(token, tail)) -> Ok(#(token, tail))
    Error(_) -> Error(ExpectedAToken)
  }
}

fn parsing_complete(program_statements: List(Node(Statement))) -> Node(Program) {
  ast.Program(token.eof, list.reverse(program_statements))
}

fn get_prefix_parse_functions(
  token: Token,
) -> Result(PrefixParseFunction, ParserError) {
  case token {
    token.Identifier(_) -> Ok(parse_identifier)
    token.Integer(_) -> Ok(parse_integer_literal)
    token.String(_) -> Ok(parse_string_literal)
    token.Bang(_) | token.Minus(_) -> Ok(parse_prefix_expression)
    token.TRUE(_) | token.FALSE(_) -> Ok(parse_boolean)
    token.LeftParenthesis(_) -> Ok(parse_grouped_expression)
    token.If(_) -> Ok(parse_if_expression)
    token.Function(_) -> Ok(parse_function_literal)
    token.LeftBracket(_) -> Ok(parse_array_literal)
    token.LeftBrace(_) -> Ok(parse_hash_literal)
    _ -> Error(NoPrefixFunctionForToken(token))
  }
}

fn get_infix_parse_functions(
  token: Token,
) -> Result(InfixParseFunction, ParserError) {
  case token {
    token.Plus(_)
    | token.Minus(_)
    | token.Asterisk(_)
    | token.Slash(_)
    | token.Equals(_)
    | token.NotEquals(_)
    | token.LesserThan(_)
    | token.GreaterThan(_) -> Ok(parse_infix_expression)
    token.LeftParenthesis(_) -> Ok(parse_call_expression)
    token.LeftBracket(_) -> Ok(parse_index_expression)
    _ -> Error(NoInfixFunctionForToken(token))
  }
}

fn token_precedence(token: Token) -> OpPrecedence {
  case token {
    token.Equals(_) | token.NotEquals(_) -> op_precedence.equals
    token.LesserThan(_) | token.GreaterThan(_) -> op_precedence.lesser_greater
    token.Plus(_) | token.Minus(_) -> op_precedence.sum
    token.Asterisk(_) | token.Slash(_) -> op_precedence.product
    token.LeftParenthesis(_) -> op_precedence.call
    token.LeftBracket(_) -> op_precedence.index
    _ -> op_precedence.lowest
  }
}
