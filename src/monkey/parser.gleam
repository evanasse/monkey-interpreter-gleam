import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import monkey/ast.{type Expression, type Program, type Statement}
import monkey/next_token.{expect_next_token, get_next_token}
import monkey/op_precedence.{type OpPrecedence}
import monkey/parser_error.{type ParserError}
import monkey/token.{type Token}

type PrefixParseFunction =
  fn(List(Token), OpPrecedence) ->
    Result(#(Expression, List(Token)), ParserError)

type InfixParseFunction =
  fn(List(Token), Expression, OpPrecedence) ->
    Result(#(Expression, List(Token)), ParserError)

pub fn parse(tokens: List(Token)) -> Result(Program, ParserError) {
  parse_loop(tokens, [])
}

fn parse_loop(
  remaining_tokens: List(Token),
  program_statements: List(Statement),
) -> Result(Program, ParserError) {
  case get_next_token(remaining_tokens) {
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
) -> Result(#(Statement, List(Token)), ParserError) {
  use #(next_token, remaining_tokens) <- result.try(get_next_token(
    remaining_tokens,
  ))
  case next_token {
    token.Let(_) -> parse_let_statement(remaining_tokens)
    token.Return(_) -> parse_return_statement(remaining_tokens)
    _ -> parse_expression_statement([next_token, ..remaining_tokens])
  }
}

fn parse_let_statement(
  remaining_tokens: List(Token),
) -> Result(#(Statement, List(Token)), ParserError) {
  use #(identifier_token, identifier_value, remaining_tokens) <- result.try(
    expect_next_token(remaining_tokens, next_token.is_identifier),
  )
  use #(_, _, remaining_tokens) <- result.try(expect_next_token(
    remaining_tokens,
    next_token.is_assign,
  ))

  use #(expression, remaining_tokens) <- result.try(parse_expression(
    remaining_tokens,
    op_precedence.lowest,
  ))

  let remaining_tokens = next_token.may_be_semicolon(remaining_tokens)

  Ok(#(
    ast.LetStatement(
      token: token.let_,
      name: ast.Identifier(token: identifier_token, value: identifier_value),
      value: expression,
    ),
    remaining_tokens,
  ))
}

fn parse_return_statement(
  remaining_tokens: List(Token),
) -> Result(#(Statement, List(Token)), ParserError) {
  use #(expression, remaining_tokens) <- result.try(parse_expression(
    remaining_tokens,
    op_precedence.lowest,
  ))

  let remaining_tokens = next_token.may_be_semicolon(remaining_tokens)

  Ok(#(
    ast.ReturnStatement(token: token.return, return_value: expression),
    remaining_tokens,
  ))
}

fn parse_expression_statement(
  remaining_tokens: List(Token),
) -> Result(#(Statement, List(Token)), ParserError) {
  use #(expression, remaining_tokens) <- result.try(parse_expression(
    remaining_tokens,
    op_precedence.lowest,
  ))

  let remaining_tokens = next_token.may_be_semicolon(remaining_tokens)

  Ok(#(ast.ExpressionStatement(expression.token, expression), remaining_tokens))
}

fn parse_expression(
  remaining_tokens: List(Token),
  op_precedence: OpPrecedence,
) -> Result(#(Expression, List(Token)), ParserError) {
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
  left_expression: Expression,
  op_precedence: OpPrecedence,
  remaining_tokens: List(Token),
) -> Result(#(Expression, List(Token)), ParserError) {
  use #(next_token, remaining_tokens) <- result.try(get_next_token(
    remaining_tokens,
  ))

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
) -> Result(#(Expression, List(Token)), ParserError) {
  use #(identifier_token, identifier_value, remaining_tokens) <- result.try(
    expect_next_token(remaining_tokens, next_token.is_identifier),
  )
  Ok(#(ast.Identifier(identifier_token, identifier_value), remaining_tokens))
}

fn parse_integer_literal(
  remaining_tokens: List(Token),
  _op_precedence: OpPrecedence,
) -> Result(#(Expression, List(Token)), ParserError) {
  use #(integer_token, integer_value, remaining_tokens) <- result.try(
    expect_next_token(remaining_tokens, next_token.is_integer),
  )
  Ok(#(ast.IntegerLiteral(integer_token, integer_value), remaining_tokens))
}

fn parse_string_literal(
  remaining_tokens: List(Token),
  _op_precedence: OpPrecedence,
) -> Result(#(Expression, List(Token)), ParserError) {
  use #(string_token, string_value, remaining_tokens) <- result.try(
    expect_next_token(remaining_tokens, next_token.is_string),
  )
  Ok(#(ast.StringLiteral(string_token, string_value), remaining_tokens))
}

fn parse_boolean(
  remaining_tokens: List(Token),
  _op_precedence: OpPrecedence,
) -> Result(#(Expression, List(Token)), ParserError) {
  use #(boolean_token, boolean_value, remaining_tokens) <- result.try(
    expect_next_token(remaining_tokens, next_token.is_boolean),
  )
  Ok(#(ast.BooleanLiteral(boolean_token, boolean_value), remaining_tokens))
}

fn parse_prefix_expression(
  remaining_tokens: List(Token),
  op_precedence: OpPrecedence,
) -> Result(#(Expression, List(Token)), ParserError) {
  use #(operator_token, operator_literal, remaining_tokens) <- result.try(
    expect_next_token(remaining_tokens, next_token.is_prefix_operator),
  )

  use #(right_expression, remaining_tokens) <- result.try(parse_expression(
    remaining_tokens,
    op_precedence.prefix,
  ))

  Ok(#(
    ast.PrefixExpression(operator_token, operator_literal, right_expression),
    remaining_tokens,
  ))
}

fn parse_infix_expression(
  remaining_tokens: List(Token),
  left_expression: Expression,
  _op_precedence: OpPrecedence,
) -> Result(#(Expression, List(Token)), ParserError) {
  use #(operator_token, operator_literal, remaining_tokens) <- result.try(
    expect_next_token(remaining_tokens, next_token.is_infix_operator),
  )

  use #(right_expression, remaining_tokens) <- result.try(parse_expression(
    remaining_tokens,
    token_precedence(operator_token),
  ))

  Ok(#(
    ast.InfixExpression(
      operator_token,
      left_expression,
      operator_literal,
      right_expression,
    ),
    remaining_tokens,
  ))
}

fn parse_grouped_expression(
  remaining_tokens: List(Token),
  op_precedence: OpPrecedence,
) -> Result(#(Expression, List(Token)), ParserError) {
  use #(_, remaining_tokens) <- result.try(get_next_token(remaining_tokens))
  use #(expression, remaining_tokens) <- result.try(parse_expression(
    remaining_tokens,
    op_precedence.lowest,
  ))
  use #(next_token, remaining_tokens) <- result.try(get_next_token(
    remaining_tokens,
  ))
  case next_token {
    token.RightParenthesis(_) -> Ok(#(expression, remaining_tokens))
    _ -> parse_expression([next_token, ..remaining_tokens], op_precedence)
  }
}

fn parse_if_expression(
  remaining_tokens: List(Token),
  op_precedence: OpPrecedence,
) -> Result(#(Expression, List(Token)), ParserError) {
  use #(if_token, _, remaining_tokens) <- result.try(expect_next_token(
    remaining_tokens,
    next_token.is_if_token,
  ))
  use #(left_parenthesis, _, remaining_tokens) <- result.try(expect_next_token(
    remaining_tokens,
    next_token.is_left_parenthesis,
  ))
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
) -> Result(#(Statement, List(Token)), ParserError) {
  use #(_, _, remaining_tokens) <- result.try(expect_next_token(
    remaining_tokens,
    next_token.is_left_brace,
  ))
  parse_block_statement_loop(remaining_tokens, [])
}

fn parse_block_statement_loop(
  remaining_tokens: List(Token),
  statements: List(Statement),
) -> Result(#(Statement, List(Token)), ParserError) {
  use #(next_token, remaining_tokens) <- result.try(get_next_token(
    remaining_tokens,
  ))
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
) -> Result(#(Expression, List(Token)), ParserError) {
  use #(function_token, _, remaining_tokens) <- result.try(expect_next_token(
    remaining_tokens,
    next_token.is_function_token,
  ))
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
) -> Result(#(List(Expression), List(Token)), ParserError) {
  use #(_, _, remaining_tokens) <- result.try(expect_next_token(
    remaining_tokens,
    next_token.is_left_parenthesis,
  ))
  parse_parameters_loop(remaining_tokens, [])
}

fn parse_parameters_loop(
  remaining_tokens: List(Token),
  parameters: List(Expression),
) -> Result(#(List(Expression), List(Token)), ParserError) {
  use #(next_token, remaining_tokens) <- result.try(get_next_token(
    remaining_tokens,
  ))
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
) -> Result(#(Expression, List(Token)), ParserError) {
  use #(left_bracket_token, _, remaining_tokens) <- result.try(
    expect_next_token(remaining_tokens, next_token.is_left_bracket),
  )
  use #(elements, remaining_tokens) <- result.try(
    parse_array_elements([left_bracket_token, ..remaining_tokens]),
  )
  Ok(#(ast.ArrayLiteral(left_bracket_token, elements), remaining_tokens))
}

fn parse_array_elements(
  remaining_tokens: List(Token),
) -> Result(#(List(Expression), List(Token)), ParserError) {
  use #(_, _, remaining_tokens) <- result.try(expect_next_token(
    remaining_tokens,
    next_token.is_left_bracket,
  ))
  parse_array_elements_loop(remaining_tokens, [])
}

fn parse_array_elements_loop(
  remaining_tokens: List(Token),
  elements: List(Expression),
) -> Result(#(List(Expression), List(Token)), ParserError) {
  use #(next_token, remaining_tokens) <- result.try(get_next_token(
    remaining_tokens,
  ))
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
  function: Expression,
  _op_precedence: OpPrecedence,
) -> Result(#(Expression, List(Token)), ParserError) {
  use #(left_parenthesis_token, _, remaining_tokens) <- result.try(
    expect_next_token(remaining_tokens, next_token.is_left_parenthesis),
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
) -> Result(#(List(Expression), List(Token)), ParserError) {
  use #(_, _, remaining_tokens) <- result.try(expect_next_token(
    remaining_tokens,
    next_token.is_left_parenthesis,
  ))
  parse_call_arguments_loop(remaining_tokens, [])
}

fn parse_call_arguments_loop(
  remaining_tokens: List(Token),
  arguments: List(Expression),
) -> Result(#(List(Expression), List(Token)), ParserError) {
  use #(next_token, remaining_tokens) <- result.try(get_next_token(
    remaining_tokens,
  ))
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
  left: Expression,
  _op_precedence: OpPrecedence,
) -> Result(#(Expression, List(Token)), ParserError) {
  use #(left_bracket_token, _, remaining_tokens) <- result.try(
    expect_next_token(remaining_tokens, next_token.is_left_bracket),
  )
  use #(index, remaining_tokens) <- result.try(parse_expression(
    remaining_tokens,
    op_precedence.lowest,
  ))
  use #(_, _, remaining_tokens) <- result.try(expect_next_token(
    remaining_tokens,
    next_token.is_right_bracket,
  ))

  Ok(#(ast.IndexExpression(left_bracket_token, left, index), remaining_tokens))
}

fn parse_hash_literal(
  remaining_tokens: List(Token),
  _op_precedence: OpPrecedence,
) -> Result(#(Expression, List(Token)), ParserError) {
  use #(left_brace_token, _, remaining_tokens) <- result.try(expect_next_token(
    remaining_tokens,
    next_token.is_left_brace,
  ))
  use #(pairs, remaining_tokens) <- result.try(parse_hash_pairs(
    remaining_tokens,
  ))

  Ok(#(ast.HashLiteral(left_brace_token, pairs), remaining_tokens))
}

fn parse_hash_pairs(
  remaining_tokens: List(Token),
) -> Result(#(List(#(Expression, Expression)), List(Token)), ParserError) {
  parse_hash_pairs_loop(remaining_tokens, [])
}

fn parse_hash_pairs_loop(
  remaining_tokens: List(Token),
  pairs: List(#(Expression, Expression)),
) -> Result(#(List(#(Expression, Expression)), List(Token)), ParserError) {
  use #(next_token, remaining_tokens) <- result.try(get_next_token(
    remaining_tokens,
  ))
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
) -> Result(#(#(Expression, Expression), List(Token)), ParserError) {
  use #(key, remaining_tokens) <- result.try(parse_expression(
    remaining_tokens,
    op_precedence,
  ))
  use #(_, _, remaining_tokens) <- result.try(expect_next_token(
    remaining_tokens,
    next_token.is_colon,
  ))
  use #(value, remaining_tokens) <- result.try(parse_expression(
    remaining_tokens,
    op_precedence,
  ))

  Ok(#(#(key, value), remaining_tokens))
}

fn parse_macro_literal(
  remaining_tokens: List(Token),
  _op_precedence: OpPrecedence,
) -> Result(#(Expression, List(Token)), ParserError) {
  use #(macro_token, _, remaining_tokens) <- result.try(
    next_token.expect_next_token(remaining_tokens, next_token.is_macro_token),
  )
  use #(parameters, remaining_tokens) <- result.try(parse_parameters(
    remaining_tokens,
  ))
  use #(macro_body_block, remaining_tokens) <- result.try(parse_block_statement(
    remaining_tokens,
  ))

  Ok(#(
    ast.MacroLiteral(macro_token, parameters, macro_body_block),
    remaining_tokens,
  ))
}

fn next_token_has_prefix_function(
  tokens: List(Token),
) -> Result(PrefixParseFunction, ParserError) {
  use #(next_token, _) <- result.try(get_next_token(tokens))
  get_prefix_parse_functions(next_token)
}

fn next_token_has_infix_function(
  tokens: List(Token),
) -> Result(InfixParseFunction, ParserError) {
  use #(next_token, _) <- result.try(get_next_token(tokens))
  get_infix_parse_functions(next_token)
}

fn parse_else_expression(
  tokens: List(Token),
) -> Result(#(Option(Statement), List(Token)), ParserError) {
  case get_next_token(tokens) {
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
    Error(_) -> Error(parser_error.ExpectedAToken)
  }
}

fn parsing_complete(program_statements: List(Statement)) -> Program {
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
    token.Macro(_) -> Ok(parse_macro_literal)
    _ -> Error(parser_error.NoPrefixFunctionForToken(token))
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
    _ -> Error(parser_error.NoInfixFunctionForToken(token))
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
