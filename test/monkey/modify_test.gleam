import gleam/list
import gleam/option
import gleeunit/should
import monkey/ast
import monkey/token

pub fn modify_test() {
  let one = fn() { ast.IntegerLiteral(token: token.Integer("1"), value: 1) }
  let two = fn() { ast.IntegerLiteral(token: token.Integer("2"), value: 2) }

  let turn_one_to_two = fn(expression: ast.Expression) -> ast.Expression {
    case expression {
      ast.IntegerLiteral(_, value) -> {
        case value {
          1 -> ast.IntegerLiteral(token.Integer("2"), value: 2)
          _ -> expression
        }
      }
      _ -> expression
    }
  }

  let tests = [
    #(ast.ExpressionNode(one()), ast.ExpressionNode(two())),
    #(
      ast.ProgramNode(
        program: ast.Program(token: token.eof, statements: [
          ast.ExpressionStatement(token: token.Integer("1"), expression: one()),
        ]),
      ),
      ast.ProgramNode(
        program: ast.Program(token: token.eof, statements: [
          ast.ExpressionStatement(token: token.Integer("2"), expression: two()),
        ]),
      ),
    ),
    #(
      ast.ExpressionNode(ast.InfixExpression(
        token: token.plus,
        left: one(),
        operator: token.plus.literal,
        right: two(),
      )),
      ast.ExpressionNode(ast.InfixExpression(
        token: token.plus,
        left: two(),
        operator: token.plus.literal,
        right: two(),
      )),
    ),
    #(
      ast.ExpressionNode(ast.InfixExpression(
        token: token.plus,
        left: two(),
        operator: token.plus.literal,
        right: one(),
      )),
      ast.ExpressionNode(ast.InfixExpression(
        token: token.plus,
        left: two(),
        operator: token.plus.literal,
        right: two(),
      )),
    ),
    #(
      ast.ExpressionNode(ast.PrefixExpression(
        token: token.minus,
        operator: token.minus.literal,
        right: one(),
      )),
      ast.ExpressionNode(ast.PrefixExpression(
        token: token.minus,
        operator: token.minus.literal,
        right: two(),
      )),
    ),
    #(
      ast.ExpressionNode(ast.IndexExpression(
        token: token.l_bracket,
        left: one(),
        index: one(),
      )),
      ast.ExpressionNode(ast.IndexExpression(
        token: token.l_bracket,
        left: two(),
        index: two(),
      )),
    ),
    #(
      ast.ExpressionNode(ast.IfExpression(
        token: token.if_,
        condition: one(),
        consequence: ast.BlockStatement(token: token.l_brace, statements: [
          ast.ExpressionStatement(token: token.Integer("1"), expression: one()),
        ]),
        alternative: option.Some(
          ast.BlockStatement(token: token.l_brace, statements: [
            ast.ExpressionStatement(
              token: token.Integer("1"),
              expression: one(),
            ),
          ]),
        ),
      )),
      ast.ExpressionNode(ast.IfExpression(
        token: token.if_,
        condition: two(),
        consequence: ast.BlockStatement(token: token.l_brace, statements: [
          ast.ExpressionStatement(token: token.Integer("2"), expression: two()),
        ]),
        alternative: option.Some(
          ast.BlockStatement(token: token.l_brace, statements: [
            ast.ExpressionStatement(
              token: token.Integer("2"),
              expression: two(),
            ),
          ]),
        ),
      )),
    ),
    #(
      ast.StatementNode(ast.ReturnStatement(
        token: token.return,
        return_value: one(),
      )),
      ast.StatementNode(ast.ReturnStatement(
        token: token.return,
        return_value: two(),
      )),
    ),
    #(
      ast.StatementNode(ast.LetStatement(
        token: token.Identifier("foo"),
        name: ast.Identifier(token: token.Identifier("foo"), value: "foo"),
        value: one(),
      )),
      ast.StatementNode(ast.LetStatement(
        token: token.Identifier("foo"),
        name: ast.Identifier(token: token.Identifier("foo"), value: "foo"),
        value: two(),
      )),
    ),
    #(
      ast.ExpressionNode(ast.FunctionLiteral(
        token: token.function,
        parameters: [],
        body: ast.BlockStatement(token: token.l_bracket, statements: [
          ast.ExpressionStatement(token: token.Integer("1"), expression: one()),
        ]),
      )),
      ast.ExpressionNode(ast.FunctionLiteral(
        token: token.function,
        parameters: [],
        body: ast.BlockStatement(token: token.l_bracket, statements: [
          ast.ExpressionStatement(token: token.Integer("2"), expression: two()),
        ]),
      )),
    ),
    #(
      ast.ExpressionNode(
        ast.ArrayLiteral(token: token.l_bracket, elements: [one(), one()]),
      ),
      ast.ExpressionNode(
        ast.ArrayLiteral(token: token.l_bracket, elements: [two(), two()]),
      ),
    ),
    #(
      ast.ExpressionNode(
        ast.HashLiteral(token: token.l_bracket, pairs: [
          #(one(), one()),
          #(one(), one()),
        ]),
      ),
      ast.ExpressionNode(
        ast.HashLiteral(token: token.l_bracket, pairs: [
          #(two(), two()),
          #(two(), two()),
        ]),
      ),
    ),
  ]

  tests
  |> list.each(fn(test_) {
    ast.modify(test_.0, turn_one_to_two) |> should.equal(test_.1)
  })
}
