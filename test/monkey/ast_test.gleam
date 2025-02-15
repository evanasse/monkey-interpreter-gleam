import gleeunit/should
import monkey/ast
import monkey/token

pub fn ast_to_string_test() {
  let program =
    ast.Program(token.eof, [
      ast.LetStatement(
        token: token.let_,
        name: ast.Identifier(token.Identifier("myVar"), "myVar"),
        value: ast.Identifier(token.Identifier("anotherVar"), "anotherVar"),
      ),
    ])

  program
  |> ast.node_to_string
  |> should.equal("let myVar = anotherVar;")
}
