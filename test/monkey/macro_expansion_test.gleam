import gleam/list
import gleam/option
import gleeunit/should
import monkey/ast
import monkey/eval
import monkey/lexer
import monkey/macro_expansion.{
  type MacroExpansionError, define_macros, expand_macros,
}
import monkey/object
import monkey/parser
import monkey/token

pub fn define_macros_test() {
  let input =
    "
let number = 1;
let function = fn(x, y) { x + y };
let mymacro = macro(x, y) { x + y; };
"

  let assert Ok(tokens) = lexer.lex(input)

  let assert Ok(program) = parser.parse(tokens)

  let expected_program =
    ast.Program(token: token.eof, statements: [
      ast.LetStatement(
        token: token.let_,
        name: ast.Identifier(token: token.Identifier("number"), value: "number"),
        value: ast.IntegerLiteral(token: token.Integer("1"), value: 1),
      ),
      ast.LetStatement(
        token: token.let_,
        name: ast.Identifier(
          token: token.Identifier("function"),
          value: "function",
        ),
        value: ast.FunctionLiteral(
          token: token.function,
          parameters: [
            ast.Identifier(token: token.Identifier("x"), value: "x"),
            ast.Identifier(token: token.Identifier("y"), value: "y"),
          ],
          body: ast.BlockStatement(token: token.l_brace, statements: [
            ast.ExpressionStatement(
              token: token.plus,
              expression: ast.InfixExpression(
                token: token.plus,
                left: ast.Identifier(token: token.Identifier("x"), value: "x"),
                operator: token.plus.literal,
                right: ast.Identifier(token: token.Identifier("y"), value: "y"),
              ),
            ),
          ]),
        ),
      ),
    ])

  let initial_env = object.new_env()

  let assert Ok(#(new_program, new_env)) = define_macros(program, initial_env)

  new_program
  |> should.equal(expected_program)

  new_env
  |> object.get("number")
  |> should.be_error

  new_env
  |> object.get("function")
  |> should.be_error

  let assert Ok(defined_macro) = object.get(new_env, "mymacro")

  let expected_macro =
    object.Macro(
      parameters: [
        ast.Identifier(token: token.Identifier("x"), value: "x"),
        ast.Identifier(token: token.Identifier("y"), value: "y"),
      ],
      body: ast.BlockStatement(token: token.l_brace, statements: [
        ast.ExpressionStatement(
          token: token.plus,
          expression: ast.InfixExpression(
            token: token.plus,
            left: ast.Identifier(token: token.Identifier("x"), value: "x"),
            operator: token.plus.literal,
            right: ast.Identifier(token: token.Identifier("y"), value: "y"),
          ),
        ),
      ]),
      env: initial_env,
    )

  defined_macro
  |> should.equal(expected_macro)
}

fn expand_macros_in_program(
  input: String,
) -> Result(#(ast.Program, object.Environment), eval.EvalError) {
  let assert Ok(tokens) = lexer.lex(input)
  let assert Ok(program) = parser.parse(tokens)
  let assert Ok(#(new_program, new_env)) =
    define_macros(program, object.new_env())
  let assert ast.ProgramNode(new_program) = expand_macros(new_program, new_env)

  Ok(#(new_program, new_env))
}

fn test_pair(t: #(String, Result(ast.Program, MacroExpansionError))) {
  let expected_result = t.1
  let expanded_program = expand_macros_in_program(t.0)
  case expanded_program {
    Ok(#(program, _env)) -> Ok(program) |> should.equal(expected_result)
    Error(e) -> Error(e) |> should.equal(Error(e))
  }
}

pub fn expand_macros_test() {
  let tests = [
    #(
      "let infixExpression = macro() { quote(1 + 2); }; infixExpression();",
      Ok(
        ast.Program(token: token.eof, statements: [
          ast.ExpressionStatement(
            token: token.plus,
            expression: ast.InfixExpression(
              token: token.plus,
              left: ast.IntegerLiteral(token: token.Integer("1"), value: 1),
              operator: token.plus.literal,
              right: ast.IntegerLiteral(token: token.Integer("2"), value: 2),
            ),
          ),
        ]),
      ),
    ),
    #(
      "let reverse = macro(a, b) { quote(unquote(b) - unquote(a)) }; reverse(2 + 2, 10 - 5)",
      Ok(
        ast.Program(token: token.eof, statements: [
          ast.ExpressionStatement(
            token: token.minus,
            expression: ast.InfixExpression(
              token: token.minus,
              left: ast.InfixExpression(
                token: token.minus,
                left: ast.IntegerLiteral(token: token.Integer("10"), value: 10),
                operator: token.minus.literal,
                right: ast.IntegerLiteral(token: token.Integer("5"), value: 5),
              ),
              operator: token.minus.literal,
              right: ast.InfixExpression(
                token: token.plus,
                left: ast.IntegerLiteral(token: token.Integer("2"), value: 2),
                operator: token.plus.literal,
                right: ast.IntegerLiteral(token: token.Integer("2"), value: 2),
              ),
            ),
          ),
        ]),
      ),
    ),
    #(
      "let unless = macro(condition, consequence, alternative) {
      quote(if ( !(unquote(condition))) {
        unquote(consequence);
      } else {
        unquote(alternative);
      });
      };

      unless(10 > 5, puts(\"lesser\"), puts(\"greater\"))",
      Ok(
        ast.Program(token: token.eof, statements: [
          ast.ExpressionStatement(
            token: token.if_,
            expression: ast.IfExpression(
              token: token.if_,
              condition: ast.PrefixExpression(
                token: token.bang,
                operator: token.bang.literal,
                right: ast.InfixExpression(
                  token: token.greater_than,
                  left: ast.IntegerLiteral(
                    token: token.Integer("10"),
                    value: 10,
                  ),
                  operator: token.greater_than.literal,
                  right: ast.IntegerLiteral(token: token.Integer("5"), value: 5),
                ),
              ),
              consequence: ast.BlockStatement(token: token.l_brace, statements: [
                ast.ExpressionStatement(
                  token: token.l_paren,
                  expression: ast.CallExpression(
                    token: token.l_paren,
                    function: ast.Identifier(
                      token: token.Identifier("puts"),
                      value: "puts",
                    ),
                    arguments: [
                      ast.StringLiteral(
                        token: token.String("lesser"),
                        value: "lesser",
                      ),
                    ],
                  ),
                ),
              ]),
              alternative: option.Some(
                ast.BlockStatement(token: token.l_brace, statements: [
                  ast.ExpressionStatement(
                    token: token.l_paren,
                    expression: ast.CallExpression(
                      token: token.l_paren,
                      function: ast.Identifier(
                        token: token.Identifier("puts"),
                        value: "puts",
                      ),
                      arguments: [
                        ast.StringLiteral(
                          token: token.String("greater"),
                          value: "greater",
                        ),
                      ],
                    ),
                  ),
                ]),
              ),
            ),
          ),
        ]),
      ),
    ),
  ]

  tests
  |> list.each(test_pair)
}
