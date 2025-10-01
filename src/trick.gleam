import glam/doc.{type Document}
import gleam/bool
import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import lazy_const
import splitter

pub opaque type Expression {
  Expression(compile: fn(State) -> Result(CompiledExpression, Error))
}

pub type Error {
  TypeMismatch(expected: Type, got: Type)
}

type CompiledExpression {
  Compiled(document: Document, type_: Type, state: State)
}

pub type Type {
  Custom(module: String, name: String, generics: List(Type))
  TypeVariable(id: Int)
}

type State {
  State(resolved_variables: Dict(Int, Type), type_variable_id: Int)
}

const type_int = Custom("gleam", "Int", [])

const type_float = Custom("gleam", "Float", [])

const type_string = Custom("gleam", "String", [])

const type_bool = Custom("gleam", "Bool", [])

const type_nil = Custom("gleam", "Nil", [])

fn type_list(element: Type) -> Type {
  Custom("gleam", "List", [element])
}

fn type_variable(state: State) -> #(State, Type) {
  let id = state.type_variable_id
  let type_ = TypeVariable(id:)
  let state = State(..state, type_variable_id: id + 1)
  #(state, type_)
}

fn compile(
  state: State,
  expression: Expression,
  continue: fn(State, CompiledExpression) -> Result(a, Error),
) -> Result(a, Error) {
  result.try(expression.compile(state), fn(compiled) {
    continue(compiled.state, compiled)
  })
}

fn compile_statement(
  state: State,
  statement: Statement,
  continue: fn(State, CompiledExpression) -> Result(a, Error),
) -> Result(a, Error) {
  result.try(statement.compile(state), fn(compiled) {
    continue(compiled.state, compiled)
  })
}

fn unify(state: State, a: Type, with b: Type) -> Result(#(State, Type), Error) {
  use <- bool.guard(a == b, Ok(#(state, a)))

  case a, b {
    TypeVariable(id:), other ->
      unify_type_variable(state, id, other, VariableFirst)
    other, TypeVariable(id:) -> unify_type_variable(state, id, other, TypeFirst)
    _, _ -> Error(TypeMismatch(expected: b, got: a))
  }
}

type Order {
  VariableFirst
  TypeFirst
}

fn unify_type_variable(
  state: State,
  id: Int,
  type_: Type,
  order: Order,
) -> Result(#(State, Type), Error) {
  case dict.get(state.resolved_variables, id) {
    Ok(resolved_type) if order == VariableFirst ->
      unify(state, resolved_type, type_)
    Ok(resolved_type) -> unify(state, type_, resolved_type)
    Error(_) -> {
      let state =
        State(
          ..state,
          resolved_variables: dict.insert(state.resolved_variables, id, type_),
        )
      Ok(#(state, type_))
    }
  }
}

pub opaque type Statement {
  Statement(compile: fn(State) -> Result(CompiledExpression, Error))
}

const width = 80

const indent = 2

pub fn expression_to_string(expression: Expression) -> Result(String, Error) {
  result.map(expression.compile(new_state()), fn(expression) {
    doc.to_string(expression.document, width)
  })
}

fn new_state() -> State {
  State(dict.new(), 0)
}

pub fn int(value: Int) -> Expression {
  use state <- Expression
  value
  |> int.to_string
  |> doc.from_string
  |> Compiled(type_int, state)
  |> Ok
}

pub fn float(value: Float) -> Expression {
  use state <- Expression
  value
  |> float.to_string
  |> doc.from_string
  |> Compiled(type_float, state)
  |> Ok
}

pub fn string(value: String) -> Expression {
  use state <- Expression
  value
  |> escape_string_literal
  |> doc.from_string
  |> Compiled(type_string, state)
  |> Ok
}

pub fn bool(value: Bool) -> Expression {
  use state <- Expression
  value
  |> bool.to_string
  |> doc.from_string
  |> Compiled(type_bool, state)
  |> Ok
}

pub fn nil() -> Expression {
  use state <- Expression
  "Nil"
  |> doc.from_string
  |> Compiled(type_nil, state)
  |> Ok
}

fn escape_string_literal(string: String) -> String {
  escape_string_literal_loop(string, escape_string_literal_splitter(), "\"")
}

fn escape_string_literal_splitter() -> splitter.Splitter {
  use <- lazy_const.new(lazy_const.defined_in(escape_string_literal_splitter))
  splitter.new(["\"", "\\"])
}

fn escape_string_literal_loop(
  string: String,
  splitter: splitter.Splitter,
  escaped: String,
) -> String {
  case splitter.split(splitter, string) {
    #(rest_of_string, "", "") -> escaped <> rest_of_string <> "\""
    #(before, split, after) ->
      escape_string_literal_loop(
        after,
        splitter,
        escaped <> before <> "\\" <> split,
      )
  }
}

fn binary_operator(
  left: Expression,
  operator: String,
  right: Expression,
  operand_type: Type,
  result_type: Type,
) -> Expression {
  Expression(do_binary_operator(
    _,
    left,
    operator,
    right,
    operand_type,
    result_type,
  ))
}

fn do_binary_operator(
  state: State,
  left: Expression,
  operator: String,
  right: Expression,
  operand_type: Type,
  result_type: Type,
) -> Result(CompiledExpression, Error) {
  use state, left <- compile(state, left)
  use state, right <- compile(state, right)
  use #(state, _) <- result.try(unify(state, left.type_, operand_type))
  use #(state, _) <- result.try(unify(state, right.type_, operand_type))

  [left.document, doc.from_string(" " <> operator <> " "), right.document]
  |> doc.concat
  |> Compiled(result_type, state)
  |> Ok
}

pub fn add(left: Expression, right: Expression) -> Expression {
  binary_operator(left, "+", right, type_int, type_int)
}

pub fn add_float(left: Expression, right: Expression) -> Expression {
  binary_operator(left, "+.", right, type_float, type_float)
}

pub fn subtract(left: Expression, right: Expression) -> Expression {
  binary_operator(left, "-", right, type_int, type_int)
}

pub fn subtract_float(left: Expression, right: Expression) -> Expression {
  binary_operator(left, "-.", right, type_float, type_float)
}

pub fn multiply(left: Expression, right: Expression) -> Expression {
  binary_operator(left, "*", right, type_int, type_int)
}

pub fn multiply_float(left: Expression, right: Expression) -> Expression {
  binary_operator(left, "*.", right, type_float, type_float)
}

pub fn divide(left: Expression, right: Expression) -> Expression {
  binary_operator(left, "/", right, type_int, type_int)
}

pub fn divide_float(left: Expression, right: Expression) -> Expression {
  binary_operator(left, "/.", right, type_float, type_float)
}

pub fn remainder(left: Expression, right: Expression) -> Expression {
  binary_operator(left, "%", right, type_int, type_int)
}

pub fn concatenate(left: Expression, right: Expression) -> Expression {
  binary_operator(left, "<>", right, type_string, type_string)
}

pub fn and(left: Expression, right: Expression) -> Expression {
  binary_operator(left, "&&", right, type_bool, type_bool)
}

pub fn or(left: Expression, right: Expression) -> Expression {
  binary_operator(left, "||", right, type_bool, type_bool)
}

pub fn equal(left: Expression, right: Expression) -> Expression {
  use state <- Expression
  let #(state, type_) = type_variable(state)
  do_binary_operator(state, left, "==", right, type_, type_bool)
}

pub fn not_equal(left: Expression, right: Expression) -> Expression {
  use state <- Expression
  let #(state, type_) = type_variable(state)
  do_binary_operator(state, left, "!=", right, type_, type_bool)
}

pub fn less_than(left: Expression, right: Expression) -> Expression {
  binary_operator(left, "<", right, type_int, type_bool)
}

pub fn less_than_float(left: Expression, right: Expression) -> Expression {
  binary_operator(left, "<.", right, type_float, type_bool)
}

pub fn less_than_or_equal(left: Expression, right: Expression) -> Expression {
  binary_operator(left, "<=", right, type_int, type_bool)
}

pub fn less_than_or_equal_float(
  left: Expression,
  right: Expression,
) -> Expression {
  binary_operator(left, "<=.", right, type_float, type_bool)
}

pub fn greater_than(left: Expression, right: Expression) -> Expression {
  binary_operator(left, ">", right, type_int, type_bool)
}

pub fn greater_than_float(left: Expression, right: Expression) -> Expression {
  binary_operator(left, ">.", right, type_float, type_bool)
}

pub fn greater_than_or_equal(left: Expression, right: Expression) -> Expression {
  binary_operator(left, ">=", right, type_int, type_bool)
}

pub fn greater_than_or_equal_float(
  left: Expression,
  right: Expression,
) -> Expression {
  binary_operator(left, ">=.", right, type_float, type_bool)
}

fn unary_operator(
  operator: String,
  value: Expression,
  type_: Type,
) -> Expression {
  use state <- Expression
  use state, value <- compile(state, value)
  use #(state, type_) <- result.try(unify(state, value.type_, type_))
  [doc.from_string(operator), value.document]
  |> doc.concat
  |> Compiled(type_, state)
  |> Ok
}

pub fn negate_int(value: Expression) -> Expression {
  unary_operator("-", value, type_int)
}

pub fn negate_bool(value: Expression) -> Expression {
  unary_operator("!", value, type_bool)
}

pub fn list(values: List(Expression)) -> Expression {
  use state <- Expression
  use values <- result.try(
    list.try_map(values, fn(value) { value.compile(state) }),
  )

  use #(state, element_type) <- result.try(
    list.try_fold(values, type_variable(state), fn(acc, value) {
      let #(state, type_) = acc
      unify(state, value.type_, type_)
    }),
  )

  values
  |> list.map(fn(value) { value.document })
  |> doc.join(doc.break(", ", ","))
  |> doc.prepend(doc.break("[", "["))
  |> doc.nest(indent)
  |> doc.append(doc.break("", ", "))
  |> doc.append(doc.from_string("]"))
  |> doc.group
  |> Compiled(type_list(element_type), state)
  |> Ok
}

fn add_message(document: Document, message: Document) -> Document {
  [
    document,
    doc.break(" ", ""),
    doc.from_string("as "),
    message,
  ]
  |> grouped
}

pub fn panic_(message: Option(Expression)) -> Expression {
  use state <- Expression
  let #(state, type_) = type_variable(state)
  use #(state, doc) <- result.map(case message {
    None -> Ok(#(state, doc.from_string("panic")))
    Some(message) -> {
      use state, message <- compile(state, message)
      use #(state, _) <- result.map(unify(state, message.type_, type_string))
      #(state, add_message(doc.from_string("panic"), message.document))
    }
  })
  Compiled(doc, type_, state)
}

pub fn todo_(message: Option(Expression)) -> Expression {
  use state <- Expression
  let #(state, type_) = type_variable(state)
  use #(state, doc) <- result.map(case message {
    None -> Ok(#(state, doc.from_string("todo")))
    Some(message) -> {
      use state, message <- compile(state, message)
      use #(state, _) <- result.map(unify(state, message.type_, type_string))
      #(state, add_message(doc.from_string("todo"), message.document))
    }
  })
  Compiled(doc, type_, state)
}

pub fn echo_(value: Expression, message: Option(Expression)) -> Expression {
  use state <- Expression
  use state, value <- compile(state, value)

  let echo_ = doc.prepend(doc.from_string("echo "), to: value.document)

  use #(state, doc) <- result.map(case message {
    None -> Ok(#(state, echo_))
    Some(message) -> {
      use state, message <- compile(state, message)
      use #(state, _) <- result.map(unify(state, message.type_, type_string))
      #(state, add_message(echo_, message.document))
    }
  })
  Compiled(doc, value.type_, state)
}

pub fn variable(
  name: String,
  value: Expression,
  continue: fn(Expression) -> Statement,
) -> Statement {
  use state <- Statement
  use state, value <- compile(state, value)

  let declaration =
    [
      doc.from_string("let "),
      doc.from_string(name),
      doc.break(" = ", " ="),
      value.document,
    ]
    |> grouped
    |> doc.append(doc.line)

  let variable_expression =
    Expression(fn(state) {
      Ok(Compiled(doc.from_string(name), value.type_, state))
    })

  let rest = continue(variable_expression)
  use rest <- result.map(rest.compile(state))

  Compiled(doc.prepend(declaration, to: rest.document), rest.type_, state)
}

fn grouped(documents: List(Document)) -> Document {
  documents |> doc.concat |> doc.nest(indent) |> doc.group
}

pub fn expression(expression: Expression) -> Statement {
  Statement(expression.compile)
}

pub fn discard(statement: Statement, continue: fn() -> Statement) -> Statement {
  use state <- Statement
  use state, statement <- compile_statement(state, statement)
  let rest = continue()
  use rest <- result.map(rest.compile(state))
  Compiled(
    doc.concat([statement.document, doc.line, rest.document]),
    rest.type_,
    state,
  )
}

pub fn block(inner: Statement) -> Expression {
  use state <- Expression
  use inner <- result.map(inner.compile(state))

  [
    doc.from_string("{"),
    doc.line,
    inner.document,
  ]
  |> doc.concat
  |> doc.nest(indent)
  |> doc.append(doc.line)
  |> doc.append(doc.from_string("}"))
  |> Compiled(inner.type_, state)
}

pub fn assert_(condition: Expression, message: Option(Expression)) -> Statement {
  use state <- Statement
  use state, condition <- compile(state, condition)
  use #(state, _) <- result.try(unify(state, condition.type_, type_bool))

  let assert_ = doc.prepend(doc.from_string("assert "), to: condition.document)

  use #(state, doc) <- result.map(case message {
    None -> Ok(#(state, assert_))
    Some(message) -> {
      use state, message <- compile(state, message)
      use #(state, _) <- result.map(unify(state, message.type_, type_string))
      #(state, add_message(assert_, message.document))
    }
  })
  Compiled(doc, type_nil, state)
}
// TODO:
// BitString
// Call
// Case
// FieldAccess
// Fn
// FnCapture
// Let assert
// Let with patterns
// Pipes
// RecordUpdate
// Tuple
// TupleIndex
// Use
