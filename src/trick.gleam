import glam/doc.{type Document}
import gleam/bool
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import lazy_const
import splitter

pub opaque type Expression(type_) {
  Expression(document: Document)
}

pub opaque type Statement(type_) {
  Statement(document: Document)
}

const width = 80

const indent = 2

pub fn expression_to_string(expression: Expression(any)) -> String {
  doc.to_string(expression.document, width)
}

pub fn int(value: Int) -> Expression(Int) {
  value |> int.to_string |> doc.from_string |> Expression
}

pub fn float(value: Float) -> Expression(Float) {
  value |> float.to_string |> doc.from_string |> Expression
}

pub fn string(value: String) -> Expression(String) {
  value |> escape_string_literal |> doc.from_string |> Expression
}

pub fn bool(value: Bool) -> Expression(Bool) {
  value |> bool.to_string |> doc.from_string |> Expression
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
  left: Expression(a),
  operator: String,
  right: Expression(a),
) -> Expression(b) {
  [left.document, doc.from_string(" " <> operator <> " "), right.document]
  |> doc.concat
  |> Expression
}

pub fn add(left: Expression(Int), right: Expression(Int)) -> Expression(Int) {
  binary_operator(left, "+", right)
}

pub fn add_float(
  left: Expression(Float),
  right: Expression(Float),
) -> Expression(Float) {
  binary_operator(left, "+.", right)
}

pub fn subtract(
  left: Expression(Int),
  right: Expression(Int),
) -> Expression(Int) {
  binary_operator(left, "-", right)
}

pub fn subtract_float(
  left: Expression(Float),
  right: Expression(Float),
) -> Expression(Float) {
  binary_operator(left, "-.", right)
}

pub fn multiply(
  left: Expression(Int),
  right: Expression(Int),
) -> Expression(Int) {
  binary_operator(left, "*", right)
}

pub fn multiply_float(
  left: Expression(Float),
  right: Expression(Float),
) -> Expression(Float) {
  binary_operator(left, "*.", right)
}

pub fn divide(left: Expression(Int), right: Expression(Int)) -> Expression(Int) {
  binary_operator(left, "/", right)
}

pub fn divide_float(
  left: Expression(Float),
  right: Expression(Float),
) -> Expression(Float) {
  binary_operator(left, "/.", right)
}

pub fn remainder(
  left: Expression(Int),
  right: Expression(Int),
) -> Expression(Int) {
  binary_operator(left, "%", right)
}

pub fn concatenate(
  left: Expression(String),
  right: Expression(String),
) -> Expression(String) {
  binary_operator(left, "<>", right)
}

pub fn and(left: Expression(Bool), right: Expression(Bool)) -> Expression(Bool) {
  binary_operator(left, "&&", right)
}

pub fn or(left: Expression(Bool), right: Expression(Bool)) -> Expression(Bool) {
  binary_operator(left, "||", right)
}

pub fn equal(left: Expression(a), right: Expression(a)) -> Expression(Bool) {
  binary_operator(left, "==", right)
}

pub fn not_equal(left: Expression(a), right: Expression(a)) -> Expression(Bool) {
  binary_operator(left, "!=", right)
}

pub fn less_than(
  left: Expression(Int),
  right: Expression(Int),
) -> Expression(Int) {
  binary_operator(left, "<", right)
}

pub fn less_than_float(
  left: Expression(Float),
  right: Expression(Float),
) -> Expression(Float) {
  binary_operator(left, "<.", right)
}

pub fn less_than_or_equal(
  left: Expression(Int),
  right: Expression(Int),
) -> Expression(Int) {
  binary_operator(left, "<=", right)
}

pub fn less_than_or_equal_float(
  left: Expression(Float),
  right: Expression(Float),
) -> Expression(Float) {
  binary_operator(left, "<=.", right)
}

pub fn greater_than(
  left: Expression(Int),
  right: Expression(Int),
) -> Expression(Int) {
  binary_operator(left, ">", right)
}

pub fn greater_than_float(
  left: Expression(Float),
  right: Expression(Float),
) -> Expression(Float) {
  binary_operator(left, ">.", right)
}

pub fn greater_than_or_equal(
  left: Expression(Int),
  right: Expression(Int),
) -> Expression(Int) {
  binary_operator(left, ">=", right)
}

pub fn greater_than_or_equal_float(
  left: Expression(Float),
  right: Expression(Float),
) -> Expression(Float) {
  binary_operator(left, ">=.", right)
}

fn unary_operator(operator: String, value: Expression(a)) -> Expression(a) {
  [doc.from_string(operator), value.document] |> doc.concat |> Expression
}

pub fn negate_int(value: Expression(Int)) -> Expression(Int) {
  unary_operator("-", value)
}

pub fn negate_bool(value: Expression(Bool)) -> Expression(Bool) {
  unary_operator("!", value)
}

pub fn list(values: List(Expression(a))) -> Expression(List(a)) {
  values
  |> list.map(fn(value) { value.document })
  |> doc.join(doc.break(", ", ","))
  |> doc.prepend(doc.break("[", "["))
  |> doc.nest(indent)
  |> doc.append(doc.break("", ", "))
  |> doc.append(doc.from_string("]"))
  |> doc.group
  |> Expression
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

pub fn panic_(message: Option(Expression(String))) -> Expression(a) {
  Expression(case message {
    None -> doc.from_string("panic")
    Some(message) -> add_message(doc.from_string("panic"), message.document)
  })
}

pub fn todo_(message: Option(Expression(String))) -> Expression(a) {
  Expression(case message {
    None -> doc.from_string("todo")
    Some(message) -> add_message(doc.from_string("todo"), message.document)
  })
}

pub fn echo_(
  value: Expression(a),
  message: Option(Expression(String)),
) -> Expression(a) {
  let echo_ = doc.prepend(doc.from_string("echo "), to: value.document)

  Expression(case message {
    None -> echo_
    Some(message) -> add_message(echo_, message.document)
  })
}

pub fn variable(
  name: String,
  value: Expression(a),
  continue: fn(Expression(a)) -> Statement(b),
) -> Statement(b) {
  let declaration =
    [
      doc.from_string("let "),
      doc.from_string(name),
      doc.break(" = ", " ="),
      value.document,
    ]
    |> grouped
    |> doc.append(doc.line)

  let variable_expression = Expression(doc.from_string(name))

  let rest = continue(variable_expression)

  Statement(doc.prepend(declaration, to: rest.document))
}

fn grouped(documents: List(Document)) -> Document {
  documents |> doc.concat |> doc.nest(indent) |> doc.group
}

pub fn expression(expression: Expression(a)) -> Statement(a) {
  Statement(expression.document)
}

pub fn discard(
  statement: Statement(a),
  continue: fn() -> Statement(b),
) -> Statement(b) {
  let rest = continue()
  Statement(doc.concat([statement.document, doc.line, rest.document]))
}

pub fn block(inner: Statement(a)) -> Expression(a) {
  [
    doc.from_string("{"),
    doc.line,
    inner.document,
  ]
  |> doc.concat
  |> doc.nest(indent)
  |> doc.append(doc.line)
  |> doc.append(doc.from_string("}"))
  |> Expression
}

pub fn assert_(
  condition: Expression(Bool),
  message: Option(Expression(String)),
) -> Statement(Nil) {
  let assert_ = doc.prepend(doc.from_string("assert "), to: condition.document)

  Statement(case message {
    None -> assert_
    Some(message) -> add_message(assert_, message.document)
  })
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
