import glam/doc.{type Document}
import gleam/bool
import gleam/float
import gleam/int
import lazy_const
import splitter

pub opaque type Expression(type_) {
  Expression(document: Document)
}

const width = 80

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

pub fn escape_string_literal(string: String) -> String {
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
