import glam/doc.{type Document}
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
