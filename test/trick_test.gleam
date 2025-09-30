import gleeunit
import trick

pub fn main() -> Nil {
  gleeunit.main()
}

pub fn int_literal_test() {
  assert 53 |> trick.int |> trick.expression_to_string == "53"
}

pub fn float_literal_test() {
  assert 3.1415 |> trick.float |> trick.expression_to_string == "3.1415"
}

pub fn string_literal_test() {
  assert "Hello! This is a string.
It has multiple lines,\nand even quotes: \". This is how you type a backslash: \\\\"
    |> trick.string
    |> trick.expression_to_string
    == "\"Hello! This is a string.
It has multiple lines,\nand even quotes: \\\". This is how you type a backslash: \\\\\\\\\""
}
