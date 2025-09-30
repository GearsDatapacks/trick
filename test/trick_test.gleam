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

pub fn binary_operator_test() {
  assert trick.add(trick.int(10), trick.int(20)) |> trick.expression_to_string
    == "10 + 20"

  assert trick.add_float(trick.float(3.14), trick.float(1.41))
    |> trick.expression_to_string
    == "3.14 +. 1.41"

  assert trick.subtract(trick.int(10), trick.int(20))
    |> trick.expression_to_string
    == "10 - 20"

  assert trick.subtract_float(trick.float(3.14), trick.float(1.41))
    |> trick.expression_to_string
    == "3.14 -. 1.41"

  assert trick.multiply(trick.int(10), trick.int(20))
    |> trick.expression_to_string
    == "10 * 20"

  assert trick.multiply_float(trick.float(3.14), trick.float(1.41))
    |> trick.expression_to_string
    == "3.14 *. 1.41"

  assert trick.divide(trick.int(10), trick.int(20))
    |> trick.expression_to_string
    == "10 / 20"

  assert trick.divide_float(trick.float(3.14), trick.float(1.41))
    |> trick.expression_to_string
    == "3.14 /. 1.41"

  assert trick.remainder(trick.int(10), trick.int(3))
    |> trick.expression_to_string
    == "10 % 3"

  assert trick.remainder(trick.int(10), trick.int(3))
    |> trick.expression_to_string
    == "10 % 3"

  assert trick.concatenate(trick.string("Hello,"), trick.string(" world!"))
    |> trick.expression_to_string
    == "\"Hello,\" <> \" world!\""

  assert trick.and(trick.bool(False), trick.bool(True))
    |> trick.expression_to_string
    == "False && True"

  assert trick.or(trick.bool(False), trick.bool(True))
    |> trick.expression_to_string
    == "False || True"

  assert trick.less_than(trick.int(10), trick.int(20))
    |> trick.expression_to_string
    == "10 < 20"

  assert trick.less_than_float(trick.float(3.14), trick.float(1.41))
    |> trick.expression_to_string
    == "3.14 <. 1.41"

  assert trick.less_than_or_equal(trick.int(10), trick.int(20))
    |> trick.expression_to_string
    == "10 <= 20"

  assert trick.less_than_or_equal_float(trick.float(3.14), trick.float(1.41))
    |> trick.expression_to_string
    == "3.14 <=. 1.41"

  assert trick.greater_than(trick.int(10), trick.int(20))
    |> trick.expression_to_string
    == "10 > 20"

  assert trick.greater_than_float(trick.float(3.14), trick.float(1.41))
    |> trick.expression_to_string
    == "3.14 >. 1.41"

  assert trick.greater_than_or_equal(trick.int(10), trick.int(20))
    |> trick.expression_to_string
    == "10 >= 20"

  assert trick.greater_than_or_equal_float(trick.float(3.14), trick.float(1.41))
    |> trick.expression_to_string
    == "3.14 >=. 1.41"

  assert trick.equal(trick.float(3.14), trick.float(1.41))
    |> trick.expression_to_string
    == "3.14 == 1.41"

  assert trick.equal(trick.int(23), trick.int(89))
    |> trick.expression_to_string
    == "23 == 89"

  assert trick.equal(trick.bool(False), trick.bool(False))
    |> trick.expression_to_string
    == "False == False"

  assert trick.not_equal(trick.float(3.14), trick.float(1.41))
    |> trick.expression_to_string
    == "3.14 != 1.41"

  assert trick.not_equal(trick.int(23), trick.int(89))
    |> trick.expression_to_string
    == "23 != 89"

  assert trick.not_equal(trick.bool(False), trick.bool(False))
    |> trick.expression_to_string
    == "False != False"
}

pub fn unary_operator_test() {
  assert 10 |> trick.int |> trick.negate_int |> trick.expression_to_string
    == "-10"

  assert False |> trick.bool |> trick.negate_bool |> trick.expression_to_string
    == "!False"
}
