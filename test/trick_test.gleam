import birdie
import gleam/list
import gleam/option.{None, Some}
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
  "Hello! This is a string.
It has multiple lines,\nand even quotes: \". This is how you type a backslash: \\\\"
  |> trick.string
  |> trick.expression_to_string
  |> birdie.snap("string_literal")
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

pub fn list_test() {
  assert [1, 2, 3, 4]
    |> list.map(trick.int)
    |> trick.list
    |> trick.expression_to_string
    == "[1, 2, 3, 4]"
}

pub fn long_list_test() {
  [
    "Some long string",
    "A different but equally long string",
    "One last long string to get past the line limit",
  ]
  |> list.map(trick.string)
  |> trick.list
  |> trick.expression_to_string
  |> birdie.snap("long_list")
}

pub fn panic_no_message_test() {
  assert trick.panic_(None) |> trick.expression_to_string == "panic"
}

pub fn panic_with_message_test() {
  "Uh oh"
  |> trick.string
  |> Some
  |> trick.panic_
  |> trick.expression_to_string
  |> birdie.snap("panic_with_message")
}

pub fn panic_with_long_message_test() {
  "Something went wrong. This message is detailed and explains why, causing it to go over the line limit"
  |> trick.string
  |> Some
  |> trick.panic_
  |> trick.expression_to_string
  |> birdie.snap("panic_with_long_message")
}

pub fn todo_no_message_test() {
  assert trick.todo_(None) |> trick.expression_to_string == "todo"
}

pub fn todo_with_message_test() {
  "This code is coming soon!"
  |> trick.string
  |> Some
  |> trick.todo_
  |> trick.expression_to_string
  |> birdie.snap("todo_with_message")
}

pub fn todo_with_long_message_test() {
  "This code is unfortunately not yet implemented. Please try again later to check if it has been written"
  |> trick.string
  |> Some
  |> trick.todo_
  |> trick.expression_to_string
  |> birdie.snap("todo_with_long_message")
}

pub fn echo_test() {
  assert 15 |> trick.int |> trick.echo_(None) |> trick.expression_to_string
    == "echo 15"
}

pub fn echo_with_message_test() {
  3.14
  |> trick.float
  |> trick.echo_(Some(trick.string("This is a pi!")))
  |> trick.expression_to_string
  |> birdie.snap("echo_with_message")
}

pub fn echo_with_long_message_test() {
  3.14
  |> trick.float
  |> trick.echo_(
    Some(trick.string(
      "This is a pi! It is the ratio between the radius and circumference of a circle.",
    )),
  )
  |> trick.expression_to_string
  |> birdie.snap("echo_with_long_message")
}

pub fn block_test() {
  let block = {
    use a <- trick.variable("a", trick.int(1))
    use b <- trick.variable("b", trick.int(2))
    trick.expression(trick.add(a, b))
  }

  block |> trick.block |> trick.expression_to_string |> birdie.snap("block")
}

pub fn block_with_discarded_expression_test() {
  let block = {
    use a <- trick.variable("a", trick.int(1))
    use b <- trick.variable("b", trick.add(a, trick.int(1)))
    use <- trick.discard(trick.expression(trick.echo_(b, None)))
    trick.expression(trick.multiply(a, b))
  }

  block
  |> trick.block
  |> trick.expression_to_string
  |> birdie.snap("block_with_discarded_expression")
}
