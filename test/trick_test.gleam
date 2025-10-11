import birdie
import gleam/list
import gleam/option.{None, Some}
import gleeunit
import trick

pub fn main() -> Nil {
  gleeunit.main()
}

fn unwrap(x: Result(a, b)) -> a {
  let assert Ok(x) = x
  x
}

pub fn int_literal_test() {
  assert 53 |> trick.int |> trick.expression_to_string |> unwrap == "53"
}

pub fn float_literal_test() {
  assert 3.1415 |> trick.float |> trick.expression_to_string |> unwrap
    == "3.1415"
}

pub fn string_literal_test() {
  "Hello! This is a string.
It has multiple lines,\nand even quotes: \". This is how you type a backslash: \\\\"
  |> trick.string
  |> trick.expression_to_string
  |> unwrap
  |> birdie.snap("string_literal")
}

pub fn binary_operator_test() {
  assert trick.add(trick.int(10), trick.int(20))
    |> trick.expression_to_string
    |> unwrap
    == "10 + 20"

  assert trick.add_float(trick.float(3.14), trick.float(1.41))
    |> trick.expression_to_string
    |> unwrap
    == "3.14 +. 1.41"

  assert trick.subtract(trick.int(10), trick.int(20))
    |> trick.expression_to_string
    |> unwrap
    == "10 - 20"

  assert trick.subtract_float(trick.float(3.14), trick.float(1.41))
    |> trick.expression_to_string
    |> unwrap
    == "3.14 -. 1.41"

  assert trick.multiply(trick.int(10), trick.int(20))
    |> trick.expression_to_string
    |> unwrap
    == "10 * 20"

  assert trick.multiply_float(trick.float(3.14), trick.float(1.41))
    |> trick.expression_to_string
    |> unwrap
    == "3.14 *. 1.41"

  assert trick.divide(trick.int(10), trick.int(20))
    |> trick.expression_to_string
    |> unwrap
    == "10 / 20"

  assert trick.divide_float(trick.float(3.14), trick.float(1.41))
    |> trick.expression_to_string
    |> unwrap
    == "3.14 /. 1.41"

  assert trick.remainder(trick.int(10), trick.int(3))
    |> trick.expression_to_string
    |> unwrap
    == "10 % 3"

  assert trick.remainder(trick.int(10), trick.int(3))
    |> trick.expression_to_string
    |> unwrap
    == "10 % 3"

  assert trick.concatenate(trick.string("Hello,"), trick.string(" world!"))
    |> trick.expression_to_string
    |> unwrap
    == "\"Hello,\" <> \" world!\""

  assert trick.and(trick.bool(False), trick.bool(True))
    |> trick.expression_to_string
    |> unwrap
    == "False && True"

  assert trick.or(trick.bool(False), trick.bool(True))
    |> trick.expression_to_string
    |> unwrap
    == "False || True"

  assert trick.less_than(trick.int(10), trick.int(20))
    |> trick.expression_to_string
    |> unwrap
    == "10 < 20"

  assert trick.less_than_float(trick.float(3.14), trick.float(1.41))
    |> trick.expression_to_string
    |> unwrap
    == "3.14 <. 1.41"

  assert trick.less_than_or_equal(trick.int(10), trick.int(20))
    |> trick.expression_to_string
    |> unwrap
    == "10 <= 20"

  assert trick.less_than_or_equal_float(trick.float(3.14), trick.float(1.41))
    |> trick.expression_to_string
    |> unwrap
    == "3.14 <=. 1.41"

  assert trick.greater_than(trick.int(10), trick.int(20))
    |> trick.expression_to_string
    |> unwrap
    == "10 > 20"

  assert trick.greater_than_float(trick.float(3.14), trick.float(1.41))
    |> trick.expression_to_string
    |> unwrap
    == "3.14 >. 1.41"

  assert trick.greater_than_or_equal(trick.int(10), trick.int(20))
    |> trick.expression_to_string
    |> unwrap
    == "10 >= 20"

  assert trick.greater_than_or_equal_float(trick.float(3.14), trick.float(1.41))
    |> trick.expression_to_string
    |> unwrap
    == "3.14 >=. 1.41"

  assert trick.equal(trick.float(3.14), trick.float(1.41))
    |> trick.expression_to_string
    |> unwrap
    == "3.14 == 1.41"

  assert trick.equal(trick.int(23), trick.int(89))
    |> trick.expression_to_string
    |> unwrap
    == "23 == 89"

  assert trick.equal(trick.bool(False), trick.bool(False))
    |> trick.expression_to_string
    |> unwrap
    == "False == False"

  assert trick.not_equal(trick.float(3.14), trick.float(1.41))
    |> trick.expression_to_string
    |> unwrap
    == "3.14 != 1.41"

  assert trick.not_equal(trick.int(23), trick.int(89))
    |> trick.expression_to_string
    |> unwrap
    == "23 != 89"

  assert trick.not_equal(trick.bool(False), trick.bool(False))
    |> trick.expression_to_string
    |> unwrap
    == "False != False"
}

const type_int = trick.Custom("gleam", "Int", [])

const type_float = trick.Custom("gleam", "Float", [])

const type_string = trick.Custom("gleam", "String", [])

const type_bool = trick.Custom("gleam", "Bool", [])

const type_nil = trick.Custom("gleam", "Nil", [])

fn type_list(element: trick.Type) -> trick.Type {
  trick.Custom("gleam", "List", [element])
}

pub fn binary_operator_type_mismatch_test() {
  let assert Error(error) =
    trick.add(trick.int(10), trick.float(20.0))
    |> trick.expression_to_string
  assert error == trick.TypeMismatch(expected: type_int, got: type_float)

  let assert Error(error) =
    trick.add_float(trick.string("Hello"), trick.int(12))
    |> trick.expression_to_string
  assert error == trick.TypeMismatch(expected: type_float, got: type_string)

  let assert Error(error) =
    trick.equal(trick.string("Hello"), trick.bool(True))
    |> trick.expression_to_string
  assert error == trick.TypeMismatch(expected: type_string, got: type_bool)

  let assert Error(error) =
    trick.not_equal(trick.nil(), trick.int(23))
    |> trick.expression_to_string
  assert error == trick.TypeMismatch(expected: type_nil, got: type_int)
}

pub fn unary_operator_test() {
  assert 10
    |> trick.int
    |> trick.negate_int
    |> trick.expression_to_string
    |> unwrap
    == "-10"

  assert False
    |> trick.bool
    |> trick.negate_bool
    |> trick.expression_to_string
    |> unwrap
    == "!False"
}

pub fn unary_operator_type_mismatch_test() {
  let assert Error(error) =
    trick.negate_int(trick.float(20.0))
    |> trick.expression_to_string
  assert error == trick.TypeMismatch(expected: type_int, got: type_float)

  let assert Error(error) =
    trick.negate_bool(trick.nil())
    |> trick.expression_to_string
  assert error == trick.TypeMismatch(expected: type_bool, got: type_nil)
}

pub fn list_test() {
  assert [1, 2, 3, 4]
    |> list.map(trick.int)
    |> trick.list
    |> trick.expression_to_string
    |> unwrap
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
  |> unwrap
  |> birdie.snap("long_list")
}

pub fn list_type_mismatch_test() {
  let assert Error(error) =
    trick.list([trick.int(10), trick.int(20), trick.float(30.0)])
    |> trick.expression_to_string
  assert error == trick.TypeMismatch(expected: type_int, got: type_float)
}

pub fn panic_no_message_test() {
  assert trick.panic_(None) |> trick.expression_to_string |> unwrap == "panic"
}

pub fn panic_with_message_test() {
  "Uh oh"
  |> trick.string
  |> Some
  |> trick.panic_
  |> trick.expression_to_string
  |> unwrap
  |> birdie.snap("panic_with_message")
}

pub fn panic_with_long_message_test() {
  "Something went wrong. This message is detailed and explains why, causing it to go over the line limit"
  |> trick.string
  |> Some
  |> trick.panic_
  |> trick.expression_to_string
  |> unwrap
  |> birdie.snap("panic_with_long_message")
}

pub fn todo_no_message_test() {
  assert trick.todo_(None) |> trick.expression_to_string |> unwrap == "todo"
}

pub fn todo_with_message_test() {
  "This code is coming soon!"
  |> trick.string
  |> Some
  |> trick.todo_
  |> trick.expression_to_string
  |> unwrap
  |> birdie.snap("todo_with_message")
}

pub fn todo_with_long_message_test() {
  "This code is unfortunately not yet implemented. Please try again later to check if it has been written"
  |> trick.string
  |> Some
  |> trick.todo_
  |> trick.expression_to_string
  |> unwrap
  |> birdie.snap("todo_with_long_message")
}

pub fn echo_test() {
  assert 15
    |> trick.int
    |> trick.echo_(None)
    |> trick.expression_to_string
    |> unwrap
    == "echo 15"
}

pub fn echo_with_message_test() {
  3.14
  |> trick.float
  |> trick.echo_(Some(trick.string("This is a pi!")))
  |> trick.expression_to_string
  |> unwrap
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
  |> unwrap
  |> birdie.snap("echo_with_long_message")
}

pub fn block_test() {
  let block = {
    use a <- trick.variable("a", trick.int(1))
    use b <- trick.variable("b", trick.int(2))
    trick.expression(trick.add(a, b))
  }

  block
  |> trick.block
  |> trick.expression_to_string
  |> unwrap
  |> birdie.snap("block")
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
  |> unwrap
  |> birdie.snap("block_with_discarded_expression")
}

pub fn unified_variable_cannot_be_changed_test() {
  let assert Error(error) =
    {
      use x <- trick.variable("x", trick.todo_(None))
      use <- trick.discard(trick.expression(trick.add(x, trick.int(1))))
      trick.expression(trick.concatenate(x, trick.string("!")))
    }
    |> trick.block
    |> trick.expression_to_string
  assert error == trick.TypeMismatch(expected: type_string, got: type_int)
}

pub fn assert_test() {
  trick.equal(trick.int(1), trick.int(2))
  |> trick.assert_(None)
  |> trick.block
  |> trick.expression_to_string
  |> unwrap
  |> birdie.snap("assert")
}

pub fn assert_with_message_test() {
  trick.equal(trick.int(1), trick.int(2))
  |> trick.assert_(Some(trick.string("This is always going to fail")))
  |> trick.block
  |> trick.expression_to_string
  |> unwrap
  |> birdie.snap("assert_with_message")
}

pub fn assert_with_long_message_test() {
  trick.equal(trick.int(1), trick.int(2))
  |> trick.assert_(
    Some(trick.string(
      "This is always going to fail, because one should not be equal to two, unless something weird is happening",
    )),
  )
  |> trick.block
  |> trick.expression_to_string
  |> unwrap
  |> birdie.snap("assert_with_long_message")
}

pub fn tuple_test() {
  assert [trick.int(1), trick.bool(False), trick.float(3.14)]
    |> trick.tuple
    |> trick.expression_to_string
    |> unwrap
    == "#(1, False, 3.14)"
}

pub fn long_tuple_test() {
  [
    trick.int(1),
    trick.bool(False),
    trick.float(3.14),
    trick.string(
      "This string is long enough to cause the tuple to exceed the line limit",
    ),
  ]
  |> trick.tuple
  |> trick.expression_to_string
  |> unwrap
  |> birdie.snap("long_tuple")
}

pub fn tuple_access_test() {
  {
    use tuple <- trick.variable(
      "tuple",
      trick.tuple([trick.int(10), trick.float(10.0), trick.string("10")]),
    )
    trick.tuple_index(tuple, 1)
    |> trick.add_float(trick.float(1.0))
    |> trick.expression
  }
  |> trick.block
  |> trick.expression_to_string
  |> unwrap
  |> birdie.snap("tuple_access")
}

pub fn tuple_access_error_test() {
  let assert Error(error) =
    [trick.int(10), trick.float(10.0)]
    |> trick.tuple
    |> trick.tuple_index(5)
    |> trick.expression_to_string
  assert error == trick.TupleIndexOutOfBounds(length: 2, index: 5)

  let assert Error(error) =
    [1, 2, 3]
    |> list.map(trick.int)
    |> trick.list
    |> trick.tuple_index(2)
    |> trick.expression_to_string
  assert error == trick.InvalidTupleAccess(type_: type_list(type_int))
}

pub fn anonymous_function_test() {
  {
    let anonymous =
      trick.anonymous({
        use a <- trick.parameter("a", type_int)
        use b <- trick.parameter("b", type_int)
        trick.add(a, b) |> trick.expression |> trick.function_body
      })

    use f <- trick.variable("f", anonymous)
    trick.expression(trick.call(f, [trick.int(1), trick.int(2)]))
  }
  |> trick.block
  |> trick.expression_to_string
  |> unwrap
  |> birdie.snap("anonymous_function")
}

pub fn anonymous_function_with_multiple_statements_test() {
  {
    use a <- trick.parameter("a", type_int)
    use b <- trick.parameter("b", type_int)
    {
      use c <- trick.variable("c", trick.add(a, b))
      use <- trick.discard(trick.expression(trick.echo_(c, None)))
      trick.expression(c)
    }
    |> trick.function_body
  }
  |> trick.anonymous
  |> trick.expression_to_string
  |> unwrap
  |> birdie.snap("anonymous_function_with_multiple_statements")
}

pub fn long_call_test() {
  {
    use concat <- trick.variable(
      "concat",
      trick.anonymous({
        use a <- trick.parameter("a", type_string)
        use b <- trick.parameter("b", type_string)
        trick.concatenate(a, b)
        |> trick.expression
        |> trick.function_body
      }),
    )

    concat
    |> trick.call([
      trick.string("This string is pretty long by itself, but"),
      trick.string(" combined with this one it goes over the line limit"),
    ])
    |> trick.expression
  }
  |> trick.block
  |> trick.expression_to_string
  |> unwrap
  |> birdie.snap("long_call")
}

pub fn invalid_call_test() {
  let assert Error(error) =
    trick.int(10)
    |> trick.call([trick.float(10.0)])
    |> trick.expression_to_string
  assert error == trick.InvalidCall(type_int)
}

pub fn incorrect_number_of_arguments_test() {
  let function =
    trick.anonymous({
      use a <- trick.parameter("a", type_int)
      a
      |> trick.add(trick.int(1))
      |> trick.expression
      |> trick.function_body
    })

  let assert Error(error) =
    function
    |> trick.call([trick.int(1), trick.int(2)])
    |> trick.expression_to_string
  assert error == trick.IncorrectNumberOfArguments(expected: 1, got: 2)
}

pub fn incorrect_argument_type_test() {
  let function =
    trick.anonymous({
      use a <- trick.parameter("a", type_int)
      a
      |> trick.add(trick.int(1))
      |> trick.expression
      |> trick.function_body
    })

  let assert Error(error) =
    function
    |> trick.call([trick.float(1.0)])
    |> trick.expression_to_string
  assert error == trick.TypeMismatch(expected: type_int, got: type_float)
}

pub fn single_expression_block_test() {
  trick.add(trick.int(1), trick.int(2))
  |> trick.expression
  |> trick.block
  |> trick.expression_to_string
  |> unwrap
  |> birdie.snap("single_expression_block")
}

pub fn function_capture_test() {
  let add =
    trick.anonymous({
      use a <- trick.parameter("a", type_int)
      use b <- trick.parameter("b", type_int)
      trick.add(a, b) |> trick.expression |> trick.function_body
    })

  {
    use add <- trick.variable("add", add)
    use add_one <- trick.variable(
      "add_one",
      trick.function_capture(add, [], [trick.int(1)]),
    )
    trick.expression(trick.add(
      trick.call(add_one, [trick.int(5)]),
      trick.int(1),
    ))
  }
  |> trick.block
  |> trick.expression_to_string
  |> unwrap
  |> birdie.snap("function_capture")
}

pub fn invalid_function_capture_test() {
  let assert Error(error) =
    trick.int(10)
    |> trick.function_capture([], [])
    |> trick.expression_to_string
  assert error == trick.InvalidCall(type_int)
}

pub fn incorrect_number_of_arguments_capture_test() {
  let function =
    trick.anonymous({
      use a <- trick.parameter("a", type_int)
      a
      |> trick.add(trick.int(1))
      |> trick.expression
      |> trick.function_body
    })

  let assert Error(error) =
    function
    |> trick.function_capture([trick.int(1), trick.int(2)], [
      trick.int(4),
      trick.int(5),
    ])
    |> trick.expression_to_string
  assert error == trick.IncorrectNumberOfArguments(expected: 1, got: 5)
}

pub fn incorrect_argument_type_capture_test() {
  let function =
    trick.anonymous({
      use a <- trick.parameter("a", type_int)
      use b <- trick.parameter("b", type_int)
      use c <- trick.parameter("c", type_int)
      a
      |> trick.add(b)
      |> trick.add(c)
      |> trick.expression
      |> trick.function_body
    })

  let assert Error(error) =
    function
    |> trick.function_capture([trick.int(1)], [trick.float(2.0)])
    |> trick.expression_to_string
  assert error == trick.TypeMismatch(expected: type_int, got: type_float)
}

pub fn function_test() {
  trick.function(
    "add",
    {
      use a <- trick.parameter("a", type_int)
      use b <- trick.parameter("b", type_int)
      trick.add(a, b)
      |> trick.expression
      |> trick.function_body
    },
    fn(_) { trick.empty() },
  )
  |> trick.to_string
  |> unwrap
  |> birdie.snap("function")
}

pub fn multiple_functions_test() {
  {
    use add <- trick.function("add", {
      use a <- trick.parameter("a", type_int)
      use b <- trick.parameter("b", type_int)
      trick.add(a, b)
      |> trick.expression
      |> trick.function_body
    })

    use _main <- trick.function(
      "main",
      trick.function_body({
        use <- trick.discard(
          trick.expression(trick.echo_(
            trick.call(add, [trick.int(1), trick.int(2)]),
            None,
          )),
        )
        trick.expression(trick.nil())
      }),
    )

    trick.empty()
  }
  |> trick.to_string
  |> unwrap
  |> birdie.snap("multiple_functions")
}

pub fn multiple_functions_type_check_correctly_test() {
  let assert Error(error) =
    {
      use add <- trick.function("add", {
        use a <- trick.parameter("a", type_int)
        use b <- trick.parameter("b", type_int)
        trick.add(a, b)
        |> trick.expression
        |> trick.function_body
      })

      trick.call(add, [trick.int(1), trick.int(2), trick.int(3)])
      |> trick.expression
      |> trick.function_body
      |> trick.function("main", _, fn(_main) { trick.empty() })
    }
    |> trick.to_string

  assert error == trick.IncorrectNumberOfArguments(expected: 2, got: 3)
}

pub fn labelled_parameter_test() {
  {
    use add <- trick.function("add", {
      use a <- trick.labelled_parameter("a", "a", type_int)
      use b <- trick.labelled_parameter("b", "b", type_int)
      trick.add(a, b)
      |> trick.expression
      |> trick.function_body
    })

    trick.call(add, [trick.int(1), trick.int(2)])
    |> trick.expression
    |> trick.function_body
    |> trick.function("main", _, fn(_main) { trick.empty() })
  }
  |> trick.to_string
  |> unwrap
  |> birdie.snap("labelled_parameter")
}

pub fn unlabelled_after_labelled_test() {
  let assert Error(error) =
    trick.function(
      "add",
      {
        use a <- trick.labelled_parameter("a", "a", type_int)
        use b <- trick.labelled_parameter("b", "b", type_int)
        use c <- trick.parameter("c", type_int)
        a
        |> trick.add(b)
        |> trick.add(c)
        |> trick.expression
        |> trick.function_body
      },
      fn(_add) { trick.empty() },
    )
    |> trick.to_string

  assert error == trick.UnlabelledParameterAfterLabelledParameter
}

pub fn recursive_function_test() {
  {
    trick.function(
      "add",
      {
        use a <- trick.labelled_parameter("a", "a", type_int)
        use b <- trick.labelled_parameter("b", "b", type_int)
        use add <- trick.recursive
        trick.call(add, [a, b])
        |> trick.expression
      },
      fn(_) { trick.empty() },
    )
  }
  |> trick.to_string
  |> unwrap
  |> birdie.snap("recursive_function")
}

pub fn recursive_function_has_correct_type_test() {
  let assert Error(error) =
    {
      trick.function(
        "add",
        {
          use a <- trick.labelled_parameter("a", "a", type_int)
          use b <- trick.labelled_parameter("b", "b", type_int)
          use add <- trick.recursive
          trick.call(add, [a, b, trick.int(1)])
          |> trick.expression
        },
        fn(_) { trick.empty() },
      )
    }
    |> trick.to_string

  assert error == trick.IncorrectNumberOfArguments(expected: 2, got: 3)
}

pub fn recursive_function_has_correct_type2_test() {
  let assert Error(error) =
    {
      trick.function(
        "add",
        {
          use a <- trick.labelled_parameter("a", "a", type_int)
          use b <- trick.labelled_parameter("b", "b", type_int)
          use add <- trick.recursive
          use <- trick.discard(
            trick.expression(trick.add_float(
              trick.call(add, [trick.int(1), trick.int(2)]),
              trick.float(1.0),
            )),
          )
          trick.add(a, b)
          |> trick.expression
        },
        fn(_) { trick.empty() },
      )
    }
    |> trick.to_string

  assert error == trick.TypeMismatch(expected: type_float, got: type_int)
}
