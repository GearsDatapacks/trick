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
  Expression(compile: fn(State) -> Result(#(State, Compiled), Error))
}

pub type Error {
  TypeMismatch(expected: Type, got: Type)
  TupleIndexOutOfBounds(length: Int, index: Int)
  InvalidTupleAccess(type_: Type)
  InvalidCall(type_: Type)
  IncorrectNumberOfArguments(expected: Int, got: Int)
}

type Compiled {
  Compiled(document: Document, type_: Type)
}

pub type Type {
  Custom(module: String, name: String, generics: List(Type))
  TypeVariable(id: Int)
  Tuple(elements: List(Type))
  Function(parameters: List(Type), return: Type)
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
  expression: Expression,
  continue: fn(Compiled) -> Expression,
) -> Expression {
  try(expression.compile, continue)
}

fn compile_statement(
  statement: Statement,
  continue: fn(Compiled) -> Expression,
) -> Expression {
  try(statement.compile, continue)
}

fn return(value: Compiled) -> Expression {
  Expression(fn(state) { Ok(#(state, value)) })
}

fn error(error: Error) -> Expression {
  Expression(fn(_state) { Error(error) })
}

fn pure(value: Result(a, Error)) -> fn(State) -> Result(#(State, a), Error) {
  fn(state) {
    case value {
      Error(error) -> Error(error)
      Ok(value) -> Ok(#(state, value))
    }
  }
}

fn try(
  f: fn(State) -> Result(#(State, a), Error),
  continue: fn(a) -> Expression,
) -> Expression {
  use state <- Expression
  case f(state) {
    Error(error) -> Error(error)
    Ok(#(state, value)) -> continue(value).compile(state)
  }
}

fn then(
  f: fn(State) -> #(State, a),
  continue: fn(a) -> Expression,
) -> Expression {
  use state <- Expression
  let #(state, value) = f(state)
  continue(value).compile(state)
}

fn compile_expressions(
  list: List(Expression),
  continue: fn(List(Compiled)) -> Expression,
) -> Expression {
  use state <- Expression
  use #(state, values) <- result.try(do_compile_expressions(state, list, []))
  continue(values).compile(state)
}

fn do_compile_expressions(
  state: State,
  list: List(Expression),
  out: List(Compiled),
) -> Result(#(State, List(Compiled)), Error) {
  case list {
    [] -> Ok(#(state, list.reverse(out)))
    [first, ..rest] ->
      case first.compile(state) {
        Error(error) -> Error(error)
        Ok(#(state, value)) ->
          do_compile_expressions(state, rest, [value, ..out])
      }
  }
}

fn fold_list(
  list: List(a),
  initial: fn(State) -> #(State, b),
  f: fn(b, a) -> fn(State) -> Result(#(State, b), Error),
  continue: fn(b) -> Expression,
) -> Expression {
  use state <- Expression
  use #(state, value) <- result.try(
    list.try_fold(list, initial(state), fn(acc, value) {
      let #(state, acc) = acc
      f(acc, value)(state)
    }),
  )

  continue(value).compile(state)
}

fn try_each(
  list: List(a),
  f: fn(a) -> fn(State) -> Result(#(State, _), Error),
  continue: fn() -> Expression,
) -> Expression {
  use state <- Expression
  use state <- result.try(
    list.try_fold(list, state, fn(state, value) {
      case f(value)(state) {
        Ok(#(state, _)) -> Ok(state)
        Error(error) -> Error(error)
      }
    }),
  )

  continue().compile(state)
}

fn unify(a: Type, with b: Type) -> fn(State) -> Result(#(State, Type), Error) {
  do_unify(_, a, b)
}

fn do_unify(
  state: State,
  a: Type,
  with b: Type,
) -> Result(#(State, Type), Error) {
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
      do_unify(state, resolved_type, type_)
    Ok(resolved_type) -> do_unify(state, type_, resolved_type)
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

fn unwrap_type(type_: Type) -> fn(State) -> #(State, Type) {
  fn(state: State) {
    let unwrapped = case type_ {
      Custom(..) | Tuple(..) | Function(..) -> type_
      TypeVariable(id:) ->
        case dict.get(state.resolved_variables, id) {
          Error(_) -> type_
          Ok(type_) -> type_
        }
    }
    #(state, unwrapped)
  }
}

pub opaque type Statement {
  Statement(compile: fn(State) -> Result(#(State, Compiled), Error))
}

const width = 80

const indent = 2

pub fn expression_to_string(expression: Expression) -> Result(String, Error) {
  case expression.compile(new_state()) {
    Ok(#(_state, expression)) -> Ok(doc.to_string(expression.document, width))
    Error(error) -> Error(error)
  }
}

fn new_state() -> State {
  State(dict.new(), 0)
}

pub fn int(value: Int) -> Expression {
  value
  |> int.to_string
  |> doc.from_string
  |> Compiled(type_int)
  |> return
}

pub fn float(value: Float) -> Expression {
  value
  |> float.to_string
  |> doc.from_string
  |> Compiled(type_float)
  |> return
}

pub fn string(value: String) -> Expression {
  value
  |> escape_string_literal
  |> doc.from_string
  |> Compiled(type_string)
  |> return
}

pub fn bool(value: Bool) -> Expression {
  value
  |> bool.to_string
  |> doc.from_string
  |> Compiled(type_bool)
  |> return
}

pub fn nil() -> Expression {
  "Nil"
  |> doc.from_string
  |> Compiled(type_nil)
  |> return
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
  use left <- compile(left)
  use right <- compile(right)
  use _ <- try(unify(left.type_, operand_type))
  use _ <- try(unify(right.type_, operand_type))

  [left.document, doc.from_string(" " <> operator <> " "), right.document]
  |> doc.concat
  |> Compiled(result_type)
  |> return
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
  use type_ <- then(type_variable)
  binary_operator(left, "==", right, type_, type_bool)
}

pub fn not_equal(left: Expression, right: Expression) -> Expression {
  use type_ <- then(type_variable)
  binary_operator(left, "!=", right, type_, type_bool)
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
  use value <- compile(value)
  use type_ <- try(unify(value.type_, type_))
  [doc.from_string(operator), value.document]
  |> doc.concat
  |> Compiled(type_)
  |> return
}

pub fn negate_int(value: Expression) -> Expression {
  unary_operator("-", value, type_int)
}

pub fn negate_bool(value: Expression) -> Expression {
  unary_operator("!", value, type_bool)
}

pub fn list(values: List(Expression)) -> Expression {
  use values <- compile_expressions(values)

  use element_type <- fold_list(values, type_variable, fn(type_, value) {
    unify(value.type_, type_)
  })

  values
  |> list.map(fn(value) { value.document })
  |> doc.join(doc.break(", ", ","))
  |> doc.prepend(doc.break("[", "["))
  |> doc.nest(indent)
  |> doc.append(doc.break("", ", "))
  |> doc.append(doc.from_string("]"))
  |> doc.group
  |> Compiled(type_list(element_type))
  |> return
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
  use type_ <- then(type_variable)
  case message {
    None -> return(Compiled(doc.from_string("panic"), type_))
    Some(message) -> {
      use message <- compile(message)
      use _ <- try(unify(message.type_, type_string))
      return(Compiled(
        add_message(doc.from_string("panic"), message.document),
        type_,
      ))
    }
  }
}

pub fn todo_(message: Option(Expression)) -> Expression {
  use type_ <- then(type_variable)
  case message {
    None -> return(Compiled(doc.from_string("todo"), type_))
    Some(message) -> {
      use message <- compile(message)
      use _ <- try(unify(message.type_, type_string))
      return(Compiled(
        add_message(doc.from_string("todo"), message.document),
        type_,
      ))
    }
  }
}

pub fn echo_(value: Expression, message: Option(Expression)) -> Expression {
  use value <- compile(value)

  let echo_ = doc.prepend(doc.from_string("echo "), to: value.document)

  case message {
    None -> return(Compiled(echo_, value.type_))
    Some(message) -> {
      use message <- compile(message)
      use _ <- try(unify(message.type_, type_string))
      return(Compiled(add_message(echo_, message.document), value.type_))
    }
  }
}

pub fn variable(
  name: String,
  value: Expression,
  continue: fn(Expression) -> Statement,
) -> Statement {
  use <- statement
  use value <- compile(value)

  let declaration =
    [
      doc.from_string("let "),
      doc.from_string(name),
      doc.break(" = ", " ="),
      value.document,
    ]
    |> grouped
    |> doc.append(doc.line)

  let variable_expression = return(Compiled(doc.from_string(name), value.type_))

  let rest = continue(variable_expression)
  use rest <- compile_statement(rest)

  return(Compiled(
    doc.prepend(declaration, to: rest.document) |> doc.force_break,
    rest.type_,
  ))
}

fn grouped(documents: List(Document)) -> Document {
  documents |> doc.concat |> doc.nest(indent) |> doc.group
}

fn statement(f: fn() -> Expression) -> Statement {
  expression(f())
}

pub fn expression(expression: Expression) -> Statement {
  Statement(expression.compile)
}

pub fn discard(discarded: Statement, continue: fn() -> Statement) -> Statement {
  use <- statement
  use statement <- compile_statement(discarded)
  let rest = continue()
  use rest <- compile_statement(rest)
  [statement.document, doc.line, rest.document]
  |> doc.concat
  |> doc.force_break
  |> Compiled(rest.type_)
  |> return
}

pub fn block(inner: Statement) -> Expression {
  use inner <- compile_statement(inner)

  [
    doc.from_string("{"),
    doc.line,
    inner.document,
  ]
  |> doc.concat
  |> doc.nest(indent)
  |> doc.append(doc.line)
  |> doc.append(doc.from_string("}"))
  |> Compiled(inner.type_)
  |> return
}

pub fn assert_(condition: Expression, message: Option(Expression)) -> Statement {
  use <- statement
  use condition <- compile(condition)
  use _ <- try(unify(condition.type_, type_bool))

  let assert_ = doc.prepend(doc.from_string("assert "), to: condition.document)

  case message {
    None -> return(Compiled(doc.force_break(assert_), type_nil))
    Some(message) -> {
      use message <- compile(message)
      use _ <- try(unify(message.type_, type_string))
      return(Compiled(
        add_message(assert_, message.document) |> doc.force_break,
        type_nil,
      ))
    }
  }
}

pub fn tuple(values: List(Expression)) -> Expression {
  use values <- compile_expressions(values)

  let #(documents, types) =
    values
    |> list.map(fn(value) { #(value.document, value.type_) })
    |> list.unzip

  documents
  |> doc.join(doc.break(", ", ","))
  |> doc.prepend(doc.break("#(", "#("))
  |> doc.nest(indent)
  |> doc.append(doc.break("", ", "))
  |> doc.append(doc.from_string(")"))
  |> doc.group
  |> Compiled(Tuple(types))
  |> return
}

pub fn tuple_index(tuple: Expression, index: Int) -> Expression {
  use tuple <- compile(tuple)
  use unwrapped <- then(unwrap_type(tuple.type_))
  use type_ <- try(
    pure(case unwrapped {
      Custom(..) as type_ | TypeVariable(..) as type_ | Function(..) as type_ ->
        Error(InvalidTupleAccess(type_))
      Tuple(elements:) ->
        case list_at(elements, index, 0) {
          Error(length) -> Error(TupleIndexOutOfBounds(length:, index:))
          Ok(type_) -> Ok(type_)
        }
    }),
  )
  [tuple.document, doc.from_string("."), doc.from_string(int.to_string(index))]
  |> doc.concat
  |> Compiled(type_)
  |> return
}

fn list_at(list: List(a), index: Int, length: Int) -> Result(a, Int) {
  case list, index {
    [first, ..], 0 -> Ok(first)
    [], _ -> Error(length)
    _, _ if index < 0 -> Error(0)
    [_, ..list], _ -> list_at(list, index - 1, length + 1)
  }
}

pub opaque type FunctionBuilder {
  FunctionBuilder(parameters: List(Parameter), body: Statement)
}

type Parameter {
  Parameter(name: String, type_: Type)
}

pub fn anonymous(function: FunctionBuilder) -> Expression {
  use body <- compile_statement(function.body)

  let parameter_list =
    [
      doc.break("(", "("),
      function.parameters
        |> list.map(fn(parameter) { doc.from_string(parameter.name) })
        |> doc.join(doc.break(", ", ",")),
    ]
    |> doc.concat
    |> doc.nest(indent)
    |> doc.append(doc.break("", ","))
    |> doc.append(doc.from_string(")"))
    |> doc.group

  let body_doc =
    [
      doc.break("{ ", "{"),
      body.document,
    ]
    |> doc.concat
    |> doc.nest(indent)
    |> doc.append(doc.break(" ", ""))
    |> doc.append(doc.from_string("}"))
    |> doc.group

  let type_ =
    Function(
      parameters: list.map(function.parameters, fn(parameter) {
        parameter.type_
      }),
      return: body.type_,
    )

  [doc.from_string("fn"), parameter_list, doc.from_string(" "), body_doc]
  |> doc.concat
  |> Compiled(type_)
  |> return
}

pub fn parameter(
  name: String,
  type_: Type,
  continue: fn(Expression) -> FunctionBuilder,
) -> FunctionBuilder {
  let expression = Compiled(doc.from_string(name), type_)
  let function = continue(return(expression))

  let parameter = Parameter(name, type_)
  FunctionBuilder(..function, parameters: [parameter, ..function.parameters])
}

pub fn function_body(body: Statement) -> FunctionBuilder {
  FunctionBuilder([], body)
}

pub fn call(function: Expression, arguments: List(Expression)) -> Expression {
  use function <- compile(function)
  use arguments <- compile_expressions(arguments)

  use called_type <- then(unwrap_type(function.type_))

  case called_type {
    Custom(..) | Tuple(..) -> error(InvalidCall(called_type))
    Function(parameters:, return: return_type) ->
      case list.strict_zip(arguments, parameters) {
        Error(Nil) -> {
          let expected_length = list.length(parameters)
          let argument_length = list.length(arguments)
          error(IncorrectNumberOfArguments(
            expected: expected_length,
            got: argument_length,
          ))
        }
        Ok(zipped) -> {
          use <- try_each(zipped, fn(pair) {
            let #(arg, param) = pair
            unify(arg.type_, with: param)
          })

          call_doc(arguments, function, return_type)
        }
      }
    TypeVariable(_) -> {
      let parameter_types = list.map(arguments, fn(argument) { argument.type_ })
      use return_type <- then(type_variable)

      let function_type =
        Function(parameters: parameter_types, return: return_type)
      use _ <- try(unify(called_type, function_type))

      call_doc(arguments, function, return_type)
    }
  }
}

fn call_doc(
  arguments: List(Compiled),
  function: Compiled,
  return_type: Type,
) -> Expression {
  [
    doc.break("(", "("),
    arguments
      |> list.map(fn(arg) { arg.document })
      |> doc.join(doc.break(", ", ",")),
  ]
  |> doc.concat
  |> doc.nest(indent)
  |> doc.prepend(function.document)
  |> doc.append(doc.break("", ","))
  |> doc.append(doc.from_string(")"))
  |> doc.group
  |> Compiled(return_type)
  |> return
}
// TODO:
// BitString
// Case
// FieldAccess
// FnCapture
// Let assert
// Let with patterns
// Pipes
// RecordUpdate
// Use
