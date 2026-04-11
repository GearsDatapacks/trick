import glam/doc.{type Document}
import gleam/bool
import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}
import gleam/string
import lazy_const
import splitter

pub type Constant

pub type Variable

pub opaque type Expression(a) {
  Expression(compile: fn(State) -> Result(#(State, Compiled), Error))
}

pub opaque type Statement {
  Statement(compile: fn(State) -> Result(#(State, Compiled), Error))
}

pub opaque type Definition {
  Definition(compile: fn(State) -> Result(#(State, Document), Error))
}

pub type Error {
  TypeMismatch(expected: ConcreteType, got: ConcreteType)
  TupleIndexOutOfBounds(length: Int, index: Int)
  InvalidTupleAccess(type_: ConcreteType)
  InvalidCall(type_: ConcreteType)
  InvalidListPrepend(type_: ConcreteType)
  IncorrectNumberOfArguments(expected: Int, got: Int)
  UnlabelledParameterAfterLabelledParameter
  UnexpectedLabelledArgument(label: String)
  UnknownLabel(label: String, available_labels: List(String))
  DuplicateLabel(label: String)
}

type Compiled {
  Compiled(document: Document, type_: ConcreteType)
}

pub opaque type Type {
  Type(compile: fn(State) -> #(State, ConcreteType))
}

pub type ConcreteType {
  Custom(module: String, name: String, generics: List(ConcreteType))
  Generic(id: Int)
  Unbound(id: Int)
  Tuple(elements: List(ConcreteType))
  Function(
    parameters: List(ConcreteType),
    return: ConcreteType,
    field_map: Option(FieldMap),
  )
}

pub type FieldMap {
  FieldMap(arity: Int, fields: Dict(String, Int))
}

type State {
  State(
    resolved_variables: Dict(Int, ConcreteType),
    type_variable_names: Dict(Int, String),
    generic_variable_names: Dict(String, ConcreteType),
    type_variable_id: Int,
    module: String,
  )
}

const type_int: ConcreteType = Custom("gleam", "Int", [])

const type_float: ConcreteType = Custom("gleam", "Float", [])

const type_string: ConcreteType = Custom("gleam", "String", [])

const type_bool: ConcreteType = Custom("gleam", "Bool", [])

const type_nil: ConcreteType = Custom("gleam", "Nil", [])

pub type Publicity {
  Public
  Internal
  Private
}

fn publicity_to_doc(publicity: Publicity) -> Document {
  case publicity {
    Public -> doc.from_string("pub ")
    Internal ->
      doc.concat([
        doc.from_string("@internal"),
        doc.line,
        doc.from_string("pub "),
      ])
    Private -> doc.empty
  }
}

fn type_list(element: ConcreteType) -> ConcreteType {
  Custom("gleam", "List", [element])
}

fn next_unbound(state: State) -> #(State, ConcreteType) {
  let id = state.type_variable_id
  let type_ = Unbound(id:)
  let state = State(..state, type_variable_id: id + 1)
  #(state, type_)
}

fn next_generic(state: State) -> #(State, ConcreteType) {
  let id = state.type_variable_id
  let type_ = Generic(id:)
  let state = State(..state, type_variable_id: id + 1)
  #(state, type_)
}

fn named_unbound(state: State, name: String) -> #(State, ConcreteType) {
  let id = state.type_variable_id
  let type_ = Unbound(id:)
  let state =
    State(
      ..state,
      type_variable_names: dict.insert(state.type_variable_names, id, name),
      type_variable_id: id + 1,
    )
  #(state, type_)
}

fn named_generic(state: State, name: String) -> #(State, ConcreteType) {
  let id = state.type_variable_id
  let type_ = Generic(id:)
  let state =
    State(
      ..state,
      type_variable_names: dict.insert(state.type_variable_names, id, name),
      type_variable_id: id + 1,
    )
  #(state, type_)
}

fn doc_to_expression(value: Compiled) -> Expression(a) {
  Expression(fn(state) { Ok(#(state, value)) })
}

fn compile_values(
  state: State,
  values: List(Expression(a)),
) -> Result(#(State, List(Compiled)), Error) {
  do_compile_values(state, values, [])
}

fn do_compile_values(
  state: State,
  values: List(Expression(a)),
  out: List(Compiled),
) -> Result(#(State, List(Compiled)), Error) {
  case values {
    [] -> Ok(#(state, list.reverse(out)))
    [value, ..values] ->
      case value.compile(state) {
        Ok(#(state, compiled)) ->
          do_compile_values(state, values, [compiled, ..out])
        Error(error) -> Error(error)
      }
  }
}

fn try_fold_with_state(
  state: State,
  list: List(elem),
  acc: acc,
  value: fn(State, acc, elem) -> Result(#(State, acc), Error),
) -> Result(#(State, acc), Error) {
  case list {
    [] -> Ok(#(state, acc))
    [first, ..rest] ->
      case value(state, acc, first) {
        Ok(#(state, acc)) -> try_fold_with_state(state, rest, acc, value)
        Error(error) -> Error(error)
      }
  }
}

fn unify(
  state: State,
  a: ConcreteType,
  with b: ConcreteType,
) -> Result(#(State, ConcreteType), Error) {
  use <- bool.guard(a == b, Ok(#(state, a)))

  let mismatch = TypeMismatch(expected: b, got: a)

  case a, b {
    Unbound(id:), other -> unify_unbound_type(state, id, other, VariableFirst)
    other, Unbound(id:) -> unify_unbound_type(state, id, other, TypeFirst)
    Custom(module: m1, name: n1, generics: g1),
      Custom(module: m2, name: n2, generics: g2)
      if m1 == m2 && n1 == n2
    -> {
      case list.strict_zip(g1, g2) {
        Error(_) -> Error(mismatch)
        Ok(generics) -> {
          let generics =
            list.try_fold(generics, #(state, []), fn(acc, pair) {
              let #(state, generics) = acc
              let #(a, b) = pair
              case unify(state, a, b) {
                Ok(#(state, type_)) -> Ok(#(state, [type_, ..generics]))
                Error(error) -> Error(error)
              }
            })
          case generics {
            Error(error) -> Error(error)
            Ok(#(state, generics)) ->
              Ok(#(state, Custom(m1, n1, list.reverse(generics))))
          }
        }
      }
    }
    Function(parameters: p1, return: r1, field_map: _),
      Function(parameters: p2, return: r2, field_map: _)
    ->
      case list.strict_zip(p1, p2) {
        Error(_) -> Error(mismatch)
        Ok(parameters) -> {
          let parameters =
            list.try_fold(parameters, #(state, []), fn(acc, pair) {
              let #(state, parameters) = acc
              let #(a, b) = pair
              case unify(state, a, b) {
                Ok(#(state, type_)) -> Ok(#(state, [type_, ..parameters]))
                Error(error) -> Error(error)
              }
            })
          case parameters {
            Error(error) -> Error(error)
            Ok(#(state, parameters)) ->
              case unify(state, r1, r2) {
                Error(error) -> Error(error)
                Ok(#(state, return)) ->
                  Ok(#(state, Function(list.reverse(parameters), return, None)))
              }
          }
        }
      }
    Tuple(elements: e1), Tuple(elements: e2) ->
      case list.strict_zip(e1, e2) {
        Error(_) -> Error(mismatch)
        Ok(elements) -> {
          let elements =
            list.try_fold(elements, #(state, []), fn(acc, pair) {
              let #(state, elements) = acc
              let #(a, b) = pair
              case unify(state, a, b) {
                Ok(#(state, type_)) -> Ok(#(state, [type_, ..elements]))
                Error(error) -> Error(error)
              }
            })
          case elements {
            Error(error) -> Error(error)
            Ok(#(state, elements)) ->
              Ok(#(state, Tuple(list.reverse(elements))))
          }
        }
      }
    _, _ -> Error(mismatch)
  }
}

type Order {
  VariableFirst
  TypeFirst
}

fn unify_unbound_type(
  state: State,
  id: Int,
  type_: ConcreteType,
  order: Order,
) -> Result(#(State, ConcreteType), Error) {
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

fn unwrap_type(state: State, type_: ConcreteType) -> ConcreteType {
  let unwrapped = case type_ {
    Custom(..) | Tuple(..) | Function(..) | Generic(..) -> type_
    Unbound(id:) ->
      case dict.get(state.resolved_variables, id) {
        Error(_) -> type_
        Ok(type_) -> type_
      }
  }
  unwrapped
}

fn instantiate(state: State, type_: ConcreteType) -> #(State, ConcreteType) {
  let #(state, type_, _) = do_instantiate(state, type_, dict.new())
  #(state, type_)
}

fn do_instantiate(
  state: State,
  type_: ConcreteType,
  instantiated: Dict(Int, ConcreteType),
) -> #(State, ConcreteType, Dict(Int, ConcreteType)) {
  case type_ {
    Custom(module:, name:, generics:) -> {
      let #(#(state, instantiated), generics) =
        list.map_fold(generics, #(state, instantiated), fn(acc, generic) {
          let #(state, instantiated) = acc
          let #(state, generic, instantiated) =
            do_instantiate(state, generic, instantiated)
          #(#(state, instantiated), generic)
        })
      #(state, Custom(module:, name:, generics:), instantiated)
    }
    Generic(id) ->
      case dict.get(instantiated, id) {
        Ok(type_) -> #(state, type_, instantiated)
        Error(_) ->
          case dict.get(state.type_variable_names, id) {
            Ok(name) -> {
              let #(state, type_) = named_unbound(state, name)
              #(state, type_, dict.insert(instantiated, id, type_))
            }
            Error(_) -> {
              let #(state, type_) = next_unbound(state)
              #(state, type_, dict.insert(instantiated, id, type_))
            }
          }
      }
    Unbound(..) -> #(state, type_, instantiated)
    Tuple(elements:) -> {
      let #(#(state, instantiated), elements) =
        list.map_fold(elements, #(state, instantiated), fn(acc, element) {
          let #(state, instantiated) = acc
          let #(state, element, instantiated) =
            do_instantiate(state, element, instantiated)
          #(#(state, instantiated), element)
        })
      #(state, Tuple(elements:), instantiated)
    }
    Function(parameters:, return:, field_map:) -> {
      let #(#(state, instantiated), parameters) =
        list.map_fold(parameters, #(state, instantiated), fn(acc, parameter) {
          let #(state, instantiated) = acc
          let #(state, parameter, instantiated) =
            do_instantiate(state, parameter, instantiated)
          #(#(state, instantiated), parameter)
        })
      let #(state, return, instantiated) =
        do_instantiate(state, return, instantiated)
      #(state, Function(parameters:, return:, field_map:), instantiated)
    }
  }
}

fn generalise(state: State, type_: ConcreteType) -> #(State, ConcreteType) {
  let #(state, type_, _) = do_generalise(state, type_, dict.new())
  #(state, type_)
}

fn do_generalise(
  state: State,
  type_: ConcreteType,
  generalised: Dict(Int, ConcreteType),
) -> #(State, ConcreteType, Dict(Int, ConcreteType)) {
  case unwrap_type(state, type_) {
    Custom(module:, name:, generics:) -> {
      let #(#(state, generalised), generics) =
        list.map_fold(generics, #(state, generalised), fn(acc, generic) {
          let #(state, generalised) = acc
          let #(state, generic, generalised) =
            do_generalise(state, generic, generalised)
          #(#(state, generalised), generic)
        })
      #(state, Custom(module:, name:, generics:), generalised)
    }
    Generic(..) -> #(state, type_, generalised)
    Unbound(id:) -> {
      case dict.get(generalised, id) {
        Ok(type_) -> #(state, type_, generalised)
        Error(_) -> {
          let #(state, generic) = next_generic(state)
          #(state, generic, dict.insert(generalised, id, generic))
        }
      }
    }
    Tuple(elements:) -> {
      let #(#(state, generalised), elements) =
        list.map_fold(elements, #(state, generalised), fn(acc, element) {
          let #(state, generalised) = acc
          let #(state, element, generalised) =
            do_generalise(state, element, generalised)
          #(#(state, generalised), element)
        })
      #(state, Tuple(elements:), generalised)
    }
    Function(parameters:, return:, field_map:) -> {
      let #(#(state, generalised), parameters) =
        list.map_fold(parameters, #(state, generalised), fn(acc, parameter) {
          let #(state, generalised) = acc
          let #(state, parameter, generalised) =
            do_generalise(state, parameter, generalised)
          #(#(state, generalised), parameter)
        })
      let #(state, return, generalised) =
        do_generalise(state, return, generalised)
      #(state, Function(parameters:, return:, field_map:), generalised)
    }
  }
}

fn instantiated(doc: Document, type_: ConcreteType) -> Expression(_) {
  use state <- Expression
  let #(state, type_) = instantiate(state, type_)
  Ok(#(state, Compiled(doc, type_)))
}

pub fn int_type() -> Type {
  concrete(type_int)
}

pub fn float_type() -> Type {
  concrete(type_float)
}

pub fn string_type() -> Type {
  concrete(type_string)
}

pub fn bool_type() -> Type {
  concrete(type_bool)
}

pub fn nil_type() -> Type {
  concrete(type_nil)
}

pub fn bit_array_type() -> Type {
  concrete(Custom("gleam", "BitArray", []))
}

pub fn utf_codepoint_type() -> Type {
  concrete(Custom("gleam", "UtfCodepoint", []))
}

pub fn list_type(element_type: Type) -> Type {
  use state <- Type
  let #(state, element_type) = element_type.compile(state)
  #(state, type_list(element_type))
}

pub fn tuple_type(elements: List(Type)) -> Type {
  use state <- Type
  let #(state, elements) =
    list.map_fold(elements, state, fn(state, element) { element.compile(state) })
  #(state, Tuple(elements:))
}

pub fn function_type(parameters: List(Type), return: Type) -> Type {
  use state <- Type
  let #(state, parameters) =
    list.map_fold(parameters, state, fn(state, parameter) {
      parameter.compile(state)
    })
  let #(state, return) = return.compile(state)
  #(state, Function(parameters:, return:, field_map: None))
}

pub fn generic(name: String) -> Type {
  use state <- Type
  case dict.get(state.generic_variable_names, name) {
    Ok(type_) -> #(state, type_)
    Error(_) -> {
      let #(state, type_) = named_generic(state, name)
      #(
        State(
          ..state,
          generic_variable_names: dict.insert(
            state.generic_variable_names,
            name,
            type_,
          ),
        ),
        type_,
      )
    }
  }
}

const width: Int = 80

const indent: Int = 2

pub fn expression_to_string(
  expression: Expression(a),
) -> Result(String, Error) {
  case expression.compile(new_state()) {
    Ok(#(_state, expression)) -> Ok(doc.to_string(expression.document, width))
    Error(error) -> Error(error)
  }
}

pub fn to_string(definition: Definition) -> Result(String, Error) {
  case definition.compile(new_state()) {
    Ok(#(_state, definition)) -> Ok(doc.to_string(definition, width))
    Error(error) -> Error(error)
  }
}

fn new_state() -> State {
  State(
    resolved_variables: dict.new(),
    type_variable_names: dict.new(),
    generic_variable_names: dict.new(),
    type_variable_id: 0,
    module: "module",
  )
}

pub fn int(value: Int) -> Expression(a) {
  value
  |> int.to_string
  |> doc.from_string
  |> Compiled(type_int)
  |> doc_to_expression
}

pub fn int_base2(value: Int) -> Expression(a) {
  value
  |> int.to_base2
  |> doc.from_string
  |> doc.prepend(doc.from_string("0b"))
  |> Compiled(type_int)
  |> doc_to_expression
}

pub fn int_base8(value: Int) -> Expression(a) {
  value
  |> int.to_base8
  |> doc.from_string
  |> doc.prepend(doc.from_string("0o"))
  |> Compiled(type_int)
  |> doc_to_expression
}

pub fn int_base16(value: Int) -> Expression(a) {
  value
  |> int.to_base16
  |> doc.from_string
  |> doc.prepend(doc.from_string("0x"))
  |> Compiled(type_int)
  |> doc_to_expression
}

pub fn float(value: Float) -> Expression(a) {
  value
  |> float.to_string
  |> doc.from_string
  |> Compiled(type_float)
  |> doc_to_expression
}

pub fn string(value: String) -> Expression(a) {
  value
  |> escape_string_literal
  |> doc.from_string
  |> Compiled(type_string)
  |> doc_to_expression
}

pub fn bool(value: Bool) -> Expression(a) {
  value
  |> bool.to_string
  |> doc.from_string
  |> Compiled(type_bool)
  |> doc_to_expression
}

pub fn nil() -> Expression(a) {
  "Nil"
  |> doc.from_string
  |> Compiled(type_nil)
  |> doc_to_expression
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
  left: Expression(_),
  operator: String,
  right: Expression(_),
  operand_type: ConcreteType,
  result_type: ConcreteType,
) -> Expression(_) {
  use state <- Expression
  use #(state, left) <- result.try(left.compile(state))
  use #(state, right) <- result.try(right.compile(state))
  use #(state, _) <- result.try(unify(state, left.type_, operand_type))
  use #(state, _) <- result.try(unify(state, right.type_, operand_type))

  Ok(#(
    state,
    [left.document, doc.from_string(" " <> operator <> " "), right.document]
      |> doc.concat
      |> Compiled(result_type),
  ))
}

pub fn add(left: Expression(_), right: Expression(_)) -> Expression(Variable) {
  binary_operator(left, "+", right, type_int, type_int)
}

pub fn add_float(
  left: Expression(_),
  right: Expression(_),
) -> Expression(Variable) {
  binary_operator(left, "+.", right, type_float, type_float)
}

pub fn subtract(
  left: Expression(_),
  right: Expression(_),
) -> Expression(Variable) {
  binary_operator(left, "-", right, type_int, type_int)
}

pub fn subtract_float(
  left: Expression(_),
  right: Expression(_),
) -> Expression(Variable) {
  binary_operator(left, "-.", right, type_float, type_float)
}

pub fn multiply(
  left: Expression(_),
  right: Expression(_),
) -> Expression(Variable) {
  binary_operator(left, "*", right, type_int, type_int)
}

pub fn multiply_float(
  left: Expression(_),
  right: Expression(_),
) -> Expression(Variable) {
  binary_operator(left, "*.", right, type_float, type_float)
}

pub fn divide(
  left: Expression(_),
  right: Expression(_),
) -> Expression(Variable) {
  binary_operator(left, "/", right, type_int, type_int)
}

pub fn divide_float(
  left: Expression(_),
  right: Expression(_),
) -> Expression(Variable) {
  binary_operator(left, "/.", right, type_float, type_float)
}

pub fn remainder(
  left: Expression(_),
  right: Expression(_),
) -> Expression(Variable) {
  binary_operator(left, "%", right, type_int, type_int)
}

pub fn concatenate(left: Expression(a), right: Expression(a)) -> Expression(a) {
  binary_operator(left, "<>", right, type_string, type_string)
}

pub fn and(left: Expression(_), right: Expression(_)) -> Expression(Variable) {
  binary_operator(left, "&&", right, type_bool, type_bool)
}

pub fn or(left: Expression(_), right: Expression(_)) -> Expression(Variable) {
  binary_operator(left, "||", right, type_bool, type_bool)
}

pub fn equal(
  left: Expression(_),
  right: Expression(_),
) -> Expression(Variable) {
  use state <- Expression
  let #(state, type_) = next_unbound(state)
  binary_operator(left, "==", right, type_, type_bool).compile(state)
}

pub fn not_equal(
  left: Expression(_),
  right: Expression(_),
) -> Expression(Variable) {
  use state <- Expression
  let #(state, type_) = next_unbound(state)
  binary_operator(left, "!=", right, type_, type_bool).compile(state)
}

pub fn less_than(
  left: Expression(_),
  right: Expression(_),
) -> Expression(Variable) {
  binary_operator(left, "<", right, type_int, type_bool)
}

pub fn less_than_float(
  left: Expression(_),
  right: Expression(_),
) -> Expression(Variable) {
  binary_operator(left, "<.", right, type_float, type_bool)
}

pub fn less_than_or_equal(
  left: Expression(_),
  right: Expression(_),
) -> Expression(Variable) {
  binary_operator(left, "<=", right, type_int, type_bool)
}

pub fn less_than_or_equal_float(
  left: Expression(_),
  right: Expression(_),
) -> Expression(Variable) {
  binary_operator(left, "<=.", right, type_float, type_bool)
}

pub fn greater_than(
  left: Expression(_),
  right: Expression(_),
) -> Expression(Variable) {
  binary_operator(left, ">", right, type_int, type_bool)
}

pub fn greater_than_float(
  left: Expression(_),
  right: Expression(_),
) -> Expression(Variable) {
  binary_operator(left, ">.", right, type_float, type_bool)
}

pub fn greater_than_or_equal(
  left: Expression(_),
  right: Expression(_),
) -> Expression(Variable) {
  binary_operator(left, ">=", right, type_int, type_bool)
}

pub fn greater_than_or_equal_float(
  left: Expression(_),
  right: Expression(_),
) -> Expression(Variable) {
  binary_operator(left, ">=.", right, type_float, type_bool)
}

fn unary_operator(
  operator: String,
  value: Expression(_),
  type_: ConcreteType,
) -> Expression(_) {
  use state <- Expression
  use #(state, value) <- result.try(value.compile(state))
  use #(state, type_) <- result.try(unify(state, value.type_, type_))
  Ok(#(
    state,
    [doc.from_string(operator), value.document]
      |> doc.concat
      |> Compiled(type_),
  ))
}

pub fn negate_int(value: Expression(a)) -> Expression(Variable) {
  unary_operator("-", value, type_int)
}

pub fn negate_bool(value: Expression(a)) -> Expression(Variable) {
  unary_operator("!", value, type_bool)
}

pub fn list(values: List(Expression(a))) -> Expression(a) {
  use state <- Expression
  use #(state, values) <- result.try(compile_values(state, values))

  let #(state, element_type) = next_unbound(state)
  use #(state, element_type) <- result.try(
    try_fold_with_state(state, values, element_type, fn(state, type_, value) {
      unify(state, value.type_, type_)
    }),
  )

  Ok(#(
    state,
    values
      |> list.map(fn(value) { value.document })
      |> doc.join(doc.break(", ", ","))
      |> doc.prepend(doc.break("[", "["))
      |> doc.nest(indent)
      |> doc.append(doc.break("", ", "))
      |> doc.append(doc.from_string("]"))
      |> doc.group
      |> Compiled(type_list(element_type)),
  ))
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

pub fn panic_(message: Option(Expression(a))) -> Expression(Variable) {
  use state <- Expression
  let #(state, type_) = next_unbound(state)
  case message {
    None -> Ok(#(state, Compiled(doc.from_string("panic"), type_)))
    Some(message) -> {
      use #(state, message) <- result.try(message.compile(state))
      use #(state, _) <- result.try(unify(state, message.type_, type_string))
      Ok(#(
        state,
        Compiled(add_message(doc.from_string("panic"), message.document), type_),
      ))
    }
  }
}

pub fn todo_(message: Option(Expression(a))) -> Expression(Variable) {
  use state <- Expression
  let #(state, type_) = next_unbound(state)
  case message {
    None -> Ok(#(state, Compiled(doc.from_string("todo"), type_)))
    Some(message) -> {
      use #(state, message) <- result.try(message.compile(state))
      use #(state, _) <- result.try(unify(state, message.type_, type_string))
      Ok(#(
        state,
        Compiled(add_message(doc.from_string("todo"), message.document), type_),
      ))
    }
  }
}

pub fn echo_(
  value: Expression(a),
  message: Option(Expression(a)),
) -> Expression(Variable) {
  use state <- Expression
  use #(state, value) <- result.try(value.compile(state))

  let echo_ = doc.prepend(doc.from_string("echo "), to: value.document)

  case message {
    None -> Ok(#(state, Compiled(echo_, value.type_)))
    Some(message) -> {
      use #(state, message) <- result.try(message.compile(state))
      use #(state, _) <- result.try(unify(state, message.type_, type_string))
      Ok(#(state, Compiled(add_message(echo_, message.document), value.type_)))
    }
  }
}

pub fn variable(
  name: String,
  value: Expression(a),
  continue: fn(Expression(Variable)) -> Statement,
) -> Statement {
  use state <- Statement
  use #(state, value) <- result.try(value.compile(state))

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
    doc_to_expression(Compiled(doc.from_string(name), value.type_))

  let rest = continue(variable_expression)
  use #(state, rest) <- result.try(rest.compile(state))

  Ok(#(
    state,
    Compiled(
      doc.prepend(declaration, to: rest.document) |> doc.force_break,
      rest.type_,
    ),
  ))
}

fn grouped(documents: List(Document)) -> Document {
  documents |> doc.concat |> doc.nest(indent) |> doc.group
}

pub fn expression(expression: Expression(a)) -> Statement {
  Statement(expression.compile)
}

pub fn discard(discarded: Statement, continue: fn() -> Statement) -> Statement {
  use state <- Statement
  use #(state, statement) <- result.try(discarded.compile(state))
  let rest = continue()
  use #(state, rest) <- result.try(rest.compile(state))
  Ok(#(
    state,
    [statement.document, doc.line, rest.document]
      |> doc.concat
      |> doc.force_break
      |> Compiled(rest.type_),
  ))
}

pub fn comment(comment: String, continue: fn() -> Statement) -> Statement {
  use state <- Statement
  let rest = continue()
  use #(state, rest) <- result.try(rest.compile(state))

  let comment =
    comment
    |> string.split("\n")
    |> list.map(fn(line) { doc.from_string("// " <> line) })
    |> doc.join(doc.line)

  Ok(#(
    state,
    [comment, doc.line, rest.document]
      |> doc.concat
      |> doc.force_break
      |> Compiled(rest.type_),
  ))
}

pub fn block(inner: Statement) -> Expression(Variable) {
  use state <- Expression
  use #(state, inner) <- result.try(inner.compile(state))

  Ok(#(
    state,
    [
      doc.break("{ ", "{"),
      inner.document,
    ]
      |> doc.concat
      |> doc.nest(indent)
      |> doc.append(doc.break(" ", ""))
      |> doc.append(doc.from_string("}"))
      |> doc.group
      |> Compiled(inner.type_),
  ))
}

pub fn assert_(
  condition: Expression(a),
  message: Option(Expression(a)),
) -> Statement {
  use state <- Statement
  use #(state, condition) <- result.try(condition.compile(state))
  use #(state, _) <- result.try(unify(state, condition.type_, type_bool))

  let assert_ = doc.prepend(doc.from_string("assert "), to: condition.document)

  case message {
    None -> Ok(#(state, Compiled(doc.force_break(assert_), type_nil)))
    Some(message) -> {
      use #(state, message) <- result.try(message.compile(state))
      use #(state, _) <- result.try(unify(state, message.type_, type_string))
      Ok(#(
        state,
        Compiled(
          add_message(assert_, message.document) |> doc.force_break,
          type_nil,
        ),
      ))
    }
  }
}

pub fn tuple(values: List(Expression(a))) -> Expression(a) {
  use state <- Expression
  use #(state, values) <- result.try(compile_values(state, values))

  let #(documents, types) =
    values
    |> list.map(fn(value) { #(value.document, value.type_) })
    |> list.unzip

  Ok(#(
    state,
    documents
      |> doc.join(doc.break(", ", ","))
      |> doc.prepend(doc.break("#(", "#("))
      |> doc.nest(indent)
      |> doc.append(doc.break("", ", "))
      |> doc.append(doc.from_string(")"))
      |> doc.group
      |> Compiled(Tuple(types)),
  ))
}

pub fn tuple_index(tuple: Expression(a), index: Int) -> Expression(Variable) {
  use state <- Expression
  use #(state, tuple) <- result.try(tuple.compile(state))
  let unwrapped = unwrap_type(state, tuple.type_)
  use type_ <- result.try(case unwrapped {
    Custom(..) | Unbound(..) | Generic(..) | Function(..) ->
      Error(InvalidTupleAccess(unwrapped))
    Tuple(elements:) ->
      case list_at(elements, index, 0) {
        Error(length) -> Error(TupleIndexOutOfBounds(length:, index:))
        Ok(type_) -> Ok(type_)
      }
  })

  Ok(#(
    state,
    [
      tuple.document,
      doc.from_string("."),
      doc.from_string(int.to_string(index)),
    ]
      |> doc.concat
      |> Compiled(type_),
  ))
}

fn list_at(list: List(a), index: Int, length: Int) -> Result(a, Int) {
  case list, index {
    [first, ..], 0 -> Ok(first)
    [], _ -> Error(length)
    _, _ if index < 0 -> Error(0)
    [_, ..list], _ -> list_at(list, index - 1, length + 1)
  }
}

pub fn prepend(
  list: Expression(a),
  elements: List(Expression(a)),
) -> Expression(a) {
  use state <- Expression
  use #(state, list) <- result.try(list.compile(state))
  use element_type <- result.try(case unwrap_type(state, list.type_) {
    Custom(module: "gleam", name: "List", generics: [type_]) -> Ok(type_)
    Custom(..) as type_
    | Generic(..) as type_
    | Unbound(..) as type_
    | Tuple(..) as type_
    | Function(..) as type_ -> Error(InvalidListPrepend(type_))
  })
  use #(state, elements) <- result.try(compile_values(state, elements))
  use state <- result.try(
    list.try_fold(elements, state, fn(state, element) {
      case unify(state, element.type_, with: element_type) {
        Ok(#(state, _)) -> Ok(state)
        Error(error) -> Error(error)
      }
    }),
  )

  Ok(#(
    state,
    elements
      |> list.map(fn(element) { element.document })
      |> doc.join(doc.break(", ", ","))
      |> doc.append(
        doc.concat([doc.break(", ", ","), doc.from_string(".."), list.document]),
      )
      |> doc.prepend(doc.break("[", "["))
      |> doc.nest(indent)
      |> doc.append(doc.soft_break)
      |> doc.append(doc.from_string("]"))
      |> doc.group
      |> Compiled(list.type_),
  ))
}

pub type Unlabelled

pub type Labelled

pub opaque type FunctionBuilder(labelling) {
  FunctionBuilder(
    compile: fn(State, String, ConcreteType, List(ConcreteType)) ->
      Result(#(State, FunctionInformation), Error),
  )
}

type FunctionInformation {
  FunctionInformation(parameters: List(Parameter), body: Statement)
}

type Parameter {
  Parameter(name: String, label: Option(String), type_: ConcreteType)
}

pub fn anonymous(
  function: FunctionBuilder(Unlabelled),
) -> Expression(Variable) {
  use state <- Expression
  let #(state, return_type) = next_unbound(state)

  use #(state, function) <- result.try(
    function.compile(state, "", return_type, []),
  )
  use #(state, body) <- result.try(function.body.compile(state))

  let parameter_list =
    [
      doc.break("(", "("),
      function.parameters
        |> list.map(parameter_to_doc(state, _))
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

  use #(state, return_type) <- result.try(unify(state, body.type_, return_type))

  let type_ =
    Function(
      parameters: list.map(function.parameters, fn(parameter) {
        parameter.type_
      }),
      return: return_type,
      field_map: None,
    )

  Ok(#(
    state,
    [doc.from_string("fn"), parameter_list, doc.from_string(" "), body_doc]
      |> doc.concat
      |> Compiled(type_),
  ))
}

pub fn recursive(
  continue: fn(Expression(Constant)) -> Statement,
) -> FunctionBuilder(Labelled) {
  use state, name, return_type, parameter_types <- FunctionBuilder

  let type_ =
    Function(parameters: parameter_types, return: return_type, field_map: None)

  let expression = doc_to_expression(Compiled(doc.from_string(name), type_))

  let body = continue(expression)

  let _ = Ok(#(state, FunctionInformation([], body)))
}

pub fn parameter(
  name: String,
  type_: Type,
  continue: fn(Expression(_)) -> FunctionBuilder(a),
) -> FunctionBuilder(a) {
  use state, function_name, function_type, parameters <- FunctionBuilder

  let #(state, type_) = type_.compile(state)

  let expression = Compiled(doc.from_string(name), type_)
  let function = continue(doc_to_expression(expression))

  let parameter = Parameter(name, None, type_)
  use #(state, function) <- result.try(function.compile(
    state,
    function_name,
    function_type,
    list.append(parameters, [type_]),
  ))

  Ok(#(
    state,
    FunctionInformation(..function, parameters: [
      parameter,
      ..function.parameters
    ]),
  ))
}

pub fn labelled_parameter(
  label: String,
  name: String,
  type_: Type,
  continue: fn(Expression(_)) -> FunctionBuilder(a),
) -> FunctionBuilder(Labelled) {
  use state, function_name, function_type, parameters <- FunctionBuilder

  let #(state, type_) = type_.compile(state)

  let expression = Compiled(doc.from_string(name), type_)
  let function = continue(doc_to_expression(expression))

  let parameter = Parameter(name, Some(label), type_)
  use #(state, function) <- result.try(function.compile(
    state,
    function_name,
    function_type,
    list.append(parameters, [type_]),
  ))

  Ok(#(
    state,
    FunctionInformation(..function, parameters: [
      parameter,
      ..function.parameters
    ]),
  ))
}

fn parameter_to_doc(state: State, parameter: Parameter) -> Document {
  case parameter.label {
    None ->
      doc.concat([
        doc.from_string(parameter.name),
        doc.from_string(": "),
        doc.from_string(print_type(state, parameter.type_)),
      ])
    Some(label) ->
      doc.concat([
        doc.from_string(label),
        doc.from_string(" "),
        doc.from_string(parameter.name),
        doc.from_string(": "),
        doc.from_string(print_type(state, parameter.type_)),
      ])
  }
}

pub fn function_body(body: Statement) -> FunctionBuilder(Unlabelled) {
  FunctionBuilder(fn(state, _name, _type, _parameters) {
    Ok(#(state, FunctionInformation([], body)))
  })
}

pub fn call(
  function: Expression(_),
  arguments: List(Expression(_)),
) -> Expression(Variable) {
  use state <- Expression
  use #(state, function) <- result.try(function.compile(state))

  use #(state, arguments) <- result.try(compile_values(state, arguments))

  let called_type = unwrap_type(state, function.type_)

  case called_type {
    Custom(..) | Tuple(..) | Generic(..) -> Error(InvalidCall(called_type))
    Function(parameters:, return: return_type, field_map: _) ->
      case list.strict_zip(arguments, parameters) {
        Error(Nil) -> {
          let expected_length = list.length(parameters)
          let argument_length = list.length(arguments)
          Error(IncorrectNumberOfArguments(
            expected: expected_length,
            got: argument_length,
          ))
        }
        Ok(zipped) -> {
          use state <- result.try(
            list.try_fold(zipped, state, fn(state, pair) {
              let #(arg, param) = pair
              case unify(state, arg.type_, with: param) {
                Ok(#(state, _)) -> Ok(state)
                Error(error) -> Error(error)
              }
            }),
          )

          let doc = call_doc(arguments, function)
          Ok(#(state, Compiled(doc, return_type)))
        }
      }
    Unbound(_) -> {
      let parameter_types = list.map(arguments, fn(argument) { argument.type_ })
      let #(state, return_type) = next_unbound(state)

      let function_type =
        Function(
          parameters: parameter_types,
          return: return_type,
          field_map: None,
        )
      use #(state, _) <- result.try(unify(state, called_type, function_type))

      let doc = call_doc(arguments, function)
      Ok(#(state, Compiled(doc, return_type)))
    }
  }
}

fn call_doc(arguments: List(Compiled), function: Compiled) -> Document {
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
}

pub fn function_capture(
  function: Expression(a),
  before_hole: List(Expression(a)),
  after_hole: List(Expression(a)),
) -> Expression(Variable) {
  use state <- Expression
  use #(state, function) <- result.try(function.compile(state))
  use #(state, before) <- result.try(compile_values(state, before_hole))
  use #(state, after) <- result.try(compile_values(state, after_hole))

  let called_type = unwrap_type(state, function.type_)
  let #(state, parameter_type) = next_unbound(state)

  let argument_types =
    list.flatten([
      list.map(before, fn(argument) { argument.type_ }),
      [parameter_type],
      list.map(after, fn(argument) { argument.type_ }),
    ])

  case called_type {
    Custom(..) | Tuple(..) | Generic(..) -> Error(InvalidCall(called_type))
    Function(parameters:, return: return_type, field_map: _) ->
      case list.strict_zip(argument_types, parameters) {
        Error(Nil) -> {
          let expected_length = list.length(parameters)
          let argument_length = list.length(argument_types)
          Error(IncorrectNumberOfArguments(
            expected: expected_length,
            got: argument_length,
          ))
        }
        Ok(zipped) -> {
          use state <- result.try(
            list.try_fold(zipped, state, fn(state, pair) {
              let #(argument, parameter) = pair
              case unify(state, argument, parameter) {
                Ok(#(state, _)) -> Ok(state)
                Error(error) -> Error(error)
              }
            }),
          )

          let type_ =
            Function(
              parameters: [parameter_type],
              return: return_type,
              field_map: None,
            )
          Ok(#(state, Compiled(capture_doc(function, before, after), type_)))
        }
      }
    Unbound(_) -> {
      let #(state, return_type) = next_unbound(state)

      let function_type =
        Function(
          parameters: argument_types,
          return: return_type,
          field_map: None,
        )
      use #(state, _) <- result.try(unify(state, called_type, function_type))

      let type_ =
        Function(
          parameters: [parameter_type],
          return: return_type,
          field_map: None,
        )
      Ok(#(state, Compiled(capture_doc(function, before, after), type_)))
    }
  }
}

fn capture_doc(
  function: Compiled,
  before: List(Compiled),
  after: List(Compiled),
) -> Document {
  [
    doc.break("(", "("),
    before
      |> list.map(fn(arg) { arg.document })
      |> doc.join(doc.break(", ", ",")),
    after
      |> list.map(fn(arg) { arg.document })
      |> list.prepend(doc.from_string("_"))
      |> doc.join(doc.break(", ", ",")),
  ]
  |> doc.concat
  |> doc.nest(indent)
  |> doc.prepend(function.document)
  |> doc.append(doc.break("", ","))
  |> doc.append(doc.from_string(")"))
  |> doc.group
}

pub fn function(
  name: String,
  publicity: Publicity,
  function: FunctionBuilder(a),
  continue: fn(Expression(Constant)) -> Definition,
) -> Definition {
  use state <- Definition

  let #(state, return_type) = next_unbound(state)

  use #(state, function) <- result.try(
    function.compile(state, name, return_type, []),
  )
  use #(state, body) <- result.try(function.body.compile(state))

  use _ <- result.try(
    list.try_fold(function.parameters, False, fn(found_labelled, parameter) {
      case parameter.label {
        None if found_labelled ->
          Error(UnlabelledParameterAfterLabelledParameter)
        None -> Ok(False)
        Some(_) -> Ok(True)
      }
    }),
  )

  let parameter_list =
    [
      doc.break("(", "("),
      function.parameters
        |> list.map(parameter_to_doc(state, _))
        |> doc.join(doc.break(", ", ",")),
    ]
    |> doc.concat
    |> doc.nest(indent)
    |> doc.append(doc.break("", ","))
    |> doc.append(doc.from_string(")"))
    |> doc.group

  let body_doc =
    [
      doc.from_string("{"),
      doc.line,
      body.document,
    ]
    |> doc.concat
    |> doc.nest(indent)
    |> doc.append(doc.line)
    |> doc.append(doc.from_string("}"))
    |> doc.group

  use #(state, return_type) <- result.try(unify(state, body.type_, return_type))

  let #(state, return_type) = generalise(state, return_type)

  use #(fields, arity) <- result.try(
    list.try_fold(function.parameters, #(dict.new(), 0), fn(pair, parameter) {
      let #(map, index) = pair
      case parameter.label {
        None -> Ok(#(map, index + 1))
        Some(label) ->
          case dict.get(map, label) {
            Error(_) -> Ok(#(dict.insert(map, label, index), index + 1))
            Ok(_) -> Error(DuplicateLabel(label:))
          }
      }
    }),
  )

  let field_map = FieldMap(arity:, fields:)

  let type_ =
    Function(
      parameters: list.map(function.parameters, fn(parameter) {
        parameter.type_
      }),
      return: return_type,
      field_map: Some(field_map),
    )

  let function_name = instantiated(doc.from_string(name), type_)

  use #(state, rest) <- result.try(continue(function_name).compile(state))

  Ok(#(
    state,
    [
      publicity_to_doc(publicity),
      doc.from_string("fn "),
      doc.from_string(name),
      parameter_list,
      doc.from_string(" -> "),
      doc.from_string(print_type(state, return_type)),
      doc.from_string(" "),
      body_doc,
      doc.lines(2),
      rest,
    ]
      |> doc.concat,
  ))
}

pub fn empty() -> Definition {
  use state <- Definition
  Ok(#(state, doc.empty))
}

pub fn constant(
  name: String,
  publicity: Publicity,
  value: Expression(Constant),
  continue: fn(Expression(Constant)) -> Definition,
) -> Definition {
  use state <- Definition

  use #(state, value) <- result.try(value.compile(state))

  let #(state, type_) = generalise(state, value.type_)

  let constant_name = instantiated(doc.from_string(name), type_)

  use #(state, rest) <- result.try(continue(constant_name).compile(state))

  Ok(#(
    state,
    [
      publicity_to_doc(publicity),
      doc.from_string("const "),
      doc.from_string(name),
      doc.from_string(": "),
      doc.from_string(print_type(state, type_)),
      doc.from_string(" = "),
      value.document,
      doc.lines(2),
      rest,
    ]
      |> doc.concat,
  ))
}

pub fn doc_comment(
  comment: String,
  continue: fn() -> Definition,
) -> Definition {
  use state <- Definition

  use #(state, rest) <- result.try(continue().compile(state))

  let comment =
    comment
    |> string.split("\n")
    |> list.map(fn(line) { doc.from_string("/// " <> line) })
    |> doc.join(doc.line)

  Ok(#(
    state,
    [
      comment,
      doc.line,
      rest,
    ]
      |> doc.concat,
  ))
}

fn print_type(state: State, type_: ConcreteType) -> String {
  case unwrap_type(state, type_) {
    Custom(module: _, name:, generics:) ->
      case generics {
        [] -> name
        _ ->
          name
          <> "("
          <> string.join(list.map(generics, print_type(state, _)), ", ")
          <> ")"
      }
    Function(parameters:, return:, field_map: _) ->
      "fn("
      <> string.join(list.map(parameters, print_type(state, _)), ", ")
      <> ") -> "
      <> print_type(state, return)
    Tuple(elements:) ->
      "#(" <> string.join(list.map(elements, print_type(state, _)), ", ") <> ")"
    Unbound(id:) | Generic(id:) ->
      case dict.get(state.type_variable_names, id) {
        Ok(name) -> name
        Error(_) -> generate_type_variable_name(id)
      }
  }
}

fn generate_type_variable_name(id: Int) -> String {
  generate_type_variable_name_loop(id, "")
}

fn generate_type_variable_name_loop(id: Int, name: String) -> String {
  case id < 26 {
    True -> letter(id) <> name
    False -> {
      let name = letter(id % 26) <> name
      let id = id / 26
      generate_type_variable_name_loop(id, name)
    }
  }
}

fn letter(id: Int) -> String {
  case id {
    0 -> "a"
    1 -> "b"
    2 -> "c"
    3 -> "d"
    4 -> "e"
    5 -> "f"
    6 -> "g"
    7 -> "h"
    8 -> "i"
    9 -> "j"
    10 -> "k"
    11 -> "l"
    12 -> "m"
    13 -> "n"
    14 -> "o"
    15 -> "p"
    16 -> "q"
    17 -> "r"
    18 -> "s"
    19 -> "t"
    20 -> "u"
    21 -> "v"
    22 -> "w"
    23 -> "x"
    24 -> "y"
    _ -> "z"
  }
}

pub opaque type Argument {
  Argument(label: Option(String), value: Expression(Variable))
}

pub fn argument(
  label: Option(String),
  value: Expression(Variable),
) -> Argument {
  Argument(label, value)
}

type CompiledArgument {
  CompiledArgument(label: Option(String), value: Compiled)
}

pub fn labelled_call(
  function: Expression(a),
  arguments: List(Argument),
) -> Expression(Variable) {
  use state <- Expression
  use #(state, function) <- result.try(function.compile(state))
  use #(state, arguments) <- result.try(
    try_fold_with_state(state, arguments, [], fn(state, arguments, argument) {
      use #(state, value) <- result.map(argument.value.compile(state))
      #(state, [CompiledArgument(label: argument.label, value:), ..arguments])
    }),
  )
  let arguments = list.reverse(arguments)

  let called_type = unwrap_type(state, function.type_)

  let field_map = case called_type {
    Custom(..) | Tuple(..) | Unbound(..) | Generic(..) -> None
    Function(field_map:, ..) -> field_map
  }

  use argument_types <- result.try(case field_map {
    None -> assert_no_labelled_arguments(arguments)
    Some(field_map) -> reorder(arguments, field_map)
  })

  let called_type = unwrap_type(state, function.type_)

  case called_type {
    Custom(..) | Tuple(..) | Generic(..) -> Error(InvalidCall(called_type))
    Function(parameters:, return: return_type, field_map: _) ->
      case list.strict_zip(argument_types, parameters) {
        Error(Nil) -> {
          let expected_length = list.length(parameters)
          let argument_length = list.length(argument_types)
          Error(IncorrectNumberOfArguments(
            expected: expected_length,
            got: argument_length,
          ))
        }
        Ok(zipped) -> {
          use state <- result.try(
            list.try_fold(zipped, state, fn(state, pair) {
              let #(arg, param) = pair
              case unify(state, arg, with: param) {
                Ok(#(state, _)) -> Ok(state)
                Error(error) -> Error(error)
              }
            }),
          )

          Ok(#(
            state,
            Compiled(labelled_call_doc(arguments, function), return_type),
          ))
        }
      }
    Unbound(_) -> {
      let #(state, return_type) = next_generic(state)

      let function_type =
        Function(
          parameters: argument_types,
          return: return_type,
          field_map: None,
        )
      use #(state, _) <- result.try(unify(state, called_type, function_type))

      Ok(#(state, Compiled(labelled_call_doc(arguments, function), return_type)))
    }
  }
}

fn labelled_call_doc(
  arguments: List(CompiledArgument),
  function: Compiled,
) -> Document {
  [
    doc.break("(", "("),
    arguments
      |> list.map(fn(arg) {
        case arg.label {
          None -> arg.value.document
          Some(label) ->
            doc.concat([
              doc.from_string(label),
              doc.from_string(": "),
              arg.value.document,
            ])
        }
      })
      |> doc.join(doc.break(", ", ",")),
  ]
  |> doc.concat
  |> doc.nest(indent)
  |> doc.prepend(function.document)
  |> doc.append(doc.break("", ","))
  |> doc.append(doc.from_string(")"))
  |> doc.group
}

fn reorder(
  arguments: List(CompiledArgument),
  field_map: FieldMap,
) -> Result(List(ConcreteType), Error) {
  // TODO: Handle incorrect arity and duplicate labels
  let argument_count = list.length(arguments)
  use <- bool.guard(
    argument_count != field_map.arity,
    Error(IncorrectNumberOfArguments(
      expected: field_map.arity,
      got: argument_count,
    )),
  )

  use #(unlabelled_fields, labelled_fields) <- result.map(split_arguments(
    arguments,
    field_map,
    dict.new(),
    [],
    set.new(),
  ))

  reorder_arguments(unlabelled_fields, labelled_fields, 0, [])
}

fn reorder_arguments(
  unlabelled_fields: List(ConcreteType),
  labelled_fields: Dict(Int, ConcreteType),
  index: Int,
  out: List(ConcreteType),
) -> List(ConcreteType) {
  case dict.get(labelled_fields, index), unlabelled_fields {
    Error(_), [] -> list.reverse(out)
    Ok(value), _ ->
      reorder_arguments(unlabelled_fields, labelled_fields, index + 1, [
        value,
        ..out
      ])
    Error(_), [first, ..rest] ->
      reorder_arguments(rest, labelled_fields, index + 1, [first, ..out])
  }
}

fn split_arguments(
  arguments: List(CompiledArgument),
  field_map: FieldMap,
  labelled: Dict(Int, ConcreteType),
  unlabelled: List(ConcreteType),
  seen_labels: Set(String),
) -> Result(#(List(ConcreteType), Dict(Int, ConcreteType)), Error) {
  case arguments {
    [] -> Ok(#(list.reverse(unlabelled), labelled))
    [first, ..rest] ->
      case first.label {
        None ->
          split_arguments(
            rest,
            field_map,
            labelled,
            [first.value.type_, ..unlabelled],
            seen_labels,
          )
        Some(label) ->
          case set.contains(seen_labels, label) {
            True -> Error(DuplicateLabel(label:))
            False ->
              case dict.get(field_map.fields, label) {
                Error(_) ->
                  Error(UnknownLabel(
                    label:,
                    available_labels: dict.keys(field_map.fields),
                  ))
                Ok(index) ->
                  split_arguments(
                    rest,
                    field_map,
                    dict.insert(labelled, index, first.value.type_),
                    unlabelled,
                    set.insert(seen_labels, label),
                  )
              }
          }
      }
  }
}

fn assert_no_labelled_arguments(
  arguments: List(CompiledArgument),
) -> Result(List(ConcreteType), Error) {
  arguments
  |> list.try_map(fn(argument) {
    case argument.label {
      None -> Ok(argument.value.type_)
      Some(label) -> Error(UnexpectedLabelledArgument(label:))
    }
  })
}

pub opaque type CustomType {
  CustomType(
    compile: fn(State, CustomTypeHead) ->
      Result(#(State, CustomTypeInfo), Error),
  )
}

pub opaque type TypeConstructors {
  TypeConstructors(
    compile: fn(State, ConcreteType) ->
      Result(#(State, List(Constructor), Definition), Error),
  )
}

type CustomTypeInfo {
  CustomTypeInfo(
    name: String,
    type_: ConcreteType,
    constructors: List(Constructor),
    parameters: List(#(String, ConcreteType)),
    rest: Definition,
  )
}

type CustomTypeHead {
  CustomTypeHead(name: String, parameters: List(#(String, ConcreteType)))
}

type Constructor {
  Constructor(name: String, fields: List(Field))
}

pub type Field {
  Field(label: Option(String), type_: Type)
}

pub fn custom_type(
  name: String,
  publicity: Publicity,
  continue: fn() -> CustomType,
) -> Definition {
  use state <- Definition

  let info = CustomTypeHead(name, parameters: [])

  let custom_type = continue()
  use #(state, custom_type) <- result.try(custom_type.compile(state, info))
  use #(state, rest) <- result.try(custom_type.rest.compile(state))

  let parameters = case custom_type.parameters {
    [] -> doc.empty
    _ ->
      doc.concat([
        doc.from_string("("),
        doc.join(
          list.map(custom_type.parameters, fn(parameter) {
            doc.from_string(parameter.0)
          }),
          doc.from_string(", "),
        ),
        doc.from_string(")"),
      ])
  }

  use <- bool.lazy_guard(custom_type.constructors == [], fn() {
    Ok(#(
      state,
      [
        doc.from_string("type "),
        doc.from_string(custom_type.name),
        parameters,
        doc.lines(2),
        rest,
      ]
        |> doc.concat,
    ))
  })

  let #(state, constructors) =
    list.map_fold(custom_type.constructors, state, fn(state, constructor) {
      case constructor.fields {
        [] -> #(state, doc.from_string(constructor.name))
        _ -> {
          let #(state, fields) =
            list.map_fold(constructor.fields, state, fn(state, field) {
              case field.label {
                None -> {
                  let #(state, type_) = field.type_.compile(state)
                  #(state, doc.from_string(print_type(state, type_)))
                }
                Some(label) -> {
                  let #(state, type_) = field.type_.compile(state)
                  #(
                    state,
                    [
                      doc.from_string(label),
                      doc.from_string(": "),
                      doc.from_string(print_type(state, type_)),
                    ]
                      |> doc.concat,
                  )
                }
              }
            })
          #(
            state,
            [
              doc.from_string(constructor.name),
              [
                doc.break("(", "("),
                doc.join(fields, doc.break(", ", ",")),
              ]
                |> doc.concat
                |> doc.nest(indent),
              doc.break("", ","),
              doc.from_string(")"),
            ]
              |> doc.concat,
          )
        }
      }
    })

  let constructors =
    constructors
    |> doc.join(doc.line)
    |> doc.prepend(doc.line)
    |> doc.group
    |> doc.nest(indent)

  Ok(#(
    state,
    [
      publicity_to_doc(publicity),
      doc.from_string("type "),
      doc.from_string(custom_type.name),
      parameters,
      doc.from_string(" {"),
      constructors,
      doc.line,
      doc.from_string("}"),
      doc.lines(2),
      rest,
    ]
      |> doc.concat,
  ))
}

pub fn constructor(
  name: String,
  fields: List(Field),
  continue: fn(Expression(Constant)) -> TypeConstructors,
) -> TypeConstructors {
  use state, type_ <- TypeConstructors

  use #(field_map_fields, arity) <- result.try(
    list.try_fold(fields, #(dict.new(), 0), fn(pair, parameter) {
      let #(map, index) = pair
      case parameter.label {
        None -> Ok(#(map, index + 1))
        Some(label) ->
          case dict.get(map, label) {
            Error(_) -> Ok(#(dict.insert(map, label, index), index + 1))
            Ok(_) -> Error(DuplicateLabel(label:))
          }
      }
    }),
  )

  let field_map = FieldMap(arity:, fields: field_map_fields)
  let #(state, parameter_types) =
    list.map_fold(fields, state, fn(state, field) { field.type_.compile(state) })
  let constructor_type = case fields {
    [] -> type_
    _ ->
      Function(
        parameters: parameter_types,
        return: type_,
        field_map: Some(field_map),
      )
  }

  let expression = instantiated(doc.from_string(name), constructor_type)

  let custom_type = continue(expression)
  use #(state, constructors, rest) <- result.try(custom_type.compile(
    state,
    type_,
  ))

  Ok(#(state, [Constructor(name:, fields:), ..constructors], rest))
}

fn concrete(type_: ConcreteType) -> Type {
  Type(fn(state) { #(state, type_) })
}

pub fn type_parameter(
  name: String,
  continue: fn(Type) -> CustomType,
) -> CustomType {
  use state, info <- CustomType

  let #(state, type_) = named_generic(state, name)

  let info =
    CustomTypeHead(
      ..info,
      parameters: list.append(info.parameters, [#(name, type_)]),
    )

  continue(concrete(type_)).compile(state, info)
}

pub fn custom_type_constructors(
  continue: fn(Type) -> TypeConstructors,
) -> CustomType {
  use state, info <- CustomType

  let parameters = list.map(info.parameters, fn(parameter) { parameter.1 })

  let type_ =
    Custom(module: state.module, name: info.name, generics: parameters)

  use #(state, constructors, rest) <- result.try(
    continue(concrete(type_)).compile(state, type_),
  )

  Ok(#(
    state,
    CustomTypeInfo(
      constructors:,
      parameters: info.parameters,
      rest:,
      name: info.name,
      type_:,
    ),
  ))
}

pub fn end_custom_type(continue: fn() -> Definition) -> TypeConstructors {
  use state, _type <- TypeConstructors

  let rest = continue()

  Ok(#(state, [], rest))
}
