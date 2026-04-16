//// Code generated using `trick` is built up by combining expressions and
//// statements into a module.
//// 
//// The `trick` API is designed to make it as hard as possible to make mistakes
//// in your generation.
//// 
//// A `trick` code generator will look something like this:
//// 
//// ```gleam
//// use pi <- trick.constant("pi", trick.Private, trick.float(3.14))
//// use circle_area <- trick.function("circle_area", trick.Public, {
////   use radius <- trick.parameter("radius", trick.float_type())
////   trick.function_body({
////     use radius_squared <- trick.variable(
////       "radius_squared",
////       trick.multiply_float(radius, radius),
////     )
////     trick.expression(trick.multiply_float(radius_squared, pi))
////   })
//// })
//// trick.end_module()
//// ```
//// 
//// The above code, when passed to [`to_string`](#to_string), will produce the
//// following code:
//// 
//// ```gleam
//// const pi = 3.14
//// 
//// pub fn circle_area(radius: Float) -> Float {
////   let radius_squared = radius *. radius
////   radius_squared *. pi
//// }
//// ```
//// 
//// The called functions more or less mimic the structure of the resulting code,
//// with the exception for a few boilerplate functions needed to convert between
//// types to appease the Gleam type system.
//// 
//// Expression generation is pretty intuitive and straightforward, but some of
//// the functions for creating custom types and top-level functions can get a
//// bit complicated due to type system limitations. See the documentation of
//// individual functions for full explanations of how they are used.
//// 
//// ## Table of contents
//// 
//// ### Definitions
//// - [`Module`](#Module)
////   - [`to_string`](#to_string)
//// - [`Publicity`](#Publicity)
//// - [`constant`](#constant)
//// - [`end_module`](#end_module)
//// - [`doc_comment`](#doc_comment)
//// - [`function`](#function)
////   - [`FunctionBuilder`](#FunctionBuilder)
////     - [`Unlabelled`](#Unlabelled)
////     - [`Labelled`](#Labelled)
////   - [`parameter`](#parameter)
////   - [`labelled_parameter`](#labelled_parameter)
////   - [`function_body`](#function_body)
////   - [`recursive`](#recursive)
//// - [`custom_type`](#custom_type)
////   - [`CustomType`](#CustomType)
////     - [`NoParameters`](#NoParameters)
////     - [`HasParameters`](#HasParameters)
////   - [`Field`](#Field)
////   - [`type_parameter`](#type_parameter)
////   - [`constructor`](#constructor)
////   - [`end_custom_type`](#end_custom_type)
//// ### Statements
//// - [`Statement`](#Statement)
//// - [`assert_`](#assert_)
//// - [`block`](#block)
//// - [`comment`](#comment)
//// - [`discard`](#discard)
//// - [`expression`](#expression)
//// - [`variable`](#variable)
//// ### Expressions
//// - [`Expression`](#Expression)
////   - [`Constant`](#Constant)
////   - [`Variable`](#Variable)
////   - [`expression_to_string`](#expression_to_string)
//// - [`int`](#int)
////   - [`int_base2`](#int_base2)
////   - [`int_base8`](#int_base8)
////   - [`int_base16`](#int_base16)
//// - [`float`](#float)
//// - [`string`](#string)
//// - [`bool`](#bool)
//// - [`nil`](#nil)
//// - [`add`](#add)
//// - [`add_float`](#add_float)
//// - [`subtract`](#subtract)
//// - [`subtract_float`](#subtract_float)
//// - [`multiply`](#multiply)
//// - [`multiply_float`](#multiply_float)
//// - [`divide`](#divide)
//// - [`divide_float`](#divide_float)
//// - [`remainder`](#remainder)
//// - [`concatenate`](#concatenate)
//// - [`and`](#and)
//// - [`or`](#or)
//// - [`equal`](#equal)
//// - [`not_equal`](#not_equal)
//// - [`less_than`](#less_than)
//// - [`less_than_float`](#less_than_float)
//// - [`less_than_or_equal`](#less_than_or_equal)
//// - [`less_than_or_equal_float`](#less_than_or_equal_float)
//// - [`greater_than`](#greater_than)
//// - [`greater_than_float`](#greater_than_float)
//// - [`greater_than_or_equal`](#greater_than_or_equal)
//// - [`greater_than_or_equal_float`](#greater_than_or_equal_float)
//// - [`negate_int`](#negate_int)
//// - [`negate_bool`](#negate_bool)
//// - [`list`](#list)
//// - [`prepend`](#prepend)
//// - [`tuple`](#tuple)
//// - [`tuple_index`](#tuple_index)
//// - [`panic_`](#panic_)
//// - [`todo_`](#todo_)
//// - [`echo_`](#echo_)
//// - [`anonymous`](#anonymous)
//// - [`call`](#call)
////   - [`Argument`](#Argument)
////   - [`function_capture`](#function_capture)
////   - [`function_capture_alt`](#function_capture_alt)
////   - [`labelled_call`](#labelled_call)
//// ### Types
//// - [`Type`](#Type)
//// - [`ConcreteType`](#ConcreteType)
//// - [`FieldMap`](#FieldMap)
//// - [`Error`](#Error)
//// - [`int_type`](#int_type)
//// - [`float_type`](#float_type)
//// - [`string_type`](#string_type)
//// - [`bool_type`](#bool_type)
//// - [`nil_type`](#nil_type)
//// - [`bit_array_type`](#bit_array_type)
//// - [`utf_codepoint_type`](#utf_codepoint_type)
//// - [`list_type`](#list_type)
//// - [`tuple_type`](#tuple_type)
//// - [`function_type`](#function_type)
//// - [`generic`](#generic)
//// 
//// ## Best practises
//// 
//// To avoid confusion, it's usually best if you **name any variables after their
//// names in the generated code**. For example, if you're defining a variable,
//// assign it to a variable of the same name in your generator code:
//// 
//// ```gleam
//// // DO:
//// use my_variable <- trick.variable("my_variable", trick.int(1))
//// 
//// // DON'T:
//// use number_one <- trick.variable("my_variable", trick.int(1))
//// ```
//// 
//// The same applies to functions, constants, and types (although you may need
//// to change these a little as your variable will be in the same scope as values).
//// This helps to avoid cases where you (accidentally or intentionally) shadow
//// a variable in the generated code, but don't shadow it in your generator code,
//// allowing out-of-scope values to be referenced.
//// 
//// **Break up your code generators into multiple functions**. While the API is
//// designed to be as ergonomic as possible, due to limitations of the Gleam
//// type system, generators can get quite verbose. Splitting separate parts of
//// the code into different functions can make it easier to read and modify.
//// After all, that's the benefit of having code generators just be plain old
//// Gleam code.
//// 

import glam/doc.{type Document}
import gleam/bool
import gleam/dict.{type Dict}
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/pair
import gleam/result
import gleam/set.{type Set}
import gleam/string
import lazy_const
import splitter

/// Indicates that an expression is constant and can be assigned to a `const`.
/// 
pub type Constant

/// Indicates that an expression includes a runtime computation can cannot be
/// assigned to a `const`.
/// 
pub type Variable

/// A single expression, either marked as [`Constant`](#Constant) or
/// [`Variable`](#Variable).
/// 
pub opaque type Expression(a) {
  Expression(compile: fn(State) -> Result(#(State, Compiled), Error))
}

/// One or more statements that can be used in a block or function body.
/// 
pub opaque type Statement {
  Statement(compile: fn(State) -> Result(#(State, Compiled), Error))
}

/// A module containing one or more definitions.
/// 
pub opaque type Module {
  Module(compile: fn(State) -> Result(#(State, CompiledModule), Error))
}

type CompiledModule {
  Empty
  Definition(Document)
}

fn separate_definition(definition: CompiledModule) -> Document {
  case definition {
    Empty -> doc.line
    Definition(document) -> doc.prepend(document, doc.lines(2))
  }
}

/// A type error.
/// 
pub type Error {
  TypeMismatch(expected: ConcreteType, got: ConcreteType)
  /// Attempting to access a non-existent tuple field 
  TupleIndexOutOfBounds(length: Int, index: Int)
  /// Attempting to perform tuple access on a value which is not a tuple
  InvalidTupleAccess(type_: ConcreteType)
  /// Attempting to call a value which is not a function
  InvalidCall(type_: ConcreteType)
  /// Attempting to prepend to a value which is not a list
  InvalidListPrepend(type_: ConcreteType)
  /// Calling a function with the incorrect number of arguments
  IncorrectNumberOfArguments(expected: Int, got: Int)
  /// Attempting to define an unlabelled function parameter after a labelled
  /// parameter
  UnlabelledParameterAfterLabelledParameter(name: String)
  /// Using a label in a call to a function with no labels
  UnexpectedLabelledArgument(label: String)
  /// Using a label which is not defined in the called function
  UnknownLabel(label: String, available_labels: List(String))
  /// Attempting to define parameters with duplicate labels
  DuplicateLabel(label: String)
  /// No capture hole is provided to a [`function_capture_alt`](#function_capture_alt)
  /// call
  NoCaptureHole
  /// More than one capture hole is provided to a
  /// [`function_capture_alt`](#function_capture_alt) call
  DuplicateCaptureHole
  /// The name of a value does not match what is expected.
  InvalidName(name: String, expected: NameCase)
  /// Attempting field access on a value which is not a custom type.
  InvalidFieldAccess(type_: ConcreteType)
  /// Attempting to access a field on a custom type which does not have said
  /// field.
  TypeDoesNotHaveField(type_: ConcreteType, field: String)
}

/// The expected case of the name for a definition.
/// 
pub type NameCase {
  SnakeCase
  PascalCase
}

fn check_name_case(name: String, casing: NameCase) -> Result(Nil, Error) {
  let matches = case casing {
    SnakeCase -> is_snake_case(name)
    PascalCase -> is_pascal_case(name)
  }

  case matches {
    True -> Ok(Nil)
    False -> Error(InvalidName(name, casing))
  }
}

fn is_pascal_case(name: String) -> Bool {
  case name {
    "A" <> name
    | "B" <> name
    | "C" <> name
    | "D" <> name
    | "E" <> name
    | "F" <> name
    | "G" <> name
    | "H" <> name
    | "I" <> name
    | "J" <> name
    | "K" <> name
    | "L" <> name
    | "M" <> name
    | "N" <> name
    | "O" <> name
    | "P" <> name
    | "Q" <> name
    | "R" <> name
    | "S" <> name
    | "T" <> name
    | "U" <> name
    | "V" <> name
    | "W" <> name
    | "X" <> name
    | "Y" <> name
    | "Z" <> name -> is_pascal_case_loop(name)
    _ -> False
  }
}

fn is_pascal_case_loop(name: String) -> Bool {
  case name {
    "" -> True
    "A" <> name
    | "B" <> name
    | "C" <> name
    | "D" <> name
    | "E" <> name
    | "F" <> name
    | "G" <> name
    | "H" <> name
    | "I" <> name
    | "J" <> name
    | "K" <> name
    | "L" <> name
    | "M" <> name
    | "N" <> name
    | "O" <> name
    | "P" <> name
    | "Q" <> name
    | "R" <> name
    | "S" <> name
    | "T" <> name
    | "U" <> name
    | "V" <> name
    | "W" <> name
    | "X" <> name
    | "Y" <> name
    | "Z" <> name
    | "a" <> name
    | "b" <> name
    | "c" <> name
    | "d" <> name
    | "e" <> name
    | "f" <> name
    | "g" <> name
    | "h" <> name
    | "i" <> name
    | "j" <> name
    | "k" <> name
    | "l" <> name
    | "m" <> name
    | "n" <> name
    | "o" <> name
    | "p" <> name
    | "q" <> name
    | "r" <> name
    | "s" <> name
    | "t" <> name
    | "u" <> name
    | "v" <> name
    | "w" <> name
    | "x" <> name
    | "y" <> name
    | "z" <> name
    | "0" <> name
    | "1" <> name
    | "2" <> name
    | "3" <> name
    | "4" <> name
    | "5" <> name
    | "6" <> name
    | "7" <> name
    | "8" <> name
    | "9" <> name -> is_pascal_case_loop(name)
    _ -> False
  }
}

fn is_snake_case(name: String) -> Bool {
  case name {
    "_" <> name
    | "a" <> name
    | "b" <> name
    | "c" <> name
    | "d" <> name
    | "e" <> name
    | "f" <> name
    | "g" <> name
    | "h" <> name
    | "i" <> name
    | "j" <> name
    | "k" <> name
    | "l" <> name
    | "m" <> name
    | "n" <> name
    | "o" <> name
    | "p" <> name
    | "q" <> name
    | "r" <> name
    | "s" <> name
    | "t" <> name
    | "u" <> name
    | "v" <> name
    | "w" <> name
    | "x" <> name
    | "y" <> name
    | "z" <> name -> is_snake_case_loop(name)
    _ -> False
  }
}

fn is_snake_case_loop(name: String) -> Bool {
  case name {
    "" -> True
    "_" <> name
    | "a" <> name
    | "b" <> name
    | "c" <> name
    | "d" <> name
    | "e" <> name
    | "f" <> name
    | "g" <> name
    | "h" <> name
    | "i" <> name
    | "j" <> name
    | "k" <> name
    | "l" <> name
    | "m" <> name
    | "n" <> name
    | "o" <> name
    | "p" <> name
    | "q" <> name
    | "r" <> name
    | "s" <> name
    | "t" <> name
    | "u" <> name
    | "v" <> name
    | "w" <> name
    | "x" <> name
    | "y" <> name
    | "z" <> name
    | "0" <> name
    | "1" <> name
    | "2" <> name
    | "3" <> name
    | "4" <> name
    | "5" <> name
    | "6" <> name
    | "7" <> name
    | "8" <> name
    | "9" <> name -> is_snake_case_loop(name)
    _ -> False
  }
}

type Compiled {
  Compiled(document: Document, type_: ConcreteType, precedence: Int)
}

const precedence_or = 1

const precedence_and = 2

const precedence_equality = 3

const precedence_compare = 4

const precedence_concatenate = 5

// const precedence_pipe = 6

const precedence_add = 7

const precedence_multiply = 8

const precedence_prefix = 9

const precedence_unit = 10

fn maybe_wrap(value: Compiled, precedence: Int) -> Document {
  case value.precedence < precedence {
    True -> block_doc(value.document)
    False -> value.document
  }
}

/// The type of a value. This is different to [`ConcreteType`](#ConcreteType)
/// in that it exists before type-checking and does not contain complete
/// information yet.
/// 
pub opaque type Type {
  Type(compile: fn(State) -> Result(#(State, ConcreteType), Error))
}

/// A known type for an expression. Unlike [`Type`](#Type), this exists after
/// type-checking and contains the full information about each type.
/// 
pub type ConcreteType {
  Custom(
    module: String,
    name: String,
    generics: List(ConcreteType),
    shared_fields: Dict(String, ConcreteType),
  )
  Generic(id: Int)
  Unbound(id: Int)
  Tuple(elements: List(ConcreteType))
  Function(
    parameters: List(ConcreteType),
    return: ConcreteType,
    field_map: Option(FieldMap),
  )
}

/// Information about the labels and arity of a function or constructor.
/// 
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
    type_variable_number: Int,
    used_type_variable_names: Set(String),
  )
}

fn type_int() -> ConcreteType {
  Custom("gleam", "Int", [], dict.new())
}

fn type_float() -> ConcreteType {
  Custom("gleam", "Float", [], dict.new())
}

fn type_string() -> ConcreteType {
  Custom("gleam", "String", [], dict.new())
}

fn type_bool() -> ConcreteType {
  Custom("gleam", "Bool", [], dict.new())
}

fn type_nil() -> ConcreteType {
  Custom("gleam", "Nil", [], dict.new())
}

/// The publicity of a top-level definition.
/// 
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
  Custom("gleam", "List", [element], dict.new())
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
      used_type_variable_names: set.insert(state.used_type_variable_names, name),
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
      used_type_variable_names: set.insert(state.used_type_variable_names, name),
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
  let a = unwrap_type(state, a)
  let b = unwrap_type(state, b)

  use <- bool.guard(a == b, Ok(#(state, a)))

  let mismatch = TypeMismatch(expected: b, got: a)

  case a, b {
    Unbound(id:), other | other, Unbound(id:) -> {
      let state =
        State(
          ..state,
          resolved_variables: dict.insert(state.resolved_variables, id, other),
        )
      Ok(#(state, other))
    }
    Custom(module: m1, name: n1, generics: g1, shared_fields:),
      Custom(module: m2, name: n2, generics: g2, shared_fields: _)
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
              Ok(#(state, Custom(m1, n1, list.reverse(generics), shared_fields)))
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

fn unwrap_type(state: State, type_: ConcreteType) -> ConcreteType {
  case type_ {
    Custom(..) | Tuple(..) | Function(..) | Generic(..) -> type_
    Unbound(id:) ->
      case dict.get(state.resolved_variables, id) {
        Error(_) -> type_
        Ok(type_) -> unwrap_type(state, type_)
      }
  }
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
  case unwrap_type(state, type_) {
    Custom(module:, name:, generics:, shared_fields:) -> {
      let #(#(state, instantiated), generics) =
        list.map_fold(generics, #(state, instantiated), fn(acc, generic) {
          let #(state, instantiated) = acc
          let #(state, generic, instantiated) =
            do_instantiate(state, generic, instantiated)
          #(#(state, instantiated), generic)
        })
      #(state, Custom(module:, name:, generics:, shared_fields:), instantiated)
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
    Custom(module:, name:, generics:, shared_fields:) -> {
      let #(#(state, generalised), generics) =
        list.map_fold(generics, #(state, generalised), fn(acc, generic) {
          let #(state, generalised) = acc
          let #(state, generic, generalised) =
            do_generalise(state, generic, generalised)
          #(#(state, generalised), generic)
        })
      #(state, Custom(module:, name:, generics:, shared_fields:), generalised)
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

fn instantiated(
  doc: Document,
  type_: ConcreteType,
  precedence: Int,
) -> Expression(_) {
  use state <- Expression
  let #(state, type_) = instantiate(state, type_)
  Ok(#(state, Compiled(doc, type_, precedence)))
}

/// Returns the `Int` type.
/// 
pub fn int_type() -> Type {
  concrete(type_int())
}

/// Returns the `Float` type.
/// 
pub fn float_type() -> Type {
  concrete(type_float())
}

/// Returns the `String` type.
/// 
pub fn string_type() -> Type {
  concrete(type_string())
}

/// Returns the `Bool` type.
/// 
pub fn bool_type() -> Type {
  concrete(type_bool())
}

/// Returns the `Nil` type.
/// 
pub fn nil_type() -> Type {
  concrete(type_nil())
}

/// Returns the `BitArray` type.
/// 
pub fn bit_array_type() -> Type {
  concrete(Custom("gleam", "BitArray", [], dict.new()))
}

/// Returns the `UtfCodepoint` type.
/// 
pub fn utf_codepoint_type() -> Type {
  concrete(Custom("gleam", "UtfCodepoint", [], dict.new()))
}

/// Returns a `List` type with the specified element type.
/// 
pub fn list_type(of element_type: Type) -> Type {
  use state <- Type
  use #(state, element_type) <- result.map(element_type.compile(state))
  #(state, type_list(element_type))
}

/// Returns a tuple type containing the specified elements.
/// 
pub fn tuple_type(containing elements: List(Type)) -> Type {
  use state <- Type
  use #(state, elements) <- result.map(
    try_map_fold(elements, state, fn(state, element) { element.compile(state) }),
  )
  #(state, Tuple(elements:))
}

fn try_map_fold(
  list: List(elem),
  acc: acc,
  f: fn(acc, elem) -> Result(#(acc, out), error),
) -> Result(#(acc, List(out)), error) {
  try_map_fold_loop(list, acc, [], f)
}

fn try_map_fold_loop(
  list: List(elem),
  acc: acc,
  out: List(out),
  f: fn(acc, elem) -> Result(#(acc, out), error),
) -> Result(#(acc, List(out)), error) {
  case list {
    [] -> Ok(#(acc, list.reverse(out)))
    [first, ..rest] ->
      case f(acc, first) {
        Ok(#(acc, next)) -> try_map_fold_loop(rest, acc, [next, ..out], f)
        Error(error) -> Error(error)
      }
  }
}

/// Returns a function type with the specified parameters and return type.
/// 
pub fn function_type(parameters: List(Type), return: Type) -> Type {
  use state <- Type
  use #(state, parameters) <- result.try(
    try_map_fold(parameters, state, fn(state, parameter) {
      parameter.compile(state)
    }),
  )
  use #(state, return) <- result.map(return.compile(state))
  #(state, Function(parameters:, return:, field_map: None))
}

/// Returns a generic type with the given name.
/// 
pub fn generic(name: String) -> Type {
  use state <- Type
  use _ <- result.map(check_name_case(name, SnakeCase))
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

/// Turns an `Expression` into a string of Gleam code.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.int(1) |> trick.add(trick.int(2)) |> trick.expression_to_string
/// // -> Ok("1 + 2")
/// ```
/// 
/// ```gleam
/// trick.int(1) |> trick.add(trick.float(2.0)) |> trick.expression_to_string
/// // -> Error(TypeMismatch(expected: Int, got: Float))
/// ```
///
pub fn expression_to_string(
  expression: Expression(a),
) -> Result(String, Error) {
  case expression.compile(new_state()) {
    Ok(#(_state, expression)) -> Ok(doc.to_string(expression.document, width))
    Error(error) -> Error(error)
  }
}

/// Turns a `Module` into a string of Gleam code.
/// 
/// ### Examples
/// 
/// ```gleam
/// {
///   use _pi <- trick.constant("pi", trick.Public, trick.float(3.14))
///   trick.end_module()
/// }
/// |> trick.to_string
/// // -> Ok("pub const pi = 3.14")
/// ```
///
pub fn to_string(module: Module) -> Result(String, Error) {
  case module.compile(new_state()) {
    Ok(#(_state, Empty)) -> Ok("")
    Ok(#(_state, Definition(document))) -> Ok(doc.to_string(document, width))
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
    type_variable_number: 0,
    used_type_variable_names: set.new(),
  )
}

/// Generates an `Int`.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.int(42) |> trick.expression_to_string
/// // -> Ok("42")
/// ```
/// 
pub fn int(value: Int) -> Expression(a) {
  value
  |> int.to_string
  |> doc.from_string
  |> Compiled(type_int(), precedence_unit)
  |> doc_to_expression
}

/// Generates an `Int` using binary syntax.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.int_base2(42) |> trick.expression_to_string
/// // -> Ok("0b101010")
/// ```
/// 
pub fn int_base2(value: Int) -> Expression(a) {
  value
  |> int.to_base2
  |> doc.from_string
  |> doc.prepend(doc.from_string("0b"))
  |> Compiled(type_int(), precedence_unit)
  |> doc_to_expression
}

/// Generates an `Int` using octal syntax.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.int_base8(42) |> trick.expression_to_string
/// // -> Ok("0o52")
/// ```
/// 
pub fn int_base8(value: Int) -> Expression(a) {
  value
  |> int.to_base8
  |> doc.from_string
  |> doc.prepend(doc.from_string("0o"))
  |> Compiled(type_int(), precedence_unit)
  |> doc_to_expression
}

/// Generates an `Int` using hexadecimal syntax.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.int_base16(42) |> trick.expression_to_string
/// // -> Ok("0x2a")
/// ```
/// 
pub fn int_base16(value: Int) -> Expression(a) {
  value
  |> int.to_base16
  |> doc.from_string
  |> doc.prepend(doc.from_string("0x"))
  |> Compiled(type_int(), precedence_unit)
  |> doc_to_expression
}

/// Generates a `Float`.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.float(3.14) |> trick.expression_to_string
/// // -> Ok("3.14")
/// ```
/// 
pub fn float(value: Float) -> Expression(a) {
  value
  |> float.to_string
  |> doc.from_string
  |> Compiled(type_float(), precedence_unit)
  |> doc_to_expression
}

/// Generates a `String`.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.string("Hello, world!") |> trick.expression_to_string
/// // -> Ok("\"Hello, world!\"")
/// ```
/// 
pub fn string(value: String) -> Expression(a) {
  value
  |> escape_string_literal
  |> doc.from_string
  |> Compiled(type_string(), precedence_unit)
  |> doc_to_expression
}

/// Generates a `Bool`.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.bool(True) |> trick.expression_to_string
/// // -> Ok("True")
/// ```
/// 
pub fn bool(value: Bool) -> Expression(a) {
  value
  |> bool.to_string
  |> doc.from_string
  |> Compiled(type_bool(), precedence_unit)
  |> doc_to_expression
}

/// Generates `Nil`.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.nil() |> trick.expression_to_string
/// // -> Ok("Nil")
/// ```
/// 
pub fn nil() -> Expression(a) {
  "Nil"
  |> doc.from_string
  |> Compiled(type_nil(), precedence_unit)
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
  precedence: Int,
) -> Expression(_) {
  use state <- Expression
  use #(state, left) <- result.try(left.compile(state))
  use #(state, right) <- result.try(right.compile(state))
  use #(state, _) <- result.try(unify(state, left.type_, operand_type))
  use #(state, _) <- result.try(unify(state, right.type_, operand_type))

  Ok(#(
    state,
    [
      maybe_wrap(left, precedence),
      doc.from_string(" " <> operator <> " "),
      maybe_wrap(right, precedence + 1),
    ]
      |> doc.concat
      |> Compiled(result_type, precedence),
  ))
}

/// Generates a `+` operation.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.add(trick.int(1), trick.int(2)) |> trick.expression_to_string
/// // -> Ok("1 + 2")
/// ```
/// 
pub fn add(left: Expression(_), right: Expression(_)) -> Expression(Variable) {
  binary_operator(left, "+", right, type_int(), type_int(), precedence_add)
}

/// Generates a `+.` operation.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.add_float(trick.float(1.0), trick.float(2.0))
/// |> trick.expression_to_string
/// // -> Ok("1.0 +. 2.0")
/// ```
/// 
pub fn add_float(
  left: Expression(_),
  right: Expression(_),
) -> Expression(Variable) {
  binary_operator(left, "+.", right, type_float(), type_float(), precedence_add)
}

/// Generates a `-` operation.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.subtract(trick.int(1), trick.int(2)) |> trick.expression_to_string
/// // -> Ok("1 - 2")
/// ```
/// 
pub fn subtract(
  from left: Expression(_),
  subtract right: Expression(_),
) -> Expression(Variable) {
  binary_operator(left, "-", right, type_int(), type_int(), precedence_add)
}

/// Generates a `-.` operation.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.subtract_float(trick.float(1.0), trick.float(2.0))
/// |> trick.expression_to_string
/// // -> Ok("1.0 -. 2.0")
/// ```
/// 
pub fn subtract_float(
  from left: Expression(_),
  subtract right: Expression(_),
) -> Expression(Variable) {
  binary_operator(left, "-.", right, type_float(), type_float(), precedence_add)
}

/// Generates a `*` operation.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.multiply(trick.int(1), trick.int(2)) |> trick.expression_to_string
/// // -> Ok("1 * 2")
/// ```
/// 
pub fn multiply(
  left: Expression(_),
  right: Expression(_),
) -> Expression(Variable) {
  binary_operator(left, "*", right, type_int(), type_int(), precedence_multiply)
}

/// Generates a `*.` operation.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.multiply_float(trick.float(1.0), trick.float(2.0))
/// |> trick.expression_to_string
/// // -> Ok("1.0 *. 2.0")
/// ```
/// 
pub fn multiply_float(
  left: Expression(_),
  right: Expression(_),
) -> Expression(Variable) {
  binary_operator(
    left,
    "*.",
    right,
    type_float(),
    type_float(),
    precedence_multiply,
  )
}

/// Generates a `/` operation.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.divide(trick.int(1), trick.int(2)) |> trick.expression_to_string
/// // -> Ok("1 / 2")
/// ```
/// 
pub fn divide(
  divide left: Expression(_),
  by right: Expression(_),
) -> Expression(Variable) {
  binary_operator(left, "/", right, type_int(), type_int(), precedence_multiply)
}

/// Generates a `/.` operation.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.divide_float(trick.float(1.0), trick.float(2.0))
/// |> trick.expression_to_string
/// // -> Ok("1.0 /. 2.0")
/// ```
/// 
pub fn divide_float(
  divide left: Expression(_),
  by right: Expression(_),
) -> Expression(Variable) {
  binary_operator(
    left,
    "/.",
    right,
    type_float(),
    type_float(),
    precedence_multiply,
  )
}

/// Generates a `%` operation.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.remainder(trick.int(1), trick.int(2)) |> trick.expression_to_string
/// // -> Ok("1 % 2")
/// ```
/// 
pub fn remainder(
  left: Expression(_),
  right: Expression(_),
) -> Expression(Variable) {
  binary_operator(left, "%", right, type_int(), type_int(), precedence_multiply)
}

/// Generates a `<>` operation.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.concatenate(trick.string("Hello"), trick.string("world"))
/// |> trick.expression_to_string
/// // -> Ok("\"Hello\" <> \"world\"")
/// ```
/// 
pub fn concatenate(left: Expression(a), right: Expression(a)) -> Expression(a) {
  binary_operator(
    left,
    "<>",
    right,
    type_string(),
    type_string(),
    precedence_concatenate,
  )
}

/// Generates a `&&` operation.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.and(trick.bool(True), trick.bool(False))
/// |> trick.expression_to_string
/// // -> Ok("True && False")
/// ```
/// 
pub fn and(left: Expression(_), right: Expression(_)) -> Expression(Variable) {
  binary_operator(left, "&&", right, type_bool(), type_bool(), precedence_and)
}

/// Generates a `||` operation.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.or(trick.bool(False), trick.bool(False))
/// |> trick.expression_to_string
/// // -> Ok("False || False")
/// ```
/// 
pub fn or(left: Expression(_), right: Expression(_)) -> Expression(Variable) {
  binary_operator(left, "||", right, type_bool(), type_bool(), precedence_or)
}

/// Generates a `==` operation. The two values must be of the same type.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.equal(trick.int(1), trick.int(1)) |> trick.expression_to_string
/// // -> Ok("1 == 1")
/// ```
/// 
/// ```gleam
/// trick.equal(trick.float(1.0), trick.float(2.0))
/// |> trick.expression_to_string
/// // -> Ok("1.0 == 2.0")
/// ```
/// 
/// ```gleam
/// trick.equal(trick.int(1), trick.float(2.0))
/// |> trick.expression_to_string
/// // -> Error(TypeMismatch(expected: Int, got: Float))
/// ```
/// 
pub fn equal(
  left: Expression(_),
  right: Expression(_),
) -> Expression(Variable) {
  use state <- Expression
  let #(state, type_) = next_unbound(state)
  binary_operator(left, "==", right, type_, type_bool(), precedence_equality).compile(
    state,
  )
}

/// Generates a `!=` operation. The two values must be of the same type.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.not_equal(trick.int(1), trick.int(1)) |> trick.expression_to_string
/// // -> Ok("1 != 1")
/// ```
/// 
/// ```gleam
/// trick.not_equal(trick.float(1.0), trick.float(2.0))
/// |> trick.expression_to_string
/// // -> Ok("1.0 != 2.0")
/// ```
/// 
/// ```gleam
/// trick.not_equal(trick.int(1), trick.float(2.0))
/// |> trick.expression_to_string
/// // -> Error(TypeMismatch(expected: Int, got: Float))
/// ```
/// 
pub fn not_equal(
  left: Expression(_),
  right: Expression(_),
) -> Expression(Variable) {
  use state <- Expression
  let #(state, type_) = next_unbound(state)
  binary_operator(left, "!=", right, type_, type_bool(), precedence_equality).compile(
    state,
  )
}

/// Generates a `<` operation.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.less_than(trick.int(1), trick.int(2)) |> trick.expression_to_string
/// // -> Ok("1 < 2")
/// ```
/// 
pub fn less_than(
  left: Expression(_),
  right: Expression(_),
) -> Expression(Variable) {
  binary_operator(left, "<", right, type_int(), type_bool(), precedence_compare)
}

/// Generates a `<.` operation.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.less_than_float(trick.float(1.0), trick.float(2.0))
/// |> trick.expression_to_string
/// // -> Ok("1.0 <. 2.0")
/// ```
/// 
pub fn less_than_float(
  left: Expression(_),
  right: Expression(_),
) -> Expression(Variable) {
  binary_operator(
    left,
    "<.",
    right,
    type_float(),
    type_bool(),
    precedence_compare,
  )
}

/// Generates a `<=` operation.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.less_than_or_equal(trick.int(1), trick.int(2))
/// |> trick.expression_to_string
/// // -> Ok("1 <= 2")
/// ```
/// 
pub fn less_than_or_equal(
  left: Expression(_),
  right: Expression(_),
) -> Expression(Variable) {
  binary_operator(
    left,
    "<=",
    right,
    type_int(),
    type_bool(),
    precedence_compare,
  )
}

/// Generates a `<=.` operation.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.less_than_or_equal_float(trick.float(1.0), trick.float(2.0))
/// |> trick.expression_to_string
/// // -> Ok("1.0 <=. 2.0")
/// ```
/// 
pub fn less_than_or_equal_float(
  left: Expression(_),
  right: Expression(_),
) -> Expression(Variable) {
  binary_operator(
    left,
    "<=.",
    right,
    type_float(),
    type_bool(),
    precedence_compare,
  )
}

/// Generates a `>` operation.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.greater_than(trick.int(1), trick.int(2))
/// |> trick.expression_to_string
/// // -> Ok("1 > 2")
/// ```
/// 
pub fn greater_than(
  left: Expression(_),
  right: Expression(_),
) -> Expression(Variable) {
  binary_operator(left, ">", right, type_int(), type_bool(), precedence_compare)
}

/// Generates a `>.` operation.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.greater_than_float(trick.float(1.0), trick.float(2.0))
/// |> trick.expression_to_string
/// // -> Ok("1.0 >. 2.0")
/// ```
/// 
pub fn greater_than_float(
  left: Expression(_),
  right: Expression(_),
) -> Expression(Variable) {
  binary_operator(
    left,
    ">.",
    right,
    type_float(),
    type_bool(),
    precedence_compare,
  )
}

/// Generates a `>=` operation.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.greater_than_or_equal(trick.int(1), trick.int(2))
/// |> trick.expression_to_string
/// // -> Ok("1 >= 2")
/// ```
/// 
pub fn greater_than_or_equal(
  left: Expression(_),
  right: Expression(_),
) -> Expression(Variable) {
  binary_operator(
    left,
    ">=",
    right,
    type_int(),
    type_bool(),
    precedence_compare,
  )
}

/// Generates a `>=.` operation.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.greater_than_or_equal_float(trick.float(1.0), trick.float(2.0))
/// |> trick.expression_to_string
/// // -> Ok("1.0 >=. 2.0")
/// ```
/// 
pub fn greater_than_or_equal_float(
  left: Expression(_),
  right: Expression(_),
) -> Expression(Variable) {
  binary_operator(
    left,
    ">=.",
    right,
    type_float(),
    type_bool(),
    precedence_compare,
  )
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
    [doc.from_string(operator), maybe_wrap(value, precedence_prefix)]
      |> doc.concat
      |> Compiled(type_, precedence_prefix),
  ))
}

/// Generates a unary `-` operation.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.negate_int(trick.int(1))
/// |> trick.expression_to_string
/// // -> Ok("-1")
/// ```
/// 
/// ```gleam
/// trick.negate_int(trick.float(1.0))
/// |> trick.expression_to_string
/// // -> Error(TypeMismatch(expected: Int, got: Float))
/// ```
/// 
pub fn negate_int(value: Expression(a)) -> Expression(Variable) {
  unary_operator("-", value, type_int())
}

/// Generates a unary `!` operation.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.negate_bool(trick.bool(True))
/// |> trick.expression_to_string
/// // -> Ok("!True")
/// ```
/// 
pub fn negate_bool(value: Expression(a)) -> Expression(Variable) {
  unary_operator("!", value, type_bool())
}

/// Generates a list of values. The values must all be of the same type.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.list([trick.int(1), trick.int(2), trick.int(3)])
/// |> trick.expression_to_string
/// // -> Ok("[1, 2, 3]")
/// ```
/// 
/// ```gleam
/// trick.list([trick.float(1.0), trick.float(2.0), trick.float(3.0)])
/// |> trick.expression_to_string
/// // -> Ok("[1.0, 2.0, 3.0]")
/// ```
/// 
/// ```gleam
/// trick.list([trick.int(1), trick.float(2.0), trick.float(3.0)])
/// |> trick.expression_to_string
/// // -> Error(TypeMismatch(expected: Int, got: Float))
/// ```
/// 
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
      |> Compiled(type_list(element_type), precedence_unit),
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

/// Generates a `panic` expression, with an optional message. If present, the
/// message must be of type `String`.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.panic_(None) |> trick.expression_to_string
/// // -> Ok("panic")
/// ```
/// 
/// ```gleam
/// trick.panic_(Some(trick.string("uh oh"))) |> trick.expression_to_string
/// // -> Ok("panic as \"uh oh\"")
/// ```
/// 
/// ```gleam
/// trick.panic_(Some(trick.int(42))) |> trick.expression_to_string
/// // -> Error(TypeMismatch(expected: String, got: Int))
/// ```
/// 
pub fn panic_(message: Option(Expression(a))) -> Expression(Variable) {
  use state <- Expression
  let #(state, type_) = next_unbound(state)
  case message {
    None ->
      Ok(#(state, Compiled(doc.from_string("panic"), type_, precedence_unit)))
    Some(message) -> {
      use #(state, message) <- result.try(message.compile(state))
      use #(state, _) <- result.try(unify(state, message.type_, type_string()))
      Ok(#(
        state,
        Compiled(
          add_message(
            doc.from_string("panic"),
            maybe_wrap(message, precedence_unit),
          ),
          type_,
          precedence_unit,
        ),
      ))
    }
  }
}

/// Generates a `todo` expression, with an optional message. If present, the
/// message must be of type `String`.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.todo_(None) |> trick.expression_to_string
/// // -> Ok("todo")
/// ```
/// 
/// ```gleam
/// trick.todo_(Some(trick.string("uh oh"))) |> trick.expression_to_string
/// // -> Ok("todo as \"uh oh\"")
/// ```
/// 
/// ```gleam
/// trick.todo_(Some(trick.int(42))) |> trick.expression_to_string
/// // -> Error(TypeMismatch(expected: String, got: Int))
/// ```
/// 
pub fn todo_(message: Option(Expression(a))) -> Expression(Variable) {
  use state <- Expression
  let #(state, type_) = next_unbound(state)
  case message {
    None ->
      Ok(#(state, Compiled(doc.from_string("todo"), type_, precedence_unit)))
    Some(message) -> {
      use #(state, message) <- result.try(message.compile(state))
      use #(state, _) <- result.try(unify(state, message.type_, type_string()))
      Ok(#(
        state,
        Compiled(
          add_message(
            doc.from_string("todo"),
            maybe_wrap(message, precedence_unit),
          ),
          type_,
          precedence_unit,
        ),
      ))
    }
  }
}

/// Generates an `echo` expression, with an optional message. If present, the
/// message must be of type `String`.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.echo_(trick.int(42), None) |> trick.expression_to_string
/// // -> Ok("echo 42")
/// ```
/// 
/// ```gleam
/// trick.echo_(trick.int(42), Some(trick.string("the answer")))
/// |> trick.expression_to_string
/// // -> Ok("echo 42 as \"the answer\"")
/// ```
/// 
/// ```gleam
/// trick.echo_(trick.int(42), Some(trick.int(42)))
/// |> trick.expression_to_string
/// // -> Error(TypeMismatch(expected: String, got: Int))
/// ```
/// 
pub fn echo_(
  value: Expression(a),
  message: Option(Expression(a)),
) -> Expression(Variable) {
  use state <- Expression
  use #(state, value) <- result.try(value.compile(state))

  let echo_ = doc.prepend(doc.from_string("echo "), to: value.document)

  case message {
    None -> Ok(#(state, Compiled(echo_, value.type_, precedence_unit)))
    Some(message) -> {
      use #(state, message) <- result.try(message.compile(state))
      use #(state, _) <- result.try(unify(state, message.type_, type_string()))
      Ok(#(
        state,
        Compiled(
          add_message(echo_, maybe_wrap(message, precedence_unit)),
          value.type_,
          precedence_unit,
        ),
      ))
    }
  }
}

/// Declares a variable in the current scope. Calls the continuing function with
/// an expression representing the variable name.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.block({
///   use x <- trick.variable("x", trick.int(1))
///   trick.expression(trick.add(x, trick.int(1)))
/// })
/// |> trick.expression_to_string
/// ```
/// 
/// Will generate:
/// ```gleam
/// {
///   let x = 1
///   x + 1
/// }
/// ```
/// 
pub fn variable(
  name: String,
  value: Expression(a),
  continue: fn(Expression(Variable)) -> Statement,
) -> Statement {
  use state <- Statement
  use _ <- result.try(check_name_case(name, SnakeCase))
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
    doc_to_expression(Compiled(
      doc.from_string(name),
      value.type_,
      precedence_unit,
    ))

  let rest = continue(variable_expression)
  use #(state, rest) <- result.try(rest.compile(state))

  Ok(#(
    state,
    Compiled(
      doc.prepend(declaration, to: rest.document) |> doc.force_break,
      rest.type_,
      precedence_unit,
    ),
  ))
}

fn grouped(documents: List(Document)) -> Document {
  documents |> doc.concat |> doc.nest(indent) |> doc.group
}

/// Turns an `Expression` into a `Statement` so it can be used in statement
/// position.
/// 
/// By default, an expression statement ends the block and doesn't allow being
/// followed by another statement. Use [`discard`](#discard) to include a
/// continuation.
/// 
pub fn expression(expression: Expression(a)) -> Statement {
  Statement(expression.compile)
}

/// Discards a terminating statement and allows continuation.
/// 
/// ```gleam
/// trick.block({
///   use <- trick.discard(trick.expression(trick.int(1)))
///   trick.expression(trick.int(2))
/// })
/// |> trick.expression_to_string
/// ```
/// 
/// Will generate:
/// 
/// ```gleam
/// {
///   1
///   2
/// }
/// ```
/// 
/// ```gleam
/// trick.block({
///   use <- trick.discard(trick.assert(trick.bool(True), None))
///   trick.assert(trick.bool(False), None)
/// })
/// |> trick.expression_to_string
/// ```
/// 
/// Will generate:
/// 
/// ```gleam
/// {
///   assert True
///   assert False
/// }
/// ```
/// 
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
      |> Compiled(rest.type_, precedence_unit),
  ))
}

/// Generates a comment.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.block({
///   use <- trick.comment("Pi to 2 decimal places")
///   trick.expression(trick.float(3.14))
/// })
/// |> trick.expression_to_string
/// ```
/// 
/// Will generate:
/// 
/// ```gleam
/// {
///   // Pi to 2 decimal places
///   3.14
/// }
/// ```
/// 
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
      |> Compiled(rest.type_, precedence_unit),
  ))
}

/// Generates a block wrapping one or more statements.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.int(1)
/// |> trick.add(trick.int(2))
/// |> trick.expression
/// |> trick.block
/// |> trick.expression_to_string
/// // -> Ok("{ 1 + 2 }")
/// ```
/// 
/// ```gleam
/// trick.block({
///   use x <- trick.variable("x", trick.int(1))
///   use y <- trick.variable("y", trick.int(2))
///   trick.expression(trick.add(x, y))
/// })
/// |> trick.expression_to_string
/// ```
/// 
/// Will generate:
/// 
/// ```gleam
/// {
///   let x = 1
///   let y = 2
///   x + y
/// }
/// ```
/// 
pub fn block(inner: Statement) -> Expression(Variable) {
  use state <- Expression
  use #(state, inner) <- result.try(inner.compile(state))

  Ok(#(state, Compiled(block_doc(inner.document), inner.type_, precedence_unit)))
}

fn block_doc(inner: Document) -> Document {
  [
    doc.break("{ ", "{"),
    inner,
  ]
  |> doc.concat
  |> doc.nest(indent)
  |> doc.append(doc.break(" ", ""))
  |> doc.append(doc.from_string("}"))
  |> doc.group
}

/// Generates an `assert` statement with an optional message. The condition must
/// be of type `Bool`, and the message, if present, must be of type `String`.
/// 
/// Like [`expression`](#expression), `assert` by default terminates the block
/// and doesn't expect a continuation. To place statements after an `assert`,
/// use [`discard`](#discard).
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.assert_(trick.bool(True), None)
/// |> trick.block
/// |> trick.expression_to_string
/// ```
/// 
/// Will generate:
/// 
/// ```gleam
/// {
///   assert True
/// }
/// ```
/// 
/// ```gleam
/// trick.assert_(trick.bool(False), Some(trick.string("This will panic")))
/// |> trick.block
/// |> trick.expression_to_string
/// ```
/// 
/// Will generate:
/// 
/// ```gleam
/// {
///   assert False as \"This will panic\"
/// }
/// ```
/// 
/// ```gleam
/// trick.assert_(trick.int(1), None)
/// |> trick.block
/// |> trick.expression_to_string
/// // -> Error(TypeMismatch(expected: Bool, got: Int))
/// ```
/// 
/// ```gleam
/// trick.assert_(trick.bool(True), Some(trick.bool(True)))
/// |> trick.block
/// |> trick.expression_to_string
/// // -> Error(TypeMismatch(expected: String, got: Bool))
/// ```
/// 
pub fn assert_(
  condition: Expression(a),
  message: Option(Expression(a)),
) -> Statement {
  use state <- Statement
  use #(state, condition) <- result.try(condition.compile(state))
  use #(state, _) <- result.try(unify(state, condition.type_, type_bool()))

  let assert_ = doc.prepend(doc.from_string("assert "), to: condition.document)

  case message {
    None ->
      Ok(#(
        state,
        Compiled(doc.force_break(assert_), type_nil(), precedence_unit),
      ))
    Some(message) -> {
      use #(state, message) <- result.try(message.compile(state))
      use #(state, _) <- result.try(unify(state, message.type_, type_string()))
      Ok(#(
        state,
        Compiled(
          add_message(assert_, maybe_wrap(message, precedence_unit))
            |> doc.force_break,
          type_nil(),
          precedence_unit,
        ),
      ))
    }
  }
}

/// Generates a tuple from the specified values. The values can be of different
/// types.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.tuple([trick.int(1), trick.float(2.0), trick.string("three")])
/// |> trick.expression_to_string
/// // -> Ok("#(1, 2.0, \"three\")")
/// ```
/// 
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
      |> Compiled(Tuple(types), precedence_unit),
  ))
}

/// Generates a tuple access expression.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.tuple([trick.int(1), trick.float(2.0), trick.string("three")])
/// |> trick.tuple_index(2)
/// |> trick.expression_to_string
/// // -> Ok("#(1, 2.0, \"three\").2")
/// ```
/// 
/// ```gleam
/// trick.tuple([trick.int(1), trick.float(2.0), trick.string("three")])
/// |> trick.tuple_index(4)
/// |> trick.expression_to_string
/// // -> Error(TupleIndexOutOfBounds(length: 3, index: 4))
/// ```
/// 
/// ```gleam
/// trick.list([trick.int(1), trick.int(2)])
/// |> trick.tuple_index(0)
/// |> trick.expression_to_string
/// // -> Error(InvalidTupleAccess(type_: List(Int)))
/// ```
/// 
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
      maybe_wrap(tuple, precedence_unit),
      doc.from_string("."),
      doc.from_string(int.to_string(index)),
    ]
      |> doc.concat
      |> Compiled(type_, precedence_unit),
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

/// Generates a list prepend expression, prepending one or more items.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.list([trick.int(2), trick.int(3)])
/// |> trick.prepend([trick.int(0), trick.int(1)])
/// |> trick.expression_to_string
/// // -> Ok("[0, 1, ..[2, 3]]")
/// ```
/// 
/// ```gleam
/// trick.list([trick.int(2), trick.int(3)])
/// |> trick.prepend([trick.float(0.0), trick.float(1.0)])
/// |> trick.expression_to_string
/// // -> Error(TypeMismatch(expected: Int, got: Float))
/// ```
/// 
/// ```gleam
/// trick.int(2)
/// |> trick.prepend([trick.int(0), trick.int(1)])
/// |> trick.expression_to_string
/// // -> InvalidListPrepend(type_: Int)
/// ```
/// 
pub fn prepend(
  to list: Expression(a),
  prepend elements: List(Expression(a)),
) -> Expression(a) {
  use state <- Expression
  use #(state, list) <- result.try(list.compile(state))
  use element_type <- result.try(case unwrap_type(state, list.type_) {
    Custom(module: "gleam", name: "List", generics: [type_], shared_fields: _) ->
      Ok(type_)
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
      |> Compiled(list.type_, precedence_unit),
  ))
}

/// Indicates that a function does not have labelled arguments and can be turned
/// into an anonymous function.
/// 
pub type Unlabelled

/// Indicates that a function has one or more labelled arguments can cannot be
/// turned into an anonymous function as anonymous functions do not support labels.
/// 
pub type Labelled

/// Information about a function which can either be turned into a function
/// definition or an anonymous function.
/// 
/// Marked as either [`Labelled`](#Labelled) or [`Unlabelled`](#Unlabelled).
/// 
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

/// Generates an anonymous function.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.anonymous({
///   use a <- trick.parameter("a", trick.int_type())
///   use b <- trick.parameter("b", trick.int_type())
///   trick.add(a, b) |> trick.expression |> trick.function_body
/// })
/// |> trick.expression_to_string
/// // -> Ok("fn(a: Int, b: Int) { a + b }")
/// ```
/// 
pub fn anonymous(
  function: FunctionBuilder(Unlabelled),
) -> Expression(Variable) {
  use state <- Expression
  let #(state, return_type) = next_unbound(state)

  use #(state, function) <- result.try(
    function.compile(state, "", return_type, []),
  )
  use #(state, body) <- result.try(function.body.compile(state))

  let #(state, parameters) =
    list.map_fold(function.parameters, state, parameter_to_doc)

  let parameter_list =
    [
      doc.break("(", "("),
      parameters
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
      |> Compiled(type_, precedence_unit),
  ))
}

/// Creates a recursive function by passing in the function name to the body.
/// 
/// Once a function is declared as recursive, no more parameters can be added.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.function("infinity", trick.Private, {
///   use parameter <- trick.parameter("parameter", trick.generic("a"))
///   use infinity <- trick.recursive
///   trick.call(infinity, parameter)
/// }, fn(_) { trick.end_module() })
/// |> trick.to_string
/// ```
/// 
/// Will generate:
/// 
/// ```gleam
/// fn infinity(parameter: a) -> b {
///   infinity(parameter)
/// }
/// ```
/// 
pub fn recursive(
  continue: fn(Expression(Constant)) -> Statement,
) -> FunctionBuilder(Labelled) {
  use state, name, return_type, parameter_types <- FunctionBuilder

  let type_ =
    Function(parameters: parameter_types, return: return_type, field_map: None)

  let expression = instantiated(doc.from_string(name), type_, precedence_unit)

  let body = continue(expression)

  let _ = Ok(#(state, FunctionInformation([], body)))
}

/// Adds an unlabelled parameter to a function definition.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.anonymous({
///   use parameter <- trick.parameter("parameter", trick.type_int())
///   trick.todo_(None) |> trick.expression |> trick.function_body
/// })
/// |> trick.expression_to_string
/// // -> Ok("fn(parameter: Int) { todo }")
/// ```
/// 
pub fn parameter(
  name: String,
  type_: Type,
  continue: fn(Expression(_)) -> FunctionBuilder(a),
) -> FunctionBuilder(a) {
  use state, function_name, function_type, parameters <- FunctionBuilder

  use _ <- result.try(check_name_case(name, SnakeCase))
  use #(state, type_) <- result.try(type_.compile(state))

  let expression = Compiled(doc.from_string(name), type_, precedence_unit)
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

/// Adds a labelled parameter to a function definition.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.function("subtract", trick.Public, {
///   use left <- trick.labelled_parameter("from", "left", trick.type_int())
///   use right <- trick.labelled_parameter("subtract", "right", trick.type_int())
///   trick.subtract(left, right) |> trick.expression |> trick.function_body
/// }, fn(_) { trick.end_module() })
/// |> trick.to_string
/// ```
/// 
/// Will generate:
/// 
/// ```gleam
/// pub fn subtract(from left: Int, subtract right: Int) -> Int {
///   left - right
/// }
/// 
pub fn labelled_parameter(
  label: String,
  name: String,
  type_: Type,
  continue: fn(Expression(_)) -> FunctionBuilder(a),
) -> FunctionBuilder(Labelled) {
  use state, function_name, function_type, parameters <- FunctionBuilder

  use _ <- result.try(check_name_case(name, SnakeCase))
  use _ <- result.try(check_name_case(label, SnakeCase))

  use #(state, type_) <- result.try(type_.compile(state))

  let expression = Compiled(doc.from_string(name), type_, precedence_unit)
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

fn parameter_to_doc(state: State, parameter: Parameter) -> #(State, Document) {
  case parameter.label {
    None -> {
      let #(state, type_) = print_type(state, parameter.type_)
      #(
        state,
        doc.concat([
          doc.from_string(parameter.name),
          doc.from_string(": "),
          doc.from_string(type_),
        ]),
      )
    }
    Some(label) -> {
      let #(state, type_) = print_type(state, parameter.type_)
      #(
        state,
        doc.concat([
          doc.from_string(label),
          doc.from_string(" "),
          doc.from_string(parameter.name),
          doc.from_string(": "),
          doc.from_string(type_),
        ]),
      )
    }
  }
}

/// Marks a statement as the body of a function, concluding the definition.
/// 
/// ### Examples
/// 
/// ```gleam
/// trick.nil()
/// |> trick.expression
/// |> trick.function_body
/// |> trick.anonymous
/// |> trick.expression_to_string
/// // -> Ok("fn() { Nil }")
/// ```
/// 
pub fn function_body(body: Statement) -> FunctionBuilder(Unlabelled) {
  FunctionBuilder(fn(state, _name, _type, _parameters) {
    Ok(#(state, FunctionInformation([], body)))
  })
}

/// Generates a function call with unlabelled arguments. To use labels in the
/// call, see [`labelled_call`](#labelled_call).
/// 
/// ### Example
/// 
/// The following examples assume a function called `add` defined as the following:
/// 
/// ```gleam
/// pub fn add(a: Int, b: Int) -> Int {
///   a + b
/// }
/// ```
/// 
/// The definition has been omitted for brevity. See [`function`](#function) for
/// examples of how to create functions.
/// 
/// ```gleam
/// trick.call(add, [trick.int(1), trick.int(2)]) |> trick.expression_to_string
/// // -> Ok("add(1, 2)")
/// ```
/// 
/// ```gleam
/// trick.call(add, [trick.float(1.0), trick.float(2.0)])
/// |> trick.expression_to_string
/// // -> Error(TypeMismatch(expected: Int, got: Float))
/// ```
/// 
/// ```gleam
/// trick.call(add, [trick.int(1), trick.int(2), trick.int(3)])
/// |> trick.expression_to_string
/// // -> Error(IncorrectNumberOfArguments(expected: 2, got: 3))
/// ```
/// 
/// ```gleam
/// trick.call(trick.int(1), [trick.int(2), trick.int(3)])
/// |> trick.expression_to_string
/// // -> Error(InvalidCall(type_: int))
/// ```
/// 
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
          Ok(#(state, Compiled(doc, return_type, precedence_unit)))
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
      Ok(#(state, Compiled(doc, return_type, precedence_unit)))
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
  |> doc.prepend(maybe_wrap(function, precedence_unit))
  |> doc.append(doc.break("", ","))
  |> doc.append(doc.from_string(")"))
  |> doc.group
}

/// Generates a function capture expression, receiving two lists of arguments.
/// The function hole goes between the two lists.
/// 
/// See also: [`function_capture_alt`](#function_capture_alt) for an alternative API.
/// 
/// ### Examples
/// 
/// ```gleam
/// {
///   use add_5_numbers <- trick.function("add_5_numbers", trick.Private, {
///     use a <- trick.parameter("a", int_type())
///     use b <- trick.parameter("b", int_type())
///     use c <- trick.parameter("c", int_type())
///     use d <- trick.parameter("d", int_type())
///     use e <- trick.parameter("e", int_type())
///     a
///     |> trick.add(b)
///     |> trick.add(c)
///     |> trick.add(d)
///     |> trick.add(e)
///     |> trick.expression
///     |> trick.function_body
///   })
/// 
///   use main <- trick.function("main", trick.Public, trick.function_body(
///     trick.expression(trick.function_capture(
///       add_5_numbers,
///       [trick.int(1), trick.int(2)],
///       [trick.int(4), trick.int(5)],
///     ))
///   ))
/// 
///   trick.end_module()
/// }
/// ```
/// 
/// Will generate:
/// 
/// ```gleam
/// fn add_5_numbers(a: Int, b: Int, c: Int, d: Int, e: Int) -> Int {
///   a + b + c + d + e
/// }
/// 
/// pub fn main() -> fn(Int) -> Int {
///   add_5_numbers(1, 2, _, 4, 5)
/// }
/// ```
/// 
pub fn function_capture(
  function: Expression(_),
  before_hole: List(Expression(_)),
  after_hole: List(Expression(_)),
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
          Ok(#(
            state,
            Compiled(
              capture_doc(function, before, after),
              type_,
              precedence_unit,
            ),
          ))
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
      Ok(#(
        state,
        Compiled(capture_doc(function, before, after), type_, precedence_unit),
      ))
    }
  }
}

/// An argument to a function capture.
/// 
pub type FunctionCaptureArgument {
  CaptureArgument(value: Expression(Variable))
  CaptureHole
}

/// An alternative experimental API to [`function_capture`](#function_capture),
/// structured more like a regular call.
/// 
/// The downside to this approach is that the type system doesn't guarantee that
/// there's exactly one type hole, so we need to report errors for that too.
/// 
/// ### Examples
/// 
/// ```gleam
/// {
///   use add_5_numbers <- trick.function("add_5_numbers", trick.Private, {
///     use a <- trick.parameter("a", int_type())
///     use b <- trick.parameter("b", int_type())
///     use c <- trick.parameter("c", int_type())
///     use d <- trick.parameter("d", int_type())
///     use e <- trick.parameter("e", int_type())
///     a
///     |> trick.add(b)
///     |> trick.add(c)
///     |> trick.add(d)
///     |> trick.add(e)
///     |> trick.expression
///     |> trick.function_body
///   })
/// 
///   use main <- trick.function("main", trick.Public, trick.function_body(
///     trick.expression(trick.function_capture_alt(add_5_numbers, [
///       CaptureArgument(trick.int(1)),
///       CaptureArgument(trick.int(2)),
///       CaptureHole,
///       CaptureArgument(trick.int(4)),
///       CaptureArgument(trick.int(5)),
///     ]))
///   ))
/// 
///   trick.end_module()
/// }
/// ```
/// 
/// Will generate:
/// 
/// ```gleam
/// fn add_5_numbers(a: Int, b: Int, c: Int, d: Int, e: Int) -> Int {
///   a + b + c + d + e
/// }
/// 
/// pub fn main() -> fn(Int) -> Int {
///   add_5_numbers(1, 2, _, 4, 5)
/// }
/// ```
/// 
pub fn function_capture_alt(
  function: Expression(a),
  arguments: List(FunctionCaptureArgument),
) -> Expression(Variable) {
  use state <- Expression

  use #(before_hole, after_hole) <- result.try(split_on_hole(
    arguments,
    [],
    [],
    False,
  ))

  function_capture(function, before_hole, after_hole).compile(state)
}

fn split_on_hole(
  arguments: List(FunctionCaptureArgument),
  before: List(Expression(_)),
  after: List(Expression(_)),
  found_hole: Bool,
) -> Result(#(List(Expression(_)), List(Expression(_))), Error) {
  case arguments {
    [] if found_hole -> Ok(#(list.reverse(before), list.reverse(after)))
    [] -> Error(NoCaptureHole)
    [CaptureHole, ..] if found_hole -> Error(DuplicateCaptureHole)
    [CaptureHole, ..arguments] -> split_on_hole(arguments, before, after, True)
    [CaptureArgument(value:), ..arguments] if found_hole ->
      split_on_hole(arguments, before, [value, ..after], found_hole)
    [CaptureArgument(value:), ..arguments] ->
      split_on_hole(arguments, [value, ..before], after, found_hole)
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
  |> doc.prepend(maybe_wrap(function, precedence_unit))
  |> doc.append(doc.break("", ","))
  |> doc.append(doc.from_string(")"))
  |> doc.group
}

/// Generates a top-level function definition, passing the function name to the
/// continuing function so it can be called later.
/// 
/// ### Examples
/// 
/// ```gleam
/// {
///   use square <- trick.function("square", trick.Private, {
///     use value <- trick.parameter("value", trick.float_type())
///     trick.multiply_float(value, value)
///     |> trick.expression
///     |> trick.function_body
///   })
/// 
///   use circle_area <- trick.function("circle_area", trick.Public, {
///     use radius <- trick.parameter("radius", trick.float_type())
///     trick.call(square, [radius])
///     |> trick.multiple_float(trick.float(3.14))
///     |> trick.expression
///     |> trick.function_body
///   })
/// 
///   use main <- trick.function("main", trick.Public, trick.function_body(
///     trick.expression(
///       trick.echo_(trick.call(circle_area, [trick.float(5.0)]), None)
///     )
///   ))
/// }
/// |> trick.to_string
/// ```
/// 
/// Will generate:
/// 
/// ```gleam
/// fn square(value: Float) -> Float {
///   value *. value
/// }
/// 
/// pub fn circle_area(radius: Float) -> Float {
///   square(radius) *. 3.14
/// }
/// 
/// pub fn main() -> Float {
///   echo circle_area(5.0)
/// }
/// ```
/// 
pub fn function(
  name: String,
  publicity: Publicity,
  function: FunctionBuilder(a),
  continue: fn(Expression(Constant)) -> Module,
) -> Module {
  use state <- Module

  use _ <- result.try(check_name_case(name, SnakeCase))
  let #(state, return_type) = next_unbound(state)

  use #(state, function) <- result.try(
    function.compile(state, name, return_type, []),
  )
  use #(state, body) <- result.try(function.body.compile(state))

  use _ <- result.try(
    list.try_fold(function.parameters, False, fn(found_labelled, parameter) {
      case parameter.label {
        None if found_labelled ->
          Error(UnlabelledParameterAfterLabelledParameter(parameter.name))
        None -> Ok(False)
        Some(_) -> Ok(True)
      }
    }),
  )

  let #(state, parameters) =
    list.map_fold(function.parameters, state, parameter_to_doc)

  let parameter_list =
    [
      doc.break("(", "("),
      parameters
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

  let function_name =
    instantiated(doc.from_string(name), type_, precedence_unit)

  use #(state, rest) <- result.try(continue(function_name).compile(state))

  let #(state, return_annotation) = print_type(state, return_type)

  Ok(#(
    state,
    Definition(
      doc.concat([
        publicity_to_doc(publicity),
        doc.from_string("fn "),
        doc.from_string(name),
        parameter_list,
        doc.from_string(" -> "),
        doc.from_string(return_annotation),
        doc.from_string(" "),
        body_doc,
        separate_definition(rest),
      ]),
    ),
  ))
}

/// Marks the end of a module.
/// 
/// ### Examples
/// 
/// ```gleam
/// {
///   use _ <- trick.constant("pi", trick.Public, trick.float(3.14))
///   trick.end_module()
/// }
/// |> trick.to_string
/// // -> Ok("pub const pi = 3.14")
/// ```
/// 
pub fn end_module() -> Module {
  use state <- Module
  Ok(#(state, Empty))
}

/// Generates a top-level constant from a constant expression, passing the name
/// of the constant to the continuing function allowing it to be used.
/// 
/// ### Examples
/// 
/// ```gleam
/// {
///   use hello <- trick.constant("hello", trick.Private, trick.string("Hello,"))
///   use world <- trick.constant("world", trick.Private, trick.string(" world!"))
///   use hello_world <- trick.constant(
///     "hello_world",
///     trick.Public,
///     trick.concatenate(hello, world),
///   )
///   trick.end_module()
/// }
/// |> trick.to_string
/// ```
/// 
/// Will generate:
/// 
/// ```gleam
/// const hello = "Hello,"
/// const world = " world!"
/// pub const hello_world = hello <> world
/// ```
/// 
pub fn constant(
  name: String,
  publicity: Publicity,
  value: Expression(Constant),
  continue: fn(Expression(Constant)) -> Module,
) -> Module {
  use state <- Module

  use _ <- result.try(check_name_case(name, SnakeCase))
  use #(state, value) <- result.try(value.compile(state))

  let #(state, type_) = generalise(state, value.type_)

  let constant_name =
    instantiated(doc.from_string(name), type_, precedence_unit)

  use #(state, rest) <- result.try(continue(constant_name).compile(state))

  let #(state, annotation) = print_type(state, type_)
  Ok(#(
    state,
    Definition(
      doc.concat([
        publicity_to_doc(publicity),
        doc.from_string("const "),
        doc.from_string(name),
        doc.from_string(": "),
        doc.from_string(annotation),
        doc.from_string(" = "),
        value.document,
        separate_definition(rest),
      ]),
    ),
  ))
}

/// Generates a doc comment in a module.
/// 
/// ### Examples
/// 
/// {
///   use <- trick.doc_comment(
///     "The ultimate answer to life, the universe, and everything."
///   )
///   use _ <- trick.constant("the_answer", trick.Public, trick.int(42))
///   trick.end_module()
/// }
/// 
pub fn doc_comment(comment: String, continue: fn() -> Module) -> Module {
  use state <- Module

  use #(state, rest) <- result.try(continue().compile(state))

  let comment =
    comment
    |> string.split("\n")
    |> list.map(fn(line) { doc.from_string("/// " <> line) })
    |> doc.join(doc.line)

  Ok(#(
    state,
    Definition(
      doc.concat([
        comment,
        doc.line,
        definition_document(rest),
      ]),
    ),
  ))
}

fn definition_document(definition: CompiledModule) -> Document {
  case definition {
    Empty -> doc.empty
    Definition(document) -> document
  }
}

fn print_type(state: State, type_: ConcreteType) -> #(State, String) {
  case unwrap_type(state, type_) {
    Custom(module: _, name:, generics:, shared_fields: _) ->
      case generics {
        [] -> #(state, name)
        _ -> {
          let #(state, generics) = list.map_fold(generics, state, print_type)
          #(state, name <> "(" <> string.join(generics, ", ") <> ")")
        }
      }
    Function(parameters:, return:, field_map: _) -> {
      let #(state, parameters) = list.map_fold(parameters, state, print_type)
      let #(state, return) = print_type(state, return)
      #(state, "fn(" <> string.join(parameters, ", ") <> ") -> " <> return)
    }
    Tuple(elements:) -> {
      let #(state, elements) = list.map_fold(elements, state, print_type)
      #(state, "#(" <> string.join(elements, ", ") <> ")")
    }
    Unbound(id:) | Generic(id:) ->
      case dict.get(state.type_variable_names, id) {
        Ok(name) -> #(state, name)
        Error(_) -> {
          let #(name, number) =
            find_appropriate_type_variable_name(
              state,
              state.type_variable_number,
            )
          #(
            State(
              ..state,
              type_variable_number: number,
              type_variable_names: dict.insert(
                state.type_variable_names,
                id,
                name,
              ),
              used_type_variable_names: set.insert(
                state.used_type_variable_names,
                name,
              ),
            ),
            name,
          )
        }
      }
  }
}

fn find_appropriate_type_variable_name(
  state: State,
  type_variable_number: Int,
) -> #(String, Int) {
  let possible_name = generate_type_variable_name(type_variable_number)
  case set.contains(state.used_type_variable_names, possible_name) {
    True -> find_appropriate_type_variable_name(state, type_variable_number + 1)
    False -> #(possible_name, type_variable_number)
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

/// A function argument with an optional label.
/// 
pub type Argument {
  Argument(label: Option(String), value: Expression(Variable))
}

type CompiledArgument {
  CompiledArgument(label: Option(String), value: Compiled)
}

/// Generates a function call, allowing you to specify labelled arguments. For
/// a call with no labelled arguments, it's more convenient to simply use
/// [`call`](#call).
/// 
/// ### Examples
/// 
/// ```gleam
/// {
///   use function_with_labels <- trick.function(
///     "function_with_labels",
///     trick.Private,
///     {
///       use _ <- trick.parameter("unlabelled", trick.int_type())
///       use _ <- trick.labelled_parameter("label", "name", trick.float_type())
///       use _ <- trick.labelled_parameter(
///         "other_label",
///         "different_name",
///         trick.bool_type(),
///       )
///       trick.todo_(None) |> trick.expression |> trick.function_body
///     },
///   )
/// 
///   use main <- trick.function("main", trick.Public, trick.function_body(
///     trick.expression(trick.labelled_call(function_with_labels, [
///       trick.Argument(None, trick.int(42)),
///       trick.Argument(Some("other_label"), trick.bool(False)),
///       trick.Argument(Some("label"), trick.float(3.14)),
///     ]))
///   ))
/// 
///   trick.end_module()
/// }
/// |> trick.to_string
/// ```
/// 
/// Will generate:
/// 
/// ```gleam
/// fn function_with_labels(
///   unlabelled: Int,
///   label name: Float,
///   other_label different_name: Bool,
/// ) -> a {
///   todo
/// }
/// 
/// pub fn main() -> a {
///   function_with_labels(42, other_label: False, label: 3.14)
/// }
/// ```
/// 
pub fn labelled_call(
  function: Expression(a),
  arguments: List(Argument),
) -> Expression(Variable) {
  use state <- Expression
  use #(state, function) <- result.try(function.compile(state))
  use #(state, arguments) <- result.try(
    try_fold_with_state(state, arguments, [], fn(state, arguments, argument) {
      use _ <- result.try(case argument.label {
        Some(label) -> check_name_case(label, SnakeCase)
        None -> Ok(Nil)
      })
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
            Compiled(
              labelled_call_doc(arguments, function),
              return_type,
              precedence_unit,
            ),
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

      Ok(#(
        state,
        Compiled(
          labelled_call_doc(arguments, function),
          return_type,
          precedence_unit,
        ),
      ))
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
  |> doc.prepend(maybe_wrap(function, precedence_unit))
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

/// Indicates that a custom type has no type parameters.
/// 
pub type NoParameters

/// Indicates that a custom type has one or more type parameters.
/// 
pub type HasParameters

/// Information about a custom type.
/// 
pub opaque type CustomType(a) {
  CustomType(
    compile: fn(State, CustomTypeHead) ->
      Result(#(State, CustomTypeInfo), Error),
  )
}

type CustomTypeInfo {
  CustomTypeInfo(
    name: String,
    type_: ConcreteType,
    constructors: List(Constructor),
    parameters: List(#(String, ConcreteType)),
    rest: Module,
  )
}

type CustomTypeHead {
  CustomTypeHead(
    name: String,
    parameters: List(#(String, ConcreteType)),
    type_: ConcreteType,
    constructors: List(Constructor),
  )
}

type Constructor {
  Constructor(name: String, fields: List(CompiledField))
}

/// The field of a custom type variant.
/// 
pub type Field {
  Field(label: Option(String), type_: Type)
}

type CompiledField {
  CompiledField(label: Option(String), type_: ConcreteType)
}

/// Begins a custom type declaration, passing the type to the continuing function
/// so it can be used in constructors as a recursive definition, or in later
/// functions and types.
/// 
/// ### Examples
/// 
/// ```gleam
/// {
///   use list <- trick.custom_type("List", trick.Public)
///   use a <- trick.type_parameter("a")
///   use empty <- trick.constructor("Empty", [])
///   use non_empty <- trick.constructor("NonEmpty", [
///     trick.Field(Some("head"), a),
///     trick.Field(Some("tail"), list),
///   ])
///   use <- trick.end_custom_type
/// 
///   trick.end_module()
/// }
/// |> trick.to_string
/// ```
/// 
/// Will generate:
/// 
/// ```gleam
/// pub type List(a) {
///   Empty
///   NonEmpty(head: a, tail: List(a))
/// }
/// ```
/// 
pub fn custom_type(
  name: String,
  publicity: Publicity,
  continue: fn(Type) -> CustomType(a),
) -> Module {
  use state <- Module

  use _ <- result.try(check_name_case(name, PascalCase))
  let #(state, unbound) = next_unbound(state)

  let info =
    CustomTypeHead(name, parameters: [], type_: unbound, constructors: [])

  let custom_type = continue(concrete(unbound))
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
      Definition(
        doc.concat([
          doc.from_string("type "),
          doc.from_string(custom_type.name),
          parameters,
          separate_definition(rest),
        ]),
      ),
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
                  let #(state, annotation) = print_type(state, field.type_)
                  #(state, doc.from_string(annotation))
                }
                Some(label) -> {
                  let #(state, annotation) = print_type(state, field.type_)
                  #(
                    state,
                    [
                      doc.from_string(label),
                      doc.from_string(": "),
                      doc.from_string(annotation),
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
    Definition(
      doc.concat([
        publicity_to_doc(publicity),
        doc.from_string("type "),
        doc.from_string(custom_type.name),
        parameters,
        doc.from_string(" {"),
        constructors,
        doc.line,
        doc.from_string("}"),
        separate_definition(rest),
      ]),
    ),
  ))
}

/// Generates a constructor for a custom types.
/// 
/// ### Examples
/// 
/// ```gleam
/// {
///   use wibble_type <- trick.custom_type("Wibble", trick.Public)
///   use wibble <- trick.constructor("Wibble", [
///     trick.Field(None, trick.int_type()),
///     trick.Field(None, trick.float_type()),
///     trick.Field(Some("a_label"), trick.string_type()),
///     trick.Field(Some("another_label"), trick.bool_type()),
///   ])
///   use <- trick.end_custom_type
/// 
///   trick.end_module()
/// }
/// |> trick.to_string
/// ```
/// 
/// Will generate:
/// 
/// ```gleam
/// pub type Wibble {
///   Wibble(Int, Float, a_label: String, another_label: Bool)
/// }
/// ```
/// 
pub fn constructor(
  name: String,
  fields: List(Field),
  continue: fn(Expression(Constant)) -> CustomType(NoParameters),
) -> CustomType(NoParameters) {
  use state, info <- CustomType

  use _ <- result.try(check_name_case(name, PascalCase))
  use #(field_map_fields, arity) <- result.try(
    list.try_fold(fields, #(dict.new(), 0), fn(pair, parameter) {
      let #(map, index) = pair
      case parameter.label {
        None -> Ok(#(map, index + 1))
        Some(label) -> {
          use _ <- result.try(check_name_case(label, SnakeCase))
          case dict.get(map, label) {
            Error(_) -> Ok(#(dict.insert(map, label, index), index + 1))
            Ok(_) -> Error(DuplicateLabel(label:))
          }
        }
      }
    }),
  )

  let field_map = FieldMap(arity:, fields: field_map_fields)
  use #(state, parameter_types) <- result.try(
    try_map_fold(fields, state, fn(state, field) { field.type_.compile(state) }),
  )

  let constructor_type = case fields {
    [] -> info.type_
    _ ->
      Function(
        parameters: parameter_types,
        return: info.type_,
        field_map: Some(field_map),
      )
  }

  let expression =
    instantiated(doc.from_string(name), constructor_type, precedence_unit)

  let custom_type = continue(expression)

  use #(state, fields) <- result.try(
    try_map_fold(fields, state, fn(state, field) {
      case field.type_.compile(state) {
        Ok(#(state, type_)) ->
          Ok(#(state, CompiledField(label: field.label, type_:)))
        Error(error) -> Error(error)
      }
    }),
  )

  use #(state, info) <- result.try(custom_type.compile(
    state,
    CustomTypeHead(..info, constructors: [
      Constructor(name:, fields:),
      ..info.constructors
    ]),
  ))

  Ok(#(
    state,
    CustomTypeInfo(..info, constructors: [
      Constructor(name:, fields:),
      ..info.constructors
    ]),
  ))
}

fn concrete(type_: ConcreteType) -> Type {
  Type(fn(state) { Ok(#(state, type_)) })
}

/// Adds a type parameter to a custom type.
/// 
/// ### Examples
/// 
/// ```gleam
/// {
///   use dict <- trick.custom_type("Dict", trick.Public)
///   use key <- trick.type_parameter("key")
///   use value <- trick.type_parameter("value")
///   use <- trick.end_custom_type
/// 
///   trick.end_module()
/// }
/// |> trick.to_string
/// // -> Ok("pub type Dict(key, value)")
/// ```
/// 
/// ```gleam
/// {
///   use box_type <- trick.custom_type("Box", trick.Public)
///   use value <- trick.type_parameter("value")
///   use box <- trick.constructor("Box", [trick.Field(None, value)])
///   use <- trick.end_custom_type
///   trick.end_module()
/// }
/// |> trick.to_string
/// ```
/// 
/// Will generate:
/// 
/// ```gleam
/// pub type Box(value) {
///   Box(value)
/// }
/// ```
/// 
pub fn type_parameter(
  name: String,
  continue: fn(Type) -> CustomType(a),
) -> CustomType(HasParameters) {
  use state, info <- CustomType

  use _ <- result.try(check_name_case(name, SnakeCase))
  let #(state, type_) = named_generic(state, name)

  let info =
    CustomTypeHead(
      ..info,
      parameters: list.append(info.parameters, [#(name, type_)]),
    )

  continue(concrete(type_)).compile(state, info)
}

/// Marks the end of a custom type definition.
/// 
/// ### Examples
/// 
/// ```gleam
/// {
///   use box_type <- trick.custom_type("Box", trick.Public)
///   use value <- trick.type_parameter("value")
///   use box <- trick.constructor("Box", [trick.Field(None, value)])
///   use <- trick.end_custom_type
///   trick.end_module()
/// }
/// |> trick.to_string
/// ```
/// 
/// Will generate:
/// 
/// ```gleam
/// pub type Box(value) {
///   Box(value)
/// }
/// ```
/// 
pub fn end_custom_type(continue: fn() -> Module) -> CustomType(a) {
  use state, info <- CustomType

  let type_ =
    Custom(
      module: state.module,
      name: info.name,
      generics: list.map(info.parameters, pair.second),
      shared_fields: find_shared_fields(state, info.constructors),
    )
  use #(state, _) <- result.try(unify(state, info.type_, with: type_))

  let rest = continue()

  Ok(#(
    state,
    CustomTypeInfo(
      name: info.name,
      type_:,
      constructors: [],
      parameters: info.parameters,
      rest:,
    ),
  ))
}

fn find_shared_fields(
  state,
  constructors: List(Constructor),
) -> Dict(String, ConcreteType) {
  case constructors {
    [] -> dict.new()
    [constructor] ->
      list.fold(constructor.fields, dict.new(), fn(fields, field) {
        case field.label {
          None -> fields
          Some(label) -> dict.insert(fields, label, field.type_)
        }
      })
    [first, ..rest] ->
      find_shared_fields_loop(
        state,
        rest,
        list.index_fold(first.fields, dict.new(), fn(fields, field, index) {
          case field.label {
            None -> fields
            Some(label) -> dict.insert(fields, label, #(index, field.type_))
          }
        }),
      )
  }
}

fn map_or(result: Result(a, e), or: b, f: fn(a) -> b) -> b {
  case result {
    Ok(value) -> f(value)
    Error(_) -> or
  }
}

fn option_map_or(option: Option(a), or: b, f: fn(a) -> b) -> b {
  case option {
    Some(value) -> f(value)
    None -> or
  }
}

fn find_shared_fields_loop(
  state: State,
  constructors: List(Constructor),
  fields: Dict(String, #(Int, ConcreteType)),
) -> Dict(String, ConcreteType) {
  case constructors {
    [] -> dict.map_values(fields, fn(_, pair) { pair.1 })
    [first, ..rest] ->
      list.index_fold(first.fields, dict.new(), fn(shared, field, index) {
        use label <- option_map_or(field.label, shared)
        use #(shared_index, shared_type) <- map_or(
          dict.get(fields, label),
          shared,
        )
        case
          index == shared_index && same_type(state, shared_type, field.type_)
        {
          True -> dict.insert(shared, label, #(shared_index, shared_type))
          False -> shared
        }
      })
      |> find_shared_fields_loop(state, rest, _)
  }
}

fn same_type(state: State, a: ConcreteType, b: ConcreteType) -> Bool {
  let a = unwrap_type(state, a)
  let b = unwrap_type(state, b)
  case a, b {
    Custom(module: m1, name: n1, generics: g1, shared_fields: _),
      Custom(module: m2, name: n2, generics: g2, shared_fields: _)
    ->
      m1 == m2
      && n1 == n2
      && case list.strict_zip(g1, g2) {
        Error(_) -> False
        Ok(zipped) ->
          list.all(zipped, fn(pair) { same_type(state, pair.0, pair.1) })
      }
    Generic(id: id1), Generic(id: id2) | Unbound(id: id1), Unbound(id: id2) ->
      id1 == id2
    Tuple(elements: e1), Tuple(elements: e2) ->
      case list.strict_zip(e1, e2) {
        Error(_) -> False
        Ok(zipped) ->
          list.all(zipped, fn(pair) { same_type(state, pair.0, pair.1) })
      }
    Function(parameters: p1, return: r1, field_map: _),
      Function(parameters: p2, return: r2, field_map: _)
    ->
      same_type(state, r1, r2)
      && case list.strict_zip(p1, p2) {
        Error(_) -> False
        Ok(zipped) ->
          list.all(zipped, fn(pair) { same_type(state, pair.0, pair.1) })
      }
    Custom(..), _
    | Generic(..), _
    | Unbound(..), _
    | Tuple(..), _
    | Function(..), _
    -> False
  }
}

/// Generates a field access expression.
/// 
/// ### Examples
/// 
/// ```gleam
/// {
///   use person <- trick.custom_type("Person", trick.public)
///   use child <- trick.constructor("Child", [
///     trick.Field(Some("age"), trick.int_type()),
///   ])
///   use adult <- trick.constructor("Adult", [
///     trick.Field(Some("age"), trick.int_type()),
///     trick.Field(Some("job"), trick.string_type()),
///   ])
///   use <- trick.end_custom_type
/// 
///   use age_after_birthday <- trick.function("age_after_birthday", trick.Public, {
///     use person <- trick.parameter("person", person)
///     person
///     |> trick.field_access("age")
///     |> trick.add(trick.int(1))
///     |> trick.expression
///     |> trick.function_body
///   })
/// 
///   trick.end_module()
/// }
/// |> trick.to_string
/// ```
/// 
/// Will generate:
/// 
/// ```gleam
/// pub type Person {
///   Child(age: Int)
///   Adult(age: Int, job: String)
/// }
/// 
/// pub fn age_after_birthday(person: Person) -> Int {
///   person.age + 1
/// }
/// ```
/// 
pub fn field_access(
  value: Expression(_),
  field: String,
) -> Expression(Variable) {
  use state <- Expression
  use #(state, value) <- result.try(value.compile(state))

  case unwrap_type(state, value.type_) {
    Generic(..) as type_
    | Unbound(..) as type_
    | Tuple(..) as type_
    | Function(..) as type_ -> Error(InvalidFieldAccess(type_))
    Custom(shared_fields:, ..) as type_ ->
      case dict.get(shared_fields, field) {
        Ok(type_) -> {
          Ok(#(
            state,
            Compiled(
              doc.concat([
                maybe_wrap(value, precedence_unit),
                doc.from_string("."),
                doc.from_string(field),
              ]),
              type_,
              precedence_unit,
            ),
          ))
        }
        Error(_) -> Error(TypeDoesNotHaveField(type_:, field:))
      }
  }
}
