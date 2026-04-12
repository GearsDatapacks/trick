# trick

[![Package Version](https://img.shields.io/hexpm/v/trick)](https://hex.pm/packages/trick)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/trick/)

`trick` is a code generation library for Gleam, based on
[`elm-codegen`](https://package.elm-lang.org/packages/mdgriffith/elm-codegen/latest/Elm).

It's designed to assist any library or application which needs to generate Gleam
code. The generated code is fully type-checked, so you can be sure that it will
compile correctly once generated.

`trick` is currently in development and not yet published on Hex. You can add
it as a git dependency in your `gleam.toml`:

```toml
[dependencies]
trick = { git = "https://github.com/GearsDatapacks/trick.git", ref = "main" }
```

Then, start generating code:

```gleam
import trick
import simplifile

pub fn main() {
  let assert Ok(code) = trick.to_string({
    use pi <- trick.constant("pi", trick.Private, trick.float(3.14))
    use circle_area <- trick.function("circle_area", trick.Public, {
      use radius <- trick.parameter("radius", trick.float_type())
      trick.function_body({
        use radius_squared <- trick.variable(
          "radius_squared",
          trick.multiply_float(radius, radius),
        )
        trick.expression(trick.multiply_float(radius_squared), pi)
      })
    })
    trick.end_module()
  })

  let assert Ok(Nil) = simplifile.write("out.gleam", code)
}
```

Until publication on Hex, documentation can be found at <https://gearsco.de/trick>.
