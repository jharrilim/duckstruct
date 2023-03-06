# Duckstruct

Duckstruct is a language that aims to bring something new to the table: "duck
constraints" on function parameters. Duckstruct is able to infer what properties
an object should respond to when passed into a function. For example, if you
have this function in Duckstruct:

```
f slope(coords) {
  coords.x / coords.y
}
```

Duckstruct is able to check at compile time that any object passed into the
`slope` function must have an "x" property and a "y" property. This effectively
allows your functions to be strongly typed in a structural manner, without the
need for concrete types.

Duckstruct also considers all values known at compile-time to be concrete types.
For example, the type for a string, `"a"`, is actually `"a"` and not `string`.
This allows Duckstruct to unify these value types at compile-time for the sake
of optimization, but also (ideally, in the future) to support IDE type hints
that can show both the value at any given point.

## Thoughts

- Type inference should be fairly similar to a regular
[Hindley Milner Type System](https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system),
except instead of doing just type inference at a nominal level, Duckstruct will
also include values as types. Because of this, inference also has the effect of
evaluating what actual values will be. This can be used for some easy
compile-time optimizations.

- During type checking, we can build a graph which maps a computed value to each
expression, starting from the innermost expressions. This will give us a way to
determine what each intermediate value is, which is particularly useful for code
analysis, ie. IDE inlay hints. We can also use this graph to simplify how we
compute the output. During evaluation traversal, we only need to traverse down
to the highest computed value, and we can skip any nodes beneath that.

- It will be easy for side effects to pollute type inference. The compiler
should provide a way to guard against this using a form of type-guard similar to
that of TypeScript's. Side effects meaning things that come from outside of the
program. Those things we can't determine a "value" type for at compile time.

- In C++, programmers [constexpr](https://en.cppreference.com/w/cpp/language/constexpr)
all-the-things (ideally). We are essentially doing the same thing but for a
language without static types, and we're applying it ourselves in the compiler
instead of having developers write `constexpr`.

- Turns out, there's a language called Idris which
[seems to explore similar concepts to Duckstruct](http://docs.idris-lang.org/en/latest/tutorial/typesfuns.html#totality).
They deal with something they call totality, which from what I can tell, is
the handling of bounded/unbounded functions/loops.

- Not having concrete types has the downside of not providing a clear way on how
to document function parameters. A potential aide in that respect might be
a language service which can generate or update a comment with some type stubs
based on the inferred type.

- The concepts explored here should be particularly useful to linters in dynamic
languages such as Python or Javascript.
