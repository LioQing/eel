# Eel

Eel is a toy functional language inspired by C++ template metaprogramming.
Applying a function with `f(x)` gives you a value Eel calls a *struct*. Add
a trailing dot, `f(x).`, to evaluate the function body and get its result.

## Try it

Start the REPL with `cargo run` from the repository root:

```text
eel> let double = fn(n) { n * 2 }
eel> double(21)
double(n=21) { (n * 2) }
eel> double(21).
42
```

To run a file, pass its path after `--`:

```bash
cargo run -- examples/cons.eel
```

The file runner uses the final value as the exit code. It accepts integers
from `0` to `255` and treats `unit` as `0`. For any other value, it reports
an error and exits with code `1`. Use `Io(print(value).)` to print a value
and exit with code `0`.

The REPL prints each expression result and saves it as `it`. A standalone
`let` or `let { ... }` adds immutable bindings without changing `it`. To
enter an expression or declaration across multiple lines, put `:{` and `:}`
on their own lines around it. Type `:help` for commands or `:quit` (`:q`)
to exit. The REPL also accepts piped input without printing prompts.

## Structs and pattern matching

Eel evaluates the arguments when you apply a function, then stores their
values in the struct. A `case` pattern like `cons(x, xs)` looks up `cons` in
the scope of the `case` and matches structs made by that function definition.
An alias matches too, but a new definition with the same name does not.
The pattern unpacks the arguments without evaluating the body. That's how the same `fn`
declaration can work as a function or as a data constructor.

```text
let cons = fn(x, xs) unit
let nil = fn() unit

let length = fn(xs) {
    case (xs) {
        nil() => 0
        cons(_, tail) => { 1 + length(tail). }
    }
}

length(cons(1, cons(2, nil()))).
// 2
```

Here `cons` and `nil` are list constructors. Their bodies return `unit`,
but the list code never evaluates them. The full
[list example](examples/cons.eel) also defines `map`, which takes a function
as an argument and uses `f(x).` to compute each new element.

## Where the idea came from

I wrote an [arithmetic expression evaluator with variable substitution][calc]
in C++ templates for fun. I wanted to try the same split between supplying
arguments and getting a result in a language of its own.

This is similar to `thunk` and `force` in call-by-push-value (CBPV):
`f(x)` suspends the function body, and `f(x).` runs it.

The rough C++ equivalents of `f(x)` and `f(x).` are `f<X>` and `f<X>::Eval`.
In the [C++ version of the list example](examples/cons.cpp), template
specializations do the work of the `case` arms. Eel runs in an interpreter
and checks types at runtime.

## I/O actions

`print(x).` builds a plan to print `x`. To run the plan, return it from a
file or REPL expression wrapped in `Io(...)`:

```text
Io(pure(6). bind print)
// Prints 6 and returns unit.
```

`pure(x).` builds a plan that returns `x` without doing I/O. When run, plans
from `print(x).` and `eprint(x).` write to standard output and standard
error, respectively, and return `unit`.

`first bind next` builds a plan that runs `first`, then applies and
evaluates the unary function `next` with the result. `next` must return
a plan without an `Io` wrapper. `bind` groups to the left by default, so
you can chain several actions.

The runner recognizes the built-in plan constructors `Pure`, `Print`, `Eprint`,
and `Bind`, even through aliases. It also accepts user-defined structs with
those names and reads their arguments by position. It reports malformed plans
when it reaches them.

The runner leaves `Io` structs inside other values or in standalone REPL
bindings alone. When a plan runs, its result becomes the file's exit code
or the REPL's `it`. If the plan returns another `Io` struct, the REPL prints
it as a value and the file runner rejects it as an invalid exit code.

## Operators

`let opl 1 sub = fn(a, b) { a - b }` declares a left-associative operator at
precedence `1`. `10 sub 3 sub 2` groups as `(10 sub 3) sub 2` and gives `5`.
With `opr`, it groups as `10 sub (3 sub 2)` and gives `9`.

Infix notation applies the function and evaluates its body in one step.
In call notation, the same operation takes a dot: `sub(10, 3).`.

Higher precedence numbers bind more tightly. Function application and the
trailing dot bind more tightly than infix operators. Use parentheses to
override the grouping.

Built-in operators are left-associative:

| Precedence | Operators |
| --- | --- |
| 4 | `*`, `/`, `%` |
| 3 | `+`, `-` |
| 2 | `<`, `<=`, `>`, `>=` |
| 1 | `==`, `!=` |

Other operators default to precedence `0` and left associativity. When two
operators have the same precedence, the second operator's associativity
decides the grouping. Precedence and associativity follow the binding's
scope, including in recursive function bodies. A new binding without `opl`
or `opr` gets the default fixity.

### Integer arithmetic

Eel uses signed 64-bit integers and does not implicitly mix integers and
floats. Out-of-range integer literals, overflow in `+`, `-`, `*`, `/`, or
`%`, and integer division or remainder by zero are Eel errors in both
debug and release builds. The overflow rule includes both
`-9223372036854775808 / -1` and `-9223372036854775808 % -1`.

## Recursive function groups

Use `let { ... }` when functions need to call each other. Each member can
call itself or any other member, regardless of declaration order.

```text
let {
    odd = fn(n) { case n { 0 => 0 _ => even(n - 1). } }
    even = fn(n) { case n { 0 => 1 _ => odd(n - 1). } }
}
even(4).
// 1
```

A group contains one or more `fn` definitions. Use distinct names within
the group, with no separators between members. The names can shadow outer
bindings, and a function parameter can shadow a group member in its own
body. A member's `opl` or `opr` declaration applies throughout the group
and in the expression that follows it.

See [`fib.eel`](examples/fib.eel) and the other files in [examples/](examples/).

[calc]: https://github.com/LioQing/pdwb/blob/7ac3a0958edebb3145c789f05b35ef2825f34ff5/ftcpp_calc/main.cpp#L512-L549
