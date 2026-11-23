# Eel

A toy programming language leveraging explicit evaluation to allow unifying the declaration of functions and types.

## Motivation

This language has been on my list of projects to create for a long time.

It was inspired by C++ template metaprogramming, where [class templates](https://en.cppreference.com/w/cpp/language/class_template.html), [classes](https://en.cppreference.com/w/cpp/language/classes.html), [using-declarations](https://en.cppreference.com/w/cpp/language/using_declaration.html), and [template specializations](https://en.cppreference.com/w/cpp/language/template_specialization.html) are used to create a program that can be evaluated at compile time. I made an [arithmetic expression evaluator in C++ template metaprogramming with variable substitution](https://github.com/LioQing/pdwb/blob/7ac3a0958edebb3145c789f05b35ef2825f34ff5/ftcpp_calc/main.cpp#L512-L549) for fun and was fascinated by it.

## Overview

Eel, being the language inspired by C++ template metaprogramming, is a language that works in essentially the same way. In fact, it should almost be able to transpile to valid C++ template metaprogramming code, the only difference is that it is dynamically typed while C++ template metaprogramming is 'statically [kinded](https://en.wikipedia.org/wiki/Kind_(type_theory))'.

The explicitness of Eel is in that it requires programmer to explicitly evaluate functions that already have their arguments applied (called a struct in Eel) to get the return value. This essentially **unifies the declaration of functions and types, and makes the function's identifier also a type identifier, allowing pattern matching on the function's identifier**.

| Eel | C++ |
| --- | --- |
| function | class template/partially specialized class |
| struct | fully specialized class |
| value | primitive type value (e.g., `int`, `float`, etc.) |
| function definition | using-declaration |
| function application | template argument substitution |
| struct evaluation | static member access |

For examples:

|     | Eel | C++ |
| --- | --- | --- |
| function | `let apply = fn(x, f) { f(x). }` | `template <typename X, template <typename> typename F> struct apply { using Eval = typename F<X>::Eval; };` |
| struct | `let x = y` | `using x = y;` |
| value | `let x = 1` | `constexpr int x = 1;` |
| function definition | `let f = fn(x) { x }` | `template <typename X> struct f { using Eval = X; };` |
| function application | `f(x)` | `f<X>` |
| struct evaluation | `x.` | `X::Eval` |

> [!NOTE]
>
> The explanation above may still be a bit ambiguous, since I am also in the process of learning how to express these concepts.

## Features

- **Explicit Evaluation**: Functions can be evaluated to structs, and structs can be evaluated to values.
- **Functional**: Supports first-class functions and higher-order functions without mutable bindings. Evaluation builds I/O actions as values without running them.
- **Dynamically Typed**: Types are checked at runtime, allowing for flexibility in function definitions and calls.

### Integer Arithmetic

Integers are signed 64-bit values, from `-9223372036854775808` to `9223372036854775807`. Out-of-range integer literals and overflow in integer `+`, `-`, `*`, `/`, and `%` produce language errors in both debug and release builds, rather than panicking or wrapping. Integer division and remainder by zero are errors, as are `-9223372036854775808 / -1` and `-9223372036854775808 % -1`. Arithmetic does not implicitly mix integers and floats.

### Operators

`let opl 1 sub = fn(a, b) { a - b }` declares a left-associative operator:
`10 sub 3 sub 2` is `(10 sub 3) sub 2`, or `5`. Use `opr` for right
associativity, which makes the same chain `10 sub (3 sub 2)`, or `9`.
Higher numeric precedence binds more tightly. Parentheses override both rules,
and function application and explicit evaluation bind more tightly than operators.

Builtin operators are left-associative, with these precedences:

| Precedence | Operators |
| --- | --- |
| 4 | `*`, `/`, `%` |
| 3 | `+`, `-` |
| 2 | `<`, `<=`, `>`, `>=` |
| 1 | `==`, `!=` |

Other operators default to left associativity and precedence `0`. When operators
of equal precedence have different associativities, the following operator's
associativity decides the grouping. Fixity follows lexical bindings, including
recursive function bodies, and inner declarations do not affect outer or sibling
expressions. A new binding without `opl` or `opr` uses the default fixity.

### Recursive Function Groups

Use `let { ... }` to define mutually recursive functions. Every function in the
group can call itself or any other member, regardless of declaration order.

```js
let {
    odd = fn(n) { case n { 0 => 0 _ => even(n - 1). } }
    even = fn(n) { case n { 0 => 1 _ => odd(n - 1). } }
}
even(4).
// 1
```

A group must contain at least one `fn` definition. Members need no separators,
and their names must be distinct within the group. They can shadow outer names,
and a parameter can shadow a group member in its own function body. As with
plain `let`, the expression after the group can use its bindings.

Each member can declare its own fixity, for example
`opr 2 sub = fn(a, b) { a - b }` inside the braces. A member's fixity is visible
throughout the group and in the following expression.

### I/O Actions

Return an `Io(plan)` struct from a file or a REPL expression to run `plan`:

```js
Io((fn(n) { pure(n + 1). })(5). bind print)
// Prints 6 and returns unit.
```

The dot only evaluates an applied function. `pure(x).` builds a plan that
returns `x` without I/O. `print(x).` and `eprint(x).` build plans that write `x`
to standard output and standard error, respectively, and return `unit`.
Eel evaluates function arguments as usual when building plans.

`first bind next` builds a plan that runs `first`, then calls and evaluates
the unary function `next` with that result. `next` must return another plan,
not an `Io` wrapper. `bind` is left-associative by default, so several actions
can be chained. The plans are ordinary structs named `Pure`, `Print`, `Eprint`,
and `Bind`. The runner uses their names and positional arguments, not their
bodies. Malformed plans are runtime errors when reached.

Only a struct named `Io` returned directly by a file or REPL expression runs
its plan. A discarded plan, an `Io` stored by a REPL `let`, or an `Io` nested
inside another value does not run. An `Io` produced by running a plan is
displayed as a result value, not run again.

## Example

Here is a simple [cons](https://en.wikipedia.org/wiki/Cons) implementation in Eel:

```js
let cons = fn(x, xs) { unit }
let nil = fn() { unit }

let length = fn(xs) {
    case (xs) {
        nil() => 0
        cons(_, xs) => { 1 + length(xs). }
    }
}

let map = fn(xs, f) {
    case (xs) {
        nil() => nil()
        cons(x, xs) => cons(f(x)., map(xs, f).)
    }
}

let list = cons(1, cons(2, cons(3, nil())))
let doubled_list = map(list, fn(x) { x * 2 }).
let length_of_list = length(list).

Io(
    print(list).
// cons (x=1, xs=cons (x=2, xs=cons (x=3, xs=nil() { unit }) { unit }) { unit }) { unit }
    bind fn(_) { print(doubled_list). }
// cons (x=2, xs=cons (x=4, xs=cons (x=6, xs=nil() { unit }) { unit }) { unit }) { unit }
    bind fn(_) { print(length_of_list). }
// 3
    bind fn(_) { pure(0). }
)
```

See more examples in the [examples](examples/) directory.

### Cons Example in C++ Template Metaprogramming

The above example can be translated to C++ template metaprogramming as follows ([cons.cpp](examples/cons.cpp)):

```cpp
#include <string>

// C++ template metaprogramming is 'statically kinded',
// and it is better to represent `*` a type as a struct
// because primitive types like `int` are treated as values instead,
// so here we define a struct to represent an Int type.
template <int V>
struct Int {
    // A printing function so that we can see the value.
    static const std::string to_string() {
        return std::to_string(V);
    }
};

// We need a way to carry out arithmetic operations on the Int type.
template <typename A, typename B>
struct Add { using Eval = void; /* This is the exhausted case. */ };

template <int A, int B>
struct Add<Int<A>, Int<B>> { using Eval = Int<A + B>; };

template <typename A, typename B>
struct Multiply { using Eval = void; };

template <int A, int B>
struct Multiply<Int<A>, Int<B>> { using Eval = Int<A * B>; };

// Following are the actual translated cons example.
template <typename X, typename XS>
struct cons {
    // A printing function so that we can see the value.
    static const std::string to_string() {
        return "cons (x=" + X::to_string() + ", xs=" + XS::to_string() + ") { unit }";
    }
};

struct nil {
    // A printing function so that we can see the value.
    static const std::string to_string() {
        return "nil() { unit }";
    }
};

template <typename XS>
struct length { using Eval = void; /* This is the exhausted case. */ };

// Template specialization are used to define cases.
template <>
struct length<nil> { using Eval = Int<0>; };

template <typename X, typename XS>
struct length<cons<X, XS>> { using Eval = typename Add<Int<1>, typename length<XS>::Eval>::Eval; };

// See how `F` is statically kinded here indicating `* -> *`.
template <typename XS, template <typename> typename F>
struct map { using Eval = void; };

template <template <typename> typename F>
struct map<nil, F> { using Eval = nil; };

template <typename X, typename XS, template <typename> typename F>
struct map<cons<X, XS>, F> { using Eval = cons<typename F<X>::Eval, typename map<XS, F>::Eval>; };

using list = cons<Int<1>, cons<Int<2>, cons<Int<3>, nil>>>;

// There are no anonymous functions in C++ template metaprogramming,
// so we define a struct to represent the function.
template <typename X>
struct MultiplyBy2 { using Eval = typename Multiply<Int<2>, X>::Eval; };

using doubled_list = typename map<list, MultiplyBy2>::Eval;

using length_of_list = typename length<list>::Eval;

#include <iostream>

int main() {
    std::cout << list::to_string() << std::endl;
    // cons (x=1, xs=cons (x=2, xs=cons (x=3, xs=nil() { unit }) { unit }) { unit }) { unit }

    std::cout << doubled_list::to_string() << std::endl;
    // cons (x=2, xs=cons (x=4, xs=cons (x=6, xs=nil() { unit }) { unit }) { unit }) { unit }

    std::cout << length_of_list::to_string() << std::endl;
    // 3

    return 0;
}
```

## Usage

Run a file by passing its path:

```bash
cargo run -- <path-to-file>
```

Start the interactive interpreter (REPL) without a path:

```bash
cargo run
```

The REPL prints expression results and saves the last successful result as `it`.
For an `Io` expression, this is the result of running the action.
Use a standalone `let` or `let { ... }` declaration to define names for the rest of
the session without changing `it`. Unlike in a file, a standalone REPL declaration
does not need a following expression.

```text
eel> let x = 40
eel> x + 2
42
eel> it * 2
84
```

To enter one expression or declaration across several lines, put `:{` and `:}` on
their own lines:

```text
eel> :{
eel| let double = fn(n) {
eel|     n * 2
eel| }
eel| :}
eel> double(21).
42
```

Type `:help` for commands, or `:quit` (`:q`) to exit. End of input exits as well.
When input is piped in, the REPL evaluates it without printing prompts.
