# Eel

A toy programming language leveraging explicit evaluation to allow evaluating functions to structs, and structs to values.

## Motivation

This language has been on my list of projects to create for a long time.

It was inspired by C++ template metaprogramming, where [class templates](https://en.cppreference.com/w/cpp/language/class_template.html), [classes](https://en.cppreference.com/w/cpp/language/classes.html), [using-declarations](https://en.cppreference.com/w/cpp/language/using_declaration.html), and [template specializations](https://en.cppreference.com/w/cpp/language/template_specialization.html) are used to create a program that can be evaluated at compile time. I made an [arithmetic expression evaluator in C++ template metaprogramming with variable substitution](https://github.com/LioQing/pdwb/blob/7ac3a0958edebb3145c789f05b35ef2825f34ff5/ftcpp_calc/main.cpp#L512-L549) for fun and was fascinated by it.

## Overview

Eel, being the language inspired by C++ template metaprogramming, is a language that works in essentially the same way. In fact, it should almost be able to transpile to valid C++ template metaprogramming code, the only difference is that it is dynamically typed while C++ template metaprogramming is somewhat 'statically typed'.

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
| function | `let apply = fn(x, f) { f(x). }` | `template <int X, template <typename> typename F> struct apply { using Eval = typename F<X>::Eval; };` |
| struct | `let x = y` | `using x = y;` |
| value | `let x = 1` | `constexpr int x = 1;` |
| function definition | `let f = fn(x) { x }` | `template <typename X> struct f { using Eval = X; };` |
| function application | `f(x)` | `apply<X, f>` |
| struct evaluation | `x.` | `X::Eval` |

> [!NOTE]
>
> The explanation above may still be a bit ambiguous, since I am also in the process of learning how to express these concepts.

## Features

- **Explicit Evaluation**: Functions can be evaluated to structs, and structs can be evaluated to values.
- **Purely Functional**: Supports first-class functions and higher-order functions, with absolutely no side effects and mutability.
- **Dynamically Typed**: Types are checked at runtime, allowing for flexibility in function definitions and calls.

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

let _ = print(list).
// cons (x=1, xs=cons (x=2, xs=cons (x=3, xs=nil() { unit }) { unit }) { unit }) { unit }

let _ = print(doubled_list).
// cons (x=2, xs=cons (x=4, xs=cons (x=6, xs=nil() { unit }) { unit }) { unit }) { unit }

let _ = print(length_of_list).
// 3

0
```

See more examples in the [examples](examples/) directory.

## Usage

```bash
cargo run -- <path-to-file>
```