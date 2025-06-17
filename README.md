# Eel

A toy programming language leveraging explicit evaluation to allow evaluating functions to structs, and structs to values.

## Features

- **Explicit Evaluation**: Functions can be evaluated to structs, and structs can be evaluated to values.
- **Functional**: Supports first-class functions and higher-order functions, with no side effects and mutability.
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
let doubledList = map(list, fn(x) { x * 2 }).
let lengthOfList = length(list).

let _ = print(list).
// cons (x=1, xs=cons (x=2, xs=cons (x=3, xs=nil() { unit }) { unit }) { unit }) { unit }

let _ = print(doubledList).
// cons (x=2, xs=cons (x=4, xs=cons (x=6, xs=nil() { unit }) { unit }) { unit }) { unit }

let _ = print(lengthOfList).
// 3

0
```

See more examples in the [examples](examples/) directory.

## Usage

```bash
cargo run -- <path-to-file>
```