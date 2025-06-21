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
        return "cons (x=" + X().to_string() + ", xs=" + XS().to_string() + ") { unit }";
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