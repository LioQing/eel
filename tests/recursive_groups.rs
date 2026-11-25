mod common;

use common::{SourceFile, assert_failure, assert_returned};

#[test]
fn test_mutual_recursion_works_in_both_declaration_orders() {
    let odd = "odd = fn(n) { case n { 0 => 0 _ => even(n - 1). } }";
    let even = "even = fn(n) { case n { 0 => 1 _ => odd(n - 1). } }";

    for (first, second) in [(odd, even), (even, odd)] {
        assert_returned(
            &format!("let {{\n    {first}\n    {second}\n}}\neven(4). + odd(5)."),
            2,
        );
    }
}

#[test]
fn test_escaped_group_member_keeps_original_peers_and_struct_names() {
    assert_returned(
        "let alias = {
             let peer = fn(n) { 0 }
             let {
                 first = fn(n) { peer(n) }
                 peer = fn(n) { n }
             }
             first
         }
         let peer = fn(n) { 0 }
         let first = fn(n) { 0 }
         let value = alias(42)
         case value { alias(42) => { case value. { peer(n) => 0 original => original. } } }",
        42,
    );
}

#[test]
fn test_escaped_deferred_struct_can_call_a_peer() {
    assert_returned(
        "let value = {
             let base = 42
             let {
                 first = fn() { second(). }
                 second = fn() { base }
             }
             first()
         }
         let base = 0
         let second = fn() { 0 }
         value.",
        42,
    );
}

#[test]
fn test_group_parameter_shadows_peer() {
    assert_returned(
        "let {
             f = fn(peer) { peer }
             peer = fn() { 0 }
         }
         f(42).",
        42,
    );
}

#[test]
fn test_group_members_use_later_operators_with_independent_fixities() {
    assert_returned(
        "let {
             first = fn() { 10 sub 3 sub 2 join 5 join 1 }
             opr 4 sub = fn(a, b) { a - b }
             opl 2 join = fn(a, b) { a * 2 + b }
         }
         first().",
        47,
    );
}

#[test]
fn test_invalid_or_bodyless_group_fails_in_file_mode() {
    for (source, diagnostic) in [
        (
            "let { f = fn() 0 f = fn() 1 } unit",
            "Duplicate name `f` in recursive let group",
        ),
        ("let { f = 42 } unit", "function expression"),
        ("let {} unit", "Parsing failed"),
        ("let { f = fn() 42 }", "Parsing failed"),
    ] {
        assert_failure(&SourceFile::new(source).run(), diagnostic);
    }
}
