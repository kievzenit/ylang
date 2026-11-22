package main;

// A type may point to itself: this is what linked lists and ASTs need.
// (There is no null literal yet, so nodes are only reached through pointers.)
type Node {
    public:
    value: i32;
    next: *Node;
}

// rvalue access through a chain of pointers
fun third_value(n: *Node) i32 {
    return n.next.next.value;
}

// lvalue access through a chain of pointers
fun relink(a: *Node, b: *Node) i32 {
    a.next = b;
    a.next.value = 42;
    return b.value;
}

fun main() i32 {
    return 0;
}
