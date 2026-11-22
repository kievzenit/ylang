package main;

// Types may point to each other, declared in any order.
type Parent {
    public:
    child: *Child;
    id: i32;
}

type Child {
    public:
    parent: *Parent;
    id: i32;
}

fun grandparent_id(c: *Child) i32 {
    return c.parent.child.parent.id;
}

fun main() i32 {
    return 0;
}
