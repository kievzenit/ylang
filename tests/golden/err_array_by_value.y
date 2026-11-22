package main;

// An array of the type itself is still infinite size; an array of pointers is not.
type Tree {
    public:
    children: [2]Tree;
}

type GoodTree {
    public:
    children: [2]*GoodTree;
}

fun main() i32 {
    return 0;
}
