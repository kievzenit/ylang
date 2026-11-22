package main;

// A type may contain, by value, a type declared after it.
type Outer {
    public:
    inner: Inner;
    tag: i32;
}

type Inner {
    public:
    x: i32;
    y: i32;
}

fun main() i32 {
    let o = Outer::{ inner = Inner::{ x = 1, y = 2 }, tag = 3 };
    o.inner.x = 10;
    let p = &o;
    p.tag = 4;
    return o.inner.x + o.inner.y + p.tag;
}
