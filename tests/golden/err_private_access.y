package main;

type Counter {
    private:
    count: i32;
}

fun peek(c: *Counter) i32 {
    return c.count;
}

fun main() i32 {
    return 0;
}
