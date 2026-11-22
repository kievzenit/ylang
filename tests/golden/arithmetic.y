package main;

fun add(a: i32, b: i32) i32 {
    return a + b;
}

fun main() i32 {
    let x = 10;
    let y: i64 = 3;
    x += add(x, 2) * 3;
    return x % 7;
}
