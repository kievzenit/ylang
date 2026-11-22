package main;

fun main() i32 {
    let sum = 0;
    for (let i = 0; i < 10; i++) {
        if (i == 3) {
            continue;
        } else if (i > 7) {
            break;
        }
        sum += i;
    }

    let n = 0;
    while (n < 5) {
        n++;
    }

    return sum + n;
}
