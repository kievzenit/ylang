package main;

type Counter {
    private:
    count: i32;

    public:
    fun get(self: *Counter) i32 {
        return self.count;
    }
}

fun main() i32 {
    return 0;
}
