build:
	go run cmd/main.go .sources/test.y 2> .out/test.ll
	clang -c -o .out/test.o .out/test.ll

build-c: build
	clang -g -o .out/test .out/test.o .sources/test.c

run: build-c
	./.out/test

debug: build-c
	clang -g -o .out/test .out/test.o .sources/test.c
	gdb .out/test

inspect: build
	llvm-objdump -d -M intel .out/test.o | less

inspect-c: build-c
	llvm-objdump -d -M intel .out/test | less