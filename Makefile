build:
	go run cmd/main.go .sources/test.y 2> .out/test.ll
	clang -c -o .out/test.o .out/test.ll

run: build
	clang -g -o .out/test .out/test.o .sources/test.c
	./.out/test

inspect: build
	llvm-objdump -d -M intel .out/test.o | less