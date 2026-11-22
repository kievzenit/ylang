LLVM_CONFIG ?= llvm-config-21
LLVM_TAG ?= llvm21

LLVM_INCDIR := $(shell $(LLVM_CONFIG) --includedir)
LLVM_LIBDIR := $(shell $(LLVM_CONFIG) --libdir)
LLVM_LIBNAME := $(shell $(LLVM_CONFIG) --libnames | sed -e 's/^lib//' -e 's/\.so$$//' -e 's/\.a$$//')

export CGO_CFLAGS := -I$(LLVM_INCDIR)
export CGO_CXXFLAGS := -I$(LLVM_INCDIR) -std=c++17
export CGO_LDFLAGS := -L$(LLVM_LIBDIR) -l$(LLVM_LIBNAME)

build:
	go run -tags $(LLVM_TAG) cmd/main.go .sources/test.y 2> .out/test.ll
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

test:
	go test -tags $(LLVM_TAG) ./...

test-update:
	go test -tags $(LLVM_TAG) ./tests -update
