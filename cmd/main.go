package main

import (
	"fmt"
	"os"

	"github.com/kievzenit/ylang/internal/compiler_errors"
	l "github.com/kievzenit/ylang/internal/lexer"
)

func main() {
	fileData, err := os.ReadFile("../sources/tokens.y")
	if err != nil {
		fmt.Println(err)
		return
	}

	eh := compiler_errors.NewErrorHandler(os.Stderr)

	lexer := l.NewLexer(fileData, eh)
	tokens := lexer.Tokenize()
	for _, token := range tokens {
		fmt.Println(token.String())
	}

	scanner := l.NewTokenScanner(tokens)
	_ = scanner
}
