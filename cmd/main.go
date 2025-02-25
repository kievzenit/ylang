package main

import (
	"fmt"
	"os"

	"github.com/kievzenit/ylang/internal/compiler_errors"
	l "github.com/kievzenit/ylang/internal/lexer"
	"github.com/kievzenit/ylang/internal/parser"
	"github.com/sanity-io/litter"
)

func main() {
	fileName := os.Args[1]
	fileData, err := os.ReadFile(fileName)
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

	parser := parser.NewParser(scanner, eh)
	translationUnit := parser.Parse()
	litter.Dump(translationUnit)
}
