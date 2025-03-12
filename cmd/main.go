package main

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/kievzenit/ylang/internal/compiler_errors"
	"github.com/kievzenit/ylang/internal/emitter"
	l "github.com/kievzenit/ylang/internal/lexer"
	"github.com/kievzenit/ylang/internal/parser"
	"github.com/kievzenit/ylang/internal/semantic_analyzer"
	"github.com/sanity-io/litter"
)

func main() {
	fileName := os.Args[1]
	fileData, err := os.ReadFile(fileName)
	if err != nil {
		fmt.Println(err)
		return
	}

	absoluteFileName, err := filepath.Abs(fileName)
	if err != nil {
		fmt.Println(err)
		return
	}

	eh := compiler_errors.NewErrorHandler(os.Stderr)

	lexer := l.NewLexer(absoluteFileName, fileData, eh)
	tokens := lexer.Tokenize()
	sanitizedTokens := make([]*l.Token, 0)
	for _, token := range tokens {
		if token.Kind == l.ONELINE_COMMENT || token.Kind == l.MULTILINE_COMMENT {
			continue
		}

		sanitizedTokens = append(sanitizedTokens, token)
		fmt.Println(token.String())
	}
	scanner := l.NewTokenScanner(sanitizedTokens)

	parser := parser.NewParser(absoluteFileName, scanner, eh)
	translationUnit := parser.Parse()
	litter.Dump(translationUnit)

	semanticAnalyzer := semantic_analyzer.NewSemanticAnalyzer(eh, translationUnit)
	fileHir := semanticAnalyzer.Analyze()
	eh.FailIfAnyError()

	emitter := emitter.NewEmitter(fileHir)
	module := emitter.Emit()
	module.Dump()
}
