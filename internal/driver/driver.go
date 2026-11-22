// Package driver runs the compiler pipeline: lexer, parser, semantic
// analyzer and emitter. It is shared by cmd/main.go and the golden tests.
package driver

import (
	"github.com/kievzenit/ylang/internal/compiler_errors"
	"github.com/kievzenit/ylang/internal/emitter"
	"github.com/kievzenit/ylang/internal/hir"
	l "github.com/kievzenit/ylang/internal/lexer"
	"github.com/kievzenit/ylang/internal/parser"
	"github.com/kievzenit/ylang/internal/semantic_analyzer"
	"tinygo.org/x/go-llvm"
)

// Analyze lexes, parses and analyzes one source file. It calls
// eh.FailIfAnyError before returning, so it only returns on success.
func Analyze(fileName string, source []byte, eh compiler_errors.ErrorHandler) *hir.FileHir {
	lexer := l.NewLexer(fileName, source, eh)
	tokens := lexer.Tokenize()
	sanitizedTokens := make([]*l.Token, 0, len(tokens))
	for _, token := range tokens {
		if token.Kind == l.ONELINE_COMMENT || token.Kind == l.MULTILINE_COMMENT {
			continue
		}
		sanitizedTokens = append(sanitizedTokens, token)
	}
	scanner := l.NewTokenScanner(sanitizedTokens)

	translationUnit := parser.NewParser(fileName, scanner, eh).Parse()

	fileHir := semantic_analyzer.NewSemanticAnalyzer(eh, translationUnit).Analyze()
	eh.FailIfAnyError()
	return fileHir
}

// Emit generates the LLVM module for analyzed HIR.
func Emit(fileHir *hir.FileHir) llvm.Module {
	return emitter.NewEmitter(fileHir).Emit()
}
