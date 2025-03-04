package compiler_errors

import (
	"fmt"
	"io"
	"os"
)

type CompilerError interface {
	GetMessage() string
	GetFileName() string
	GetLine() int
	GetColumn() int
	GetLength() int
}

type ErrorHandler interface {
	AddError(err CompilerError)
	FailNow()
	FailIfAnyError()
}

type CompilerErrorHandler struct {
	errors []CompilerError
	writer io.Writer
}

func NewErrorHandler(outputWriter io.Writer) ErrorHandler {
	return &CompilerErrorHandler{
		errors: make([]CompilerError, 0),
		writer: outputWriter,
	}
}

func (eh *CompilerErrorHandler) AddError(err CompilerError) {
	eh.errors = append(eh.errors, err)
}

func (eh *CompilerErrorHandler) FailNow() {
	fmt.Fprintln(eh.writer, "Build failed with errors:")
	fmt.Fprintln(eh.writer)

	for _, err := range eh.errors {
		fmt.Fprintf(eh.writer, "ERROR: %s\n", err.GetMessage())
		fmt.Fprintf(eh.writer, "---> %s:%d:%d\n", err.GetFileName(), err.GetLine(), err.GetColumn())
		fmt.Fprintln(eh.writer)
	}

	os.Exit(1)
}

func (eh *CompilerErrorHandler) FailIfAnyError() {
	if len(eh.errors) > 0 {
		eh.FailNow()
	}
}
