package compiler_errors

import (
	"fmt"
	"io"
	"os"
)

type CompilerError interface {
	GetMessage() string
}

type ErrorHandler interface {
	AddError(err CompilerError)
	FailNow()
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

	for _, err := range eh.errors {
		fmt.Fprintf(eh.writer, "ERROR: %s\n", err.GetMessage())
	}

	os.Exit(1)
}
