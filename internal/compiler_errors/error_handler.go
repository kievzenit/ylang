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
	exit   func()
}

func NewErrorHandler(outputWriter io.Writer) ErrorHandler {
	return NewErrorHandlerWithExit(outputWriter, func() { os.Exit(1) })
}

// NewErrorHandlerWithExit is like NewErrorHandler, but FailNow calls exit
// instead of terminating the process. exit must not return normally (for
// example it can panic), because callers of FailNow assume it does not return.
func NewErrorHandlerWithExit(outputWriter io.Writer, exit func()) ErrorHandler {
	return &CompilerErrorHandler{
		errors: make([]CompilerError, 0),
		writer: outputWriter,
		exit:   exit,
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

	eh.exit()
	panic("ErrorHandler exit function returned")
}

func (eh *CompilerErrorHandler) FailIfAnyError() {
	if len(eh.errors) > 0 {
		eh.FailNow()
	}
}
