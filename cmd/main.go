package main

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/kievzenit/ylang/internal/compiler_errors"
	"github.com/kievzenit/ylang/internal/driver"
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

	fileHir := driver.Analyze(absoluteFileName, fileData, eh)
	litter.Dump(fileHir)

	module := driver.Emit(fileHir)
	module.Dump()
}
