package hir

import (
	types "github.com/kievzenit/ylang/internal/hir/types"
)

type FileHir struct {
	FuncPrototypes []types.FunctionType
	Stmts          []TopStmtHir
}
