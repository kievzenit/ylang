package hir

import (
	types "github.com/kievzenit/ylang/internal/hir/types"
)

type FileHir struct {
	FuncPrototypes []*types.FunctionType
	Types          map[string]*types.UserType

	GlobalVarDecls []*VarDeclStmtHir
	FuncDecls      []*FuncDeclStmtHir
}
