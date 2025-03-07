package hir

import types "github.com/kievzenit/ylang/internal/hir/types"

type StmtHir interface {
	StmtHirNode()
}

type TopStmtHir interface {
	StmtHir
	TopStmtHirNode()
}

type ExprStmtHir struct {
	Expr ExprHir
}

type ScopeStmtHir struct {
	Stmts []StmtHir
}

type FuncDeclStmtHir struct {
	types.FunctionType
	Body *ScopeStmtHir
}

type VarDeclStmtHir struct {
	types.Type
	Value ExprHir
}

type ReturnStmtHir struct {
	Expr ExprHir
}

func (VarDeclStmtHir) TopStmtHirNode()  {}
func (FuncDeclStmtHir) TopStmtHirNode() {}

func (ExprStmtHir) StmtHirNode()     {}
func (ScopeStmtHir) StmtHirNode()    {}
func (FuncDeclStmtHir) StmtHirNode() {}
func (VarDeclStmtHir) StmtHirNode()  {}
func (ReturnStmtHir) StmtHirNode()   {}
