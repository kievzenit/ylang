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
	Name  string
	Value ExprHir
}

type IfStmtHir struct {
	Cond ExprHir
	Body *ScopeStmtHir
	Else StmtHir
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
func (IfStmtHir) StmtHirNode()       {}
func (ReturnStmtHir) StmtHirNode()   {}
