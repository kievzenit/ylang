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
	*types.FunctionType
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

type WhileStmtHir struct {
	Cond ExprHir
	Body *ScopeStmtHir
}

type DoWhileStmtHir struct {
	Cond ExprHir
	Body *ScopeStmtHir
}

type LoopStmtHir struct {
	Body *ScopeStmtHir
}

type ForStmtHir struct {
	Init []StmtHir
	Cond ExprHir
	Post []ExprHir
	Body *ScopeStmtHir
}

type ReturnStmtHir struct {
	Expr ExprHir
}

type ContinueStmtHir struct{}

type BreakStmtHir struct{}

type BreakAllStmtHir struct{}

func (VarDeclStmtHir) TopStmtHirNode()  {}
func (FuncDeclStmtHir) TopStmtHirNode() {}

func (ExprStmtHir) StmtHirNode()     {}
func (ScopeStmtHir) StmtHirNode()    {}
func (FuncDeclStmtHir) StmtHirNode() {}
func (VarDeclStmtHir) StmtHirNode()  {}
func (IfStmtHir) StmtHirNode()       {}
func (WhileStmtHir) StmtHirNode()    {}
func (DoWhileStmtHir) StmtHirNode()  {}
func (LoopStmtHir) StmtHirNode()     {}
func (ForStmtHir) StmtHirNode()      {}
func (ReturnStmtHir) StmtHirNode()   {}
func (ContinueStmtHir) StmtHirNode() {}
func (BreakStmtHir) StmtHirNode()    {}
func (BreakAllStmtHir) StmtHirNode() {}
