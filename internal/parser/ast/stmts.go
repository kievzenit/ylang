package ast

type Stmt interface {
	StmtNode()
}

type ScopeStmt struct {
	Stmts []Stmt
}

type TopStmt interface {
	Stmt
	TopStmtNode()
}

type FuncDeclStmt struct {
	Name       string
	ReturnType string
	Args       []FuncArg
	Body       *ScopeStmt
	Extern     bool
}

type FuncArg struct {
	Name string
	Type string
}

type VarDeclStmt struct {
	Name   string
	Value  Expr
	Static bool
	Const  bool
}

type ExprStmt struct {
	Expr Expr
}

func (f *FuncDeclStmt) TopStmtNode() {}
func (v *VarDeclStmt) TopStmtNode()  {}

func (s *ScopeStmt) StmtNode()    {}
func (f *FuncDeclStmt) StmtNode() {}
func (v *VarDeclStmt) StmtNode()  {}
func (e *ExprStmt) StmtNode()     {}
