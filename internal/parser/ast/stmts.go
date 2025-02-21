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

type LoopStmt struct {
	Body *ScopeStmt
}

type WhileStmt struct {
	Cond Expr
	Body *ScopeStmt
}

type DoWhileStmt struct {
	Cond Expr
	Body *ScopeStmt
}

type ElseIf struct {
	Cond Expr
	Body *ScopeStmt
}

type IfStmt struct {
	Cond   Expr
	Body   *ScopeStmt
	ElseIf []ElseIf
	Else   *ScopeStmt
}

type ExprStmt struct {
	Expr Expr
}

type ReturnStmt struct {
	Expr Expr
}

type BreakStmt struct{}

type ContinueStmt struct{}

type BreakAllStmt struct{}

func (f *FuncDeclStmt) TopStmtNode() {}
func (v *VarDeclStmt) TopStmtNode()  {}

func (s *ScopeStmt) StmtNode()    {}
func (f *FuncDeclStmt) StmtNode() {}
func (v *VarDeclStmt) StmtNode()  {}
func (e *ExprStmt) StmtNode()     {}
func (i *IfStmt) StmtNode()       {}
func (l *LoopStmt) StmtNode()     {}
func (w *WhileStmt) StmtNode()    {}
func (d *DoWhileStmt) StmtNode()  {}
func (r *ReturnStmt) StmtNode()   {}
func (b *BreakStmt) StmtNode()    {}
func (c *ContinueStmt) StmtNode() {}
func (b *BreakAllStmt) StmtNode() {}
