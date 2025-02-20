package ast

type Stmt interface {
	StmtNode()
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

func (v *VarDeclStmt) StmtNode() {}
func (e *ExprStmt) StmtNode()    {}
