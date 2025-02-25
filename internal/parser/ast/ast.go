package ast

type TranslationUnit struct {
	Package *PackageStmt
	Stmts []TopStmt
}

type PackageStmt struct {
	Name string
}

type Stmt interface {
	StmtNode()
}

type TopStmt interface {
	Stmt
	TopStmtNode()
}

type Expr interface {
	ExprNode()
}
