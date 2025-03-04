package ast

import "github.com/kievzenit/ylang/internal/lexer"

type AstNode interface {
	AstNode()
	FirstToken() *lexer.Token
}

type TranslationUnit struct {
	Package *PackageStmt
	Stmts   []TopStmt
}

type PackageStmt struct {
	StartToken *lexer.Token

	Name string
}

type Stmt interface {
	AstNode
	StmtNode()
}

type TopStmt interface {
	Stmt
	TopStmtNode()
}

type Expr interface {
	AstNode
	ExprNode()
}

func (p *PackageStmt) AstNode() {}
func (p *PackageStmt) FirstToken() *lexer.Token {
	return p.StartToken
}
