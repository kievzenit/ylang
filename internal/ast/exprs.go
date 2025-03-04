package ast

import "github.com/kievzenit/ylang/internal/lexer"

type BoolExpr struct {
	StartToken *lexer.Token

	Value bool
}

type IntExpr struct {
	StartToken *lexer.Token

	Value int64
}

type FloatExpr struct {
	StartToken *lexer.Token

	Value float64
}

type CharExpr struct {
	StartToken *lexer.Token

	Value byte
}

type StringExpr struct {
	StartToken *lexer.Token

	Value string
}

type IdentExpr struct {
	StartToken *lexer.Token

	Value string
}

type AssignExpr struct {
	StartToken *lexer.Token

	Ident *IdentExpr
	Op    *lexer.Token
	Value Expr
}

type CallExpr struct {
	StartToken *lexer.Token

	Name string
	Args []Expr
}

type ArraySubscriptExpr struct {
	StartToken *lexer.Token

	Left  Expr
	Index Expr
}

type MemberAccessExpr struct {
	StartToken *lexer.Token

	Left  Expr
	Right Expr
}

type CastExpr struct {
	StartToken *lexer.Token

	Left       Expr
	NewIdent   *IdentExpr
	CastToType string
}

type BinaryExpr struct {
	StartToken *lexer.Token

	Left  Expr
	Op    *lexer.Token
	Right Expr
}

func (b *BoolExpr) AstNode()           {}
func (i *IntExpr) AstNode()            {}
func (f *FloatExpr) AstNode()          {}
func (c *CharExpr) AstNode()           {}
func (s *StringExpr) AstNode()         {}
func (i *IdentExpr) AstNode()          {}
func (a *AssignExpr) AstNode()         {}
func (c *CallExpr) AstNode()           {}
func (a *ArraySubscriptExpr) AstNode() {}
func (m *MemberAccessExpr) AstNode()   {}
func (c *CastExpr) AstNode()           {}
func (b *BinaryExpr) AstNode()         {}

func (b *BoolExpr) FirstToken() *lexer.Token           { return b.StartToken }
func (i *IntExpr) FirstToken() *lexer.Token            { return i.StartToken }
func (f *FloatExpr) FirstToken() *lexer.Token          { return f.StartToken }
func (c *CharExpr) FirstToken() *lexer.Token           { return c.StartToken }
func (s *StringExpr) FirstToken() *lexer.Token         { return s.StartToken }
func (i *IdentExpr) FirstToken() *lexer.Token          { return i.StartToken }
func (a *AssignExpr) FirstToken() *lexer.Token         { return a.StartToken }
func (c *CallExpr) FirstToken() *lexer.Token           { return c.StartToken }
func (a *ArraySubscriptExpr) FirstToken() *lexer.Token { return a.StartToken }
func (m *MemberAccessExpr) FirstToken() *lexer.Token   { return m.StartToken }
func (c *CastExpr) FirstToken() *lexer.Token           { return c.StartToken }
func (b *BinaryExpr) FirstToken() *lexer.Token         { return b.StartToken }

func (b *BoolExpr) ExprNode()           {}
func (i *IntExpr) ExprNode()            {}
func (f *FloatExpr) ExprNode()          {}
func (c *CharExpr) ExprNode()           {}
func (s *StringExpr) ExprNode()         {}
func (i *IdentExpr) ExprNode()          {}
func (a *AssignExpr) ExprNode()         {}
func (c *CallExpr) ExprNode()           {}
func (a *ArraySubscriptExpr) ExprNode() {}
func (m *MemberAccessExpr) ExprNode()   {}
func (c *CastExpr) ExprNode()           {}
func (b *BinaryExpr) ExprNode()         {}
