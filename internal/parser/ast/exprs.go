package ast

import "github.com/kievzenit/ylang/internal/lexer"

type Expr interface {
	ExprNode()
}

type BoolExpr struct {
	Value bool
}

type IntExpr struct {
	Value int64
}

type FloatExpr struct {
	Value float64
}

type CharExpr struct {
	Value byte
}

type StringExpr struct {
	Value string
}

type IdentExpr struct {
	Value string
}

type CallExpr struct {
	Name string
	Args []Expr
}

type BinaryExpr struct {
	Left  Expr
	Op    lexer.Token
	Right Expr
}

func (b *BoolExpr) ExprNode()   {}
func (i *IntExpr) ExprNode()    {}
func (f *FloatExpr) ExprNode()  {}
func (c *CharExpr) ExprNode()   {}
func (s *StringExpr) ExprNode() {}
func (i *IdentExpr) ExprNode()  {}
func (c *CallExpr) ExprNode()   {}
func (b *BinaryExpr) ExprNode() {}
