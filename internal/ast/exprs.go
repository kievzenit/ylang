package ast

import "github.com/kievzenit/ylang/internal/lexer"

type BoolExpr struct {
	StartToken *lexer.Token

	Value bool
}

type IntType string

const (
	IntNone     IntType = ""
	Int8Type    IntType = "i8"
	Int16Type   IntType = "i16"
	Int32Type   IntType = "i32"
	Int64Type   IntType = "i64"
	Int128Type  IntType = "i128"
	Uint1Type   IntType = "u1"
	Uint8Type   IntType = "u8"
	Uint16Type  IntType = "u16"
	Uint32Type  IntType = "u32"
	Uint64Type  IntType = "u64"
	Uint128Type IntType = "u128"
)

type IntExpr struct {
	StartToken *lexer.Token

	Value        int64
	ExplicitType IntType
}

type FloatType string

const (
	FloatNone    FloatType = ""
	Float16Type  FloatType = "f16"
	Float32Type  FloatType = "f32"
	Float64Type  FloatType = "f64"
	Float80Type  FloatType = "f80"
	Float128Type FloatType = "f128"
)

type FloatExpr struct {
	StartToken *lexer.Token

	Value        float64
	ExplicitType FloatType
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

type PrefixExpr struct {
	StartToken *lexer.Token

	Op    *lexer.Token
	Right Expr
}

type PostfixExpr struct {
	StartToken *lexer.Token

	Left  Expr
	Op    *lexer.Token
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
func (b *PrefixExpr) AstNode()         {}
func (b *PostfixExpr) AstNode()        {}
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
func (b *PrefixExpr) FirstToken() *lexer.Token         { return b.StartToken }
func (b *PostfixExpr) FirstToken() *lexer.Token        { return b.StartToken }
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
func (b *PrefixExpr) ExprNode()         {}
func (b *PostfixExpr) ExprNode()        {}
func (b *BinaryExpr) ExprNode()         {}
