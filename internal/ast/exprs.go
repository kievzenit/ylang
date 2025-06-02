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

	Left  Expr
	Op    *lexer.Token
	Right Expr
}

type CallExpr struct {
	StartToken *lexer.Token

	Name string
	Args []Expr
}

type TypeMemberInstantiation struct {
	Name string
	Expr
}

type TypeInstantiationExpr struct {
	StartToken *lexer.Token

	TypeName       TypeNode
	Instantiations []TypeMemberInstantiation
}

type TypeConstructionExpr struct {
	StartToken *lexer.Token

	TypeName TypeNode
	Args     []Expr
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
	CastToType TypeNode
}

type PrefixExpr struct {
	StartToken *lexer.Token

	Op    *lexer.Token
	Right Expr
}

type PostfixExpr struct {
	StartToken *lexer.Token

	Left Expr
	Op   *lexer.Token
}

type BinaryExpr struct {
	StartToken *lexer.Token

	Left  Expr
	Op    *lexer.Token
	Right Expr
}

func (BoolExpr) AstNode()              {}
func (IntExpr) AstNode()               {}
func (FloatExpr) AstNode()             {}
func (CharExpr) AstNode()              {}
func (StringExpr) AstNode()            {}
func (IdentExpr) AstNode()             {}
func (AssignExpr) AstNode()            {}
func (CallExpr) AstNode()              {}
func (TypeInstantiationExpr) AstNode() {}
func (TypeConstructionExpr) AstNode()  {}
func (ArraySubscriptExpr) AstNode()    {}
func (MemberAccessExpr) AstNode()      {}
func (CastExpr) AstNode()              {}
func (PrefixExpr) AstNode()            {}
func (PostfixExpr) AstNode()           {}
func (BinaryExpr) AstNode()            {}

func (e *BoolExpr) FirstToken() *lexer.Token              { return e.StartToken }
func (e *IntExpr) FirstToken() *lexer.Token               { return e.StartToken }
func (e *FloatExpr) FirstToken() *lexer.Token             { return e.StartToken }
func (e *CharExpr) FirstToken() *lexer.Token              { return e.StartToken }
func (e *StringExpr) FirstToken() *lexer.Token            { return e.StartToken }
func (e *IdentExpr) FirstToken() *lexer.Token             { return e.StartToken }
func (e *AssignExpr) FirstToken() *lexer.Token            { return e.StartToken }
func (e *CallExpr) FirstToken() *lexer.Token              { return e.StartToken }
func (e *TypeInstantiationExpr) FirstToken() *lexer.Token { return e.StartToken }
func (e *TypeConstructionExpr) FirstToken() *lexer.Token  { return e.StartToken }
func (e *ArraySubscriptExpr) FirstToken() *lexer.Token    { return e.StartToken }
func (e *MemberAccessExpr) FirstToken() *lexer.Token      { return e.StartToken }
func (e *CastExpr) FirstToken() *lexer.Token              { return e.StartToken }
func (e *PrefixExpr) FirstToken() *lexer.Token            { return e.StartToken }
func (e *PostfixExpr) FirstToken() *lexer.Token           { return e.StartToken }
func (e *BinaryExpr) FirstToken() *lexer.Token            { return e.StartToken }

func (BoolExpr) ExprNode()              {}
func (IntExpr) ExprNode()               {}
func (FloatExpr) ExprNode()             {}
func (CharExpr) ExprNode()              {}
func (StringExpr) ExprNode()            {}
func (IdentExpr) ExprNode()             {}
func (AssignExpr) ExprNode()            {}
func (CallExpr) ExprNode()              {}
func (TypeInstantiationExpr) ExprNode() {}
func (TypeConstructionExpr) ExprNode()  {}
func (ArraySubscriptExpr) ExprNode()    {}
func (MemberAccessExpr) ExprNode()      {}
func (CastExpr) ExprNode()              {}
func (PrefixExpr) ExprNode()            {}
func (PostfixExpr) ExprNode()           {}
func (BinaryExpr) ExprNode()            {}
