package hir

import (
	"reflect"

	types "github.com/kievzenit/ylang/internal/hir/types"
	"github.com/kievzenit/ylang/internal/lexer"
)

type ExprHir interface {
	ExprHirNode()
	ExprType() types.Type
	IsConst() bool
	AddressCouldBeTaken() bool
}

func IsNilExpr(expr ExprHir) bool {
	if expr == nil {
		return true
	}

	return reflect.ValueOf(expr).IsNil()
}

type BoolExprHir struct {
	types.Type
	Value bool
}

type IntExprHir struct {
	types.Type
	Value int64
}

type FloatExprHir struct {
	types.Type
	Value float64
}

type IdentExprHir struct {
	types.Type
	Name string
}

type ArgIdentExprHir struct {
	types.Type
	Name  string
	Index int
}

type AssignExprHir struct {
	types.Type
	Ident *IdentExprHir
	Value ExprHir
}

type TypeMemberInstantiation struct {
	MemberPosition int
	MemberName     string
	ExprHir
}

type TypeInstantiationExprHir struct {
	types.Type
	TypeName       string
	Instantiations []TypeMemberInstantiation
}

type MemberAccessExprHir struct {
	types.Type

	Left  ExprHir
	Right ExprHir
}

type CallExprHir struct {
	types.Type
	Name string
	Args []ExprHir
}

type UnaryOp int

const (
	Plus UnaryOp = iota
	Negate
	Not
	BitNot
	Inc
	Dec
	AddressOf
	Dereference
)

func UnaryOpFromTokenKind(kind lexer.TokenKind) UnaryOp {
	switch kind {
	case lexer.PLUS:
		return Plus
	case lexer.MINUS:
		return Negate
	case lexer.BAND:
		return AddressOf
	case lexer.ASTERISK:
		return Dereference
	case lexer.TILDE:
		return BitNot
	case lexer.XMARK:
		return Not
	case lexer.INC:
		return Inc
	case lexer.DEC:
		return Dec
	default:
		panic("unexpected token kind")
	}
}

type PrefixExprHir struct {
	types.Type
	Op   UnaryOp
	Expr ExprHir
}

type PostfixExprHir struct {
	types.Type
	Op   UnaryOp
	Expr ExprHir
}

type CastExprHir interface {
	ExprHir
	CastExprHirNode()
}

type OperatorCastExprHir struct {
	NewType types.Type
	OldType types.Type
	Expr    ExprHir
}

type DownCastExprHir struct {
	NewType types.Type
	OldType types.Type
	Expr    ExprHir
}

type UpCastExprHir struct {
	NewType types.Type
	OldType types.Type
	Expr    ExprHir
}

type BinaryOp int

const (
	Add BinaryOp = iota
	Sub
	Mul
	Div
	Mod
	Band
	Bor
	Xor
	Shl
	Shr
	Eq
	Ne
	Lt
	Gt
	Le
	Ge
	Land
	Lor
)

func BinOpFromTokenKind(kind lexer.TokenKind) BinaryOp {
	switch kind {
	case lexer.PLUS:
		return Add
	case lexer.MINUS:
		return Sub
	case lexer.ASTERISK:
		return Mul
	case lexer.SLASH:
		return Div
	case lexer.PERCENT:
		return Mod
	case lexer.BAND:
		return Band
	case lexer.BOR:
		return Bor
	case lexer.XOR:
		return Xor
	case lexer.LT:
		return Lt
	case lexer.GT:
		return Gt
	case lexer.LEQ:
		return Le
	case lexer.GEQ:
		return Ge
	case lexer.EQ:
		return Eq
	case lexer.NEQ:
		return Ne
	case lexer.LAND:
		return Land
	case lexer.LOR:
		return Lor
	default:
		panic("unexpected token kind")
	}
}

type BinaryExprHir struct {
	types.Type
	Left  ExprHir
	Op    BinaryOp
	Right ExprHir
}

func (BoolExprHir) ExprHirNode()              {}
func (IntExprHir) ExprHirNode()               {}
func (FloatExprHir) ExprHirNode()             {}
func (IdentExprHir) ExprHirNode()             {}
func (ArgIdentExprHir) ExprHirNode()          {}
func (AssignExprHir) ExprHirNode()            {}
func (TypeInstantiationExprHir) ExprHirNode() {}
func (MemberAccessExprHir) ExprHirNode()      {}
func (CallExprHir) ExprHirNode()              {}
func (PrefixExprHir) ExprHirNode()            {}
func (PostfixExprHir) ExprHirNode()           {}
func (OperatorCastExprHir) ExprHirNode()      {}
func (DownCastExprHir) ExprHirNode()          {}
func (UpCastExprHir) ExprHirNode()            {}
func (BinaryExprHir) ExprHirNode()            {}

func (e BoolExprHir) ExprType() types.Type              { return e.Type }
func (e IntExprHir) ExprType() types.Type               { return e.Type }
func (e FloatExprHir) ExprType() types.Type             { return e.Type }
func (e IdentExprHir) ExprType() types.Type             { return e.Type }
func (e ArgIdentExprHir) ExprType() types.Type          { return e.Type }
func (e AssignExprHir) ExprType() types.Type            { return e.Type }
func (e TypeInstantiationExprHir) ExprType() types.Type { return e.Type }
func (e MemberAccessExprHir) ExprType() types.Type      { return e.Type }
func (e CallExprHir) ExprType() types.Type              { return e.Type }
func (e PrefixExprHir) ExprType() types.Type            { return e.Type }
func (e PostfixExprHir) ExprType() types.Type           { return e.Type }
func (e OperatorCastExprHir) ExprType() types.Type      { return e.NewType }
func (e DownCastExprHir) ExprType() types.Type          { return e.NewType }
func (e UpCastExprHir) ExprType() types.Type            { return e.NewType }
func (e BinaryExprHir) ExprType() types.Type            { return e.Type }

func (BoolExprHir) IsConst() bool              { return true }
func (IntExprHir) IsConst() bool               { return true }
func (FloatExprHir) IsConst() bool             { return true }
func (IdentExprHir) IsConst() bool             { return false }
func (ArgIdentExprHir) IsConst() bool          { return false }
func (AssignExprHir) IsConst() bool            { return false }
func (TypeInstantiationExprHir) IsConst() bool { return false }
func (MemberAccessExprHir) IsConst() bool      { return false }
func (CallExprHir) IsConst() bool              { return false }
func (PrefixExprHir) IsConst() bool            { return false }
func (PostfixExprHir) IsConst() bool           { return false }
func (OperatorCastExprHir) IsConst() bool      { return false }
func (DownCastExprHir) IsConst() bool          { return false }
func (UpCastExprHir) IsConst() bool            { return false }
func (e BinaryExprHir) IsConst() bool          { return e.Left.IsConst() && e.Right.IsConst() }

func (BoolExprHir) AddressCouldBeTaken() bool              { return false }
func (IntExprHir) AddressCouldBeTaken() bool               { return false }
func (FloatExprHir) AddressCouldBeTaken() bool             { return false }
func (IdentExprHir) AddressCouldBeTaken() bool             { return true }
func (ArgIdentExprHir) AddressCouldBeTaken() bool          { return true }
func (AssignExprHir) AddressCouldBeTaken() bool            { return false }
func (TypeInstantiationExprHir) AddressCouldBeTaken() bool { return false }
func (MemberAccessExprHir) AddressCouldBeTaken() bool      { return true }
func (CallExprHir) AddressCouldBeTaken() bool              { return false }
func (PrefixExprHir) AddressCouldBeTaken() bool            { return false }
func (PostfixExprHir) AddressCouldBeTaken() bool           { return false }
func (OperatorCastExprHir) AddressCouldBeTaken() bool      { return false }
func (DownCastExprHir) AddressCouldBeTaken() bool          { return false }
func (UpCastExprHir) AddressCouldBeTaken() bool            { return false }
func (BinaryExprHir) AddressCouldBeTaken() bool            { return false }

func (OperatorCastExprHir) CastExprHirNode() {}
func (DownCastExprHir) CastExprHirNode()     {}
func (UpCastExprHir) CastExprHirNode()       {}
