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
}

func IsNilExpr(expr ExprHir) bool {
	if expr == nil {
		return true
	}

	return reflect.ValueOf(expr).IsNil()
}

type LvalueExprHir interface {
	ExprHir
	LvalueExprHirNode()
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
	Left  LvalueExprHir
	Right ExprHir
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
	UnaryPlus UnaryOp = iota
	UnaryNegate
	UnaryNot
	UnaryBitNot
	UnaryInc
	UnaryDec
	UnaryAddressOf
	UnaryDereference
)

func UnaryOpFromTokenKind(kind lexer.TokenKind) UnaryOp {
	switch kind {
	case lexer.PLUS:
		return UnaryPlus
	case lexer.MINUS:
		return UnaryNegate
	case lexer.BAND:
		return UnaryAddressOf
	case lexer.ASTERISK:
		return UnaryDereference
	case lexer.TILDE:
		return UnaryBitNot
	case lexer.XMARK:
		return UnaryNot
	case lexer.INC:
		return UnaryInc
	case lexer.DEC:
		return UnaryDec
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
	BinaryAdd BinaryOp = iota
	BinarySub
	BinaryMul
	BinaryDiv
	BinaryMod
	BinaryBand
	BinaryBor
	BinaryXor
	BinaryShl
	BinaryShr
	BinaryEq
	BinaryNe
	BinaryLt
	BinaryGt
	BinaryLe
	BinaryGe
	BinaryLand
	BinaryLor
)

func BinOpFromTokenKind(kind lexer.TokenKind) BinaryOp {
	switch kind {
	case lexer.PLUS:
		return BinaryAdd
	case lexer.MINUS:
		return BinarySub
	case lexer.ASTERISK:
		return BinaryMul
	case lexer.SLASH:
		return BinaryDiv
	case lexer.PERCENT:
		return BinaryMod
	case lexer.BAND:
		return BinaryBand
	case lexer.BOR:
		return BinaryBor
	case lexer.XOR:
		return BinaryXor
	case lexer.LT:
		return BinaryLt
	case lexer.GT:
		return BinaryGt
	case lexer.LEQ:
		return BinaryLe
	case lexer.GEQ:
		return BinaryGe
	case lexer.EQ:
		return BinaryEq
	case lexer.NEQ:
		return BinaryNe
	case lexer.LAND:
		return BinaryLand
	case lexer.LOR:
		return BinaryLor
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

func (IdentExprHir) LvalueExprHirNode()        {}
func (MemberAccessExprHir) LvalueExprHirNode() {}
func (PrefixExprHir) LvalueExprHirNode()       {}
func (PostfixExprHir) LvalueExprHirNode()      {}

func (OperatorCastExprHir) CastExprHirNode() {}
func (DownCastExprHir) CastExprHirNode()     {}
func (UpCastExprHir) CastExprHirNode()       {}
