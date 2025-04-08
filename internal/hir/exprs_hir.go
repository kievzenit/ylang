package hir

import (
	"reflect"

	types "github.com/kievzenit/ylang/internal/hir/types"
	"github.com/kievzenit/ylang/internal/lexer"
)

type ExprHir interface {
	ExprHirNode()
	ExprType() types.Type
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
	MemberName string
	ExprHir
}

type TypeInstantiationExprHir struct {
	types.Type
	TypeName       string
	Instantiations []TypeMemberInstantiation
}

type CallExprHir struct {
	types.Type
	Name string
	Args []ExprHir
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
func (CallExprHir) ExprHirNode()              {}
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
func (e CallExprHir) ExprType() types.Type              { return e.Type }
func (e OperatorCastExprHir) ExprType() types.Type      { return e.NewType }
func (e DownCastExprHir) ExprType() types.Type          { return e.NewType }
func (e UpCastExprHir) ExprType() types.Type            { return e.NewType }
func (e BinaryExprHir) ExprType() types.Type            { return e.Type }

func (OperatorCastExprHir) CastExprHirNode() {}
func (DownCastExprHir) CastExprHirNode()     {}
func (UpCastExprHir) CastExprHirNode()       {}
