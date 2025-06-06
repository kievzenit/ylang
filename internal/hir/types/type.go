package hir_types

type Type interface {
	Type() string
	SameAs(t Type) bool
	IsConst() bool
	SetIsConst(bool)
	GetMember(string) (Type, bool)
	CanBeImplicitlyCastedTo(t Type) bool
	CanBeExplicitlyCastedTo(t Type) bool
}
