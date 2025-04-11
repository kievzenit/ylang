package hir_types

type Type interface {
	Type() string
	GetMember(string) (Type, bool)
	CanBeImplicitlyCastedTo(t Type) bool
	CanBeExplicitlyCastedTo(t Type) bool
}
