package hir_types

type Type interface {
	Type() string
	CanBeImplicitlyCastedTo(t Type) bool
	CanBeExplicitlyCastedTo(t Type) bool
}
