package hir_types

type Type interface {
	Type() string
	SameAs(t Type) bool
	IsConst() bool
	GetMember(string) (MemberEntry, bool)
	CanBeImplicitlyCastedTo(t Type) bool
	CanBeExplicitlyCastedTo(t Type) bool
}
