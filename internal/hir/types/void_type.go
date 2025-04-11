package hir_types

type VoidType struct{}

func (VoidType) Type() string {
	return "void"
}

func (VoidType) GetMember(name string) (Type, bool) {
	return nil, false
}

func (VoidType) CanBeImplicitlyCastedTo(t Type) bool {
	return false
}

func (VoidType) CanBeExplicitlyCastedTo(t Type) bool {
	return false
}