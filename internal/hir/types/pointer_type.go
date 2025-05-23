package hir_types

type PointerType struct {
	InnerType Type
}

func (t *PointerType) Type() string {
	return "*" + t.InnerType.Type()
}

func (t *PointerType) GetMember(name string) (Type, bool) {
	return nil, false
}

func (t *PointerType) CanBeImplicitlyCastedTo(_ Type) bool {
	return false
}

func (t *PointerType) CanBeExplicitlyCastedTo(_ Type) bool {
	return false
}