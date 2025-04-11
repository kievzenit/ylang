package hir_types

type BoolType struct{}

func (BoolType) Type() string {
	return "bool"
}

func (BoolType) GetMember(name string) (Type, bool) {
	return nil, false
}

func (BoolType) CanBeImplicitlyCastedTo(t Type) bool {
	intType, ok := t.(*IntType)
	if ok {
		return intType.Bits == 1
	}

	return false
}

func (BoolType) CanBeExplicitlyCastedTo(t Type) bool {
	_, ok := t.(*IntType)
	return ok
}