package hir_types

type VoidType struct{}

func (*VoidType) Type() string {
	return "void"
}

func (*VoidType) SameAs(t Type) bool {
	if _, ok := t.(*VoidType); ok {
		return true
	}
	return false
}

func (*VoidType) IsConst() bool {
	return true
}

func (*VoidType) SetIsConst(bool) {
	panic("cannot set is const of a void type")
}

func (*VoidType) GetMember(name string) (Type, bool) {
	return nil, false
}

func (*VoidType) CanBeImplicitlyCastedTo(t Type) bool {
	return false
}

func (*VoidType) CanBeExplicitlyCastedTo(t Type) bool {
	return false
}