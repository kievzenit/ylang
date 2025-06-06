package hir_types

type BoolType struct{
	Const bool
}

func (*BoolType) Type() string {
	return "bool"
}

func (t *BoolType) SameAs(other Type) bool {
	if t.IsConst() != t.IsConst() {
		return false
	}

	if _, ok := other.(*BoolType); ok {
		return true
	}

	return false
}

func (b *BoolType) IsConst() bool {
	return b.Const
}

func (b *BoolType) SetIsConst(isConst bool) {
	b.Const = isConst
}

func (*BoolType) GetMember(name string) (Type, bool) {
	return nil, false
}

func (*BoolType) CanBeImplicitlyCastedTo(t Type) bool {
	intType, ok := t.(*IntType)
	if ok {
		return intType.Bits == 1
	}

	return false
}

func (*BoolType) CanBeExplicitlyCastedTo(t Type) bool {
	_, ok := t.(*IntType)
	return ok
}