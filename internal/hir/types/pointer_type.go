package hir_types

type PointerType struct {
	InnerType Type
	Const     bool
}

func (t *PointerType) Type() string {
	return "*" + t.InnerType.Type()
}

func (t *PointerType) SameAs(other Type) bool {
	if t.IsConst() != other.IsConst() {
		return false
	}

	if pointerType, ok := other.(*PointerType); ok {
		return t.InnerType.SameAs(pointerType.InnerType)
	}
	
	return false
}

func (t *PointerType) IsConst() bool {
	return t.Const
}

func (t *PointerType) SetIsConst(isConst bool) {
	t.Const = isConst
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
