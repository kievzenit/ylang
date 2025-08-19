package hir_types

import "fmt"

type ArrayType struct {
	ItemType Type
	Size     int
	Const    bool
}

func (a *ArrayType) Type() string {
	return fmt.Sprintf("[%d]%s", a.Size, a.ItemType.Type())
}

func (a *ArrayType) SameAs(t Type) bool {
	if a.IsConst() != t.IsConst() {
		return false
	}

	if arrayType, ok := t.(*ArrayType); ok {
		return a.Size == arrayType.Size && a.ItemType.SameAs(arrayType.ItemType)
	}

	return false
}

func (a *ArrayType) IsConst() bool {
	return a.Const
}

func (a *ArrayType) SetIsConst(isConst bool) {
	a.Const = isConst
}

func (a *ArrayType) GetMember(name string) (MemberEntry, bool) {
	if name == "length" {
		return MemberEntry{
			MemberType:     &IntType{Signed: true, Bits: 32},
			ParentType:     a,
			AccessModifier: AccessModifierPublic,
		}, true
	}
	return MemberEntry{}, false
}

func (a *ArrayType) CanBeImplicitlyCastedTo(t Type) bool {
	return false
}

func (a *ArrayType) CanBeExplicitlyCastedTo(t Type) bool {
	return false
}
