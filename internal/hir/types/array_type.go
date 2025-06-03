package hir_types

import "fmt"

type ArrayType struct {
	ItemType Type
	Size     int
}

func (a *ArrayType) Type() string {
	return fmt.Sprintf("[%d]%s", a.Size, a.ItemType.Type())
}

func (a *ArrayType) GetMember(name string) (Type, bool) {
	if name == "length" {
		return &IntType{Signed: true, Bits: 32}, true
	}
	return nil, false
}

func (a *ArrayType) CanBeImplicitlyCastedTo(t Type) bool {
	return false
}

func (a *ArrayType) CanBeExplicitlyCastedTo(t Type) bool {
	return false
}