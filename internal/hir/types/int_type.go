package hir_types

import "fmt"

type IntType struct {
	Signed bool
	Bits   int
	Const  bool
}

func (i *IntType) Type() string {
	if i.Signed {
		return fmt.Sprintf("i%d", i.Bits)
	}
	return fmt.Sprintf("u%d", i.Bits)
}

func (i *IntType) SameAs(t Type) bool {
	if i.IsConst() != t.IsConst() {
		return false
	}

	if intType, ok := t.(*IntType); ok {
		return i.Signed == intType.Signed && i.Bits == intType.Bits
	}

	return false
}

func (i *IntType) IsConst() bool {
	return i.Const
}

func (i *IntType) SetIsConst(isConst bool) {
	i.Const = isConst
}

func (*IntType) GetMember(name string) (Type, bool) {
	return nil, false
}

func (i *IntType) CanBeImplicitlyCastedTo(t Type) bool {
	intType, ok := t.(*IntType)
	if !ok {
		return false
	}

	if i.Signed != intType.Signed {
		return false
	}

	return i.Bits < intType.Bits
}

func (i *IntType) CanBeExplicitlyCastedTo(t Type) bool {
	if _, ok := t.(*IntType); ok {
		return true
	}

	// if _, ok := t.(*FloatType); ok {
	// 	return true
	// }

	if _, ok := t.(*BoolType); ok && i.Bits == 1 {
		return true
	}

	return false
}
