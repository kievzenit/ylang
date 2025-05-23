package hir_types

import "fmt"

type FloatType struct {
	Bits int
}

func (f *FloatType) Type() string {
	return fmt.Sprintf("f%d", f.Bits)
}

func (f *FloatType) GetMember(name string) (Type, bool) {
	return nil, false
}

func (f *FloatType) CanBeImplicitlyCastedTo(t Type) bool {
	floatType, ok := t.(*FloatType)
	if !ok {
		return false
	}

	return f.Bits < floatType.Bits
}

func (f *FloatType) CanBeExplicitlyCastedTo(t Type) bool {
	// if _, ok := t.(*IntType); ok {
	// 	return true
	// }

	if _, ok := t.(*FloatType); ok {
		return true
	}

	return false
}
