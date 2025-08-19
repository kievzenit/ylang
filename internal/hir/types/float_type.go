package hir_types

import "fmt"

type FloatType struct {
	Bits  int
	Const bool
}

func (f *FloatType) Type() string {
	return fmt.Sprintf("f%d", f.Bits)
}

func (f *FloatType) SameAs(t Type) bool {
	if f.IsConst() != t.IsConst() {
		return false
	}

	if floatType, ok := t.(*FloatType); ok {
		return f.Bits == floatType.Bits
	}
	
	return false
}

func (f *FloatType) IsConst() bool {
	return f.Const
}

func (f *FloatType) SetIsConst(isConst bool) {
	f.Const = isConst
}

func (f *FloatType) GetMember(name string) (MemberEntry, bool) {
	return MemberEntry{}, false
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
