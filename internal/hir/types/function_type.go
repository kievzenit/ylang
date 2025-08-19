package hir_types

import "fmt"

type FunctionType struct {
	Name       string
	Args       []FunctionArgType
	ReturnType Type
	Extern     bool
}

type FunctionArgType struct {
	Name string
	Type
}

func (f *FunctionType) Type() string {
	return fmt.Sprintf("fun(%s)", f.Name)
}

func (f *FunctionType) SameAs(t Type) bool {
	if funcType, ok := t.(*FunctionType); ok {
		if f.Extern != funcType.Extern || f.Name != funcType.Name || len(f.Args) != len(funcType.Args) {
			return false
		}
		if !f.ReturnType.SameAs(funcType.ReturnType) {
			return false
		}
		for i, arg := range f.Args {
			if !arg.Type.SameAs(funcType.Args[i].Type) {
				return false
			}
		}
		return true
	}
	return false
}

func (f *FunctionType) IsConst() bool {
	return true
}

func (*FunctionType) SetIsConst(bool) {
	panic("cannot set is const for a function type")
}

func (*FunctionType) GetMember(name string) (MemberEntry, bool) {
	return MemberEntry{}, false
}

func (*FunctionType) CanBeImplicitlyCastedTo(t Type) bool {
	return false
}

func (*FunctionType) CanBeExplicitlyCastedTo(t Type) bool {
	return false
}
