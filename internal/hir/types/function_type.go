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

func (FunctionType) GetMember(name string) (Type, bool) {
	return nil, false
}

func (FunctionType) CanBeImplicitlyCastedTo(t Type) bool {
	return false
}

func (FunctionType) CanBeExplicitlyCastedTo(t Type) bool {
	return false
}
