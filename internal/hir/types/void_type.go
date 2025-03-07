package hir_types

type VoidType struct{}

func (VoidType) Type() string {
	return "void"
}