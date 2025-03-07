package hir_types

type BoolType struct{}

func (BoolType) Type() string {
	return "bool"
}