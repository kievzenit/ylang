package hir_types

import "fmt"

type FloatType struct {
	Bits int
}

func (f *FloatType) Type() string {
	return fmt.Sprintf("f%d", f.Bits)
}