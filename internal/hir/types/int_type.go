package hir_types

import "fmt"

type IntType struct {
	Signed bool
	Bits   int
}

func (i *IntType) Type() string {
	if i.Signed {
		return fmt.Sprintf("i%d", i.Bits)
	}
	return fmt.Sprintf("u%d", i.Bits)
}
