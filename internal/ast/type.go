package ast

import "fmt"

type TypeNode interface {
	TypeNode()
	TypeName() string
}

type IdentTypeNode struct {
	Name string
}

type ArrayTypeNode struct {
	Size      int
	InnerType TypeNode
}

type SliceTypeNode struct {
	InnerType TypeNode
}

type PointerTypeNode struct {
	InnerType TypeNode
}

func (IdentTypeNode) TypeNode()   {}
func (ArrayTypeNode) TypeNode()   {}
func (SliceTypeNode) TypeNode()   {}
func (PointerTypeNode) TypeNode() {}

func (t *IdentTypeNode) TypeName() string {
	return t.Name
}
func (t *ArrayTypeNode) TypeName() string {
	return fmt.Sprintf("[%d]%s", t.Size, t.InnerType.TypeName())
}
func (t *SliceTypeNode) TypeName() string {
	return fmt.Sprintf("[]%s", t.InnerType.TypeName())
}
func (t *PointerTypeNode) TypeName() string {
	return fmt.Sprintf("*%s", t.InnerType.TypeName())
}
