package ast

import "fmt"

type TypeNode interface {
	TypeNode()
	TypeName() string
	IsConst() bool
}

type IdentTypeNode struct {
	Name string
	Const bool
}

type ArrayTypeNode struct {
	Size      int
	InnerType TypeNode
	Const bool
}

type SliceTypeNode struct {
	InnerType TypeNode
	Const bool
}

type PointerTypeNode struct {
	InnerType TypeNode
	Const bool
}

func (*IdentTypeNode) TypeNode()   {}
func (*ArrayTypeNode) TypeNode()   {}
func (*SliceTypeNode) TypeNode()   {}
func (*PointerTypeNode) TypeNode() {}

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

func (t *IdentTypeNode) IsConst() bool {
	return t.Const
}
func (t *ArrayTypeNode) IsConst() bool {
	return t.Const
}
func (t *SliceTypeNode) IsConst() bool {
	return t.Const
}
func (t *PointerTypeNode) IsConst() bool {
	return t.Const
}
