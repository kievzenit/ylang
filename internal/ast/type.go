package ast

type TypeNode interface {
	TypeNode()
	TypeName() string
	GetInnerMostType() TypeNode
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
	return t.GetInnerMostType().TypeName()
}
func (t *SliceTypeNode) TypeName() string {
	return t.GetInnerMostType().TypeName()
}
func (t *PointerTypeNode) TypeName() string {
	return t.GetInnerMostType().TypeName()
}

func (t *IdentTypeNode) GetInnerMostType() TypeNode {
	return t
}
func (t *ArrayTypeNode) GetInnerMostType() TypeNode {
	return t.InnerType.GetInnerMostType()
}
func (t *SliceTypeNode) GetInnerMostType() TypeNode {
	return t.InnerType.GetInnerMostType()
}
func (t *PointerTypeNode) GetInnerMostType() TypeNode {
	return t.InnerType.GetInnerMostType()
}
