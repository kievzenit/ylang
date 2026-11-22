package hir_types

type AccessModifier int

const (
	AccessModifierPublic AccessModifier = iota
	AccessModifierPrivate
)

type MemberEntry struct {
	MemberType Type
	ParentType Type
	AccessModifier
}

type FunctionEntry struct {
	*FunctionType
	AccessModifier
}

type UserType struct {
	Name            string
	Members         map[string]MemberEntry
	MemberPositions map[string]int
	MemberFunctions map[string]FunctionEntry
	Const           bool
}

func (t *UserType) Type() string {
	return t.Name
}

// SameAs compares user types nominally. Each declared type has exactly one
// UserType instance, so comparing names is enough. Comparing members
// structurally would recurse forever on self-referential types (next *Node).
func (t *UserType) SameAs(other Type) bool {
	if t.IsConst() != other.IsConst() {
		return false
	}

	userType, ok := other.(*UserType)
	return ok && t.Name == userType.Name
}

func (t *UserType) IsConst() bool {
	return t.Const
}

func (t *UserType) SetIsConst(isConst bool) {
	t.Const = isConst
}

func (t *UserType) GetMember(name string) (MemberEntry, bool) {
	member, ok := t.Members[name]
	return member, ok
}

func (t *UserType) CanBeImplicitlyCastedTo(_ Type) bool {
	return false
}

func (t *UserType) CanBeExplicitlyCastedTo(_ Type) bool {
	return false
}
