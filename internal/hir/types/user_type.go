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

func (t *UserType) SameAs(other Type) bool {
	if t.IsConst() != other.IsConst() {
		return false
	}

	if userType, ok := other.(*UserType); ok {
		if t.Name != userType.Name {
			return false
		}
		if len(t.Members) != len(userType.Members) {
			return false
		}
		for name, memberEntry := range t.Members {
			if otherMemberEntry, exists := userType.Members[name]; !exists ||
				(!memberEntry.MemberType.SameAs(otherMemberEntry.MemberType) &&
					memberEntry.AccessModifier != otherMemberEntry.AccessModifier) {
				return false
			}
		}
		return true
	}
	return false
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
