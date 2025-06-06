package hir_types

type UserType struct {
	Name            string
	Members         map[string]Type
	MemberPositions map[string]int
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
		for name, member := range t.Members {
			if otherMember, exists := userType.Members[name]; !exists || !member.SameAs(otherMember) {
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

func (t *UserType) GetMember(name string) (Type, bool) {
	member, ok := t.Members[name]
	return member, ok
}

func (t *UserType) CanBeImplicitlyCastedTo(_ Type) bool {
	return false
}

func (t *UserType) CanBeExplicitlyCastedTo(_ Type) bool {
	return false
}
