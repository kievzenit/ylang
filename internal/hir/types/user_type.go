package hir_types

type UserType struct {
	Name            string
	Members         map[string]Type
	MemberPositions map[string]int
}

func (t *UserType) Type() string {
	return t.Name
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
