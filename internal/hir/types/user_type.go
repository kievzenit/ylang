package hir_types

type UserType struct {
	Name string
	Members map[string]Type
}

func (t *UserType) Type() string {
	return t.Name
}

func (t *UserType) CanBeImplicitlyCastedTo(_ Type) bool {
	return false
}

func (t *UserType) CanBeExplicitlyCastedTo(_ Type) bool {
	return false
}