package semantic_analyzer

import (
	"fmt"

	"github.com/kievzenit/ylang/internal/ast"
	types "github.com/kievzenit/ylang/internal/hir/types"
)

type TypeResolver struct {
	builtinTypesMap         map[string]types.Type
	customTypesMap          map[string]types.Type
	userTypesMap            map[string]*types.UserType
	forwardDeclaredTypesMap map[string]*ast.TypeDeclStmt
}

func (tr *TypeResolver) defineBuiltInTypes() {
	tr.builtinTypesMap["bool"] = &types.BoolType{}

	tr.builtinTypesMap["i8"] = &types.IntType{
		Signed: true,
		Bits:   8,
	}
	tr.builtinTypesMap["i16"] = &types.IntType{
		Signed: true,
		Bits:   16,
	}
	tr.builtinTypesMap["i32"] = &types.IntType{
		Signed: true,
		Bits:   32,
	}
	tr.builtinTypesMap["i64"] = &types.IntType{
		Signed: true,
		Bits:   64,
	}
	tr.builtinTypesMap["i128"] = &types.IntType{
		Signed: true,
		Bits:   128,
	}
	tr.builtinTypesMap["u1"] = &types.IntType{
		Signed: false,
		Bits:   1,
	}
	tr.builtinTypesMap["u8"] = &types.IntType{
		Signed: false,
		Bits:   8,
	}
	tr.builtinTypesMap["u16"] = &types.IntType{
		Signed: false,
		Bits:   16,
	}
	tr.builtinTypesMap["u32"] = &types.IntType{
		Signed: false,
		Bits:   32,
	}
	tr.builtinTypesMap["u64"] = &types.IntType{
		Signed: false,
		Bits:   64,
	}
	tr.builtinTypesMap["u128"] = &types.IntType{
		Signed: false,
		Bits:   128,
	}

	// sa.typesMap["f16"] = &types.FloatType{
	// 	Bits: 16,
	// }
	tr.builtinTypesMap["f32"] = &types.FloatType{
		Bits: 32,
	}
	tr.builtinTypesMap["f64"] = &types.FloatType{
		Bits: 64,
	}
	tr.builtinTypesMap["f80"] = &types.FloatType{
		Bits: 80,
	}
	tr.builtinTypesMap["f128"] = &types.FloatType{
		Bits: 128,
	}

	tr.builtinTypesMap["void"] = &types.VoidType{}
}

func NewTypeResolver() *TypeResolver {
	tr := &TypeResolver{
		builtinTypesMap:         make(map[string]types.Type),
		customTypesMap:          make(map[string]types.Type),
		userTypesMap:            make(map[string]*types.UserType),
		forwardDeclaredTypesMap: make(map[string]*ast.TypeDeclStmt),
	}
	tr.defineBuiltInTypes()
	return tr
}

func (tr *TypeResolver) GetUserTypes() map[string]*types.UserType {
	return tr.userTypesMap
}

func (tr *TypeResolver) AddUserType(name string, userType *types.UserType) {
	if _, exists := tr.userTypesMap[name]; exists {
		panic("user type already exists: " + name)
	}
	tr.userTypesMap[name] = userType
}

func (tr *TypeResolver) GetUserType(name string) (*types.UserType, bool) {
	t, ok := tr.userTypesMap[name]
	return t, ok
}

func (tr *TypeResolver) GetBuiltInType(name string) (types.Type, bool) {
	t, ok := tr.builtinTypesMap[name]
	return t, ok
}

func (tr *TypeResolver) GetType(astType ast.TypeNode) (types.Type, bool) {
	if arrayType, ok := astType.(*ast.ArrayTypeNode); ok {
		innerType, ok := tr.GetType(arrayType.InnerType)
		if !ok {
			return nil, false
		}
		return &types.ArrayType{
			ItemType: innerType,
			Size:     arrayType.Size,
		}, true
	}

	if ptrType, ok := astType.(*ast.PointerTypeNode); ok {
		innerType, ok := tr.GetType(ptrType.InnerType)
		if !ok {
			return nil, false
		}
		return &types.PointerType{
			InnerType: innerType,
		}, true
	}

	if t, ok := tr.builtinTypesMap[astType.TypeName()]; ok {
		return t, true
	}

	if t, ok := tr.userTypesMap[astType.TypeName()]; ok {
		return t, true
	}

	return nil, false
}

func (tr *TypeResolver) IsBuiltInType(name string) bool {
	_, ok := tr.builtinTypesMap[name]
	return ok
}

func (tr *TypeResolver) VoidType() types.Type {
	return tr.builtinTypesMap["void"]
}

func (tr *TypeResolver) BoolType() types.Type {
	return tr.builtinTypesMap["bool"]
}

func (tr *TypeResolver) IntType(bits int) types.Type {
	switch {
	case bits == 8:
		return tr.builtinTypesMap["i8"]
	case bits == 16:
		return tr.builtinTypesMap["i16"]
	case bits == 32:
		return tr.builtinTypesMap["i32"]
	case bits == 64:
		return tr.builtinTypesMap["i64"]
	case bits == 128:
		return tr.builtinTypesMap["i128"]
	default:
		panic("invalid int type bits: " + fmt.Sprint(bits))
	}
}

func (tr *TypeResolver) UIntType(bits int) types.Type {
	switch {
	case bits == 1:
		return tr.builtinTypesMap["u1"]
	case bits == 8:
		return tr.builtinTypesMap["u8"]
	case bits == 16:
		return tr.builtinTypesMap["u16"]
	case bits == 32:
		return tr.builtinTypesMap["u32"]
	case bits == 64:
		return tr.builtinTypesMap["u64"]
	case bits == 128:
		return tr.builtinTypesMap["u128"]
	default:
		panic("invalid uint type bits: " + fmt.Sprint(bits))
	}
}

func (tr *TypeResolver) FloatType(bits int) types.Type {
	switch {
	// case bits == 16:
	// 	return tr.builtinTypesMap["f16"]
	case bits == 32:
		return tr.builtinTypesMap["f32"]
	case bits == 64:
		return tr.builtinTypesMap["f64"]
	case bits == 80:
		return tr.builtinTypesMap["f80"]
	case bits == 128:
		return tr.builtinTypesMap["f128"]
	default:
		panic("invalid float type bits: " + fmt.Sprint(bits))
	}
}
