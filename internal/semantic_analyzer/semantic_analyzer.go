package semantic_analyzer

import (
	"fmt"

	"github.com/kievzenit/ylang/internal/ast"
	"github.com/kievzenit/ylang/internal/compiler_errors"
	"github.com/kievzenit/ylang/internal/hir"
	types "github.com/kievzenit/ylang/internal/hir/types"
	"github.com/kievzenit/ylang/internal/lexer"
)

type SemanticError struct {
	message string

	fileName string
	line     int
	column   int
}

func (sa *SemanticError) GetMessage() string  { return sa.message }
func (sa *SemanticError) GetFileName() string { return sa.fileName }
func (sa *SemanticError) GetLine() int        { return sa.line }
func (sa *SemanticError) GetColumn() int      { return sa.column }
func (sa *SemanticError) GetLength() int      { return 0 }

func newSemanticError(
	message string,
	fileName string,
	line int,
	column int,
) *SemanticError {
	return &SemanticError{
		message: message,

		fileName: fileName,
		line:     line,
		column:   column,
	}
}

type scope struct {
	parent    *scope
	variables map[string]varDefinition
}

type varDefinition struct {
	Const bool
	Type  types.Type
}

func (s *scope) lookupVar(name string) (varDefinition, bool) {
	t, ok := s.variables[name]
	if ok {
		return t, true
	}

	if s.parent != nil {
		return s.parent.lookupVar(name)
	}

	return varDefinition{}, false
}

func (s *scope) defineVar(name string, v varDefinition) {
	if _, ok := s.variables[name]; ok {
		panic("cannot redefine variable")
	}
	s.variables[name] = v
}

type argDefinition struct {
	Type  types.Type
	Index int
}

type SemanticAnalyzer struct {
	eh              compiler_errors.ErrorHandler
	translationUnit ast.TranslationUnit

	scope    *scope
	funcArgs map[string]argDefinition

	typesMap map[string]types.Type
	funcsMap map[string]types.FunctionType
}

func NewSemanticAnalyzer(
	eh compiler_errors.ErrorHandler,
	translationUnit ast.TranslationUnit) *SemanticAnalyzer {
	return &SemanticAnalyzer{
		eh:              eh,
		translationUnit: translationUnit,

		scope:    &scope{variables: make(map[string]varDefinition)},
		funcArgs: make(map[string]argDefinition),

		typesMap: make(map[string]types.Type),
		funcsMap: make(map[string]types.FunctionType),
	}
}

func (sa *SemanticAnalyzer) enterScope() {
	sa.scope = &scope{parent: sa.scope, variables: make(map[string]varDefinition)}
}

func (sa *SemanticAnalyzer) exitScope() {
	sa.scope = sa.scope.parent
}

func (sa *SemanticAnalyzer) Analyze() *hir.FileHir {
	sa.defineBuiltInTypes()
	sa.scanTranslationUnitForFunctions()

	topStmts := sa.analyzeTranslationUnit(sa.translationUnit)
	funcTypes := make([]types.FunctionType, 0)
	for _, funcType := range sa.funcsMap {
		funcTypes = append(funcTypes, funcType)
	}
	return &hir.FileHir{
		FuncPrototypes: funcTypes,
		Stmts:          topStmts,
	}
}

func (sa *SemanticAnalyzer) defineBuiltInTypes() {
	sa.typesMap["bool"] = &types.BoolType{}

	sa.typesMap["i8"] = &types.IntType{
		Signed: true,
		Bits:   8,
	}
	sa.typesMap["i16"] = &types.IntType{
		Signed: true,
		Bits:   16,
	}
	sa.typesMap["i32"] = &types.IntType{
		Signed: true,
		Bits:   32,
	}
	sa.typesMap["i64"] = &types.IntType{
		Signed: true,
		Bits:   64,
	}
	sa.typesMap["i128"] = &types.IntType{
		Signed: true,
		Bits:   128,
	}
	sa.typesMap["u1"] = &types.IntType{
		Signed: false,
		Bits:   1,
	}
	sa.typesMap["u8"] = &types.IntType{
		Signed: false,
		Bits:   8,
	}
	sa.typesMap["u16"] = &types.IntType{
		Signed: false,
		Bits:   16,
	}
	sa.typesMap["u32"] = &types.IntType{
		Signed: false,
		Bits:   32,
	}
	sa.typesMap["u64"] = &types.IntType{
		Signed: false,
		Bits:   64,
	}
	sa.typesMap["u128"] = &types.IntType{
		Signed: false,
		Bits:   128,
	}

	// sa.typesMap["f16"] = &types.FloatType{
	// 	Bits: 16,
	// }
	sa.typesMap["f32"] = &types.FloatType{
		Bits: 32,
	}
	sa.typesMap["f64"] = &types.FloatType{
		Bits: 64,
	}
	sa.typesMap["f80"] = &types.FloatType{
		Bits: 80,
	}
	sa.typesMap["f128"] = &types.FloatType{
		Bits: 128,
	}

	sa.typesMap["void"] = &types.VoidType{}
}

func (sa *SemanticAnalyzer) scanTranslationUnitForFunctions() {
	// TODO: change func decl stmt to have tokens for return type and for all args
	for _, topStmt := range sa.translationUnit.Stmts {
		switch topStmt.(type) {
		case *ast.FuncDeclStmt:
			funcDeclStmt := topStmt.(*ast.FuncDeclStmt)
			returnType, ok := sa.typesMap[funcDeclStmt.ReturnType]
			if !ok {
				sa.eh.AddError(
					newSemanticError(
						fmt.Sprintf("return type %s not defined", funcDeclStmt.ReturnType),
						"",
						0,
						0,
						// topStmt.StartToken.Metadata.FileName,
						// topStmt.StartToken.Metadata.Line,
						// topStmt.StartToken.Metadata.Column,
					),
				)
			}

			args := make([]types.FunctionArgType, 0)
			for _, arg := range funcDeclStmt.Args {
				argType, ok := sa.typesMap[arg.Type]
				if !ok {
					sa.eh.AddError(
						newSemanticError(
							fmt.Sprintf("argument type %s not defined", arg.Type),
							"",
							0,
							0,
						),
					)
				}
				args = append(args, types.FunctionArgType{
					Name: arg.Name,
					Type: argType,
				})
			}

			sa.funcsMap[funcDeclStmt.Name] = types.FunctionType{
				Name:       funcDeclStmt.Name,
				Args:       args,
				ReturnType: returnType,
				Extern:     funcDeclStmt.Extern,
			}
		default:
			continue
		}
	}
}

func (sa *SemanticAnalyzer) analyzeTranslationUnit(translationUnit ast.TranslationUnit) []hir.TopStmtHir {
	topStmts := make([]hir.TopStmtHir, 0)
	for _, topStmt := range translationUnit.Stmts {
		topStmts = append(topStmts, sa.analyzeTopStmt(topStmt))
	}

	return topStmts
}

func (sa *SemanticAnalyzer) analyzeTopStmt(topStmt ast.TopStmt) hir.TopStmtHir {
	switch topStmt.(type) {
	case *ast.FuncDeclStmt:
		return sa.analyzeFuncDeclStmt(topStmt.(*ast.FuncDeclStmt))
	case *ast.VarDeclStmt:
		return sa.analyzeVarDeclStmt(topStmt.(*ast.VarDeclStmt))
	default:
		panic("not implemented")
	}
}

func (sa *SemanticAnalyzer) analyzeFuncDeclStmt(funcDeclStmt *ast.FuncDeclStmt) *hir.FuncDeclStmtHir {
	sa.enterScope()
	defer sa.exitScope()

	if funcDeclStmt.Extern {
		if funcDeclStmt.Body != nil {
			sa.eh.AddError(
				newSemanticError(
					"extern function cannot have a body",
					funcDeclStmt.StartToken.Metadata.FileName,
					funcDeclStmt.StartToken.Metadata.Line,
					funcDeclStmt.StartToken.Metadata.Column,
				),
			)
			return nil
		}
		return &hir.FuncDeclStmtHir{
			FunctionType: sa.funcsMap[funcDeclStmt.Name],
			Body:         nil,
		}
	}

	functionType := sa.funcsMap[funcDeclStmt.Name]
	for i, arg := range functionType.Args {
		sa.funcArgs[arg.Name] = argDefinition{
			Type:  arg.Type,
			Index: i,
		}
	}

	if funcDeclStmt.Body == nil {
		sa.eh.AddError(
			newSemanticError(
				"function must have a body",
				funcDeclStmt.StartToken.Metadata.FileName,
				funcDeclStmt.StartToken.Metadata.Line,
				funcDeclStmt.StartToken.Metadata.Column,
			),
		)
		return nil
	}
	funcScope := sa.analyzeScopeStmt(funcDeclStmt.Body)

	var lastStmt hir.StmtHir
	if len(funcScope.Stmts) != 0 {
		lastStmt = funcScope.Stmts[len(funcScope.Stmts)-1]
	}
	if _, ok := lastStmt.(*hir.ReturnStmtHir); !ok {
		if functionType.ReturnType != sa.typesMap["void"] {
			sa.eh.AddError(
				newSemanticError(
					"function must return a value",
					funcDeclStmt.StartToken.Metadata.FileName,
					funcDeclStmt.StartToken.Metadata.Line,
					funcDeclStmt.StartToken.Metadata.Column,
				),
			)
			return nil
		}

		funcScope.Stmts = append(funcScope.Stmts, &hir.ReturnStmtHir{Expr: nil})
	}

	sa.funcArgs = make(map[string]argDefinition)

	return &hir.FuncDeclStmtHir{
		FunctionType: functionType,
		Body:         funcScope,
	}
}

func (sa *SemanticAnalyzer) analyzeStmt(stmt ast.Stmt) hir.StmtHir {
	switch stmt.(type) {
	case *ast.ScopeStmt:
		return sa.analyzeScopeStmt(stmt.(*ast.ScopeStmt))
	case *ast.VarDeclStmt:
		return sa.analyzeVarDeclStmt(stmt.(*ast.VarDeclStmt))
	case *ast.ExprStmt:
		return sa.analyzeExprStmt(stmt.(*ast.ExprStmt))
	default:
		panic("not implemented")
	}
}

func (sa *SemanticAnalyzer) analyzeScopeStmt(scopeStmt *ast.ScopeStmt) *hir.ScopeStmtHir {
	sa.enterScope()
	defer sa.exitScope()

	stmts := make([]hir.StmtHir, 0)
	for _, stmt := range scopeStmt.Stmts {
		stmts = append(stmts, sa.analyzeStmt(stmt))
	}

	return &hir.ScopeStmtHir{
		Stmts: stmts,
	}
}

func (sa *SemanticAnalyzer) analyzeVarDeclStmt(varDeclStmt *ast.VarDeclStmt) *hir.VarDeclStmtHir {
	varDef, defined := sa.scope.lookupVar(varDeclStmt.Name)
	if defined {
		sa.eh.AddError(
			newSemanticError(
				fmt.Sprintf("variable %s already defined", varDeclStmt.Name),
				varDeclStmt.StartToken.Metadata.FileName,
				varDeclStmt.StartToken.Metadata.Line,
				varDeclStmt.StartToken.Metadata.Column,
			),
		)
		return nil
	}
	_, defined = sa.funcArgs[varDeclStmt.Name]
	if defined {
		sa.eh.AddError(
			newSemanticError(
				fmt.Sprintf("variable %s shadows function argument", varDeclStmt.Name),
				varDeclStmt.StartToken.Metadata.FileName,
				varDeclStmt.StartToken.Metadata.Line,
				varDeclStmt.StartToken.Metadata.Column,
			),
		)
		return nil
	}

	valueExpr := sa.analyzeExpr(varDeclStmt.Value)
	if hir.IsNilExpr(valueExpr) {
		return nil
	}

	if valueExpr.ExprType() == sa.typesMap["void"] {
		sa.eh.AddError(
			newSemanticError(
				"variable cannot be of type void",
				varDeclStmt.StartToken.Metadata.FileName,
				varDeclStmt.StartToken.Metadata.Line,
				varDeclStmt.StartToken.Metadata.Column,
			),
		)
		return nil
	}

	if varDeclStmt.ExplicitType != "" {
		varExplicitType, ok := sa.typesMap[varDeclStmt.ExplicitType]
		if !ok {
			sa.eh.AddError(
				newSemanticError(
					fmt.Sprintf("type %s not defined", varDeclStmt.ExplicitType),
					varDeclStmt.StartToken.Metadata.FileName,
					varDeclStmt.StartToken.Metadata.Line,
					varDeclStmt.StartToken.Metadata.Column,
				),
			)
			return nil
		}

		valueExpr = sa.tryImplicitCast(valueExpr, varExplicitType)
		if varExplicitType != valueExpr.ExprType() {
			sa.eh.AddError(
				newSemanticError(
					"variable type mismatch",
					varDeclStmt.StartToken.Metadata.FileName,
					varDeclStmt.StartToken.Metadata.Line,
					varDeclStmt.StartToken.Metadata.Column,
				),
			)
			return nil
		}
	}

	sa.scope.defineVar(varDeclStmt.Name, varDefinition{
		Const: varDeclStmt.Const,
		Type:  valueExpr.ExprType(),
	})

	return &hir.VarDeclStmtHir{
		Type:  varDef.Type,
		Name:  varDeclStmt.Name,
		Value: valueExpr,
	}
}

func (sa *SemanticAnalyzer) analyzeExprStmt(exprStmt *ast.ExprStmt) *hir.ExprStmtHir {
	expr := sa.analyzeExpr(exprStmt.Expr)
	return &hir.ExprStmtHir{
		Expr: expr,
	}
}

func (sa *SemanticAnalyzer) analyzeExpr(expr ast.Expr) hir.ExprHir {
	switch expr.(type) {
	case *ast.BinaryExpr:
		return sa.analyzeBinaryExpr(expr.(*ast.BinaryExpr))
	case *ast.AssignExpr:
		return sa.analyzeAssignExpr(expr.(*ast.AssignExpr))
	case *ast.CallExpr:
		return sa.analyzeCallExpr(expr.(*ast.CallExpr))
	case *ast.IdentExpr:
		return sa.analyzeIdentExpr(expr.(*ast.IdentExpr))
	case *ast.IntExpr:
		return sa.analyzeIntExpr(expr.(*ast.IntExpr))
	case *ast.FloatExpr:
		return sa.analyzeFloatExpr(expr.(*ast.FloatExpr))
	case *ast.BoolExpr:
		return sa.analyzeBoolExpr(expr.(*ast.BoolExpr))
	case *ast.CastExpr:
		return sa.analyzeCastExpr(expr.(*ast.CastExpr))
	default:
		panic("not implemented")
	}
}

func (sa *SemanticAnalyzer) tryImplicitCast(exprHir hir.ExprHir, hirType types.Type) hir.ExprHir {
	if exprHir.ExprType() == hirType {
		return exprHir
	}

	if exprHir.ExprType().CanBeImplicitlyCastedTo(hirType) {
		return &hir.UpCastExprHir{
			NewType: hirType,
			OldType: exprHir.ExprType(),
			Expr:    exprHir,
		}
	}

	return exprHir
}

func (sa *SemanticAnalyzer) analyzeBinaryExpr(binaryExpr *ast.BinaryExpr) *hir.BinaryExprHir {
	left := sa.analyzeExpr(binaryExpr.Left)
	right := sa.analyzeExpr(binaryExpr.Right)

	if hir.IsNilExpr(left) || hir.IsNilExpr(right) {
		return nil
	}

	left = sa.tryImplicitCast(left, right.ExprType())
	right = sa.tryImplicitCast(right, left.ExprType())
	if left.ExprType() != right.ExprType() {
		sa.eh.AddError(
			newSemanticError(
				"binary expression operands must be of the same type",
				binaryExpr.Op.Metadata.FileName,
				binaryExpr.Op.Metadata.Line,
				binaryExpr.Op.Metadata.Column,
			),
		)
		return nil
	}

	return &hir.BinaryExprHir{
		Type:  left.ExprType(),
		Left:  left,
		Op:    hir.BinOpFromTokenKind(binaryExpr.Op.Kind),
		Right: right,
	}
}

func (sa *SemanticAnalyzer) analyzeAssignExpr(assignExpr *ast.AssignExpr) *hir.AssignExprHir {
	varDef, defined := sa.scope.lookupVar(assignExpr.Ident.Value)
	if !defined {
		_, defined := sa.funcArgs[assignExpr.Ident.Value]
		if !defined {
			sa.eh.AddError(
				newSemanticError(
					fmt.Sprintf("variable %s not defined", assignExpr.Ident.Value),
					assignExpr.StartToken.Metadata.FileName,
					assignExpr.StartToken.Metadata.Line,
					assignExpr.StartToken.Metadata.Column,
				),
			)
			return nil
		}

		sa.eh.AddError(
			newSemanticError(
				"cannot assign to a function argument",
				assignExpr.Op.Metadata.FileName,
				assignExpr.Op.Metadata.Line,
				assignExpr.Op.Metadata.Column,
			),
		)
		return nil
	}

	expr := sa.analyzeExpr(assignExpr.Value)

	if hir.IsNilExpr(expr) {
		return nil
	}

	if varDef.Const {
		sa.eh.AddError(
			newSemanticError(
				"cannot assign to a constant",
				assignExpr.Op.Metadata.FileName,
				assignExpr.Op.Metadata.Line,
				assignExpr.Op.Metadata.Column,
			),
		)
		return nil
	}

	expr = sa.tryImplicitCast(expr, varDef.Type)
	if varDef.Type != expr.ExprType() {
		sa.eh.AddError(
			newSemanticError(
				"assignment type mismatch",
				assignExpr.Op.Metadata.FileName,
				assignExpr.Op.Metadata.Line,
				assignExpr.Op.Metadata.Column,
			),
		)
		return nil
	}

	switch assignExpr.Op.Kind {
	case lexer.ASSIGN:
		break
	case lexer.ADD_ASSIGN:
		expr = &hir.BinaryExprHir{
			Type:  varDef.Type,
			Left:  &hir.IdentExprHir{Type: varDef.Type, Name: assignExpr.Ident.Value},
			Op:    hir.Add,
			Right: expr,
		}
	case lexer.SUB_ASSIGN:
		expr = &hir.BinaryExprHir{
			Type:  varDef.Type,
			Left:  &hir.IdentExprHir{Type: varDef.Type, Name: assignExpr.Ident.Value},
			Op:    hir.Sub,
			Right: expr,
		}
	case lexer.MUL_ASSIGN:
		expr = &hir.BinaryExprHir{
			Type:  varDef.Type,
			Left:  &hir.IdentExprHir{Type: varDef.Type, Name: assignExpr.Ident.Value},
			Op:    hir.Mul,
			Right: expr,
		}
	case lexer.DIV_ASSIGN:
		expr = &hir.BinaryExprHir{
			Type:  varDef.Type,
			Left:  &hir.IdentExprHir{Type: varDef.Type, Name: assignExpr.Ident.Value},
			Op:    hir.Div,
			Right: expr,
		}
	case lexer.MOD_ASSIGN:
		expr = &hir.BinaryExprHir{
			Type:  varDef.Type,
			Left:  &hir.IdentExprHir{Type: varDef.Type, Name: assignExpr.Ident.Value},
			Op:    hir.Mod,
			Right: expr,
		}
	case lexer.BAND_ASSIGN:
		expr = &hir.BinaryExprHir{
			Type:  varDef.Type,
			Left:  &hir.IdentExprHir{Type: varDef.Type, Name: assignExpr.Ident.Value},
			Op:    hir.Band,
			Right: expr,
		}
	case lexer.BOR_ASSIGN:
		expr = &hir.BinaryExprHir{
			Type:  varDef.Type,
			Left:  &hir.IdentExprHir{Type: varDef.Type, Name: assignExpr.Ident.Value},
			Op:    hir.Bor,
			Right: expr,
		}
	case lexer.XOR_ASSIGN:
		expr = &hir.BinaryExprHir{
			Type:  varDef.Type,
			Left:  &hir.IdentExprHir{Type: varDef.Type, Name: assignExpr.Ident.Value},
			Op:    hir.Xor,
			Right: expr,
		}
	case lexer.SHL_ASSIGN:
		expr = &hir.BinaryExprHir{
			Type:  varDef.Type,
			Left:  &hir.IdentExprHir{Type: varDef.Type, Name: assignExpr.Ident.Value},
			Op:    hir.Shl,
			Right: expr,
		}
	case lexer.SHR_ASSIGN:
		expr = &hir.BinaryExprHir{
			Type:  varDef.Type,
			Left:  &hir.IdentExprHir{Type: varDef.Type, Name: assignExpr.Ident.Value},
			Op:    hir.Shr,
			Right: expr,
		}
	}

	return &hir.AssignExprHir{
		Type:  varDef.Type,
		Value: expr,
		Ident: &hir.IdentExprHir{
			Type: varDef.Type,
			Name: assignExpr.Ident.Value,
		},
	}
}

func (sa *SemanticAnalyzer) analyzeCallExpr(callExpr *ast.CallExpr) *hir.CallExprHir {
	funcType, ok := sa.funcsMap[callExpr.Name]
	if !ok {
		sa.eh.AddError(
			newSemanticError(
				fmt.Sprintf("function %s not defined", callExpr.Name),
				callExpr.StartToken.Metadata.FileName,
				callExpr.StartToken.Metadata.Line,
				callExpr.StartToken.Metadata.Column,
			),
		)
		return nil
	}

	args := make([]hir.ExprHir, 0)
	for i, arg := range callExpr.Args {
		argExpr := sa.analyzeExpr(arg)
		if hir.IsNilExpr(argExpr) {
			continue
		}

		argType := funcType.Args[i].Type
		argExpr = sa.tryImplicitCast(argExpr, argType)
		if argType != argExpr.ExprType() {
			sa.eh.AddError(
				newSemanticError(
					"function call argument type mismatch",
					arg.FirstToken().Metadata.FileName,
					arg.FirstToken().Metadata.Line,
					arg.FirstToken().Metadata.Column,
				),
			)
			continue
		}

		args = append(args, argExpr)
	}

	return &hir.CallExprHir{
		Type: funcType.ReturnType,
		Name: callExpr.Name,
		Args: args,
	}
}

func (sa *SemanticAnalyzer) analyzeIdentExpr(identExpr *ast.IdentExpr) hir.ExprHir {
	varDef, defined := sa.scope.lookupVar(identExpr.Value)
	if !defined {
		arfDef, defined := sa.funcArgs[identExpr.Value]
		if !defined {
			sa.eh.AddError(
				newSemanticError(
					fmt.Sprintf("variable %s not defined", identExpr.Value),
					identExpr.StartToken.Metadata.FileName,
					identExpr.StartToken.Metadata.Line,
					identExpr.StartToken.Metadata.Column,
				),
			)
			return nil
		}

		return &hir.ArgIdentExprHir{
			Type:  arfDef.Type,
			Name:  identExpr.Value,
			Index: arfDef.Index,
		}
	}

	return &hir.IdentExprHir{
		Type: varDef.Type,
		Name: identExpr.Value,
	}
}

func (sa *SemanticAnalyzer) analyzeIntExpr(intExpr *ast.IntExpr) *hir.IntExprHir {
	if intExpr.ExplicitType == "" {
		return &hir.IntExprHir{
			Type:  sa.typesMap["i32"],
			Value: intExpr.Value,
		}
	}

	return &hir.IntExprHir{
		Type:  sa.typesMap[string(intExpr.ExplicitType)],
		Value: intExpr.Value,
	}
}

func (sa *SemanticAnalyzer) analyzeFloatExpr(floatExpr *ast.FloatExpr) *hir.FloatExprHir {
	if floatExpr.ExplicitType == ast.FloatNone {
		return &hir.FloatExprHir{
			Type:  sa.typesMap["f32"],
			Value: floatExpr.Value,
		}
	}

	return &hir.FloatExprHir{
		Type:  sa.typesMap[string(floatExpr.ExplicitType)],
		Value: floatExpr.Value,
	}
}

func (sa *SemanticAnalyzer) analyzeBoolExpr(boolExpr *ast.BoolExpr) *hir.BoolExprHir {
	return &hir.BoolExprHir{
		Type:  sa.typesMap["bool"],
		Value: boolExpr.Value,
	}
}

func (sa *SemanticAnalyzer) analyzeCastExpr(castExpr *ast.CastExpr) hir.ExprHir {
	left := sa.analyzeExpr(castExpr.Left)
	if hir.IsNilExpr(left) {
		return nil
	}

	newType, ok := sa.typesMap[castExpr.CastToType]
	if !ok {
		sa.eh.AddError(
			newSemanticError(
				fmt.Sprintf("type %s not defined", castExpr.CastToType),
				castExpr.StartToken.Metadata.FileName,
				castExpr.StartToken.Metadata.Line,
				castExpr.StartToken.Metadata.Column,
			),
		)
		return nil
	}

	left = sa.tryImplicitCast(left, newType)
	if left.ExprType() == newType {
		return left
	}

	if !left.ExprType().CanBeExplicitlyCastedTo(newType) {
		sa.eh.AddError(
			newSemanticError(
				"cannot cast",
				castExpr.StartToken.Metadata.FileName,
				castExpr.StartToken.Metadata.Line,
				castExpr.StartToken.Metadata.Column,
			),
		)
		return nil
	}

	// TODO: fix this
	newIntType, ok := newType.(*types.IntType)
	if ok {
		leftIntType, ok := left.ExprType().(*types.IntType)
		if ok {
			if leftIntType.Bits < newIntType.Bits {
				return &hir.UpCastExprHir{
					Expr:    left,
					OldType: left.ExprType(),
					NewType: newType,
				}
			}

			return &hir.DownCastExprHir{
				Expr:    left,
				OldType: left.ExprType(),
				NewType: newType,
			}
		}

		_, ok = left.ExprType().(*types.BoolType)
		if ok {
			return &hir.UpCastExprHir{
				Expr:    left,
				OldType: left.ExprType(),
				NewType: newType,
			}
		}
	}

	newFloatType, ok := newType.(*types.FloatType)
	if ok {
		leftFloatType, ok := left.ExprType().(*types.FloatType)
		if ok {
			if leftFloatType.Bits < newFloatType.Bits {
				return &hir.UpCastExprHir{
					Expr:    left,
					OldType: left.ExprType(),
					NewType: newType,
				}
			}
		}

		return &hir.DownCastExprHir{
			Expr:    left,
			OldType: left.ExprType(),
			NewType: newType,
		}
	}

	panic("not implemented")
}
