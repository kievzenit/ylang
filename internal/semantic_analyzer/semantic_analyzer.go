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

	scope       *scope
	funcArgs    map[string]argDefinition
	funcRetType types.Type

	forwardTypeDeclarations  map[string]*ast.TypeDeclStmt
	currentlyProcessingTypes map[string]struct{}

	builtinTypesMap map[string]types.Type
	customTypesMap  map[string]*types.UserType
	funcsMap        map[string]types.FunctionType

	loopDepth int
}

func (sa *SemanticAnalyzer) enterLoop() {
	sa.loopDepth++
}

func (sa *SemanticAnalyzer) exitLoop() {
	sa.loopDepth--
}

func (sa *SemanticAnalyzer) inLoop() bool {
	return sa.loopDepth > 0
}

func (sa *SemanticAnalyzer) getType(name string) (types.Type, bool) {
	if t, ok := sa.builtinTypesMap[name]; ok {
		return t, true
	}

	if t, ok := sa.customTypesMap[name]; ok {
		return t, true
	}

	return nil, false
}

func NewSemanticAnalyzer(
	eh compiler_errors.ErrorHandler,
	translationUnit ast.TranslationUnit) *SemanticAnalyzer {
	return &SemanticAnalyzer{
		eh:              eh,
		translationUnit: translationUnit,

		scope:    &scope{variables: make(map[string]varDefinition)},
		funcArgs: make(map[string]argDefinition),

		builtinTypesMap: make(map[string]types.Type),
		customTypesMap:  make(map[string]*types.UserType),
		funcsMap:        make(map[string]types.FunctionType),

		forwardTypeDeclarations:  make(map[string]*ast.TypeDeclStmt),
		currentlyProcessingTypes: make(map[string]struct{}),
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
	sa.scanTranslationUnitForTypeDeclStmts()
	sa.scanTranslationUnitForFuncDeclStmts()

	topStmts := sa.analyzeTranslationUnit(sa.translationUnit)
	funcTypes := make([]types.FunctionType, 0)
	for _, funcType := range sa.funcsMap {
		funcTypes = append(funcTypes, funcType)
	}
	return &hir.FileHir{
		FuncPrototypes: funcTypes,
		Types:          sa.customTypesMap,
		Stmts:          topStmts,
	}
}

func (sa *SemanticAnalyzer) defineBuiltInTypes() {
	sa.builtinTypesMap["bool"] = &types.BoolType{}

	sa.builtinTypesMap["i8"] = &types.IntType{
		Signed: true,
		Bits:   8,
	}
	sa.builtinTypesMap["i16"] = &types.IntType{
		Signed: true,
		Bits:   16,
	}
	sa.builtinTypesMap["i32"] = &types.IntType{
		Signed: true,
		Bits:   32,
	}
	sa.builtinTypesMap["i64"] = &types.IntType{
		Signed: true,
		Bits:   64,
	}
	sa.builtinTypesMap["i128"] = &types.IntType{
		Signed: true,
		Bits:   128,
	}
	sa.builtinTypesMap["u1"] = &types.IntType{
		Signed: false,
		Bits:   1,
	}
	sa.builtinTypesMap["u8"] = &types.IntType{
		Signed: false,
		Bits:   8,
	}
	sa.builtinTypesMap["u16"] = &types.IntType{
		Signed: false,
		Bits:   16,
	}
	sa.builtinTypesMap["u32"] = &types.IntType{
		Signed: false,
		Bits:   32,
	}
	sa.builtinTypesMap["u64"] = &types.IntType{
		Signed: false,
		Bits:   64,
	}
	sa.builtinTypesMap["u128"] = &types.IntType{
		Signed: false,
		Bits:   128,
	}

	// sa.typesMap["f16"] = &types.FloatType{
	// 	Bits: 16,
	// }
	sa.builtinTypesMap["f32"] = &types.FloatType{
		Bits: 32,
	}
	sa.builtinTypesMap["f64"] = &types.FloatType{
		Bits: 64,
	}
	sa.builtinTypesMap["f80"] = &types.FloatType{
		Bits: 80,
	}
	sa.builtinTypesMap["f128"] = &types.FloatType{
		Bits: 128,
	}

	sa.builtinTypesMap["void"] = &types.VoidType{}
}

func (sa *SemanticAnalyzer) scanTranslationUnitForTypeDeclStmts() {
	sa.forwardTypeDeclarations = make(map[string]*ast.TypeDeclStmt)

	for _, topStmt := range sa.translationUnit.Stmts {
		typeDeclStmt, ok := topStmt.(*ast.TypeDeclStmt)
		if !ok {
			continue
		}

		_, exists := sa.builtinTypesMap[typeDeclStmt.Name]
		if exists {
			sa.eh.AddError(
				newSemanticError(
					fmt.Sprintf("cannot define type %s, because builtin type with this name exist", typeDeclStmt.Name),
					typeDeclStmt.StartToken.Metadata.FileName,
					typeDeclStmt.StartToken.Metadata.Line,
					typeDeclStmt.StartToken.Metadata.Column,
				),
			)
			continue
		}

		_, exists = sa.forwardTypeDeclarations[typeDeclStmt.Name]
		if exists {
			sa.eh.AddError(
				newSemanticError(
					fmt.Sprintf("type %s already defined", typeDeclStmt.Name),
					typeDeclStmt.StartToken.Metadata.FileName,
					typeDeclStmt.StartToken.Metadata.Line,
					typeDeclStmt.StartToken.Metadata.Column,
				),
			)
			continue
		}

		sa.forwardTypeDeclarations[typeDeclStmt.Name] = typeDeclStmt
	}

	for _, typeDeclStmt := range sa.forwardTypeDeclarations {
		sa.analyzeTypeDeclStmt(typeDeclStmt)
	}
}

func (sa *SemanticAnalyzer) scanTranslationUnitForFuncDeclStmts() {
	for _, topStmt := range sa.translationUnit.Stmts {
		funcDeclStmt, ok := topStmt.(*ast.FuncDeclStmt)
		if !ok {
			continue
		}

		returnType, ok := sa.getType(funcDeclStmt.ReturnType)
		if !ok {
			sa.eh.AddError(
				newSemanticError(
					fmt.Sprintf("return type %s not defined", funcDeclStmt.ReturnType),
					funcDeclStmt.StartToken.Metadata.FileName,
					funcDeclStmt.StartToken.Metadata.Line,
					funcDeclStmt.StartToken.Metadata.Column,
				),
			)
		}

		args := make([]types.FunctionArgType, 0)
		for _, arg := range funcDeclStmt.Args {
			argType, ok := sa.getType(arg.Type)
			if !ok {
				sa.eh.AddError(
					newSemanticError(
						fmt.Sprintf("argument type %s not defined", arg.Type),
						funcDeclStmt.StartToken.Metadata.FileName,
						funcDeclStmt.StartToken.Metadata.Line,
						funcDeclStmt.StartToken.Metadata.Column,
					),
				)
			}

			if argType == sa.builtinTypesMap["void"] {
				sa.eh.AddError(
					newSemanticError(
						"argument cannot be of type void",
						funcDeclStmt.StartToken.Metadata.FileName,
						funcDeclStmt.StartToken.Metadata.Line,
						funcDeclStmt.StartToken.Metadata.Column,
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
	}
}

func (sa *SemanticAnalyzer) analyzeTypeDeclStmt(typeDeclStmt *ast.TypeDeclStmt) types.Type {
	if userType, ok := sa.customTypesMap[typeDeclStmt.Name]; ok {
		return userType
	}

	sa.currentlyProcessingTypes[typeDeclStmt.Name] = struct{}{}
	members := make(map[string]types.Type)
	memberPositions := make(map[string]int)

	for position, member := range typeDeclStmt.Members {
		memberType, ok := sa.getType(member.Type)
		switch {
		case ok && memberType == sa.builtinTypesMap["void"]:
			sa.eh.AddError(
				newSemanticError(
					"member cannot be of type void",
					typeDeclStmt.StartToken.Metadata.FileName,
					typeDeclStmt.StartToken.Metadata.Line,
					typeDeclStmt.StartToken.Metadata.Column,
				),
			)
			continue
		case ok && memberType != sa.builtinTypesMap["void"]:
			members[member.Name] = memberType
			memberPositions[member.Name] = position
			continue
		}

		memberTypeDeclStmt, ok := sa.forwardTypeDeclarations[member.Type]
		if !ok {
			sa.eh.AddError(
				newSemanticError(
					fmt.Sprintf("type %s not defined", member.Type),
					typeDeclStmt.StartToken.Metadata.FileName,
					typeDeclStmt.StartToken.Metadata.Line,
					typeDeclStmt.StartToken.Metadata.Column,
				),
			)
			continue
		}

		if _, ok := sa.currentlyProcessingTypes[member.Type]; ok {
			for typeName := range sa.currentlyProcessingTypes {
				typeDeclStmt, _ := sa.forwardTypeDeclarations[typeName]
				sa.eh.AddError(
					newSemanticError(
						fmt.Sprintf("type %s is recursively defined", typeDeclStmt.Name),
						typeDeclStmt.StartToken.Metadata.FileName,
						typeDeclStmt.StartToken.Metadata.Line,
						typeDeclStmt.StartToken.Metadata.Column,
					),
				)
			}
			continue
		}

		memberType = sa.analyzeTypeDeclStmt(memberTypeDeclStmt)
		members[member.Name] = memberType
		memberPositions[member.Name] = position
	}

	userType := &types.UserType{
		Name:            typeDeclStmt.Name,
		Members:         members,
		MemberPositions: memberPositions,
	}
	sa.customTypesMap[userType.Name] = userType
	delete(sa.currentlyProcessingTypes, typeDeclStmt.Name)
	return userType
}

func (sa *SemanticAnalyzer) analyzeTranslationUnit(translationUnit ast.TranslationUnit) []hir.TopStmtHir {
	topStmts := make([]hir.TopStmtHir, 0)
	for _, topStmt := range translationUnit.Stmts {
		topStmtHir := sa.analyzeTopStmt(topStmt)
		if topStmtHir == nil {
			continue
		}

		topStmts = append(topStmts, topStmtHir)
	}

	return topStmts
}

func (sa *SemanticAnalyzer) analyzeTopStmt(topStmt ast.TopStmt) hir.TopStmtHir {
	switch topStmt.(type) {
	case *ast.FuncDeclStmt:
		return sa.analyzeFuncDeclStmt(topStmt.(*ast.FuncDeclStmt))
	case *ast.VarDeclStmt:
		return sa.analyzeVarDeclStmt(topStmt.(*ast.VarDeclStmt))
	case *ast.TypeDeclStmt:
		return nil
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
	sa.funcRetType = functionType.ReturnType
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
		if functionType.ReturnType != sa.builtinTypesMap["void"] {
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

	sa.funcRetType = nil
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
	case *ast.IfStmt:
		return sa.analyzeIfStmt(stmt.(*ast.IfStmt))
	case *ast.WhileStmt:
		return sa.analyzeWhileStmt(stmt.(*ast.WhileStmt))
	case *ast.DoWhileStmt:
		return sa.analyzeDoWhileStmt(stmt.(*ast.DoWhileStmt))
	case *ast.LoopStmt:
		return sa.analyzeLoopStmt(stmt.(*ast.LoopStmt))
	case *ast.ForStmt:
		return sa.analyzeForStmt(stmt.(*ast.ForStmt))
	case *ast.ReturnStmt:
		return sa.analyzeReturnStmt(stmt.(*ast.ReturnStmt))
	case *ast.ContinueStmt:
		return sa.analyzeContinueStmt(stmt.(*ast.ContinueStmt))
	case *ast.BreakStmt:
		return sa.analyzeBreakStmt(stmt.(*ast.BreakStmt))
	case *ast.BreakAllStmt:
		return sa.analyzeBreakAllStmt(stmt.(*ast.BreakAllStmt))
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
	skip := false

	for _, stmt := range scopeStmt.Stmts {
		stmtHir := sa.analyzeStmt(stmt)
		if !skip {
			stmts = append(stmts, stmtHir)
		}

		if _, ok := stmt.(ast.ControlFlowStmt); ok {
			skip = true
		}
	}

	return &hir.ScopeStmtHir{
		Stmts: stmts,
	}
}

func (sa *SemanticAnalyzer) analyzeIfStmt(ifStmt *ast.IfStmt) *hir.IfStmtHir {
	failed := false

	condExpr := sa.analyzeExpr(ifStmt.Cond)
	if hir.IsNilExpr(condExpr) {
		return nil
	}

	condExpr = sa.tryImplicitCast(condExpr, sa.builtinTypesMap["bool"])
	if condExpr.ExprType() != sa.builtinTypesMap["bool"] {
		failed = true
		sa.eh.AddError(
			newSemanticError(
				"if condition must be of type bool",
				ifStmt.StartToken.Metadata.FileName,
				ifStmt.StartToken.Metadata.Line,
				ifStmt.StartToken.Metadata.Column,
			),
		)
	}

	ifBody := sa.analyzeScopeStmt(ifStmt.Body)

	var topElseIf *hir.IfStmtHir
	var lastElseIf *hir.IfStmtHir
	for _, elseIfStmt := range ifStmt.ElseIf {
		elseIfCond := sa.analyzeExpr(elseIfStmt.Cond)
		if hir.IsNilExpr(elseIfCond) {
			failed = true
			continue
		}

		elseIfCond = sa.tryImplicitCast(elseIfCond, sa.builtinTypesMap["bool"])
		if elseIfCond.ExprType() != sa.builtinTypesMap["bool"] {
			failed = true
			sa.eh.AddError(
				newSemanticError(
					"if condition must be of type bool",
					elseIfStmt.StartToken.Metadata.FileName,
					elseIfStmt.StartToken.Metadata.Line,
					elseIfStmt.StartToken.Metadata.Column,
				),
			)
			continue
		}

		elseIfBody := sa.analyzeScopeStmt(elseIfStmt.Body)

		if lastElseIf == nil {
			lastElseIf = &hir.IfStmtHir{
				Cond: elseIfCond,
				Body: elseIfBody,
				Else: nil,
			}
			topElseIf = lastElseIf
			continue
		}

		lastElseIf.Else = &hir.IfStmtHir{
			Cond: elseIfCond,
			Body: elseIfBody,
			Else: nil,
		}

		lastElseIf = lastElseIf.Else.(*hir.IfStmtHir)
	}

	var elseStmt hir.StmtHir
	if topElseIf == nil {
		elseStmt = nil
	} else {
		elseStmt = topElseIf
	}
	switch {
	case ifStmt.Else != nil && lastElseIf != nil:
		lastElseIf.Else = sa.analyzeScopeStmt(ifStmt.Else)
	case ifStmt.Else != nil && lastElseIf == nil:
		elseStmt = sa.analyzeScopeStmt(ifStmt.Else)
	}

	if failed {
		return nil
	}

	return &hir.IfStmtHir{
		Cond: condExpr,
		Body: ifBody,
		Else: elseStmt,
	}
}

func (sa *SemanticAnalyzer) analyzeWhileStmt(whileStmt *ast.WhileStmt) *hir.WhileStmtHir {
	sa.enterLoop()
	defer sa.exitLoop()

	condExpr := sa.analyzeExpr(whileStmt.Cond)
	if hir.IsNilExpr(condExpr) {
		return nil
	}

	condExpr = sa.tryImplicitCast(condExpr, sa.builtinTypesMap["bool"])
	if condExpr.ExprType() != sa.builtinTypesMap["bool"] {
		sa.eh.AddError(
			newSemanticError(
				"while condition must be of type bool",
				whileStmt.StartToken.Metadata.FileName,
				whileStmt.StartToken.Metadata.Line,
				whileStmt.StartToken.Metadata.Column,
			),
		)
		return nil
	}

	body := sa.analyzeScopeStmt(whileStmt.Body)

	return &hir.WhileStmtHir{
		Cond: condExpr,
		Body: body,
	}
}

func (sa *SemanticAnalyzer) analyzeDoWhileStmt(doWhileStmt *ast.DoWhileStmt) *hir.DoWhileStmtHir {
	sa.enterLoop()
	defer sa.exitLoop()

	body := sa.analyzeScopeStmt(doWhileStmt.Body)

	condExpr := sa.analyzeExpr(doWhileStmt.Cond)
	if hir.IsNilExpr(condExpr) {
		return nil
	}

	condExpr = sa.tryImplicitCast(condExpr, sa.builtinTypesMap["bool"])
	if condExpr.ExprType() != sa.builtinTypesMap["bool"] {
		sa.eh.AddError(
			newSemanticError(
				"do while condition must be of type bool",
				doWhileStmt.StartToken.Metadata.FileName,
				doWhileStmt.StartToken.Metadata.Line,
				doWhileStmt.StartToken.Metadata.Column,
			),
		)
		return nil
	}

	return &hir.DoWhileStmtHir{
		Cond: condExpr,
		Body: body,
	}
}

func (sa *SemanticAnalyzer) analyzeLoopStmt(loopStmt *ast.LoopStmt) *hir.LoopStmtHir {
	sa.enterLoop()
	defer sa.exitLoop()

	body := sa.analyzeScopeStmt(loopStmt.Body)

	return &hir.LoopStmtHir{
		Body: body,
	}
}

func (sa *SemanticAnalyzer) analyzeForStmt(forStmt *ast.ForStmt) *hir.ForStmtHir {
	sa.enterLoop()
	defer sa.exitLoop()
	sa.enterScope()
	defer sa.exitScope()

	failed := false

	init := make([]hir.StmtHir, 0)
	for _, stmt := range forStmt.Init {
		stmtHir := sa.analyzeStmt(stmt)
		if stmtHir == nil {
			failed = true
			continue
		}

		init = append(init, stmtHir)
	}

	condExpr := sa.analyzeExpr(forStmt.Cond)
	if hir.IsNilExpr(condExpr) {
		failed = true
	}

	if !hir.IsNilExpr(condExpr) {
		condExpr = sa.tryImplicitCast(condExpr, sa.builtinTypesMap["bool"])
		if condExpr.ExprType() != sa.builtinTypesMap["bool"] {
			sa.eh.AddError(
				newSemanticError(
					"for condition must be of type bool",
					forStmt.StartToken.Metadata.FileName,
					forStmt.StartToken.Metadata.Line,
					forStmt.StartToken.Metadata.Column,
				),
			)
			failed = true
		}
	}

	post := make([]hir.ExprHir, 0)
	for _, expr := range forStmt.Post {
		exprHir := sa.analyzeExpr(expr)
		if exprHir == nil {
			failed = true
			continue
		}

		post = append(post, exprHir)
	}

	body := sa.analyzeScopeStmt(forStmt.Body)
	if body == nil {
		failed = true
	}

	if failed {
		return nil
	}

	return &hir.ForStmtHir{
		Init: init,
		Cond: condExpr,
		Post: post,
		Body: body,
	}
}

func (sa *SemanticAnalyzer) analyzeReturnStmt(returnStmt *ast.ReturnStmt) *hir.ReturnStmtHir {
	if returnStmt.Expr == nil {
		if sa.funcRetType != sa.builtinTypesMap["void"] {
			sa.eh.AddError(
				newSemanticError(
					"function must return a value",
					returnStmt.StartToken.Metadata.FileName,
					returnStmt.StartToken.Metadata.Line,
					returnStmt.StartToken.Metadata.Column,
				),
			)
			return nil
		}

		return &hir.ReturnStmtHir{
			Expr: nil,
		}
	}

	valueExpr := sa.analyzeExpr(returnStmt.Expr)
	if hir.IsNilExpr(valueExpr) {
		return nil
	}

	valueExpr = sa.tryImplicitCast(valueExpr, sa.funcRetType)
	if sa.funcRetType != valueExpr.ExprType() {
		sa.eh.AddError(
			newSemanticError(
				"function return type mismatch",
				returnStmt.StartToken.Metadata.FileName,
				returnStmt.StartToken.Metadata.Line,
				returnStmt.StartToken.Metadata.Column,
			),
		)
		return nil
	}

	return &hir.ReturnStmtHir{
		Expr: valueExpr,
	}
}

func (sa *SemanticAnalyzer) analyzeContinueStmt(continueStmt *ast.ContinueStmt) *hir.ContinueStmtHir {
	if !sa.inLoop() {
		sa.eh.AddError(
			newSemanticError(
				"continue statement must be inside a loop",
				continueStmt.StartToken.Metadata.FileName,
				continueStmt.StartToken.Metadata.Line,
				continueStmt.StartToken.Metadata.Column,
			),
		)
		return nil
	}

	return &hir.ContinueStmtHir{}
}

func (sa *SemanticAnalyzer) analyzeBreakStmt(breakStmt *ast.BreakStmt) *hir.BreakStmtHir {
	if !sa.inLoop() {
		sa.eh.AddError(
			newSemanticError(
				"break statement must be inside a loop",
				breakStmt.StartToken.Metadata.FileName,
				breakStmt.StartToken.Metadata.Line,
				breakStmt.StartToken.Metadata.Column,
			),
		)
		return nil
	}

	return &hir.BreakStmtHir{}
}

func (sa *SemanticAnalyzer) analyzeBreakAllStmt(breakAllStmt *ast.BreakAllStmt) *hir.BreakAllStmtHir {
	if !sa.inLoop() {
		sa.eh.AddError(
			newSemanticError(
				"breakall statement must be inside a loop",
				breakAllStmt.StartToken.Metadata.FileName,
				breakAllStmt.StartToken.Metadata.Line,
				breakAllStmt.StartToken.Metadata.Column,
			),
		)
		return nil
	}

	return &hir.BreakAllStmtHir{}
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

	if valueExpr.ExprType() == sa.builtinTypesMap["void"] {
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
		varExplicitType, ok := sa.getType(varDeclStmt.ExplicitType)
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

		if varExplicitType == sa.builtinTypesMap["void"] {
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
	case *ast.TypeInstantiationExpr:
		return sa.analyzeTypeInstantiationExpr(expr.(*ast.TypeInstantiationExpr))
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

	var binExprType types.Type
	switch binaryExpr.Op.Kind {
	case lexer.LAND, lexer.LOR:
		if left.ExprType() != sa.builtinTypesMap["bool"] {
			sa.eh.AddError(
				newSemanticError(
					"logical operator operands must be of type bool",
					binaryExpr.Op.Metadata.FileName,
					binaryExpr.Op.Metadata.Line,
					binaryExpr.Op.Metadata.Column,
				),
			)
			return nil
		}
		binExprType = sa.builtinTypesMap["bool"]
	case lexer.EQ, lexer.NEQ, lexer.LT, lexer.GT, lexer.GEQ, lexer.LEQ:
		binExprType = sa.builtinTypesMap["bool"]
	default:
		binExprType = left.ExprType()
	}

	return &hir.BinaryExprHir{
		Type:  binExprType,
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

func (sa *SemanticAnalyzer) analyzeTypeInstantiationExpr(typeInstantiationExpr *ast.TypeInstantiationExpr) *hir.TypeInstantiationExprHir {
	userType, exists := sa.customTypesMap[typeInstantiationExpr.TypeName]
	if !exists {
		sa.eh.AddError(
			newSemanticError(
				fmt.Sprintf("type %s not defined", typeInstantiationExpr.TypeName),
				typeInstantiationExpr.StartToken.Metadata.FileName,
				typeInstantiationExpr.StartToken.Metadata.Line,
				typeInstantiationExpr.StartToken.Metadata.Column,
			),
		)
		return nil
	}

	instantiations := make([]hir.TypeMemberInstantiation, 0)
	membersSet := make(map[string]struct{})
	failed := false
	for _, instantiation := range typeInstantiationExpr.Instantiations {
		memberType, exists := userType.Members[instantiation.Name]
		if !exists {
			sa.eh.AddError(
				newSemanticError(
					fmt.Sprintf("member %s not found in type %s", instantiation.Name, typeInstantiationExpr.TypeName),
					typeInstantiationExpr.StartToken.Metadata.FileName,
					typeInstantiationExpr.StartToken.Metadata.Line,
					typeInstantiationExpr.StartToken.Metadata.Column,
				),
			)
			failed = true
			continue
		}

		instantiationExpr := sa.analyzeExpr(instantiation.Expr)
		if hir.IsNilExpr(instantiationExpr) {
			failed = true
			continue
		}

		instantiationExpr = sa.tryImplicitCast(instantiationExpr, memberType)
		if memberType != instantiationExpr.ExprType() {
			sa.eh.AddError(
				newSemanticError(
					fmt.Sprintf("member %s type mismatch", instantiation.Name),
					typeInstantiationExpr.StartToken.Metadata.FileName,
					typeInstantiationExpr.StartToken.Metadata.Line,
					typeInstantiationExpr.StartToken.Metadata.Column,
				),
			)
			failed = true
			continue
		}

		instantiations = append(instantiations, hir.TypeMemberInstantiation{
			MemberName: instantiation.Name,
			ExprHir: instantiationExpr,
		})
		membersSet[instantiation.Name] = struct{}{}
	}

	for memberName := range userType.Members {
		_, exists := membersSet[memberName]
		if !exists {
			sa.eh.AddError(
				newSemanticError(
					fmt.Sprintf("member %s not initialized", memberName),
					typeInstantiationExpr.StartToken.Metadata.FileName,
					typeInstantiationExpr.StartToken.Metadata.Line,
					typeInstantiationExpr.StartToken.Metadata.Column,
				),
			)
			failed = true
		}
	}

	if failed {
		return nil
	}

	return &hir.TypeInstantiationExprHir{
		Type:           userType,
		TypeName:       typeInstantiationExpr.TypeName,
		Instantiations: instantiations,
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
			Type:  sa.builtinTypesMap["i32"],
			Value: intExpr.Value,
		}
	}

	return &hir.IntExprHir{
		Type:  sa.builtinTypesMap[string(intExpr.ExplicitType)],
		Value: intExpr.Value,
	}
}

func (sa *SemanticAnalyzer) analyzeFloatExpr(floatExpr *ast.FloatExpr) *hir.FloatExprHir {
	if floatExpr.ExplicitType == ast.FloatNone {
		return &hir.FloatExprHir{
			Type:  sa.builtinTypesMap["f32"],
			Value: floatExpr.Value,
		}
	}

	return &hir.FloatExprHir{
		Type:  sa.builtinTypesMap[string(floatExpr.ExplicitType)],
		Value: floatExpr.Value,
	}
}

func (sa *SemanticAnalyzer) analyzeBoolExpr(boolExpr *ast.BoolExpr) *hir.BoolExprHir {
	return &hir.BoolExprHir{
		Type:  sa.builtinTypesMap["bool"],
		Value: boolExpr.Value,
	}
}

func (sa *SemanticAnalyzer) analyzeCastExpr(castExpr *ast.CastExpr) hir.ExprHir {
	left := sa.analyzeExpr(castExpr.Left)
	if hir.IsNilExpr(left) {
		return nil
	}

	newType, ok := sa.builtinTypesMap[castExpr.CastToType]
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
