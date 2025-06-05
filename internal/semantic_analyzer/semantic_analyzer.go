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

	typeResolver *TypeResolver

	funcsMap map[string]types.FunctionType

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

func NewSemanticAnalyzer(
	eh compiler_errors.ErrorHandler,
	translationUnit ast.TranslationUnit) *SemanticAnalyzer {
	return &SemanticAnalyzer{
		eh:              eh,
		translationUnit: translationUnit,

		scope:    &scope{variables: make(map[string]varDefinition)},
		funcArgs: make(map[string]argDefinition),

		funcsMap: make(map[string]types.FunctionType),

		typeResolver: NewTypeResolver(),

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
	sa.scanTranslationUnitForTypeDeclStmts()
	sa.scanTranslationUnitForFuncDeclStmts()

	topStmts := sa.analyzeTranslationUnit(sa.translationUnit)
	funcTypes := make([]types.FunctionType, 0)
	for _, funcType := range sa.funcsMap {
		funcTypes = append(funcTypes, funcType)
	}
	return &hir.FileHir{
		FuncPrototypes: funcTypes,
		Types:          sa.typeResolver.GetUserTypes(),
		Stmts:          topStmts,
	}
}

func (sa *SemanticAnalyzer) scanTranslationUnitForTypeDeclStmts() {
	sa.forwardTypeDeclarations = make(map[string]*ast.TypeDeclStmt)

	for _, topStmt := range sa.translationUnit.Stmts {
		typeDeclStmt, ok := topStmt.(*ast.TypeDeclStmt)
		if !ok {
			continue
		}

		exists := sa.typeResolver.IsBuiltInType(typeDeclStmt.Name)
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

		returnType, ok := sa.typeResolver.GetType(funcDeclStmt.ReturnType)
		if !ok {
			sa.eh.AddError(
				newSemanticError(
					fmt.Sprintf("return type %s not defined", funcDeclStmt.ReturnType.TypeName()),
					funcDeclStmt.StartToken.Metadata.FileName,
					funcDeclStmt.StartToken.Metadata.Line,
					funcDeclStmt.StartToken.Metadata.Column,
				),
			)
		}

		args := make([]types.FunctionArgType, 0)
		for _, arg := range funcDeclStmt.Args {
			argType, ok := sa.typeResolver.GetType(arg.Type)
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

			if argType.SameAs(sa.typeResolver.VoidType()) {
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
	if userType, ok := sa.typeResolver.GetUserType(typeDeclStmt.Name); ok {
		return userType
	}

	sa.currentlyProcessingTypes[typeDeclStmt.Name] = struct{}{}
	members := make(map[string]types.Type)
	memberPositions := make(map[string]int)

	for position, member := range typeDeclStmt.Members {
		memberType, ok := sa.typeResolver.GetType(member.Type)
		switch {
		case ok && memberType.SameAs(sa.typeResolver.VoidType()):
			sa.eh.AddError(
				newSemanticError(
					"member cannot be of type void",
					typeDeclStmt.StartToken.Metadata.FileName,
					typeDeclStmt.StartToken.Metadata.Line,
					typeDeclStmt.StartToken.Metadata.Column,
				),
			)
			continue
		case ok && !memberType.SameAs(sa.typeResolver.VoidType()):
			members[member.Name] = memberType
			memberPositions[member.Name] = position
			continue
		}

		memberTypeDeclStmt, ok := sa.forwardTypeDeclarations[member.Type.TypeName()]
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

		if _, ok := sa.currentlyProcessingTypes[member.Type.TypeName()]; ok {
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
	sa.typeResolver.AddUserType(userType.Name, userType)
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
		if !functionType.ReturnType.SameAs(sa.typeResolver.VoidType()) {
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

	condExpr = sa.tryImplicitCast(condExpr, sa.typeResolver.BoolType())
	if !condExpr.ExprType().SameAs(sa.typeResolver.BoolType()) {
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

		elseIfCond = sa.tryImplicitCast(elseIfCond, sa.typeResolver.BoolType())
		if !elseIfCond.ExprType().SameAs(sa.typeResolver.BoolType()) {
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

	condExpr = sa.tryImplicitCast(condExpr, sa.typeResolver.BoolType())
	if !condExpr.ExprType().SameAs(sa.typeResolver.BoolType()) {
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

	condExpr = sa.tryImplicitCast(condExpr, sa.typeResolver.BoolType())
	if !condExpr.ExprType().SameAs(sa.typeResolver.BoolType()) {
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
		condExpr = sa.tryImplicitCast(condExpr, sa.typeResolver.BoolType())
		if !condExpr.ExprType().SameAs(sa.typeResolver.BoolType()) {
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
		if !sa.funcRetType.SameAs(sa.typeResolver.VoidType()) {
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
	if sa.funcRetType == nil {
		return nil
	}

	if !sa.funcRetType.SameAs(valueExpr.ExprType()) {
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

	if valueExpr.ExprType().SameAs(sa.typeResolver.VoidType()) {
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

	if varDeclStmt.ExplicitType != nil {
		varExplicitType, ok := sa.typeResolver.GetType(varDeclStmt.ExplicitType)
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

		if varExplicitType.SameAs(sa.typeResolver.VoidType()) {
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

		if !varExplicitType.SameAs(valueExpr.ExprType()) {
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
	case *ast.PrefixExpr:
		return sa.analyzePrefixExpr(expr.(*ast.PrefixExpr))
	case *ast.PostfixExpr:
		return sa.analyzePostfixExpr(expr.(*ast.PostfixExpr))
	case *ast.TypeInstantiationExpr:
		return sa.analyzeTypeInstantiationExpr(expr.(*ast.TypeInstantiationExpr))
	case *ast.MemberAccessExpr:
		return sa.analyzeMemberAccessExpr(expr.(*ast.MemberAccessExpr))
	case *ast.ArrayExpr:
		return sa.analyzeArrayExpr(expr.(*ast.ArrayExpr))
	case *ast.CallExpr:
		return sa.analyzeCallExpr(expr.(*ast.CallExpr))
	case *ast.ArraySubscriptExpr:
		return sa.analyzeArraySubscriptExpr(expr.(*ast.ArraySubscriptExpr))
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
	if exprHir.ExprType().SameAs(hirType) {
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
	if !left.ExprType().SameAs(right.ExprType()) {
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
		left = sa.tryImplicitCast(left, sa.typeResolver.BoolType())
		right = sa.tryImplicitCast(right, sa.typeResolver.BoolType())
		if !left.ExprType().SameAs(sa.typeResolver.BoolType()) || !right.ExprType().SameAs(sa.typeResolver.BoolType()) {
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
		binExprType = sa.typeResolver.BoolType()
	case lexer.EQ, lexer.NEQ, lexer.LT, lexer.GT, lexer.GEQ, lexer.LEQ:
		binExprType = sa.typeResolver.BoolType()
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
	if unaryExpr, ok := assignExpr.Left.(ast.UnaryExpr); ok {
		op := unaryExpr.GetOp()
		if op.Kind != lexer.ASTERISK {
			sa.eh.AddError(
				newSemanticError(
					fmt.Sprintf("assignment cannot be used with unary operator %s", op.Value),
					assignExpr.Op.Metadata.FileName,
					assignExpr.Op.Metadata.Line,
					assignExpr.Op.Metadata.Column,
				),
			)
			return nil
		}
	}

	left := sa.analyzeExpr(assignExpr.Left)
	if hir.IsNilExpr(left) {
		return nil
	}

	right := sa.analyzeExpr(assignExpr.Right)
	if hir.IsNilExpr(right) {
		return nil
	}

	right = sa.tryImplicitCast(right, left.ExprType())
	if !right.ExprType().SameAs(left.ExprType()) {
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
		right = &hir.BinaryExprHir{
			Type:  left.ExprType(),
			Left:  left,
			Op:    hir.BinaryAdd,
			Right: right,
		}
	case lexer.SUB_ASSIGN:
		right = &hir.BinaryExprHir{
			Type:  left.ExprType(),
			Left:  left,
			Op:    hir.BinarySub,
			Right: right,
		}
	case lexer.MUL_ASSIGN:
		right = &hir.BinaryExprHir{
			Type:  left.ExprType(),
			Left:  left,
			Op:    hir.BinaryMul,
			Right: right,
		}
	case lexer.DIV_ASSIGN:
		right = &hir.BinaryExprHir{
			Type:  left.ExprType(),
			Left:  left,
			Op:    hir.BinaryDiv,
			Right: right,
		}
	case lexer.MOD_ASSIGN:
		right = &hir.BinaryExprHir{
			Type:  left.ExprType(),
			Left:  left,
			Op:    hir.BinaryMod,
			Right: right,
		}
	case lexer.BAND_ASSIGN:
		right = &hir.BinaryExprHir{
			Type:  left.ExprType(),
			Left:  left,
			Op:    hir.BinaryBand,
			Right: right,
		}
	case lexer.BOR_ASSIGN:
		right = &hir.BinaryExprHir{
			Type:  left.ExprType(),
			Left:  left,
			Op:    hir.BinaryBor,
			Right: right,
		}
	case lexer.XOR_ASSIGN:
		right = &hir.BinaryExprHir{
			Type:  left.ExprType(),
			Left:  left,
			Op:    hir.BinaryXor,
			Right: right,
		}
	case lexer.SHL_ASSIGN:
		right = &hir.BinaryExprHir{
			Type:  left.ExprType(),
			Left:  left,
			Op:    hir.BinaryShl,
			Right: right,
		}
	case lexer.SHR_ASSIGN:
		right = &hir.BinaryExprHir{
			Type:  left.ExprType(),
			Left:  left,
			Op:    hir.BinaryShr,
			Right: right,
		}
	}

	return &hir.AssignExprHir{
		Type:  left.ExprType(),
		Left:  left.(hir.LvalueExprHir),
		Right: right,
	}
}

func (sa *SemanticAnalyzer) analyzePrefixExpr(prefixExpr *ast.PrefixExpr) hir.ExprHir {
	rightExprHir := sa.analyzeExpr(prefixExpr.Right)
	if hir.IsNilExpr(rightExprHir) {
		return nil
	}

	_, isInt := rightExprHir.ExprType().(*types.IntType)
	_, isFloat := rightExprHir.ExprType().(*types.FloatType)

	switch prefixExpr.Op.Kind {
	case lexer.PLUS:
		if !isInt && !isFloat {
			sa.eh.AddError(
				newSemanticError(
					"unary plus operator can only be applied to numeric types",
					prefixExpr.Op.Metadata.FileName,
					prefixExpr.Op.Metadata.Line,
					prefixExpr.Op.Metadata.Column,
				),
			)
			return nil
		}

		return rightExprHir
	case lexer.MINUS:
		if !isInt && !isFloat {
			sa.eh.AddError(
				newSemanticError(
					"unary minus operator can only be applied to numeric types",
					prefixExpr.Op.Metadata.FileName,
					prefixExpr.Op.Metadata.Line,
					prefixExpr.Op.Metadata.Column,
				),
			)
			return nil
		}

		return &hir.PrefixExprHir{
			Type: rightExprHir.ExprType(),
			Op:   hir.UnaryNegate,
			Expr: rightExprHir,
		}
	case lexer.INC:
		if !isInt && !isFloat {
			sa.eh.AddError(
				newSemanticError(
					"increment operator can only be applied to numeric types",
					prefixExpr.Op.Metadata.FileName,
					prefixExpr.Op.Metadata.Line,
					prefixExpr.Op.Metadata.Column,
				),
			)
			return nil
		}

		return &hir.PrefixExprHir{
			Type: rightExprHir.ExprType(),
			Op:   hir.UnaryInc,
			Expr: rightExprHir,
		}
	case lexer.DEC:
		if !isInt && !isFloat {
			sa.eh.AddError(
				newSemanticError(
					"decrement operator can only be applied to numeric types",
					prefixExpr.Op.Metadata.FileName,
					prefixExpr.Op.Metadata.Line,
					prefixExpr.Op.Metadata.Column,
				),
			)
			return nil
		}

		return &hir.PrefixExprHir{
			Type: rightExprHir.ExprType(),
			Op:   hir.UnaryDec,
			Expr: rightExprHir,
		}
	case lexer.TILDE:
		if !isInt {
			sa.eh.AddError(
				newSemanticError(
					"bitwise NOT operator can only be applied to integer types",
					prefixExpr.Op.Metadata.FileName,
					prefixExpr.Op.Metadata.Line,
					prefixExpr.Op.Metadata.Column,
				),
			)
			return nil
		}

		return &hir.PrefixExprHir{
			Type: rightExprHir.ExprType(),
			Op:   hir.UnaryBitNot,
			Expr: rightExprHir,
		}
	case lexer.XMARK:
		rightExprHir = sa.tryImplicitCast(rightExprHir, sa.typeResolver.BoolType())
		if !rightExprHir.ExprType().SameAs(sa.typeResolver.BoolType()) {
			sa.eh.AddError(
				newSemanticError(
					"unary NOT operator can only be applied to boolean types",
					prefixExpr.Op.Metadata.FileName,
					prefixExpr.Op.Metadata.Line,
					prefixExpr.Op.Metadata.Column,
				),
			)
			return nil
		}

		return &hir.PrefixExprHir{
			Type: rightExprHir.ExprType(),
			Op:   hir.UnaryNot,
			Expr: rightExprHir,
		}
	case lexer.BAND:
		if _, ok := rightExprHir.(hir.LvalueExprHir); !ok {
			sa.eh.AddError(
				newSemanticError(
					"address-of operator can only be applied to lvalues",
					prefixExpr.Op.Metadata.FileName,
					prefixExpr.Op.Metadata.Line,
					prefixExpr.Op.Metadata.Column,
				),
			)
			return nil
		}

		return &hir.PrefixExprHir{
			Type: &types.PointerType{InnerType: rightExprHir.ExprType()},
			Op:   hir.UnaryAddressOf,
			Expr: rightExprHir,
		}
	case lexer.ASTERISK:
		pointerType, ok := rightExprHir.ExprType().(*types.PointerType)
		if !ok {
			sa.eh.AddError(
				newSemanticError(
					"dereference operator can only be applied to pointer types",
					prefixExpr.Op.Metadata.FileName,
					prefixExpr.Op.Metadata.Line,
					prefixExpr.Op.Metadata.Column,
				),
			)
			return nil
		}

		return &hir.PrefixExprHir{
			Type: pointerType.InnerType,
			Op:   hir.UnaryDereference,
			Expr: rightExprHir,
		}
	default:
		panic("unsupported prefix operator")
	}
}

func (sa *SemanticAnalyzer) analyzePostfixExpr(postfixExpr *ast.PostfixExpr) *hir.PostfixExprHir {
	leftExprHir := sa.analyzeExpr(postfixExpr.Left)
	if hir.IsNilExpr(leftExprHir) {
		return nil
	}

	_, isInt := leftExprHir.ExprType().(*types.IntType)
	_, isFloat := leftExprHir.ExprType().(*types.FloatType)

	switch postfixExpr.Op.Kind {
	case lexer.INC:
		if !isInt && !isFloat {
			sa.eh.AddError(
				newSemanticError(
					"increment operator can only be applied to numeric types",
					postfixExpr.Op.Metadata.FileName,
					postfixExpr.Op.Metadata.Line,
					postfixExpr.Op.Metadata.Column,
				),
			)
			return nil
		}

		return &hir.PostfixExprHir{
			Type: leftExprHir.ExprType(),
			Op:   hir.UnaryInc,
			Expr: leftExprHir,
		}
	case lexer.DEC:
		if !isInt && !isFloat {
			sa.eh.AddError(
				newSemanticError(
					"decrement operator can only be applied to numeric types",
					postfixExpr.Op.Metadata.FileName,
					postfixExpr.Op.Metadata.Line,
					postfixExpr.Op.Metadata.Column,
				),
			)
			return nil
		}

		return &hir.PostfixExprHir{
			Type: leftExprHir.ExprType(),
			Op:   hir.UnaryDec,
			Expr: leftExprHir,
		}
	default:
		panic("unsupported postfix operator")
	}
}

func (sa *SemanticAnalyzer) analyzeTypeInstantiationExpr(typeInstantiationExpr *ast.TypeInstantiationExpr) *hir.TypeInstantiationExprHir {
	userType, exists := sa.typeResolver.GetUserType(typeInstantiationExpr.TypeName.TypeName())
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
		if !memberType.SameAs(instantiationExpr.ExprType()) {
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
			MemberPosition: userType.MemberPositions[instantiation.Name],
			MemberName:     instantiation.Name,
			ExprHir:        instantiationExpr,
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
		TypeName:       typeInstantiationExpr.TypeName.TypeName(),
		Instantiations: instantiations,
	}
}

func (sa *SemanticAnalyzer) analyzeMemberAccessExpr(memberAccessExpr *ast.MemberAccessExpr) *hir.MemberAccessExprHir {
	left := sa.analyzeExpr(memberAccessExpr.Left)
	if hir.IsNilExpr(left) {
		return nil
	}

	switch memberAccessExpr.Right.(type) {
	case *ast.IdentExpr:
		right := memberAccessExpr.Right.(*ast.IdentExpr)
		memberType, ok := left.ExprType().GetMember(right.Value)
		if !ok {
			sa.eh.AddError(
				newSemanticError(
					fmt.Sprintf("member %s not found in type %s", right.Value, left.ExprType().Type()),
					memberAccessExpr.StartToken.Metadata.FileName,
					memberAccessExpr.StartToken.Metadata.Line,
					memberAccessExpr.StartToken.Metadata.Column,
				),
			)
			return nil
		}

		return &hir.MemberAccessExprHir{
			Type: memberType,
			Left: left,
			Right: &hir.IdentExprHir{
				Type: memberType,
				Name: right.Value,
			},
		}
	case *ast.CallExpr:
		panic("not implemented")
	default:
		panic("unreachable")
	}
}

func (sa *SemanticAnalyzer) analyzeArrayExpr(arrayExpr *ast.ArrayExpr) *hir.ArrayExprHir {
	elements := make([]hir.ExprHir, 0)
	exprHir := sa.analyzeExpr(arrayExpr.Elements[0])
	if hir.IsNilExpr(exprHir) {
		return nil
	}
	elements = append(elements, exprHir)

	errorOccured := false
	arrayItemType := exprHir.ExprType()
	for _, expr := range arrayExpr.Elements[1:] {
		exprHir = sa.analyzeExpr(expr)
		if hir.IsNilExpr(exprHir) {
			return nil
		}

		exprHir = sa.tryImplicitCast(exprHir, arrayItemType)
		if !exprHir.ExprType().SameAs(arrayItemType) {
			sa.eh.AddError(
				newSemanticError(
					"array element type mismatch",
					expr.FirstToken().Metadata.FileName,
					expr.FirstToken().Metadata.Line,
					expr.FirstToken().Metadata.Column,
				),
			)
			errorOccured = true
			continue
		}

		elements = append(elements, exprHir)
	}

	if errorOccured {
		return nil
	}

	return &hir.ArrayExprHir{
		Type:     &types.ArrayType{ItemType: arrayItemType, Size: len(elements)},
		Elements: elements,
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
		if !argType.SameAs(argExpr.ExprType()) {
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

func (sa *SemanticAnalyzer) analyzeArraySubscriptExpr(arraySubscriptExpr *ast.ArraySubscriptExpr) *hir.ArraySubscriptExprHir {
	leftExpr := sa.analyzeExpr(arraySubscriptExpr.Left)
	if hir.IsNilExpr(leftExpr) {
		return nil
	}

	if _, ok := leftExpr.ExprType().(*types.ArrayType); !ok {
		sa.eh.AddError(
			newSemanticError(
				"array subscript can only be applied to array types",
				arraySubscriptExpr.StartToken.Metadata.FileName,
				arraySubscriptExpr.StartToken.Metadata.Line,
				arraySubscriptExpr.StartToken.Metadata.Column,
			),
		)
		return nil
	}

	indexExpr := sa.analyzeExpr(arraySubscriptExpr.Index)
	if hir.IsNilExpr(indexExpr) {
		return nil
	}

	indexExpr = sa.tryImplicitCast(indexExpr, sa.typeResolver.IntType(32))
	if !indexExpr.ExprType().SameAs(sa.typeResolver.IntType(32)) {
		sa.eh.AddError(
			newSemanticError(
				"array subscript index must be of type i32",
				arraySubscriptExpr.StartToken.Metadata.FileName,
				arraySubscriptExpr.StartToken.Metadata.Line,
				arraySubscriptExpr.StartToken.Metadata.Column,
			),
		)
		return nil
	}

	return &hir.ArraySubscriptExprHir{
		Type:  leftExpr.ExprType().(*types.ArrayType).ItemType,
		Left:  leftExpr,
		Index: indexExpr,
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
	switch intExpr.ExplicitType {
	case ast.IntNone:
		return &hir.IntExprHir{
			Type:  sa.typeResolver.IntType(32),
			Value: intExpr.Value,
		}
	case ast.Int8Type:
		return &hir.IntExprHir{
			Type:  sa.typeResolver.IntType(8),
			Value: intExpr.Value,
		}
	case ast.Int16Type:
		return &hir.IntExprHir{
			Type:  sa.typeResolver.IntType(16),
			Value: intExpr.Value,
		}
	case ast.Int32Type:
		return &hir.IntExprHir{
			Type:  sa.typeResolver.IntType(32),
			Value: intExpr.Value,
		}
	case ast.Int64Type:
		return &hir.IntExprHir{
			Type:  sa.typeResolver.IntType(64),
			Value: intExpr.Value,
		}
	case ast.Int128Type:
		return &hir.IntExprHir{
			Type:  sa.typeResolver.IntType(128),
			Value: intExpr.Value,
		}
	case ast.Uint8Type:
		return &hir.IntExprHir{
			Type:  sa.typeResolver.UIntType(8),
			Value: intExpr.Value,
		}
	case ast.Uint16Type:
		return &hir.IntExprHir{
			Type:  sa.typeResolver.UIntType(16),
			Value: intExpr.Value,
		}
	case ast.Uint32Type:
		return &hir.IntExprHir{
			Type:  sa.typeResolver.UIntType(32),
			Value: intExpr.Value,
		}
	case ast.Uint64Type:
		return &hir.IntExprHir{
			Type:  sa.typeResolver.UIntType(64),
			Value: intExpr.Value,
		}
	case ast.Uint128Type:
		return &hir.IntExprHir{
			Type:  sa.typeResolver.UIntType(128),
			Value: intExpr.Value,
		}
	default:
		panic("unreachable")
	}
}

func (sa *SemanticAnalyzer) analyzeFloatExpr(floatExpr *ast.FloatExpr) *hir.FloatExprHir {
	switch floatExpr.ExplicitType {
	case ast.FloatNone:
		return &hir.FloatExprHir{
			Type:  sa.typeResolver.FloatType(32),
			Value: floatExpr.Value,
		}
	// case ast.Float16Type:
	// 	return &hir.FloatExprHir{
	// 		Type:  sa.typeResolver.FloatType(16),
	// 		Value: floatExpr.Value,
	// 	}
	case ast.Float32Type:
		return &hir.FloatExprHir{
			Type:  sa.typeResolver.FloatType(32),
			Value: floatExpr.Value,
		}
	case ast.Float64Type:
		return &hir.FloatExprHir{
			Type:  sa.typeResolver.FloatType(64),
			Value: floatExpr.Value,
		}
	case ast.Float80Type:
		return &hir.FloatExprHir{
			Type:  sa.typeResolver.FloatType(80),
			Value: floatExpr.Value,
		}
	case ast.Float128Type:
		return &hir.FloatExprHir{
			Type:  sa.typeResolver.FloatType(128),
			Value: floatExpr.Value,
		}
	}

	panic("unreachable")
}

func (sa *SemanticAnalyzer) analyzeBoolExpr(boolExpr *ast.BoolExpr) *hir.BoolExprHir {
	return &hir.BoolExprHir{
		Type:  sa.typeResolver.BoolType(),
		Value: boolExpr.Value,
	}
}

func (sa *SemanticAnalyzer) analyzeCastExpr(castExpr *ast.CastExpr) hir.ExprHir {
	left := sa.analyzeExpr(castExpr.Left)
	if hir.IsNilExpr(left) {
		return nil
	}

	if _, ok := castExpr.CastToType.(*ast.IdentTypeNode); !ok {
		sa.eh.AddError(
			newSemanticError(
				"cannot cast to a complex types, like arrays, or slices, or pointers",
				castExpr.StartToken.Metadata.FileName,
				castExpr.StartToken.Metadata.Line,
				castExpr.StartToken.Metadata.Column,
			),
		)
		return nil
	}

	newType, ok := sa.typeResolver.GetBuiltInType(castExpr.CastToType.TypeName())
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
	if left.ExprType().SameAs(newType) {
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
