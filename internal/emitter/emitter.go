package emitter

import (
	"fmt"

	"github.com/kievzenit/ylang/internal/hir"
	hir_types "github.com/kievzenit/ylang/internal/hir/types"
	"tinygo.org/x/go-llvm"
)

type Emitter struct {
	fileHir *hir.FileHir

	typesMap         map[string]llvm.Type
	variablesMap     map[string]llvm.Value
	funcsMap         map[string]llvm.Value
	initFunctionsMap map[string]llvm.Value

	context llvm.Context
	module  llvm.Module
	builder llvm.Builder

	currentFunc            llvm.Value
	currentAllocBasicBlock llvm.BasicBlock

	controlFlowHappen        bool
	loopsContinueBasicBlocks []llvm.BasicBlock

	loopsBreakBasicBlock []llvm.BasicBlock

	nextBasicBlock llvm.BasicBlock
}

func NewEmitter(fileHir *hir.FileHir) *Emitter {
	context := llvm.NewContext()
	return &Emitter{
		fileHir: fileHir,

		typesMap:         make(map[string]llvm.Type),
		variablesMap:     make(map[string]llvm.Value),
		funcsMap:         make(map[string]llvm.Value),
		initFunctionsMap: make(map[string]llvm.Value),

		context: context,
		module:  context.NewModule("main"),
		builder: context.NewBuilder(),

		loopsContinueBasicBlocks: make([]llvm.BasicBlock, 0),

		loopsBreakBasicBlock: make([]llvm.BasicBlock, 0),
	}
}

func (e *Emitter) Emit() llvm.Module {
	e.declareTypes()
	e.declareFuncPrototypes()

	e.emitForFileHir(e.fileHir)

	return e.module
}

func (e *Emitter) declareTypes() {
	e.typesMap["bool"] = e.context.Int1Type()

	e.typesMap["i8"] = e.context.Int8Type()
	e.typesMap["i16"] = e.context.Int16Type()
	e.typesMap["i32"] = e.context.Int32Type()
	e.typesMap["i64"] = e.context.Int64Type()
	e.typesMap["i128"] = e.context.IntType(128)

	e.typesMap["u1"] = e.context.Int1Type()
	e.typesMap["u8"] = e.context.Int8Type()
	e.typesMap["u16"] = e.context.Int16Type()
	e.typesMap["u32"] = e.context.Int32Type()
	e.typesMap["u64"] = e.context.Int64Type()
	e.typesMap["u128"] = e.context.IntType(128)

	e.typesMap["f32"] = e.context.FloatType()
	e.typesMap["f64"] = e.context.DoubleType()
	e.typesMap["f80"] = e.context.X86FP80Type()
	e.typesMap["f128"] = e.context.FP128Type()

	e.typesMap["void"] = e.context.VoidType()

	for _, userType := range e.fileHir.Types {
		e.emitForType(userType)
	}
}

func (e *Emitter) emitForType(userType *hir_types.UserType) llvm.Type {
	customStruct := e.context.StructCreateNamed(userType.Name)
	fieldTypes := make([]llvm.Type, 0)
	for _, member := range userType.Members {
		llvmType, exists := e.typesMap[member.Type()]
		if exists {
			fieldTypes = append(fieldTypes, llvmType)
			continue
		}

		innerUserType, ok := member.(*hir_types.UserType)
		if !ok {
			panic("type should be either builtin or user type, this was not")
		}

		innerStruct := e.emitForType(innerUserType)
		fieldTypes = append(fieldTypes, innerStruct)
	}
	customStruct.StructSetBody(fieldTypes, false)
	e.typesMap[userType.Name] = customStruct

	initFunctionType := llvm.FunctionType(customStruct, fieldTypes, false)
	initFunction := llvm.AddFunction(
		e.module,
		fmt.Sprintf("compiler::%s::init", userType.Name),
		initFunctionType)
	initFunction.SetLinkage(llvm.PrivateLinkage)
	alwaysInlineAttr := e.context.CreateEnumAttribute(llvm.AttributeKindID("alwaysinline"), 0)
	noCallbackAttr := e.context.CreateEnumAttribute(llvm.AttributeKindID("nocallback"), 0)
	noMergeAttr := e.context.CreateEnumAttribute(llvm.AttributeKindID("nomerge"), 0)
	noRecurseAttr := e.context.CreateEnumAttribute(llvm.AttributeKindID("norecurse"), 0)
	willReturnAttr := e.context.CreateEnumAttribute(llvm.AttributeKindID("willreturn"), 0)
	noSyncAttr := e.context.CreateEnumAttribute(llvm.AttributeKindID("nosync"), 0)
	noUnwindAttr := e.context.CreateEnumAttribute(llvm.AttributeKindID("nounwind"), 0)
	initFunction.AddFunctionAttr(alwaysInlineAttr)
	initFunction.AddFunctionAttr(noCallbackAttr)
	initFunction.AddFunctionAttr(noMergeAttr)
	initFunction.AddFunctionAttr(noRecurseAttr)
	initFunction.AddFunctionAttr(willReturnAttr)
	initFunction.AddFunctionAttr(noSyncAttr)
	initFunction.AddFunctionAttr(noUnwindAttr)
	e.initFunctionsMap[userType.Name] = initFunction

	entryBasicBlock := e.context.AddBasicBlock(initFunction, "entry")
	e.builder.SetInsertPointAtEnd(entryBasicBlock)
	initRetAlloca := e.builder.CreateAlloca(customStruct, "inittmp")
	for memberName := range userType.Members {
		memberPosition := userType.MemberPositions[memberName]
		memberGep := e.builder.CreateStructGEP(
			customStruct,
			initRetAlloca,
			memberPosition,
			fmt.Sprintf("%s::%s", userType.Name, memberName),
		)
		e.builder.CreateStore(initFunction.Param(memberPosition), memberGep)
	}

	loadedRetValue := e.builder.CreateLoad(customStruct, initRetAlloca, "initrettmp")
	e.builder.CreateRet(loadedRetValue)

	return customStruct
}

func (e *Emitter) declareFuncPrototypes() {
	for _, funcType := range e.fileHir.FuncPrototypes {
		funcName := funcType.Name
		returnType := e.typesMap[funcType.ReturnType.Type()]
		argsTypes := make([]llvm.Type, 0)
		for _, arg := range funcType.Args {
			argsTypes = append(argsTypes, e.typesMap[arg.Type.Type()])
		}
		funcType := llvm.FunctionType(returnType, argsTypes, false)
		funcValue := llvm.AddFunction(e.module, funcName, funcType)
		e.funcsMap[funcName] = funcValue

		framePointerAttr := e.context.CreateStringAttribute("frame-pointer", "all")
		noTrappingMathAttr := e.context.CreateStringAttribute("no-trapping-math", "true")
		stackProtectorBufferSizeAttr := e.context.CreateStringAttribute("stack-protector-buffer-size", "8")
		funcValue.AddFunctionAttr(framePointerAttr)
		funcValue.AddFunctionAttr(noTrappingMathAttr)
		funcValue.AddFunctionAttr(stackProtectorBufferSizeAttr)
	}
}

func (e *Emitter) emitForFileHir(fileHir *hir.FileHir) {
	for _, topStmtHir := range fileHir.Stmts {
		e.emitForTopStmtHir(topStmtHir)
	}
}

func (e *Emitter) emitForTopStmtHir(topStmtHir hir.TopStmtHir) {
	switch topStmtHir.(type) {
	case *hir.FuncDeclStmtHir:
		e.emitForFuncDeclStmtHir(topStmtHir.(*hir.FuncDeclStmtHir))
	default:
		panic("not implemented")
	}
}

func (e *Emitter) emitForFuncDeclStmtHir(funcDeclStmtHir *hir.FuncDeclStmtHir) {
	if funcDeclStmtHir.Body == nil {
		return
	}

	funcValue := e.funcsMap[funcDeclStmtHir.Name]
	e.currentFunc = funcValue

	allocBasicBlock := llvm.AddBasicBlock(funcValue, "alloc")
	e.currentAllocBasicBlock = allocBasicBlock

	entryBasicBlock := llvm.AddBasicBlock(funcValue, "entry")

	unreachableBasicBlock := llvm.AddBasicBlock(funcValue, "unreachable")
	e.nextBasicBlock = unreachableBasicBlock
	e.builder.SetInsertPointAtEnd(unreachableBasicBlock)
	e.builder.CreateUnreachable()

	e.builder.SetInsertPointAtEnd(entryBasicBlock)

	e.emitForScopeStmtHir(funcDeclStmtHir.Body)

	e.builder.SetInsertPointAtEnd(allocBasicBlock)
	e.builder.CreateBr(entryBasicBlock)
	e.currentAllocBasicBlock = llvm.BasicBlock{}
	e.nextBasicBlock = llvm.BasicBlock{}
	e.controlFlowHappen = false
	e.loopsContinueBasicBlocks = make([]llvm.BasicBlock, 0)
	e.loopsBreakBasicBlock = make([]llvm.BasicBlock, 0)
}

func (e *Emitter) emitForStmtHir(stmtHir hir.StmtHir) {
	switch stmtHir.(type) {
	case *hir.ScopeStmtHir:
		e.emitForScopeStmtHir(stmtHir.(*hir.ScopeStmtHir))
	case *hir.VarDeclStmtHir:
		e.emitForVarDeclStmtHir(stmtHir.(*hir.VarDeclStmtHir))
	case *hir.IfStmtHir:
		e.emitForIfStmtHir(stmtHir.(*hir.IfStmtHir))
	case *hir.WhileStmtHir:
		e.emitForWhileStmtHir(stmtHir.(*hir.WhileStmtHir))
	case *hir.DoWhileStmtHir:
		e.emitForDoWhileStmtHir(stmtHir.(*hir.DoWhileStmtHir))
	case *hir.LoopStmtHir:
		e.emitForLoopStmtHir(stmtHir.(*hir.LoopStmtHir))
	case *hir.ForStmtHir:
		e.emitForForStmtHir(stmtHir.(*hir.ForStmtHir))
	case *hir.ReturnStmtHir:
		e.emitForReturnStmtHir(stmtHir.(*hir.ReturnStmtHir))
	case *hir.ContinueStmtHir:
		e.emitForContinueStmtHir(stmtHir.(*hir.ContinueStmtHir))
	case *hir.BreakStmtHir:
		e.emitForBreakStmtHir(stmtHir.(*hir.BreakStmtHir))
	case *hir.BreakAllStmtHir:
		e.emitForBreakAllStmtHir(stmtHir.(*hir.BreakAllStmtHir))
	case *hir.ExprStmtHir:
		e.emitForExprStmtHir(stmtHir.(*hir.ExprStmtHir))
	default:
		panic("not implemented")
	}
}

func (e *Emitter) emitForScopeStmtHir(scopeStmtHir *hir.ScopeStmtHir) {
	for _, stmtHir := range scopeStmtHir.Stmts {
		e.emitForStmtHir(stmtHir)
	}
}

func (e *Emitter) emitForVarDeclStmtHir(varDeclStmtHir *hir.VarDeclStmtHir) {
	currBasicBlock := e.builder.GetInsertBlock()
	e.builder.SetInsertPointAtEnd(e.currentAllocBasicBlock)
	allocValue := e.builder.CreateAlloca(
		e.typesMap[varDeclStmtHir.Value.ExprType().Type()],
		varDeclStmtHir.Name,
	)
	e.variablesMap[varDeclStmtHir.Name] = allocValue
	e.builder.SetInsertPointAtEnd(currBasicBlock)

	varValue := e.emitForExprHir(varDeclStmtHir.Value)
	e.builder.CreateStore(varValue, allocValue)
}

func (e *Emitter) emitForIfStmtHir(ifStmtHir *hir.IfStmtHir) {
	privNextBasicBlock := e.nextBasicBlock

	checkBlock := e.context.AddBasicBlock(e.currentFunc, "ifcheck")
	ifBody := e.context.AddBasicBlock(e.currentFunc, "ifbody")
	elseBlock := e.context.AddBasicBlock(e.currentFunc, "ifelse")
	afterIfBlock := e.context.AddBasicBlock(e.currentFunc, "ifafter")

	checkBlock.MoveBefore(e.nextBasicBlock)
	ifBody.MoveBefore(e.nextBasicBlock)
	elseBlock.MoveBefore(e.nextBasicBlock)
	afterIfBlock.MoveBefore(e.nextBasicBlock)

	e.builder.CreateBr(checkBlock)
	e.builder.SetInsertPointAtEnd(checkBlock)

	e.nextBasicBlock = ifBody
	condResult := e.emitForExprHir(ifStmtHir.Cond)
	e.builder.CreateCondBr(condResult, ifBody, elseBlock)

	e.nextBasicBlock = elseBlock
	e.builder.SetInsertPointAtEnd(ifBody)
	e.emitForScopeStmtHir(ifStmtHir.Body)
	if !e.controlFlowHappen {
		e.builder.CreateBr(afterIfBlock)
	}
	e.controlFlowHappen = false

	e.builder.SetInsertPointAtEnd(elseBlock)
	if ifStmtHir.Else != nil {
		e.nextBasicBlock = afterIfBlock
		e.emitForStmtHir(ifStmtHir.Else)
	}

	if !e.controlFlowHappen {
		e.builder.CreateBr(afterIfBlock)
	}
	e.controlFlowHappen = false

	e.builder.SetInsertPointAtEnd(afterIfBlock)
	e.nextBasicBlock = privNextBasicBlock
}

func (e *Emitter) emitForWhileStmtHir(whileStmtHir *hir.WhileStmtHir) {
	privNextBasicBlock := e.nextBasicBlock

	checkBlock := e.context.AddBasicBlock(e.currentFunc, "whilecheck")
	bodyBlock := e.context.AddBasicBlock(e.currentFunc, "whilebody")
	afterBlock := e.context.AddBasicBlock(e.currentFunc, "whileafter")

	e.loopsContinueBasicBlocks = append(e.loopsContinueBasicBlocks, checkBlock)
	e.loopsBreakBasicBlock = append(e.loopsBreakBasicBlock, afterBlock)

	checkBlock.MoveBefore(e.nextBasicBlock)
	bodyBlock.MoveBefore(e.nextBasicBlock)
	afterBlock.MoveBefore(e.nextBasicBlock)

	e.builder.CreateBr(checkBlock)
	e.builder.SetInsertPointAtEnd(checkBlock)

	e.nextBasicBlock = bodyBlock
	e.builder.SetInsertPointAtEnd(checkBlock)
	condValue := e.emitForExprHir(whileStmtHir.Cond)
	e.builder.CreateCondBr(condValue, bodyBlock, afterBlock)

	e.builder.SetInsertPointAtEnd(bodyBlock)
	e.nextBasicBlock = afterBlock
	e.emitForScopeStmtHir(whileStmtHir.Body)
	if !e.controlFlowHappen {
		e.builder.CreateBr(checkBlock)
	}
	e.controlFlowHappen = false

	e.builder.SetInsertPointAtEnd(afterBlock)
	e.nextBasicBlock = privNextBasicBlock
	e.loopsContinueBasicBlocks = e.loopsContinueBasicBlocks[:len(e.loopsContinueBasicBlocks)-1]
	e.loopsBreakBasicBlock = e.loopsBreakBasicBlock[:len(e.loopsBreakBasicBlock)-1]
}

func (e *Emitter) emitForDoWhileStmtHir(doWhileStmtHir *hir.DoWhileStmtHir) {
	privNextBasicBlock := e.nextBasicBlock

	bodyBlock := e.context.AddBasicBlock(e.currentFunc, "dowhilebody")
	checkBlock := e.context.AddBasicBlock(e.currentFunc, "dowhilecheck")
	afterBlock := e.context.AddBasicBlock(e.currentFunc, "dowhileafter")

	e.loopsContinueBasicBlocks = append(e.loopsContinueBasicBlocks, checkBlock)
	e.loopsBreakBasicBlock = append(e.loopsBreakBasicBlock, afterBlock)

	bodyBlock.MoveBefore(e.nextBasicBlock)
	checkBlock.MoveBefore(e.nextBasicBlock)
	afterBlock.MoveBefore(e.nextBasicBlock)

	e.builder.CreateBr(bodyBlock)
	e.builder.SetInsertPointAtEnd(bodyBlock)

	e.nextBasicBlock = checkBlock
	e.emitForScopeStmtHir(doWhileStmtHir.Body)
	if !e.controlFlowHappen {
		e.builder.CreateBr(checkBlock)
	}
	e.controlFlowHappen = false

	e.nextBasicBlock = afterBlock
	e.builder.SetInsertPointAtEnd(checkBlock)
	condValue := e.emitForExprHir(doWhileStmtHir.Cond)
	e.builder.CreateCondBr(condValue, bodyBlock, afterBlock)

	e.builder.SetInsertPointAtEnd(afterBlock)
	e.nextBasicBlock = privNextBasicBlock
	e.loopsContinueBasicBlocks = e.loopsContinueBasicBlocks[:len(e.loopsContinueBasicBlocks)-1]
	e.loopsBreakBasicBlock = e.loopsBreakBasicBlock[:len(e.loopsBreakBasicBlock)-1]
}

func (e *Emitter) emitForLoopStmtHir(loopStmtHir *hir.LoopStmtHir) {
	privNextBasicBlock := e.nextBasicBlock

	bodyBlock := e.context.AddBasicBlock(e.currentFunc, "loopbody")
	afterBlock := e.context.AddBasicBlock(e.currentFunc, "loopafter")

	e.loopsContinueBasicBlocks = append(e.loopsContinueBasicBlocks, bodyBlock)
	e.loopsBreakBasicBlock = append(e.loopsBreakBasicBlock, afterBlock)

	bodyBlock.MoveBefore(e.nextBasicBlock)
	afterBlock.MoveBefore(e.nextBasicBlock)

	e.builder.CreateBr(bodyBlock)
	e.builder.SetInsertPointAtEnd(bodyBlock)

	e.nextBasicBlock = afterBlock
	e.builder.SetInsertPointAtEnd(bodyBlock)
	e.emitForScopeStmtHir(loopStmtHir.Body)
	if !e.controlFlowHappen {
		e.builder.CreateBr(bodyBlock)
	}
	e.controlFlowHappen = false

	e.builder.SetInsertPointAtEnd(afterBlock)
	e.nextBasicBlock = privNextBasicBlock
	e.loopsContinueBasicBlocks = e.loopsContinueBasicBlocks[:len(e.loopsContinueBasicBlocks)-1]
	e.loopsBreakBasicBlock = e.loopsBreakBasicBlock[:len(e.loopsBreakBasicBlock)-1]
}

func (e *Emitter) emitForForStmtHir(forStmtHir *hir.ForStmtHir) {
	privNextBasicBlock := e.nextBasicBlock

	initBlock := e.context.AddBasicBlock(e.currentFunc, "forinit")
	checkBlock := e.context.AddBasicBlock(e.currentFunc, "forcheck")
	bodyBlock := e.context.AddBasicBlock(e.currentFunc, "forbody")
	postBlock := e.context.AddBasicBlock(e.currentFunc, "forpost")
	afterBlock := e.context.AddBasicBlock(e.currentFunc, "forafter")

	e.loopsContinueBasicBlocks = append(e.loopsContinueBasicBlocks, postBlock)
	e.loopsBreakBasicBlock = append(e.loopsBreakBasicBlock, afterBlock)

	initBlock.MoveBefore(e.nextBasicBlock)
	checkBlock.MoveBefore(e.nextBasicBlock)
	bodyBlock.MoveBefore(e.nextBasicBlock)
	postBlock.MoveBefore(e.nextBasicBlock)
	afterBlock.MoveBefore(e.nextBasicBlock)

	e.builder.CreateBr(initBlock)
	e.builder.SetInsertPointAtEnd(initBlock)
	e.nextBasicBlock = checkBlock
	for _, stmt := range forStmtHir.Init {
		e.emitForStmtHir(stmt)
	}

	e.builder.CreateBr(checkBlock)
	e.builder.SetInsertPointAtEnd(checkBlock)
	e.nextBasicBlock = bodyBlock
	condValue := e.emitForExprHir(forStmtHir.Cond)
	e.builder.CreateCondBr(condValue, bodyBlock, afterBlock)

	e.builder.SetInsertPointAtEnd(bodyBlock)
	e.nextBasicBlock = postBlock
	e.emitForScopeStmtHir(forStmtHir.Body)
	if !e.controlFlowHappen {
		e.builder.CreateBr(postBlock)
	}
	e.controlFlowHappen = false

	e.builder.SetInsertPointAtEnd(postBlock)
	e.nextBasicBlock = afterBlock
	for _, expr := range forStmtHir.Post {
		e.emitForExprHir(expr)
	}
	e.builder.CreateBr(checkBlock)

	e.builder.SetInsertPointAtEnd(afterBlock)
	e.nextBasicBlock = privNextBasicBlock
	e.loopsContinueBasicBlocks = e.loopsContinueBasicBlocks[:len(e.loopsContinueBasicBlocks)-1]
	e.loopsBreakBasicBlock = e.loopsBreakBasicBlock[:len(e.loopsBreakBasicBlock)-1]
}

func (e *Emitter) emitForReturnStmtHir(returnStmtHir *hir.ReturnStmtHir) {
	e.controlFlowHappen = true

	if returnStmtHir.Expr != nil {
		value := e.emitForExprHir(returnStmtHir.Expr)
		e.builder.CreateRet(value)
		return
	}

	e.builder.CreateRetVoid()
}

func (e *Emitter) emitForContinueStmtHir(_ *hir.ContinueStmtHir) {
	e.controlFlowHappen = true
	e.builder.CreateBr(e.loopsContinueBasicBlocks[len(e.loopsContinueBasicBlocks)-1])
}

func (e *Emitter) emitForBreakStmtHir(_ *hir.BreakStmtHir) {
	e.controlFlowHappen = true
	e.builder.CreateBr(e.loopsBreakBasicBlock[len(e.loopsBreakBasicBlock)-1])
}

func (e *Emitter) emitForBreakAllStmtHir(_ *hir.BreakAllStmtHir) {
	e.controlFlowHappen = true
	e.builder.CreateBr(e.loopsBreakBasicBlock[0])
}

func (e *Emitter) emitForExprStmtHir(expStmtHir *hir.ExprStmtHir) {
	e.emitForExprHir(expStmtHir.Expr)
}

func (e *Emitter) emitForExprHir(exprHir hir.ExprHir) llvm.Value {
	switch exprHir.(type) {
	case *hir.UpCastExprHir:
		return e.emitForUpCastExprHir(exprHir.(*hir.UpCastExprHir))
	case *hir.DownCastExprHir:
		return e.emitForDownCastExprHir(exprHir.(*hir.DownCastExprHir))
	case *hir.AssignExprHir:
		return e.emitForAssignExprHir(exprHir.(*hir.AssignExprHir))
	case *hir.TypeInstantiationExprHir:
		return e.emitForTypeInstantiationExprHir(exprHir.(*hir.TypeInstantiationExprHir))
	case *hir.BinaryExprHir:
		return e.emitForBinExprHir(exprHir.(*hir.BinaryExprHir))
	case *hir.IdentExprHir:
		return e.emitForIdentExprHir(exprHir.(*hir.IdentExprHir))
	case *hir.ArgIdentExprHir:
		return e.emitForArgIdentExprHir(exprHir.(*hir.ArgIdentExprHir))
	case *hir.IntExprHir:
		return e.emitForIntExprHir(exprHir.(*hir.IntExprHir))
	case *hir.FloatExprHir:
		return e.emitForFloatExprHir(exprHir.(*hir.FloatExprHir))
	case *hir.CallExprHir:
		return e.emitForCallExprHir(exprHir.(*hir.CallExprHir))
	case *hir.BoolExprHir:
		return e.emitForBoolExprHir(exprHir.(*hir.BoolExprHir))
	default:
		panic("not implemented")
	}
}

func (e *Emitter) emitForUpCastExprHir(upCastExprHir *hir.UpCastExprHir) llvm.Value {
	value := e.emitForExprHir(upCastExprHir.Expr)

	_, ok := upCastExprHir.ExprType().(*hir_types.BoolType)
	if ok {
		return e.builder.CreateZExt(value, e.typesMap[upCastExprHir.ExprType().Type()], "upcasttmp")
	}

	intType, ok := upCastExprHir.ExprType().(*hir_types.IntType)
	switch {
	case ok && intType.Signed:
		return e.builder.CreateSExt(value, e.typesMap[upCastExprHir.ExprType().Type()], "upcasttmp")
	case ok && !intType.Signed:
		return e.builder.CreateZExt(value, e.typesMap[upCastExprHir.ExprType().Type()], "upcasttmp")
	}

	_, ok = upCastExprHir.ExprType().(*hir_types.FloatType)
	if ok {
		return e.builder.CreateFPExt(value, e.typesMap[upCastExprHir.ExprType().Type()], "upcasttmp")
	}

	panic("not implemented")
}

func (e *Emitter) emitForDownCastExprHir(downCastExprHir *hir.DownCastExprHir) llvm.Value {
	value := e.emitForExprHir(downCastExprHir.Expr)

	_, ok := downCastExprHir.ExprType().(*hir_types.IntType)
	if ok {
		return e.builder.CreateTrunc(value, e.typesMap[downCastExprHir.ExprType().Type()], "downcasttmp")
	}

	_, ok = downCastExprHir.ExprType().(*hir_types.FloatType)
	if ok {
		return e.builder.CreateFPTrunc(value, e.typesMap[downCastExprHir.ExprType().Type()], "downcasttmp")
	}

	panic("not implemented")
}

func (e *Emitter) emitForAssignExprHir(assignExprHir *hir.AssignExprHir) llvm.Value {
	allocValue := e.variablesMap[assignExprHir.Ident.Name]
	value := e.emitForExprHir(assignExprHir.Value)
	e.builder.CreateStore(value, allocValue)
	return value
}

func (e *Emitter) emitForTypeInstantiationExprHir(typeInstantiationExprHir *hir.TypeInstantiationExprHir) llvm.Value {
	initFunction := e.initFunctionsMap[typeInstantiationExprHir.TypeName]

	args := make([]llvm.Value, len(typeInstantiationExprHir.Instantiations))
	for _, instantiation := range typeInstantiationExprHir.Instantiations {
		exprHir := e.emitForExprHir(instantiation.ExprHir)
		args[instantiation.MemberPosition] = exprHir
	}
	return e.builder.CreateCall(initFunction.GlobalValueType(), initFunction, args, "")
}

func (e *Emitter) emitForBinExprHir(binExprHir *hir.BinaryExprHir) llvm.Value {
	var isInt bool
	var isUint bool
	var isFloat bool

	intType, ok := binExprHir.Left.ExprType().(*hir_types.IntType)
	if ok && intType.Signed {
		isInt = true
	}
	if ok && !intType.Signed {
		isUint = true
	}

	_, ok = binExprHir.Left.ExprType().(*hir_types.FloatType)
	if ok {
		isFloat = true
	}

	switch binExprHir.Op {
	case hir.Add:
		leftValue := e.emitForExprHir(binExprHir.Left)
		rightValue := e.emitForExprHir(binExprHir.Right)

		switch {
		case isInt, isUint:
			return e.builder.CreateAdd(leftValue, rightValue, "addtmp")
		case isFloat:
			return e.builder.CreateFAdd(leftValue, rightValue, "addtmp")
		default:
			panic("not implemented")
		}
	case hir.Sub:
		leftValue := e.emitForExprHir(binExprHir.Left)
		rightValue := e.emitForExprHir(binExprHir.Right)

		switch {
		case isInt, isUint:
			return e.builder.CreateSub(leftValue, rightValue, "subtmp")
		case isFloat:
			return e.builder.CreateFSub(leftValue, rightValue, "subtmp")
		default:
			panic("not implemented")
		}
	case hir.Mul:
		leftValue := e.emitForExprHir(binExprHir.Left)
		rightValue := e.emitForExprHir(binExprHir.Right)

		switch {
		case isInt, isUint:
			return e.builder.CreateMul(leftValue, rightValue, "multmp")
		case isFloat:
			return e.builder.CreateFMul(leftValue, rightValue, "multmp")
		default:
			panic("not implemented")
		}
	case hir.Div:
		leftValue := e.emitForExprHir(binExprHir.Left)
		rightValue := e.emitForExprHir(binExprHir.Right)

		switch {
		case isInt:
			return e.builder.CreateSDiv(leftValue, rightValue, "divtmp")
		case isUint:
			return e.builder.CreateUDiv(leftValue, rightValue, "divtmp")
		case isFloat:
			return e.builder.CreateFDiv(leftValue, rightValue, "divtmp")
		default:
			panic("not implemented")
		}
	case hir.Mod:
		leftValue := e.emitForExprHir(binExprHir.Left)
		rightValue := e.emitForExprHir(binExprHir.Right)

		switch {
		case isInt:
			return e.builder.CreateSRem(leftValue, rightValue, "modtmp")
		case isUint:
			return e.builder.CreateURem(leftValue, rightValue, "modtmp")
		case isFloat:
			return e.builder.CreateFRem(leftValue, rightValue, "modtmp")
		default:
			panic("not implemented")
		}
	case hir.Gt:
		leftValue := e.emitForExprHir(binExprHir.Left)
		rightValue := e.emitForExprHir(binExprHir.Right)

		switch {
		case isInt:
			return e.builder.CreateICmp(llvm.IntSGT, leftValue, rightValue, "gttmp")
		case isUint:
			return e.builder.CreateICmp(llvm.IntUGT, leftValue, rightValue, "gttmp")
		case isFloat:
			return e.builder.CreateFCmp(llvm.FloatOGT, leftValue, rightValue, "gttmp")
		default:
			panic("not implemented")
		}
	case hir.Ge:
		leftValue := e.emitForExprHir(binExprHir.Left)
		rightValue := e.emitForExprHir(binExprHir.Right)

		switch {
		case isInt:
			return e.builder.CreateICmp(llvm.IntSGE, leftValue, rightValue, "getmp")
		case isUint:
			return e.builder.CreateICmp(llvm.IntUGE, leftValue, rightValue, "getmp")
		case isFloat:
			return e.builder.CreateFCmp(llvm.FloatOGE, leftValue, rightValue, "getmp")
		default:
			panic("not implemented")
		}
	case hir.Lt:
		leftValue := e.emitForExprHir(binExprHir.Left)
		rightValue := e.emitForExprHir(binExprHir.Right)

		switch {
		case isInt:
			return e.builder.CreateICmp(llvm.IntSLT, leftValue, rightValue, "lttmp")
		case isUint:
			return e.builder.CreateICmp(llvm.IntULT, leftValue, rightValue, "lttmp")
		case isFloat:
			return e.builder.CreateFCmp(llvm.FloatOLT, leftValue, rightValue, "lttmp")
		default:
			panic("not implemented")
		}
	case hir.Le:
		leftValue := e.emitForExprHir(binExprHir.Left)
		rightValue := e.emitForExprHir(binExprHir.Right)

		switch {
		case isInt:
			return e.builder.CreateICmp(llvm.IntSLE, leftValue, rightValue, "letmp")
		case isUint:
			return e.builder.CreateICmp(llvm.IntULE, leftValue, rightValue, "letmp")
		case isFloat:
			return e.builder.CreateFCmp(llvm.FloatOLE, leftValue, rightValue, "letmp")
		default:
			panic("not implemented")
		}
	case hir.Eq:
		leftValue := e.emitForExprHir(binExprHir.Left)
		rightValue := e.emitForExprHir(binExprHir.Right)

		switch {
		case isInt, isUint:
			return e.builder.CreateICmp(llvm.IntEQ, leftValue, rightValue, "eqtmp")
		case isFloat:
			return e.builder.CreateFCmp(llvm.FloatOEQ, leftValue, rightValue, "eqtmp")
		default:
			panic("not implemented")
		}
	case hir.Ne:
		leftValue := e.emitForExprHir(binExprHir.Left)
		rightValue := e.emitForExprHir(binExprHir.Right)

		switch {
		case isInt, isUint:
			return e.builder.CreateICmp(llvm.IntNE, leftValue, rightValue, "netmp")
		case isFloat:
			return e.builder.CreateFCmp(llvm.FloatONE, leftValue, rightValue, "netmp")
		default:
			panic("not implemented")
		}
	case hir.Land:
		privNextBasicBlock := e.nextBasicBlock

		checkBlock := e.context.AddBasicBlock(e.currentFunc, "andcheck")
		trueBlock := e.context.AddBasicBlock(e.currentFunc, "andtrue")
		mergeBlock := e.context.AddBasicBlock(e.currentFunc, "andmerge")

		checkBlock.MoveBefore(e.nextBasicBlock)
		trueBlock.MoveBefore(e.nextBasicBlock)
		mergeBlock.MoveBefore(e.nextBasicBlock)

		e.builder.CreateBr(checkBlock)
		e.builder.SetInsertPointAtEnd(checkBlock)

		e.nextBasicBlock = trueBlock
		leftValue := e.emitForExprHir(binExprHir.Left)
		lastBlockInCheck := e.builder.GetInsertBlock()
		e.builder.CreateCondBr(leftValue, trueBlock, mergeBlock)

		e.builder.SetInsertPointAtEnd(trueBlock)

		e.nextBasicBlock = mergeBlock
		rightValue := e.emitForExprHir(binExprHir.Right)
		lastBlockInTrue := e.builder.GetInsertBlock()
		e.builder.CreateBr(mergeBlock)

		e.builder.SetInsertPointAtEnd(mergeBlock)

		phi := e.builder.CreatePHI(e.typesMap["bool"], "andphi")
		phi.AddIncoming(
			[]llvm.Value{
				llvm.ConstInt(e.typesMap["bool"], 0, false),
			},
			[]llvm.BasicBlock{lastBlockInCheck},
		)
		phi.AddIncoming([]llvm.Value{rightValue}, []llvm.BasicBlock{lastBlockInTrue})

		e.nextBasicBlock = privNextBasicBlock

		return phi
	case hir.Lor:
		privNextBasicBlock := e.nextBasicBlock

		checkBlock := e.context.AddBasicBlock(e.currentFunc, "orcheck")
		falseBlock := e.context.AddBasicBlock(e.currentFunc, "orfalse")
		mergeBlock := e.context.AddBasicBlock(e.currentFunc, "ormerge")

		checkBlock.MoveBefore(e.nextBasicBlock)
		falseBlock.MoveBefore(e.nextBasicBlock)
		mergeBlock.MoveBefore(e.nextBasicBlock)

		e.builder.CreateBr(checkBlock)
		e.builder.SetInsertPointAtEnd(checkBlock)

		e.nextBasicBlock = falseBlock
		leftValue := e.emitForExprHir(binExprHir.Left)
		lastBlockInCheck := e.builder.GetInsertBlock()
		e.builder.CreateCondBr(leftValue, mergeBlock, falseBlock)

		e.builder.SetInsertPointAtEnd(falseBlock)

		e.nextBasicBlock = mergeBlock
		rightValue := e.emitForExprHir(binExprHir.Right)
		lastBlockInFalse := e.builder.GetInsertBlock()
		e.builder.CreateBr(mergeBlock)

		e.builder.SetInsertPointAtEnd(mergeBlock)

		phi := e.builder.CreatePHI(e.typesMap["bool"], "orphi")
		phi.AddIncoming(
			[]llvm.Value{
				llvm.ConstInt(e.typesMap["bool"], 1, false),
			},
			[]llvm.BasicBlock{lastBlockInCheck},
		)
		phi.AddIncoming([]llvm.Value{rightValue}, []llvm.BasicBlock{lastBlockInFalse})

		e.nextBasicBlock = privNextBasicBlock

		return phi
	default:
		panic("not implemented")
	}
}

func (e *Emitter) emitForArgIdentExprHir(argIdentExprHir *hir.ArgIdentExprHir) llvm.Value {
	return e.currentFunc.Param(argIdentExprHir.Index)
}

func (e *Emitter) emitForIdentExprHir(identExprHir *hir.IdentExprHir) llvm.Value {
	identType := e.typesMap[identExprHir.ExprType().Type()]
	identValue := e.variablesMap[identExprHir.Name]
	return e.builder.CreateLoad(identType, identValue, "loadtmp")
}

func (e *Emitter) emitForCallExprHir(callExprHir *hir.CallExprHir) llvm.Value {
	funcValue := e.funcsMap[callExprHir.Name]
	args := make([]llvm.Value, 0)
	for _, arg := range callExprHir.Args {
		args = append(args, e.emitForExprHir(arg))
	}
	return e.builder.CreateCall(funcValue.GlobalValueType(), funcValue, args, "")
}

func (e *Emitter) emitForIntExprHir(intExprHir *hir.IntExprHir) llvm.Value {
	intType := intExprHir.ExprType().(*hir_types.IntType)
	return llvm.ConstInt(e.typesMap[intExprHir.ExprType().Type()], uint64(intExprHir.Value), intType.Signed)
}

func (e *Emitter) emitForFloatExprHir(floatExprHir *hir.FloatExprHir) llvm.Value {
	return llvm.ConstFloat(e.typesMap[floatExprHir.ExprType().Type()], float64(floatExprHir.Value))
}

func (e *Emitter) emitForBoolExprHir(boolExprHir *hir.BoolExprHir) llvm.Value {
	var intValue uint64
	if boolExprHir.Value {
		intValue = 1
	} else {
		intValue = 0
	}
	return llvm.ConstInt(e.typesMap[boolExprHir.ExprType().Type()], intValue, false)
}
