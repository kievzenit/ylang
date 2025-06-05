package emitter

import (
	"fmt"
	"math"

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

func (e *Emitter) llvmTypeExistsForType(hirType hir_types.Type) bool {
	if ptrType, ok := hirType.(*hir_types.PointerType); ok {
		return e.llvmTypeExistsForType(ptrType.InnerType)
	}

	if arrayType, ok := hirType.(*hir_types.ArrayType); ok {
		return e.llvmTypeExistsForType(arrayType.ItemType)
	}

	if _, ok := e.typesMap[hirType.Type()]; ok {
		return true
	}

	return false
}

func (e *Emitter) getLlvmTypeForType(hirType hir_types.Type) llvm.Type {
	if ptrType, ok := hirType.(*hir_types.PointerType); ok {
		return llvm.PointerType(e.getLlvmTypeForType(ptrType.InnerType), 0)
	}

	if arrayType, ok := hirType.(*hir_types.ArrayType); ok {
		return llvm.ArrayType(e.getLlvmTypeForType(arrayType.ItemType), arrayType.Size)
	}

	if llvmType, ok := e.typesMap[hirType.Type()]; ok {
		return llvmType
	}

	panic("type not found")
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
	if llvmType, ok := e.typesMap[userType.Name]; ok {
		return llvmType
	}

	customStruct := e.context.StructCreateNamed(userType.Name)
	fieldTypes := make([]llvm.Type, len(userType.Members))
	for memberName, pos := range userType.MemberPositions {
		memberType := userType.Members[memberName]
		llvmTypeAlreadyExists := e.llvmTypeExistsForType(memberType)
		if llvmTypeAlreadyExists {
			fieldTypes[pos] = e.getLlvmTypeForType(memberType)
			continue
		}

		innerUserType, ok := memberType.(*hir_types.UserType)
		if !ok {
			panic("type should be either builtin or user type, this was not")
		}

		innerStruct := e.emitForType(innerUserType)
		fieldTypes[pos] = innerStruct
	}
	customStruct.StructSetBody(fieldTypes, false)
	e.typesMap[userType.Name] = customStruct

	initFunctionParams := make([]llvm.Type, 0, len(userType.Members)+1)
	initFunctionParams = append(initFunctionParams, llvm.PointerType(customStruct, 0))
	initFunctionParams = append(initFunctionParams, fieldTypes...)

	initFunctionType := llvm.FunctionType(e.context.VoidType(), initFunctionParams, false)
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

	noaliasAttr := e.context.CreateEnumAttribute(llvm.AttributeKindID("noalias"), 0)
	writableAttr := e.context.CreateEnumAttribute(llvm.AttributeKindID("writable"), 0)
	nonnullAttr := e.context.CreateEnumAttribute(llvm.AttributeKindID("nonnull"), 0)
	nocaptureAttr := e.context.CreateEnumAttribute(llvm.AttributeKindID("nocapture"), 0)
	sretAttr := e.context.CreateTypeAttribute(llvm.AttributeKindID("sret"), customStruct)
	initFunction.AddAttributeAtIndex(1, noaliasAttr)
	initFunction.AddAttributeAtIndex(1, writableAttr)
	initFunction.AddAttributeAtIndex(1, nonnullAttr)
	initFunction.AddAttributeAtIndex(1, nocaptureAttr)
	initFunction.AddAttributeAtIndex(1, sretAttr)

	entryBasicBlock := e.context.AddBasicBlock(initFunction, "entry")
	e.builder.SetInsertPointAtEnd(entryBasicBlock)
	firstParam := initFunction.Param(0)
	for memberName := range userType.Members {
		memberPosition := userType.MemberPositions[memberName]
		memberGep := e.builder.CreateStructGEP(
			customStruct,
			firstParam,
			memberPosition+1,
			fmt.Sprintf("%s::%s", userType.Name, memberName),
		)
		e.builder.CreateStore(initFunction.Param(memberPosition+1), memberGep)
	}
	e.builder.CreateRetVoid()

	return customStruct
}

func (e *Emitter) declareFuncPrototypes() {
	for _, funcType := range e.fileHir.FuncPrototypes {
		funcName := funcType.Name
		returnType := e.getLlvmTypeForType(funcType.ReturnType)
		argsTypes := make([]llvm.Type, 0)
		for _, arg := range funcType.Args {
			argsTypes = append(argsTypes, e.getLlvmTypeForType(arg.Type))
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
		e.getLlvmTypeForType(varDeclStmtHir.Value.ExprType()),
		varDeclStmtHir.Name,
	)
	e.variablesMap[varDeclStmtHir.Name] = allocValue
	e.builder.SetInsertPointAtEnd(currBasicBlock)

	if typeInstantiationExprHir, ok := varDeclStmtHir.Value.(*hir.TypeInstantiationExprHir); ok {
		varValue := e.emitForTypeInstantiationExprHir(typeInstantiationExprHir, allocValue)
		e.builder.CreateStore(varValue, allocValue)
		return
	}

	if arrayExprHir, ok := varDeclStmtHir.Value.(*hir.ArrayExprHir); ok {
		e.emitForArrayExprHir(arrayExprHir, allocValue)
		return
	}

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

	if returnStmtHir.Expr == nil {
		e.builder.CreateRetVoid()
		return
	}

	value := e.emitForExprHir(returnStmtHir.Expr)
	e.builder.CreateRet(value)
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
	case *hir.PrefixExprHir:
		return e.emitForPrefixExprHir(exprHir.(*hir.PrefixExprHir))
	case *hir.PostfixExprHir:
		return e.emitForPostfixExprHir(exprHir.(*hir.PostfixExprHir))
	case *hir.TypeInstantiationExprHir:
		return e.emitForTypeInstantiationExprHir(exprHir.(*hir.TypeInstantiationExprHir), llvm.Value{})
	case *hir.MemberAccessExprHir:
		return e.emitForMemberAccessExprHir(exprHir.(*hir.MemberAccessExprHir), 0)
	case *hir.BinaryExprHir:
		return e.emitForBinExprHir(exprHir.(*hir.BinaryExprHir))
	case *hir.ArrayExprHir:
		return e.emitForArrayExprHir(exprHir.(*hir.ArrayExprHir), llvm.Value{})
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

func (e *Emitter) getPtrToMemberAccessExprHir(memberAccessExprHir *hir.MemberAccessExprHir, depth int) llvm.Value {
	var leftValue llvm.Value
	if leftIdentExprHir, ok := memberAccessExprHir.Left.(*hir.IdentExprHir); ok {
		leftValue = e.variablesMap[leftIdentExprHir.Name]
	} else if leftMemberExprHir, ok := memberAccessExprHir.Left.(*hir.MemberAccessExprHir); ok {
		leftValue = e.getPtrToMemberAccessExprHir(leftMemberExprHir, depth+1)
	} else {
		panic("unreachable")
	}

	leftMemberType, ok := memberAccessExprHir.Left.ExprType().(*hir_types.UserType)
	if !ok {
		panic("member access should be user type")
	}

	rightIdentExprHir, ok := memberAccessExprHir.Right.(*hir.IdentExprHir)
	if !ok {
		panic("unreachable")
	}

	memberPosition := leftMemberType.MemberPositions[rightIdentExprHir.Name]
	memberGep := e.builder.CreateStructGEP(
		e.getLlvmTypeForType(leftMemberType),
		leftValue,
		memberPosition,
		fmt.Sprintf("%s::%s", leftMemberType.Name, rightIdentExprHir.Name),
	)

	return memberGep
}

func (e *Emitter) getPtrToLvalueExprHirValue(lvalueExprHir hir.LvalueExprHir) llvm.Value {
	switch lvalueExprHir.(type) {
	case *hir.IdentExprHir:
		identExprHir := lvalueExprHir.(*hir.IdentExprHir)
		identValue := e.variablesMap[identExprHir.Name]
		return identValue
	case *hir.MemberAccessExprHir:
		memberExprHir := lvalueExprHir.(*hir.MemberAccessExprHir)
		return e.getPtrToMemberAccessExprHir(memberExprHir, 0)
	case *hir.PrefixExprHir:
		prefixExprHir := lvalueExprHir.(*hir.PrefixExprHir)
		// assuming that prefix expression is always * operator
		ptrPtrValue := e.getPtrToLvalueExprHirValue(prefixExprHir.Expr.(hir.LvalueExprHir))
		return e.builder.CreateLoad(
			e.getLlvmTypeForType(prefixExprHir.Expr.ExprType()),
			ptrPtrValue,
			"loadtmp",
		)
	default:
		panic("unreachable")
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
	ptrValue := e.getPtrToLvalueExprHirValue(assignExprHir.Left)
	value := e.emitForExprHir(assignExprHir.Right)
	e.builder.CreateStore(value, ptrValue)
	return value
}

func (e *Emitter) emitForPrefixExprHir(prefixExprHir *hir.PrefixExprHir) llvm.Value {
	switch prefixExprHir.Op {
	case hir.UnaryPlus:
		value := e.emitForExprHir(prefixExprHir.Expr)
		return value
	case hir.UnaryNegate:
		value := e.emitForExprHir(prefixExprHir.Expr)
		intType, ok := prefixExprHir.ExprType().(*hir_types.IntType)
		if ok && intType.Signed {
			return e.builder.CreateNSWSub(
				llvm.ConstInt(e.typesMap[intType.Type()], 0, true),
				value,
				"unarynegatetmp",
			)
		}

		if ok && !intType.Signed {
			return e.builder.CreateNUWSub(
				llvm.ConstInt(e.typesMap[intType.Type()], 0, true),
				value,
				"unarynegatetmp",
			)
		}

		_, ok = prefixExprHir.ExprType().(*hir_types.FloatType)
		if ok {
			return e.builder.CreateFNeg(value, "unarynegatetmp")
		}

		panic("unreachable")
	case hir.UnaryBitNot:
		value := e.emitForExprHir(prefixExprHir.Expr)
		intType, _ := prefixExprHir.ExprType().(*hir_types.IntType)
		return e.builder.CreateXor(
			value,
			llvm.ConstInt(e.typesMap[intType.Type()], math.MaxUint64, true),
			"unarybitnottmp",
		)
	case hir.UnaryNot:
		value := e.emitForExprHir(prefixExprHir.Expr)
		return e.builder.CreateXor(
			value,
			llvm.ConstInt(e.typesMap["bool"], 1, true),
			"unarynottmp",
		)
	case hir.UnaryInc:
		value := e.emitForExprHir(prefixExprHir.Expr)
		ptrValue := e.getPtrToLvalueExprHirValue(prefixExprHir.Expr.(hir.LvalueExprHir))

		intType, ok := prefixExprHir.ExprType().(*hir_types.IntType)
		if ok && intType.Signed {
			resultValue := e.builder.CreateNSWAdd(
				value,
				llvm.ConstInt(e.typesMap[intType.Type()], 1, true),
				"unaryinctmp",
			)
			e.builder.CreateStore(resultValue, ptrValue)
			return resultValue
		}

		if ok && !intType.Signed {
			resultValue := e.builder.CreateNUWAdd(
				value,
				llvm.ConstInt(e.typesMap[intType.Type()], 1, true),
				"unaryinctmp",
			)
			e.builder.CreateStore(resultValue, ptrValue)
			return resultValue
		}

		_, ok = prefixExprHir.ExprType().(*hir_types.FloatType)
		if ok {
			resultValue := e.builder.CreateFAdd(
				value,
				llvm.ConstFloat(e.typesMap[prefixExprHir.ExprType().Type()], 1),
				"unaryinctmp",
			)
			e.builder.CreateStore(resultValue, ptrValue)
			return resultValue
		}

		panic("unreachable")
	case hir.UnaryDec:
		value := e.emitForExprHir(prefixExprHir.Expr)
		ptrValue := e.getPtrToLvalueExprHirValue(prefixExprHir.Expr.(hir.LvalueExprHir))

		intType, ok := prefixExprHir.ExprType().(*hir_types.IntType)
		if ok && intType.Signed {
			resultValue := e.builder.CreateNSWSub(
				value,
				llvm.ConstInt(e.typesMap[intType.Type()], 1, true),
				"unarydectmp",
			)
			e.builder.CreateStore(resultValue, ptrValue)
			return resultValue
		}

		if ok && !intType.Signed {
			resultValue := e.builder.CreateNUWSub(
				value,
				llvm.ConstInt(e.typesMap[intType.Type()], 1, true),
				"unarydectmp",
			)
			e.builder.CreateStore(resultValue, ptrValue)
			return resultValue
		}

		floatType, ok := prefixExprHir.ExprType().(*hir_types.FloatType)
		if ok {
			resultValue := e.builder.CreateFSub(
				value,
				llvm.ConstFloat(e.typesMap[floatType.Type()], 1),
				"unarydectmp",
			)
			e.builder.CreateStore(resultValue, ptrValue)
			return resultValue
		}

		panic("unreachable")
	case hir.UnaryAddressOf:
		ptrValue := e.getPtrToLvalueExprHirValue(prefixExprHir.Expr.(hir.LvalueExprHir))
		return ptrValue
	case hir.UnaryDereference:
		value := e.emitForExprHir(prefixExprHir.Expr)
		innerPtrType := e.getLlvmTypeForType(prefixExprHir.ExprType())
		derefValue := e.builder.CreateLoad(innerPtrType, value, "dereftmp")
		return derefValue
	default:
		panic("unreachable")
	}
}

func (e *Emitter) emitForPostfixExprHir(postfixExprHir *hir.PostfixExprHir) llvm.Value {
	value := e.emitForExprHir(postfixExprHir.Expr)
	switch postfixExprHir.Op {
	case hir.UnaryInc:
		ptrValue := e.getPtrToLvalueExprHirValue(postfixExprHir.Expr.(hir.LvalueExprHir))

		intType, ok := postfixExprHir.ExprType().(*hir_types.IntType)
		if ok && intType.Signed {
			resultValue := e.builder.CreateNSWAdd(
				value,
				llvm.ConstInt(e.typesMap[intType.Type()], 1, true),
				"unaryinctmp",
			)
			e.builder.CreateStore(resultValue, ptrValue)
			return value
		}

		if ok && !intType.Signed {
			resultValue := e.builder.CreateNUWAdd(
				value,
				llvm.ConstInt(e.typesMap[intType.Type()], 1, true),
				"unaryinctmp",
			)
			e.builder.CreateStore(resultValue, ptrValue)
			return value
		}

		floatType, ok := postfixExprHir.ExprType().(*hir_types.FloatType)
		if ok {
			resultValue := e.builder.CreateFAdd(
				value,
				llvm.ConstFloat(e.typesMap[floatType.Type()], 1),
				"unaryinctmp",
			)
			e.builder.CreateStore(resultValue, ptrValue)
			return value
		}

		panic("unreachable")
	case hir.UnaryDec:
		ptrValue := e.getPtrToLvalueExprHirValue(postfixExprHir.Expr.(hir.LvalueExprHir))

		intType, ok := postfixExprHir.ExprType().(*hir_types.IntType)
		if ok && intType.Signed {
			resultValue := e.builder.CreateNSWSub(
				value,
				llvm.ConstInt(e.typesMap[intType.Type()], 1, true),
				"unarydectmp",
			)
			e.builder.CreateStore(resultValue, ptrValue)
			return value
		}

		if ok && !intType.Signed {
			resultValue := e.builder.CreateNUWSub(
				value,
				llvm.ConstInt(e.typesMap[intType.Type()], 1, true),
				"unarydectmp",
			)
			e.builder.CreateStore(resultValue, ptrValue)
			return value
		}

		floatType, ok := postfixExprHir.ExprType().(*hir_types.FloatType)
		if ok {
			resultValue := e.builder.CreateFSub(
				value,
				llvm.ConstFloat(e.typesMap[floatType.Type()], 1),
				"unarydectmp",
			)
			e.builder.CreateStore(resultValue, ptrValue)
			return value
		}

		panic("unreachable")
	default:
		panic("unreachable")
	}
}

func (e *Emitter) emitForTypeInstantiationExprHir(typeInstantiationExprHir *hir.TypeInstantiationExprHir, ptrToStruct llvm.Value) llvm.Value {
	ptrCreated := false
	structLlvmType := e.getLlvmTypeForType(typeInstantiationExprHir.Type)

	if ptrToStruct.IsNil() {
		ptrCreated = true
		currentBasicBlock := e.builder.GetInsertBlock()
		e.builder.SetInsertPointAtEnd(e.currentAllocBasicBlock)
		ptrToStruct = e.builder.CreateAlloca(
			structLlvmType,
			fmt.Sprintf("invisible_var_%s", typeInstantiationExprHir.TypeName),
		)
		e.builder.SetInsertPointAtEnd(currentBasicBlock)
	}

	initFunction := e.initFunctionsMap[typeInstantiationExprHir.TypeName]

	args := make([]llvm.Value, len(typeInstantiationExprHir.Instantiations)+1)
	args[0] = ptrToStruct
	for _, instantiation := range typeInstantiationExprHir.Instantiations {
		var value llvm.Value
		if innerTypeInstantiationExprHir, ok := instantiation.ExprHir.(*hir.TypeInstantiationExprHir); ok {
			memberGep := e.builder.CreateStructGEP(
				structLlvmType,
				ptrToStruct,
				instantiation.MemberPosition,
				fmt.Sprintf("%s::%s", typeInstantiationExprHir.TypeName, instantiation.MemberName),
			)
			value = e.emitForTypeInstantiationExprHir(innerTypeInstantiationExprHir, memberGep)
		} else if arrayExprHir, ok := instantiation.ExprHir.(*hir.ArrayExprHir); ok {
			memberGep := e.builder.CreateStructGEP(
				structLlvmType,
				ptrToStruct,
				instantiation.MemberPosition,
				fmt.Sprintf("%s::%s", typeInstantiationExprHir.TypeName, instantiation.MemberName),
			)
			value = e.emitForArrayExprHir(arrayExprHir, memberGep)
		} else {
			value = e.emitForExprHir(instantiation.ExprHir)
		}

		args[instantiation.MemberPosition+1] = value
	}

	e.builder.CreateCall(initFunction.GlobalValueType(), initFunction, args, "")

	if ptrCreated {
		return e.builder.CreateLoad(structLlvmType, ptrToStruct, "loadtmp")
	}

	return ptrToStruct
}

func (e *Emitter) emitForMemberAccessExprHir(memberAccessExprHir *hir.MemberAccessExprHir, depth int) llvm.Value {
	var leftValue llvm.Value
	if leftIdentExprHir, ok := memberAccessExprHir.Left.(*hir.IdentExprHir); ok {
		leftValue = e.variablesMap[leftIdentExprHir.Name]
	} else if leftMemberExprHir, ok := memberAccessExprHir.Left.(*hir.MemberAccessExprHir); ok {
		leftValue = e.emitForMemberAccessExprHir(leftMemberExprHir, depth+1)
	} else {
		leftValue = e.emitForExprHir(memberAccessExprHir.Left)
	}

	leftMemberType, ok := memberAccessExprHir.Left.ExprType().(*hir_types.UserType)
	if !ok {
		panic("member access should be user type")
	}

	rightIdentExprHir, ok := memberAccessExprHir.Right.(*hir.IdentExprHir)
	if !ok {
		panic("not implemented")
	}

	memberPosition := leftMemberType.MemberPositions[rightIdentExprHir.Name]
	// used with bar().member access
	if leftValue.Type().TypeKind() == llvm.StructTypeKind {
		return e.builder.CreateExtractValue(leftValue, memberPosition, "memberaccesstmp")
	}

	memberGep := e.builder.CreateStructGEP(
		e.getLlvmTypeForType(leftMemberType),
		leftValue,
		memberPosition,
		fmt.Sprintf("%s::%s", leftMemberType.Name, rightIdentExprHir.Name),
	)
	memberAccessExprType := e.getLlvmTypeForType(memberAccessExprHir.Type)

	if depth == 0 {
		return e.builder.CreateLoad(memberAccessExprType, memberGep, "memberaccesstmp")
	}

	return memberGep
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
	case hir.BinaryAdd:
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
	case hir.BinarySub:
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
	case hir.BinaryMul:
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
	case hir.BinaryDiv:
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
	case hir.BinaryMod:
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
	case hir.BinaryGt:
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
	case hir.BinaryGe:
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
	case hir.BinaryLt:
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
	case hir.BinaryLe:
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
	case hir.BinaryEq:
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
	case hir.BinaryNe:
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
	case hir.BinaryLand:
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
	case hir.BinaryLor:
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

func (e *Emitter) emitForArrayExprHir(arrayExprHir *hir.ArrayExprHir, ptrToArray llvm.Value) llvm.Value {
	ptrCreated := false
	arrayLlvmType := arrayExprHir.ExprType().(*hir_types.ArrayType)

	if ptrToArray.IsNil() {
		ptrCreated = true
		currentBasicBlock := e.builder.GetInsertBlock()
		e.builder.SetInsertPointAtEnd(e.currentAllocBasicBlock)
		ptrToArray = e.builder.CreateAlloca(
			e.getLlvmTypeForType(arrayLlvmType),
			fmt.Sprintf("invisible_array_%d_x_%s", arrayLlvmType.Size, arrayLlvmType.ItemType.Type()),
		)
		e.builder.SetInsertPointAtEnd(currentBasicBlock)
	}

	for i, exprHir := range arrayExprHir.Elements {
		var arrayGep llvm.Value
		if i == 0 {
			arrayGep = ptrToArray
		} else {
			arrayGep = e.builder.CreateInBoundsGEP(
				e.getLlvmTypeForType(arrayLlvmType.ItemType),
				ptrToArray,
				[]llvm.Value{llvm.ConstInt(e.typesMap["i32"], uint64(i), false)},
				"arraygep",
			)
		}

		if typeInstantiationExprHir, ok := exprHir.(*hir.TypeInstantiationExprHir); ok {
			e.emitForTypeInstantiationExprHir(typeInstantiationExprHir, arrayGep)
			continue
		} else if arrayExprHir, ok := exprHir.(*hir.ArrayExprHir); ok {
			e.emitForArrayExprHir(arrayExprHir, arrayGep)
			continue
		}

		value := e.emitForExprHir(exprHir)
		e.builder.CreateStore(value, arrayGep)
	}

	if ptrCreated {
		return e.builder.CreateLoad(
			e.getLlvmTypeForType(arrayLlvmType),
			ptrToArray,
			"loadtmp",
		)
	}

	return ptrToArray
}

func (e *Emitter) emitForArgIdentExprHir(argIdentExprHir *hir.ArgIdentExprHir) llvm.Value {
	return e.currentFunc.Param(argIdentExprHir.Index)
}

func (e *Emitter) emitForIdentExprHir(identExprHir *hir.IdentExprHir) llvm.Value {
	identType := e.getLlvmTypeForType(identExprHir.ExprType())
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
