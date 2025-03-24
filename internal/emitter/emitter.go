package emitter

import (
	"github.com/kievzenit/ylang/internal/hir"
	hir_types "github.com/kievzenit/ylang/internal/hir/types"
	"tinygo.org/x/go-llvm"
)

type Emitter struct {
	fileHir *hir.FileHir

	typesMap     map[string]llvm.Type
	variablesMap map[string]llvm.Value
	funcsMap     map[string]llvm.Value

	context llvm.Context
	module  llvm.Module
	builder llvm.Builder

	currentFunc            llvm.Value
	currentAllocBasicBlock llvm.BasicBlock
}

func NewEmitter(fileHir *hir.FileHir) *Emitter {
	context := llvm.NewContext()
	return &Emitter{
		fileHir: fileHir,

		typesMap:     make(map[string]llvm.Type),
		variablesMap: make(map[string]llvm.Value),
		funcsMap:     make(map[string]llvm.Value),

		context: context,
		module:  context.NewModule("main"),
		builder: context.NewBuilder(),
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
	e.builder.SetInsertPointAtEnd(entryBasicBlock)

	for _, stmtHir := range funcDeclStmtHir.Body.Stmts {
		e.emitForStmtHir(stmtHir)
	}

	e.builder.SetInsertPointAtEnd(allocBasicBlock)
	e.builder.CreateBr(entryBasicBlock)
	e.currentAllocBasicBlock = llvm.BasicBlock{}
}

func (e *Emitter) emitForStmtHir(stmtHir hir.StmtHir) {
	switch stmtHir.(type) {
	case *hir.VarDeclStmtHir:
		e.emitForVarDeclStmtHir(stmtHir.(*hir.VarDeclStmtHir))
	case *hir.ReturnStmtHir:
		e.emitForReturnStmtHir(stmtHir.(*hir.ReturnStmtHir))
	case *hir.ExprStmtHir:
		e.emitForExprStmtHir(stmtHir.(*hir.ExprStmtHir))
	default:
		panic("not implemented")
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

func (e *Emitter) emitForReturnStmtHir(returnStmtHir *hir.ReturnStmtHir) {
	if returnStmtHir.Expr != nil {
		value := e.emitForExprHir(returnStmtHir.Expr)
		e.builder.CreateRet(value)
		return
	}

	e.builder.CreateRetVoid()
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

func (e *Emitter) emitForBinExprHir(binExprHir *hir.BinaryExprHir) llvm.Value {
	leftValue := e.emitForExprHir(binExprHir.Left)
	rightValue := e.emitForExprHir(binExprHir.Right)

	var isInt bool
	var isUint bool
	var isFloat bool

	intType, ok := binExprHir.ExprType().(*hir_types.IntType)
	if ok && intType.Signed {
		isInt = true
	}
	if ok && !intType.Signed {
		isUint = true
	}

	_, ok = binExprHir.ExprType().(*hir_types.FloatType)
	if ok {
		isFloat = true
	}

	switch binExprHir.Op {
	case hir.Add:
		switch {
		case isInt, isUint:
			return e.builder.CreateAdd(leftValue, rightValue, "addtmp")
		case isFloat:
			return e.builder.CreateFAdd(leftValue, rightValue, "addtmp")
		default:
			panic("not implemented")
		}
	case hir.Sub:
		switch {
		case isInt, isUint:
			return e.builder.CreateSub(leftValue, rightValue, "subtmp")
		case isFloat:
			return e.builder.CreateFSub(leftValue, rightValue, "subtmp")
		default:
			panic("not implemented")
		}
	case hir.Mul:
		switch {
		case isInt, isUint:
			return e.builder.CreateMul(leftValue, rightValue, "multmp")
		case isFloat:
			return e.builder.CreateFMul(leftValue, rightValue, "multmp")
		default:
			panic("not implemented")
		}
	case hir.Div:
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
