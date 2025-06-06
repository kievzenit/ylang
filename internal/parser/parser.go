package parser

import (
	"fmt"
	"strconv"

	"slices"

	"github.com/kievzenit/ylang/internal/ast"
	"github.com/kievzenit/ylang/internal/compiler_errors"
	"github.com/kievzenit/ylang/internal/lexer"
)

type UnexpectedExpectedError struct {
	Unexpected lexer.TokenKind
	Expected   lexer.TokenKind

	FileName string
	Line     int
	Column   int
	Length   int
}

func (e *UnexpectedExpectedError) GetMessage() string {
	return fmt.Sprintf("unexpected token: '%s', expected: '%s'", e.Unexpected.String(), e.Expected.String())
}

func (e *UnexpectedExpectedError) GetFileName() string {
	return e.FileName
}

func (e *UnexpectedExpectedError) GetLine() int {
	return e.Line
}

func (e *UnexpectedExpectedError) GetColumn() int {
	return e.Column
}

func (e *UnexpectedExpectedError) GetLength() int {
	return e.Length
}

type UnexpectedExpectedManyError struct {
	Unexpected lexer.TokenKind
	Expected   []lexer.TokenKind

	FileName string
	Line     int
	Column   int
	Length   int
}

func (e *UnexpectedExpectedManyError) GetMessage() string {
	expectedKinds := make([]string, len(e.Expected))
	for i, kind := range e.Expected {
		expectedKinds[i] = kind.String()
	}
	return fmt.Sprintf("unexpected token: '%s', expected one of: '%s'", e.Unexpected.String(), expectedKinds)
}

func (e *UnexpectedExpectedManyError) GetFileName() string {
	return e.FileName
}

func (e *UnexpectedExpectedManyError) GetLine() int {
	return e.Line
}

func (e *UnexpectedExpectedManyError) GetColumn() int {
	return e.Column
}

func (e *UnexpectedExpectedManyError) GetLength() int {
	return e.Length
}

type UnexpectedError struct {
	Unexpected lexer.TokenKind

	FileName string
	Line     int
	Column   int
	Length   int
}

func (e *UnexpectedError) GetMessage() string {
	return fmt.Sprintf("unexpected token: '%s'", e.Unexpected.String())
}

func (e *UnexpectedError) GetFileName() string {
	return e.FileName
}

func (e *UnexpectedError) GetLine() int {
	return e.Line
}

func (e *UnexpectedError) GetColumn() int {
	return e.Column
}

func (e *UnexpectedError) GetLength() int {
	return e.Length
}

type Parser struct {
	fileName string

	scanner lexer.TokenScanner
	eh      compiler_errors.ErrorHandler

	curr *lexer.Token
}

var bindingPowerLookup map[lexer.TokenKind]int = map[lexer.TokenKind]int{
	lexer.LAND:     10,
	lexer.LOR:      10,
	lexer.LT:       20,
	lexer.LEQ:      20,
	lexer.GT:       20,
	lexer.GEQ:      20,
	lexer.EQ:       20,
	lexer.NEQ:      20,
	lexer.PLUS:     30,
	lexer.MINUS:    30,
	lexer.ASTERISK: 40,
	lexer.SLASH:    40,
	lexer.PERCENT:  40,
	lexer.BAND:     50,
	lexer.BOR:      50,
	lexer.XOR:      50,
	lexer.SHL:      50,
	lexer.SHR:      50,
}

func NewParser(fileName string, scanner lexer.TokenScanner, eh compiler_errors.ErrorHandler) *Parser {
	return &Parser{
		fileName: fileName,
		scanner:  scanner,
		eh:       eh,
		curr:     scanner.Read(),
	}
}

func (p *Parser) Parse() ast.TranslationUnit {
	packageStmt := p.parsePackageStmt()

	stmts := make([]ast.TopStmt, 0)
	for p.scanner.HasTokens() {
		stmts = append(stmts, p.parseTopSmt())
	}

	return ast.TranslationUnit{
		Package: packageStmt,
		Stmts:   stmts,
	}
}

func (p *Parser) parsePackageStmt() *ast.PackageStmt {
	var name string

	p.expect(lexer.PACKAGE)
	startToken := p.curr
	p.read()

	p.expect(lexer.IDENT)
	name += p.curr.Value
	p.read()

	for p.scanner.HasTokens() && p.curr.Kind != lexer.SEMICOLON {
		p.expect(lexer.COLONCOLON)
		name += "::"
		p.read()

		p.expect(lexer.IDENT)
		name += p.curr.Value
		p.read()
	}

	p.expect(lexer.SEMICOLON)
	p.read()

	return &ast.PackageStmt{
		StartToken: startToken,

		Name: name,
	}
}

func (p *Parser) parseTopSmt() ast.TopStmt {
	switch p.curr.Kind {
	case lexer.EXTERN:
		startToken := p.curr
		p.read()

		if p.curr.Kind == lexer.FUN {
			return p.parseFuncDeclStmt(true, startToken)
		}

		if p.curr.Kind == lexer.TYPE {
			return p.parseTypeStmt(true, startToken)
		}
	case lexer.FUN:
		return p.parseFuncDeclStmt(false, nil)
	case lexer.TYPE:
		return p.parseTypeStmt(false, nil)
	case lexer.STATIC:
		startToken := p.curr
		p.read()
		return p.parseVarDeclStmt(true, startToken, true)
	}

	p.eh.AddError(&UnexpectedError{
		Unexpected: p.curr.Kind,
	})
	p.eh.FailNow()
	panic("unreachable")
}

func (p *Parser) parseTypeStmt(extern bool, startToken *lexer.Token) *ast.TypeDeclStmt {
	p.expect(lexer.TYPE)
	if startToken == nil {
		startToken = p.curr
	}
	p.read()

	p.expect(lexer.IDENT)
	typeName := p.curr.Value
	p.read()

	p.expect(lexer.LBRACE)
	p.read()

	typeDeclStmt := &ast.TypeDeclStmt{
		StartToken: startToken,

		Name:         typeName,
		Members:      make([]ast.TypeMember, 0),
		Funcs:        make([]ast.TypeFuncMember, 0),
		Constructors: make([]ast.TypeConstructor, 0),
		Destructors:  make([]ast.TypeDestructor, 0),
		Extern:       extern,
	}

	if p.curr.Kind == lexer.RBRACE {
		return typeDeclStmt
	}

	for p.scanner.HasTokens() && p.curr.Kind != lexer.RBRACE {
		p.parseTypeMembers(typeDeclStmt)
	}

	p.expect(lexer.RBRACE)
	p.read()

	return typeDeclStmt
}

func (p *Parser) parseTypeMembers(typeDeclStmt *ast.TypeDeclStmt) {
	p.expectAny(lexer.PUBLIC, lexer.PRIVATE)
	accessModifier := p.curr
	p.read()

	p.expect(lexer.COLON)
	p.read()

	for p.scanner.HasTokens() && p.curr.Kind != lexer.RBRACE {
		switch p.curr.Kind {
		case lexer.PUBLIC, lexer.PRIVATE:
			if len(typeDeclStmt.Members) == 0 && len(typeDeclStmt.Funcs) == 0 {
				p.eh.AddError(&UnexpectedError{
					Unexpected: p.curr.Kind,
				})
				p.eh.FailNow()
			}
			return
		case lexer.IDENT:
			memberName := p.curr.Value
			p.read()

			p.expect(lexer.COLON)
			p.read()

			memberType := p.parseTypeIdentifier()

			p.expect(lexer.SEMICOLON)
			p.read()

			typeDeclStmt.Members = append(typeDeclStmt.Members, ast.TypeMember{
				Name:           memberName,
				Type:           memberType,
				AccessModifier: accessModifier,
			})
		case lexer.FUN:
			memberFunc := p.parseFuncDeclStmt(false, nil)
			typeDeclStmt.Funcs = append(typeDeclStmt.Funcs, ast.TypeFuncMember{
				FuncDeclStmt:   memberFunc,
				AccessModifier: accessModifier,
			})
		case lexer.CTOR:
			p.read()

			p.expect(lexer.LPAREN)
			p.read()

			args := make([]ast.FuncArg, 0)
			for p.scanner.HasTokens() && p.curr.Kind != lexer.RPAREN {
				p.expect(lexer.IDENT)
				argName := p.curr.Value
				p.read()

				p.expect(lexer.COLON)
				p.read()

				argType := p.parseTypeIdentifier()

				args = append(args, ast.FuncArg{
					Name: argName,
					Type: argType,
				})

				if p.curr.Kind == lexer.COMMA {
					p.read()
				}
			}

			p.expect(lexer.RPAREN)
			p.read()

			body := p.parseScopeStmt()

			typeDeclStmt.Constructors = append(typeDeclStmt.Constructors, ast.TypeConstructor{
				Args:           args,
				Body:           body,
				AccessModifier: accessModifier,
			})
		case lexer.DTOR:
			p.read()

			p.expect(lexer.LPAREN)
			p.read()

			p.expect(lexer.RPAREN)
			p.read()

			body := p.parseScopeStmt()

			typeDeclStmt.Destructors = append(typeDeclStmt.Destructors, ast.TypeDestructor{
				Body:           body,
				AccessModifier: accessModifier,
			})
		default:
			p.eh.AddError(&UnexpectedError{
				Unexpected: p.curr.Kind,
			})
			p.eh.FailNow()
		}
	}

	if len(typeDeclStmt.Members) == 0 && len(typeDeclStmt.Funcs) == 0 {
		p.eh.AddError(&UnexpectedError{
			Unexpected: p.curr.Kind,
		})
		p.eh.FailNow()
	}
}

func (p *Parser) parseFuncDeclStmt(
	extern bool,
	startToken *lexer.Token,
) *ast.FuncDeclStmt {
	p.expect(lexer.FUN)
	if startToken == nil {
		startToken = p.curr
	}
	p.read()

	p.expect(lexer.IDENT)
	name := p.curr.Value
	p.read()

	p.expect(lexer.LPAREN)
	p.read()

	args := make([]ast.FuncArg, 0)
	for p.scanner.HasTokens() && p.curr.Kind != lexer.RPAREN {
		p.expect(lexer.IDENT)
		argName := p.curr.Value
		p.read()

		p.expect(lexer.COLON)
		p.read()

		argType := p.parseTypeIdentifier()

		args = append(args, ast.FuncArg{
			Name: argName,
			Type: argType,
		})

		if p.curr.Kind == lexer.COMMA {
			p.read()
		}
	}

	p.expect(lexer.RPAREN)
	p.read()

	returnType := p.parseTypeIdentifier()

	if p.curr.Kind == lexer.SEMICOLON {
		p.read()
		return &ast.FuncDeclStmt{
			StartToken: startToken,

			Name:       name,
			ReturnType: returnType,
			Args:       args,
			Body:       nil,
			Extern:     extern,
		}
	}

	body := p.parseScopeStmt()

	return &ast.FuncDeclStmt{
		StartToken: startToken,

		Name:       name,
		ReturnType: returnType,
		Args:       args,
		Body:       body,
		Extern:     extern,
	}
}

func (p *Parser) parseStmt() ast.Stmt {
	switch p.curr.Kind {
	case lexer.IF, lexer.DO, lexer.WHILE, lexer.LOOP, lexer.FOR:
		return p.parseControlStmt()
	case lexer.RETURN, lexer.CONTINUE, lexer.BREAK, lexer.BREAKALL:
		return p.parseJumpStmt()
	case lexer.RBRACE:
		return p.parseScopeStmt()
	}

	return p.parseLocalStmt(true)
}

func (p *Parser) parseControlStmt() ast.Stmt {
	switch p.curr.Kind {
	case lexer.IF:
		return p.parseIfStmt()
	case lexer.DO:
		return p.parseDoWhileStmt()
	case lexer.WHILE:
		return p.parseWhileStmt()
	case lexer.LOOP:
		return p.parseLoopStmt()
	case lexer.FOR:
		return p.parseForStmt()
	}

	p.eh.AddError(&UnexpectedError{
		Unexpected: p.curr.Kind,
	})
	p.eh.FailNow()
	panic("unreachable")
}

func (p *Parser) parseJumpStmt() ast.Stmt {
	switch p.curr.Kind {
	case lexer.RETURN:
		return p.parseReturnStmt()
	case lexer.CONTINUE:
		return p.parseContinueStmt()
	case lexer.BREAK:
		return p.parseBreakStmt()
	case lexer.BREAKALL:
		return p.parseBreakAllStmt()
	}

	p.eh.AddError(&UnexpectedError{
		Unexpected: p.curr.Kind,
	})
	p.eh.FailNow()
	panic("unreachable")
}

func (p *Parser) parseLocalStmt(expectSemicolon bool) ast.Stmt {
	switch p.curr.Kind {
	case lexer.STATIC:
		startToken := p.curr
		p.read()
		return p.parseVarDeclStmt(true, startToken, expectSemicolon)
	case lexer.CONST:
		return p.parseVarDeclStmt(false, nil, expectSemicolon)
	case lexer.LET:
		return p.parseVarDeclStmt(false, nil, expectSemicolon)
	}

	return p.parseExprStmt(expectSemicolon)
}

func (p *Parser) parseReturnStmt() *ast.ReturnStmt {
	p.expect(lexer.RETURN)
	startToken := p.curr
	p.read()

	if p.curr.Kind == lexer.SEMICOLON {
		p.read()
		return &ast.ReturnStmt{
			StartToken: startToken,
		}
	}

	expr := p.parseExpr()
	p.expect(lexer.SEMICOLON)
	p.read()

	return &ast.ReturnStmt{
		StartToken: startToken,

		Expr: expr,
	}
}

func (p *Parser) parseContinueStmt() *ast.ContinueStmt {
	p.expect(lexer.CONTINUE)
	startToken := p.curr
	p.read()

	p.expect(lexer.SEMICOLON)
	p.read()

	return &ast.ContinueStmt{
		StartToken: startToken,
	}
}

func (p *Parser) parseBreakStmt() *ast.BreakStmt {
	p.expect(lexer.BREAK)
	startToken := p.curr
	p.read()

	p.expect(lexer.SEMICOLON)
	p.read()

	return &ast.BreakStmt{
		StartToken: startToken,
	}
}

func (p *Parser) parseBreakAllStmt() *ast.BreakAllStmt {
	p.expect(lexer.BREAKALL)
	startToken := p.curr
	p.read()

	p.expect(lexer.SEMICOLON)
	p.read()

	return &ast.BreakAllStmt{
		StartToken: startToken,
	}
}

func (p *Parser) parseIfStmt() *ast.IfStmt {
	p.expect(lexer.IF)
	startToken := p.curr
	p.read()

	cond := p.parseParenExpr()
	body := p.parseScopeStmt()

	if p.curr.Kind != lexer.ELSE {
		return &ast.IfStmt{
			StartToken: startToken,

			Cond:   cond,
			Body:   body,
			Else:   nil,
			ElseIf: nil,
		}
	}

	elseIfs := make([]ast.ElseIf, 0)
	for p.scanner.HasTokens() && p.curr.Kind == lexer.ELSE {
		startElseIfToken := p.curr
		p.read()
		if p.curr.Kind == lexer.IF {
			p.read()
			cond := p.parseParenExpr()
			body := p.parseScopeStmt()
			elseIfs = append(elseIfs, ast.ElseIf{
				StartToken: startElseIfToken,

				Cond: cond,
				Body: body,
			})
			continue
		}

		p.unread()
		break
	}

	if p.curr.Kind != lexer.ELSE {
		return &ast.IfStmt{
			StartToken: startToken,

			Cond:   cond,
			Body:   body,
			Else:   nil,
			ElseIf: elseIfs,
		}
	}

	p.expect(lexer.ELSE)
	p.read()
	elseBody := p.parseScopeStmt()

	return &ast.IfStmt{
		StartToken: startToken,

		Cond:   cond,
		Body:   body,
		Else:   elseBody,
		ElseIf: elseIfs,
	}
}

func (p *Parser) parseLoopStmt() *ast.LoopStmt {
	p.expect(lexer.LOOP)
	startToken := p.curr
	p.read()

	body := p.parseScopeStmt()

	return &ast.LoopStmt{
		StartToken: startToken,

		Body: body,
	}
}

func (p *Parser) parseForStmt() *ast.ForStmt {
	p.expect(lexer.FOR)
	startToken := p.curr
	p.read()

	p.expect(lexer.LPAREN)
	p.read()

	init := make([]ast.Stmt, 0)
	for p.scanner.HasTokens() && p.curr.Kind != lexer.SEMICOLON {
		init = append(init, p.parseLocalStmt(false))

		if p.curr.Kind == lexer.COMMA {
			p.read()
			continue
		} else if p.curr.Kind == lexer.SEMICOLON {
			break
		}

		p.eh.AddError(&UnexpectedExpectedManyError{
			Unexpected: p.curr.Kind,
			Expected:   []lexer.TokenKind{lexer.COMMA, lexer.SEMICOLON},
		})
		p.eh.FailNow()
	}

	p.expect(lexer.SEMICOLON)
	p.read()

	cond := p.parseExpr()
	p.expect(lexer.SEMICOLON)
	p.read()

	post := make([]ast.Expr, 0)
	for p.scanner.HasTokens() && p.curr.Kind != lexer.RPAREN {
		post = append(post, p.parseExpr())

		if p.curr.Kind == lexer.COMMA {
			p.read()
			continue
		} else if p.curr.Kind == lexer.RPAREN {
			break
		}

		p.eh.AddError(&UnexpectedExpectedManyError{
			Unexpected: p.curr.Kind,
			Expected:   []lexer.TokenKind{lexer.COMMA, lexer.RPAREN},
		})
		p.eh.FailNow()
	}

	p.expect(lexer.RPAREN)
	p.read()

	body := p.parseScopeStmt()

	return &ast.ForStmt{
		StartToken: startToken,

		Init: init,
		Cond: cond,
		Post: post,
		Body: body,
	}
}

func (p *Parser) parseDoWhileStmt() *ast.DoWhileStmt {
	p.expect(lexer.DO)
	startToken := p.curr
	p.read()

	body := p.parseScopeStmt()

	p.expect(lexer.WHILE)
	p.read()

	cond := p.parseParenExpr()

	p.expect(lexer.SEMICOLON)
	p.read()

	return &ast.DoWhileStmt{
		StartToken: startToken,

		Cond: cond,
		Body: body,
	}
}

func (p *Parser) parseWhileStmt() *ast.WhileStmt {
	p.expect(lexer.WHILE)
	startToken := p.curr
	p.read()

	cond := p.parseParenExpr()
	body := p.parseScopeStmt()

	return &ast.WhileStmt{
		StartToken: startToken,

		Cond: cond,
		Body: body,
	}
}

func (p *Parser) parseScopeStmt() *ast.ScopeStmt {
	p.expect(lexer.LBRACE)
	startToken := p.curr
	p.read()

	stmts := make([]ast.Stmt, 0)
	for p.scanner.HasTokens() && p.curr.Kind != lexer.RBRACE {
		stmts = append(stmts, p.parseStmt())
	}

	p.expect(lexer.RBRACE)
	p.read()

	return &ast.ScopeStmt{
		StartToken: startToken,

		Stmts: stmts,
	}
}

func (p *Parser) parseVarDeclStmt(
	static bool,
	startToken *lexer.Token,
	expectSemicolon bool,
) *ast.VarDeclStmt {
	var explicitType ast.TypeNode

	p.expectAny(lexer.LET, lexer.CONST)
	if startToken == nil {
		startToken = p.curr
	}
	isConst := p.curr.Kind == lexer.CONST

	p.read()
	p.expect(lexer.IDENT)
	varName := p.curr.Value
	p.read()

	if p.curr.Kind == lexer.COLON {
		p.read()
		explicitType = p.parseTypeIdentifier()
	}

	p.expect(lexer.ASSIGN)

	p.read()
	expr := p.parseExpr()

	if expectSemicolon {
		p.expect(lexer.SEMICOLON)
		p.read()
	}

	return &ast.VarDeclStmt{
		StartToken: startToken,

		Name:         varName,
		ExplicitType: explicitType,
		Value:        expr,
		Static:       static,
		Const:        isConst,
	}
}

func (p *Parser) parseExprStmt(expectSemicolon bool) ast.Stmt {
	expr := p.parseExpr()

	if expectSemicolon {
		p.expect(lexer.SEMICOLON)
		p.read()
	}

	return &ast.ExprStmt{
		Expr: expr,
	}
}

func (p *Parser) parseBaseTypeIdentifier(isConst bool) *ast.IdentTypeNode {
	var typeName string

	p.expect(lexer.IDENT)
	typeName = p.curr.Value
	p.read()

	// TODO: add support for package name specified in type name
	// for p.scanner.HasTokens() && p.curr.Kind == lexer.COLONCOLON {
	// 	typeName += p.curr.Value
	// 	p.read()

	// 	p.expect(lexer.IDENT)
	// 	typeName += p.curr.Value
	// 	p.read()
	// }

	return &ast.IdentTypeNode{
		Name:  typeName,
		Const: isConst,
	}
}

func (p *Parser) parseTypeIdentifierInner(isCurrConst bool) ast.TypeNode {
	p.expectAny(lexer.IDENT, lexer.LBRACKET, lexer.ASTERISK)

	switch p.curr.Kind {
	case lexer.IDENT:
		return p.parseBaseTypeIdentifier(isCurrConst)
	case lexer.LBRACKET:
		p.read()

		if p.curr.Kind == lexer.RBRACKET {
			p.read()
			return &ast.SliceTypeNode{
				InnerType: p.parseTypeIdentifier(),
				Const:     isCurrConst,
			}
		}

		p.expect(lexer.INT)
		size, _ := strconv.Atoi(p.curr.Value)
		p.read()

		p.expect(lexer.RBRACKET)
		p.read()

		return &ast.ArrayTypeNode{
			Size:      size,
			InnerType: p.parseTypeIdentifier(),
			Const:     isCurrConst,
		}
	case lexer.ASTERISK:
		p.read()

		return &ast.PointerTypeNode{
			InnerType: p.parseTypeIdentifier(),
			Const:     isCurrConst,
		}
	default:
		panic("unreachable")
	}
}

func (p *Parser) parseTypeIdentifier() ast.TypeNode {
	isInnerConst := false
	if p.curr.Kind == lexer.CONST {
		isInnerConst = true
		p.read()
	}

	return p.parseTypeIdentifierInner(isInnerConst)
}

func (p *Parser) parseExpr() ast.Expr {
	left := p.parseAssignExpr()
	return p.parseBinaryExpr(left, 0)
}

func (p *Parser) parseUnaryExpr() ast.Expr {
	if p.isCurrAny(
		lexer.INC,
		lexer.DEC,
		lexer.PLUS,
		lexer.MINUS,
		lexer.XMARK,
		lexer.TILDE,
		lexer.BAND,
		lexer.ASTERISK,
	) {
		op := p.curr
		p.read()

		expr := p.parsePrimaryExpr()

		return &ast.PrefixExpr{
			StartToken: op,

			Op:    op,
			Right: expr,
		}
	}

	expr := p.parsePrimaryExpr()

	if p.isCurrAny(lexer.INC, lexer.DEC) {
		op := p.curr
		p.read()

		return &ast.PostfixExpr{
			StartToken: op,

			Left: expr,
			Op:   op,
		}
	}

	return expr
}

func (p *Parser) parsePrimaryExpr() ast.Expr {
	var expr ast.Expr
	switch p.curr.Kind {
	case lexer.LPAREN:
		expr = p.parseParenExpr()
	case lexer.LBRACKET:
		expr = p.parseArrayExpression()
	case lexer.IDENT:
		p.read()
		if p.curr.Kind == lexer.LPAREN {
			p.unread()
			expr = p.parseCallExpr()
			break
		}

		if p.isCurrAny(lexer.TYPE_INIT, lexer.TYPE_CONSTRUCT) {
			p.unread()
			expr = p.parseTypeExpr()
			break
		}

		p.unread()
		expr = p.parseIdentExpr()
	default:
		expr = p.parseLiteralExpr()
	}

	for p.scanner.HasTokens() && p.isCurrAny(lexer.DOT, lexer.LBRACKET, lexer.CAST) {
		switch p.curr.Kind {
		case lexer.DOT:
			p.read()

			p.expect(lexer.IDENT)
			memberName := p.curr.Value
			p.read()

			var right ast.Expr
			switch p.curr.Kind {
			case lexer.LPAREN:
				p.unread()
				right = p.parseCallExpr()
			default:
				right = &ast.IdentExpr{
					StartToken: p.curr,

					Value: memberName,
				}
			}

			expr = &ast.MemberAccessExpr{
				StartToken: expr.FirstToken(),

				Left:  expr,
				Right: right,
			}
		case lexer.LBRACKET:
			p.read()

			indexExpr := p.parseExpr()

			p.expect(lexer.RBRACKET)
			p.read()

			expr = &ast.ArraySubscriptExpr{
				StartToken: expr.FirstToken(),

				Left:  expr,
				Index: indexExpr,
			}
		case lexer.CAST:
			p.read()

			if p.curr.Kind != lexer.LBRACE {
				typeName := p.parseTypeIdentifier()

				expr = &ast.CastExpr{
					StartToken: expr.FirstToken(),

					Left:       expr,
					NewIdent:   nil,
					CastToType: typeName,
				}
				continue
			}

			p.expect(lexer.LBRACE)
			p.read()

			newIdent := p.parseIdentExpr()

			p.expect(lexer.COLON)
			p.read()

			typeName := p.parseTypeIdentifier()

			p.expect(lexer.RBRACE)
			p.read()

			expr = &ast.CastExpr{
				StartToken: expr.FirstToken(),

				Left:       expr,
				NewIdent:   newIdent,
				CastToType: typeName,
			}
		}
	}

	return expr
}

func (p *Parser) parseAssignExpr() ast.Expr {
	unaryExpr := p.parseUnaryExpr()

	if !p.isCurrAny(lexer.ASSIGN,
		lexer.ADD_ASSIGN,
		lexer.SUB_ASSIGN,
		lexer.MUL_ASSIGN,
		lexer.DIV_ASSIGN,
		lexer.MOD_ASSIGN,
		lexer.BAND_ASSIGN,
		lexer.BOR_ASSIGN,
		lexer.XOR_ASSIGN,
		lexer.SHR_ASSIGN,
		lexer.SHL_ASSIGN) {
		return unaryExpr
	}

	op := p.curr
	p.read()

	value := p.parseExpr()

	return &ast.AssignExpr{
		StartToken: unaryExpr.FirstToken(),

		Left:  unaryExpr,
		Op:    op,
		Right: value,
	}
}

func (p *Parser) parseParenExpr() ast.Expr {
	p.expect(lexer.LPAREN)
	p.read()

	expr := p.parseExpr()

	p.expect(lexer.RPAREN)
	p.read()

	return expr
}

func (p *Parser) parseArrayExpression() ast.Expr {
	p.expect(lexer.LBRACKET)
	startToken := p.curr
	p.read()

	elements := make([]ast.Expr, 0)
	expr := p.parseExpr()
	elements = append(elements, expr)

	for p.scanner.HasTokens() && p.curr.Kind != lexer.RBRACKET {
		p.expect(lexer.COMMA)
		p.read()

		expr := p.parseExpr()
		elements = append(elements, expr)
	}

	p.expect(lexer.RBRACKET)
	p.read()

	return &ast.ArrayExpr{
		StartToken: startToken,

		Elements: elements,
	}
}

func (p *Parser) parseTypeExpr() ast.Expr {
	startToken := p.curr
	typeName := p.parseBaseTypeIdentifier(false)

	p.expectAny(lexer.TYPE_INIT, lexer.TYPE_CONSTRUCT)
	switch p.curr.Kind {
	case lexer.TYPE_INIT:
		return p.parseTypeInstantiationExpr(typeName, startToken)
	case lexer.TYPE_CONSTRUCT:
		return p.parseTypeConstructionExpr(typeName, startToken)
	default:
		panic("unreachable")
	}
}

func (p *Parser) parseTypeInstantiationExpr(
	typeName ast.TypeNode,
	startToken *lexer.Token,
) *ast.TypeInstantiationExpr {
	p.expect(lexer.TYPE_INIT)
	p.read()

	instantiations := make([]ast.TypeMemberInstantiation, 0)
	for p.scanner.HasTokens() && p.curr.Kind != lexer.RBRACE {
		p.expect(lexer.IDENT)
		memberName := p.curr.Value
		p.read()

		p.expect(lexer.ASSIGN)
		p.read()

		instantiationExpr := p.parseExpr()

		instantiations = append(instantiations, ast.TypeMemberInstantiation{
			Name: memberName,
			Expr: instantiationExpr,
		})

		if p.curr.Kind == lexer.COMMA {
			p.read()
		}
	}

	p.expect(lexer.RBRACE)
	p.read()

	return &ast.TypeInstantiationExpr{
		StartToken: startToken,

		TypeName:       typeName,
		Instantiations: instantiations,
	}
}

func (p *Parser) parseTypeConstructionExpr(
	typeName ast.TypeNode,
	startToken *lexer.Token,
) *ast.TypeConstructionExpr {
	p.expect(lexer.TYPE_CONSTRUCT)
	p.read()

	args := make([]ast.Expr, 0)
	for p.scanner.HasTokens() && p.curr.Kind != lexer.RPAREN {
		args = append(args, p.parseExpr())
		if p.curr.Kind == lexer.COMMA {
			p.read()
		}
	}

	p.expect(lexer.RPAREN)
	p.read()

	return &ast.TypeConstructionExpr{
		StartToken: startToken,

		TypeName: typeName,
		Args:     args,
	}
}

func (p *Parser) parseBinaryExpr(left ast.Expr, bindingPower int) ast.Expr {
	for {
		op := p.curr
		currentBindingPower, ok := bindingPowerLookup[op.Kind]
		if !ok || currentBindingPower < bindingPower {
			return left
		}
		p.read()

		right := p.parseAssignExpr()

		nextBindingPower, ok := bindingPowerLookup[p.curr.Kind]
		if !ok || currentBindingPower < nextBindingPower {
			right = p.parseBinaryExpr(right, currentBindingPower+10)
		}

		left = &ast.BinaryExpr{
			StartToken: left.FirstToken(),

			Left:  left,
			Op:    op,
			Right: right,
		}
	}
}

func (p *Parser) parseCallExpr() *ast.CallExpr {
	p.expect(lexer.IDENT)
	startToken := p.curr
	name := p.curr.Value
	p.read()

	p.expect(lexer.LPAREN)
	p.read()

	args := make([]ast.Expr, 0)
	for p.scanner.HasTokens() && p.curr.Kind != lexer.RPAREN {
		args = append(args, p.parseExpr())
		if p.curr.Kind == lexer.COMMA {
			p.read()
		}
	}

	p.expect(lexer.RPAREN)
	p.read()

	return &ast.CallExpr{
		StartToken: startToken,

		Name: name,
		Args: args,
	}
}

func (p *Parser) parseIdentExpr() *ast.IdentExpr {
	p.expect(lexer.IDENT)

	startToken := p.curr
	ident := p.curr.Value
	p.read()

	return &ast.IdentExpr{
		StartToken: startToken,

		Value: ident,
	}
}

func (p *Parser) parseLiteralExpr() ast.Expr {
	switch p.curr.Kind {
	case lexer.INT:
		return p.parseIntegerExpr()
	case lexer.FLOAT:
		return p.parseFloatExpr()
	case lexer.BOOL:
		return p.parseBoolExpr()
	case lexer.CHAR:
		return p.parseCharExpr()
	case lexer.STRING:
		return p.parseStringExpr()
	}

	p.unexpected(p.curr.Kind)
	panic("unreachable")
}

func (p *Parser) parseIntegerExpr() *ast.IntExpr {
	p.expect(lexer.INT)
	startToken := p.curr

	int, err := strconv.ParseInt(p.curr.Value, 10, 64)
	if err != nil {
		panic(err)
	}

	p.read()
	explicitType := ast.IntNone
	if p.curr.Kind == lexer.COLON {
		p.read()
		explicitTypeString := p.curr.Value
		switch explicitTypeString {
		case string(ast.Int8Type):
			explicitType = ast.Int8Type
		case string(ast.Int16Type):
			explicitType = ast.Int16Type
		case string(ast.Int32Type):
			explicitType = ast.Int32Type
		case string(ast.Int64Type):
			explicitType = ast.Int64Type
		case string(ast.Int128Type):
			explicitType = ast.Int128Type
		case string(ast.Uint1Type):
			explicitType = ast.Uint1Type
		case string(ast.Uint8Type):
			explicitType = ast.Uint8Type
		case string(ast.Uint16Type):
			explicitType = ast.Uint16Type
		case string(ast.Uint32Type):
			explicitType = ast.Uint32Type
		case string(ast.Uint64Type):
			explicitType = ast.Uint64Type
		case string(ast.Uint128Type):
			explicitType = ast.Uint128Type
		default:
			p.eh.AddError(&UnexpectedError{
				Unexpected: p.curr.Kind,
			})
			p.eh.FailNow()
		}

		p.read()
	}

	return &ast.IntExpr{
		StartToken: startToken,

		Value:        int,
		ExplicitType: explicitType,
	}
}

func (p *Parser) parseFloatExpr() *ast.FloatExpr {
	p.expect(lexer.FLOAT)
	startToken := p.curr

	float, err := strconv.ParseFloat(p.curr.Value, 64)
	if err != nil {
		panic(err)
	}

	p.read()
	explicitType := ast.FloatNone
	if p.curr.Kind == lexer.COLON {
		p.read()
		explicitTypeString := p.curr.Value
		switch explicitTypeString {
		case string(ast.Float16Type):
			explicitType = ast.Float16Type
		case string(ast.Float32Type):
			explicitType = ast.Float32Type
		case string(ast.Float64Type):
			explicitType = ast.Float64Type
		case string(ast.Float80Type):
			explicitType = ast.Float80Type
		case string(ast.Float128Type):
			explicitType = ast.Float128Type
		default:
			p.eh.AddError(&UnexpectedError{
				Unexpected: p.curr.Kind,
			})
			p.eh.FailNow()
		}

		p.read()
	}

	return &ast.FloatExpr{
		StartToken: startToken,

		Value:        float,
		ExplicitType: explicitType,
	}
}

func (p *Parser) parseBoolExpr() *ast.BoolExpr {
	p.expect(lexer.BOOL)
	startToken := p.curr

	bool, err := strconv.ParseBool(p.curr.Value)
	if err != nil {
		panic(err)
	}

	p.read()

	return &ast.BoolExpr{
		StartToken: startToken,

		Value: bool,
	}
}

func (p *Parser) parseCharExpr() *ast.CharExpr {
	p.expect(lexer.CHAR)
	startToken := p.curr

	byte := p.curr.Value[0]
	p.read()

	return &ast.CharExpr{
		StartToken: startToken,

		Value: byte,
	}
}

func (p *Parser) parseStringExpr() *ast.StringExpr {
	p.expect(lexer.STRING)
	startToken := p.curr
	p.read()

	return &ast.StringExpr{
		StartToken: startToken,

		Value: p.curr.Value,
	}
}

func (p *Parser) read() *lexer.Token {
	p.curr = p.scanner.Read()
	return p.curr
}

func (p *Parser) unread() *lexer.Token {
	p.curr = p.scanner.Unread()
	return p.curr
}

func (p *Parser) expect(kind lexer.TokenKind) {
	if p.curr.Kind != kind {
		p.eh.AddError(&UnexpectedExpectedError{
			Unexpected: p.curr.Kind,
			Expected:   kind,

			FileName: p.fileName,
			Line:     p.curr.Metadata.Line,
			Column:   p.curr.Metadata.Column,
			Length:   p.curr.Metadata.Length,
		})
		p.eh.FailNow()
	}
}

func (p *Parser) expectAny(kinds ...lexer.TokenKind) {
	found := p.isCurrAny(kinds...)
	if found {
		return
	}

	p.eh.AddError(&UnexpectedExpectedManyError{
		Unexpected: p.curr.Kind,
		Expected:   kinds,

		FileName: p.fileName,
		Line:     p.curr.Metadata.Line,
		Column:   p.curr.Metadata.Column,
		Length:   p.curr.Metadata.Length,
	})
	p.eh.FailNow()
}

func (p *Parser) isCurrAny(kinds ...lexer.TokenKind) bool {
	return slices.Contains(kinds, p.curr.Kind)
}

func (p *Parser) unexpected(kind lexer.TokenKind) {
	p.eh.AddError(&UnexpectedError{
		Unexpected: kind,

		FileName: p.fileName,
		Line:     p.curr.Metadata.Line,
		Column:   p.curr.Metadata.Column,
		Length:   p.curr.Metadata.Length,
	})
	p.eh.FailNow()
}
