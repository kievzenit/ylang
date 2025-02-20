package parser

import (
	"fmt"
	"strconv"

	"github.com/kievzenit/ylang/internal/compiler_errors"
	"github.com/kievzenit/ylang/internal/lexer"
	"github.com/kievzenit/ylang/internal/parser/ast"
)

type UnexpectedExpectedError struct {
	Unexpected lexer.TokenKind
	Expected   lexer.TokenKind
}

func (e *UnexpectedExpectedError) GetMessage() string {
	return fmt.Sprintf("unexpected token: '%s', expected: '%s'", e.Unexpected.String(), e.Expected.String())
}

type UnexpectedExpectedManyError struct {
	Unexpected lexer.TokenKind
	Expected   []lexer.TokenKind
}

func (e *UnexpectedExpectedManyError) GetMessage() string {
	expectedKinds := make([]string, len(e.Expected))
	for i, kind := range e.Expected {
		expectedKinds[i] = kind.String()
	}
	return fmt.Sprintf("unexpected token: '%s', expected one of: '%s'", e.Unexpected.String(), expectedKinds)
}

type UnexpectedError struct {
	Unexpected lexer.TokenKind
}

func (e *UnexpectedError) GetMessage() string {
	return fmt.Sprintf("unexpected token: '%s'", e.Unexpected.String())
}

type Parser struct {
	scanner lexer.TokenScanner
	eh      compiler_errors.ErrorHandler

	curr lexer.Token
}

var bindingPowerLookup map[lexer.TokenKind]int = map[lexer.TokenKind]int{
	lexer.PLUS:     10,
	lexer.MINUS:    10,
	lexer.ASTERISK: 20,
	lexer.SLASH:    20,
	lexer.PERCENT:  20,
}

func NewParser(scanner lexer.TokenScanner, eh compiler_errors.ErrorHandler) *Parser {
	return &Parser{
		scanner: scanner,
		eh:      eh,
		curr:    scanner.Read(),
	}
}

func (p *Parser) Parse() []ast.TopStmt {
	stmts := make([]ast.TopStmt, 0)
	for p.scanner.HasTokens() {
		stmts = append(stmts, p.parseTopSmt())
	}

	return stmts
}

func (p *Parser) parseTopSmt() ast.TopStmt {
	switch p.curr.Kind {
	case lexer.EXTERN:
		p.read()
		return p.parseFuncDeclStmt(true)
	case lexer.FUN:
		return p.parseFuncDeclStmt(false)
	case lexer.STATIC:
		p.read()
		return p.parseVarDeclStmt(true)
	case lexer.CONST:
		return p.parseVarDeclStmt(false)
	case lexer.LET:
		return p.parseVarDeclStmt(false)
	}

	p.eh.AddError(&UnexpectedError{
		Unexpected: p.curr.Kind,
	})
	p.eh.FailNow()
	panic("unreachable")
}

func (p *Parser) parseFuncDeclStmt(extern bool) *ast.FuncDeclStmt {
	p.expect(lexer.FUN)
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

		p.expect(lexer.IDENT)
		argType := p.curr.Value
		p.read()

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

	p.expect(lexer.IDENT)
	returnType := p.curr.Value
	p.read()

	body := p.parseScopeStmt()

	return &ast.FuncDeclStmt{
		Name:  name,
		ReturnType: returnType,
		Args:  args,
		Body:  body,
		Extern: extern,
	}
}

func (p *Parser) parseStmt() ast.Stmt {
	switch p.curr.Kind {
	case lexer.STATIC:
		p.read()
		return p.parseVarDeclStmt(true)
	case lexer.CONST:
		return p.parseVarDeclStmt(false)
	case lexer.LET:
		return p.parseVarDeclStmt(false)
	}

	return p.parseExprStmt()
}

func (p *Parser) parseScopeStmt() *ast.ScopeStmt {
	p.expect(lexer.LBRACE)
	p.read()

	stmts := make([]ast.Stmt, 0)
	for p.scanner.HasTokens() && p.curr.Kind != lexer.RBRACE {
		stmts = append(stmts, p.parseStmt())
	}

	p.expect(lexer.RBRACE)
	p.read()

	return &ast.ScopeStmt{
		Stmts: stmts,
	}
}

func (p *Parser) parseVarDeclStmt(static bool) *ast.VarDeclStmt {
	p.expectAny(lexer.LET, lexer.CONST)
	isConst := p.curr.Kind == lexer.CONST

	p.read()
	p.expect(lexer.IDENT)
	varName := p.curr.Value

	p.read()
	p.expect(lexer.ASSIGN)

	p.read()
	expr := p.parseExpr()
	p.expect(lexer.SEMICOLON)

	p.read()

	return &ast.VarDeclStmt{
		Name:   varName,
		Value:  expr,
		Static: static,
		Const:  isConst,
	}
}

func (p *Parser) parseExprStmt() ast.Stmt {
	expr := p.parseExpr()
	p.expect(lexer.SEMICOLON)
	p.read()

	return &ast.ExprStmt{
		Expr: expr,
	}
}

func (p *Parser) parseExpr() ast.Expr {
	left := p.parsePrimaryExpr()
	return p.parseBinaryExpr(left, 0)
}

func (p *Parser) parsePrimaryExpr() ast.Expr {
	if p.curr.Kind == lexer.IDENT {
		p.read()
		if p.curr.Kind == lexer.LPAREN {
			p.unread()
			return p.parseCallExpr()
		}

		p.unread()
		return p.parseIdentExpr()
	}

	return p.parseLiteralExpr()
}

func (p *Parser) parseBinaryExpr(left ast.Expr, bindingPower int) ast.Expr {
	for {
		op := p.curr
		currentBindingPower, ok := bindingPowerLookup[op.Kind]
		if !ok || currentBindingPower < bindingPower {
			return left
		}
		p.read()

		right := p.parsePrimaryExpr()

		nextBindingPower, ok := bindingPowerLookup[p.curr.Kind]
		if !ok || currentBindingPower < nextBindingPower {
			right = p.parseBinaryExpr(right, currentBindingPower+10)
		}

		left = &ast.BinaryExpr{
			Left:  left,
			Op:    op,
			Right: right,
		}
	}
}

func (p *Parser) parseCallExpr() ast.Expr {
	p.expect(lexer.IDENT)
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
		Name: name,
		Args: args,
	}
}

func (p *Parser) parseIdentExpr() ast.Expr {
	p.expect(lexer.IDENT)

	ident := p.curr.Value
	p.read()

	return &ast.IdentExpr{
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

func (p *Parser) parseIntegerExpr() ast.Expr {
	p.expect(lexer.INT)

	int, err := strconv.ParseInt(p.curr.Value, 10, 64)
	if err != nil {
		panic(err)
	}

	p.read()

	return &ast.IntExpr{
		Value: int,
	}
}

func (p *Parser) parseFloatExpr() ast.Expr {
	p.expect(lexer.FLOAT)

	float, err := strconv.ParseFloat(p.curr.Value, 64)
	if err != nil {
		panic(err)
	}

	p.read()

	return &ast.FloatExpr{
		Value: float,
	}
}

func (p *Parser) parseBoolExpr() ast.Expr {
	p.expect(lexer.BOOL)

	bool, err := strconv.ParseBool(p.curr.Value)
	if err != nil {
		panic(err)
	}

	p.read()

	return &ast.BoolExpr{
		Value: bool,
	}
}

func (p *Parser) parseCharExpr() ast.Expr {
	p.expect(lexer.CHAR)

	byte := p.curr.Value[0]
	p.read()

	return &ast.CharExpr{
		Value: byte,
	}
}

func (p *Parser) parseStringExpr() ast.Expr {
	p.expect(lexer.STRING)
	p.read()

	return &ast.StringExpr{
		Value: p.curr.Value,
	}
}

func (p *Parser) read() lexer.Token {
	p.curr = p.scanner.Read()
	return p.curr
}

func (p *Parser) unread() lexer.Token {
	p.curr = p.scanner.Unread()
	return p.curr
}

func (p *Parser) expect(kind lexer.TokenKind) {
	if p.curr.Kind != kind {
		p.eh.AddError(&UnexpectedExpectedError{
			Unexpected: p.curr.Kind,
			Expected:   kind,
		})
		p.eh.FailNow()
	}
}

func (p *Parser) expectAny(kinds ...lexer.TokenKind) {
	for _, kind := range kinds {
		if p.curr.Kind == kind {
			return
		}
	}
	p.eh.AddError(&UnexpectedExpectedManyError{
		Unexpected: p.curr.Kind,
		Expected:   kinds,
	})
	p.eh.FailNow()
}

func (p *Parser) unexpected(kind lexer.TokenKind) {
	p.eh.AddError(&UnexpectedError{
		Unexpected: kind,
	})
	p.eh.FailNow()
}
