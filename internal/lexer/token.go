package lexer

import (
	"fmt"
)

type TokenKind int

const (
	EOF TokenKind = iota

	INT
	FLOAT
	BOOL
	CHAR
	STRING

	IDENT

	PLUS     // +
	MINUS    // -
	ASTERISK // *
	SLASH    // /
	PERCENT  // %

	ASSIGN // =

	ADD_ASSIGN // +=
	SUB_ASSIGN // -=
	MUL_ASSIGN // *=
	DIV_ASSIGN // /=
	MOD_ASSIGN // %=

	INC // ++
	DEC // --

	BAND // &
	BOR  // |
	BXOR // ^
	SHR  // >>
	SHL  // <<

	LAND // &&
	LOR  // ||

	EQ  // ==
	NEQ // !=
	LT  // <
	LEQ // <=
	GT  // >
	GEQ // >=

	LPAREN   // (
	LBRACKET // [
	LBRACE   // {

	RPAREN   // )
	RBRACKET // ]
	RBRACE   // }

	COLON      // :
	COLONCOLON // ::
	SEMICOLON  // ;
	DOT        // .
	COMMA      // ,
	RANGE      // ..
	ELLIPSIS   // ...
	CAST       // >-
	XMARK      // !
	QMARK      // ?

	PACKAGE
	EXPORT
	EXTERN
	STATIC
	TYPE
	PUBLIC
	PRIVATE
	FUN
	LET
	CONST
	LOOP
	WHILE
	DO
	FOR
	IF
	ELSE
	CONTINUE
	BREAK
	BREAKALL
	RETURN
)

func (tk TokenKind) String() string {
	switch tk {
	case EOF:
		return "EOF"
	case INT:
		return "INT"
	case FLOAT:
		return "FLOAT"
	case BOOL:
		return "BOOL"
	case CHAR:
		return "CHAR"
	case STRING:
		return "STRING"
	case IDENT:
		return "IDENT"
	case PLUS:
		return "PLUS"
	case MINUS:
		return "MINUS"
	case ASTERISK:
		return "ASTERISK"
	case SLASH:
		return "SLASH"
	case PERCENT:
		return "PERCENT"
	case ASSIGN:
		return "ASSIGN"
	case ADD_ASSIGN:
		return "ADD_ASSIGN"
	case SUB_ASSIGN:
		return "SUB_ASSIGN"
	case MUL_ASSIGN:
		return "MUL_ASSIGN"
	case DIV_ASSIGN:
		return "DIV_ASSIGN"
	case MOD_ASSIGN:
		return "MOD_ASSIGN"
	case INC:
		return "INC"
	case DEC:
		return "DEC"
	case BAND:
		return "BAND"
	case BOR:
		return "BOR"
	case BXOR:
		return "BXOR"
	case SHR:
		return "SHR"
	case SHL:
		return "SHL"
	case LAND:
		return "LAND"
	case LOR:
		return "LOR"
	case EQ:
		return "EQ"
	case NEQ:
		return "NEQ"
	case LT:
		return "LT"
	case LEQ:
		return "LEQ"
	case GT:
		return "GT"
	case GEQ:
		return "GEQ"
	case LPAREN:
		return "LPAREN"
	case LBRACKET:
		return "LBRACKET"
	case LBRACE:
		return "LBRACE"
	case RPAREN:
		return "RPAREN"
	case RBRACKET:
		return "RBRACKET"
	case RBRACE:
		return "RBRACE"
	case COLON:
		return "COLON"
	case COLONCOLON:
		return "COLONCOLON"
	case SEMICOLON:
		return "SEMICOLON"
	case DOT:
		return "DOT"
	case COMMA:
		return "COMMA"
	case RANGE:
		return "RANGE"
	case ELLIPSIS:
		return "ELLIPSIS"
	case CAST:
		return "CAST"
	case XMARK:
		return "XMARK"
	case QMARK:
		return "QMARK"
	case PACKAGE:
		return "PACKAGE"
	case EXPORT:
		return "EXPORT"
	case EXTERN:
		return "EXTERN"
	case STATIC:
		return "STATIC"
	case TYPE:
		return "TYPE"
	case PUBLIC:
		return "PUBLIC"
	case PRIVATE:
		return "PRIVATE"
	case FUN:
		return "FUN"
	case LET:
		return "LET"
	case CONST:
		return "CONST"
	case LOOP:
		return "LOOP"
	case WHILE:
		return "WHILE"
	case DO:
		return "DO"
	case FOR:
		return "FOR"
	case IF:
		return "IF"
	case ELSE:
		return "ELSE"
	case CONTINUE:
		return "CONTINUE"
	case BREAK:
		return "BREAK"
	case BREAKALL:
		return "BREAKALL"
	case RETURN:
		return "RETURN"
	default:
		panic(fmt.Sprintf("TokenKind.String(): received illegal token kind: %d", tk))
	}
}

type Token struct {
	Kind  TokenKind
	Value string
}

func (t *Token) hasActualValue() bool {
	switch t.Kind {
	case INT, FLOAT, BOOL, CHAR, STRING, IDENT:
		return true
	}

	return false
}

func (t *Token) String() string {
	if !t.hasActualValue() {
		return fmt.Sprintf("%s()", t.Kind)
	}

	return fmt.Sprintf("%s(%s)", t.Kind, t.Value)
}
