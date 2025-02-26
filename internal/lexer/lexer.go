package lexer

import (
	"fmt"

	"github.com/kievzenit/ylang/internal/compiler_errors"
)

type LexerError struct {
	Message string
}

func newUnexpectedError(unexpected byte) *LexerError {
	return &LexerError{
		Message: fmt.Sprintf("unexpected character: '%s'", string(unexpected)),
	}
}

func newUnexpectedExpectedError(unexpected byte, expected byte) *LexerError {
	return &LexerError{
		Message: fmt.Sprintf(
			"expected '%s', but got: '%s', instead",
			string(expected),
			string(unexpected)),
	}
}

func newExpectedError(expected byte) *LexerError {
	return &LexerError{
		Message: fmt.Sprintf("expected '%s'", string(expected)),
	}
}

func (e *LexerError) GetMessage() string {
	return e.Message
}

type Lexer struct {
	buf []byte
	pos int

	line, col int

	eh compiler_errors.ErrorHandler
}

func NewLexer(buf []byte, eh compiler_errors.ErrorHandler) *Lexer {
	return &Lexer{
		buf: buf,
		pos: 0,

		line: 1,
		col:  1,

		eh: eh,
	}
}

func (l *Lexer) Tokenize() []Token {
	tokens := make([]Token, 0)

	for l.hasChars() {
		switch {
		case l.isCurrSkippable():
			if l.isCurrNewline() {
				l.line++
				l.col = 1
			}
			break

		case l.isCurrDigit():
			tokens = append(tokens, l.processNumber())
			break

		case l.isCurrIdentifier():
			tokens = append(tokens, l.processIdentifier())
			break

		case l.read() == '\'':
			tokens = append(tokens, l.processCharLiteral())
			break

		case l.read() == '"':
			tokens = append(tokens, l.processStringLiteral())
			break

		case l.isCurrPunctuation():
			tokens = append(tokens, l.processPunctuation())
			break

		default:
			l.eh.AddError(newUnexpectedError(l.read()))
			l.eh.FailNow()
		}

		l.advance()
	}

	tokens = append(tokens, Token{
		Kind:  EOF,
		Value: EOF.String(),
	})

	return tokens
}

func (l *Lexer) isCurrIdentifier() bool {
	return (l.read() >= 'a' && l.read() <= 'z') || (l.read() >= 'A' && l.read() <= 'Z') || l.read() == '_'
}

func (l *Lexer) isCurrDigit() bool {
	return l.read() >= '0' && l.read() <= '9'
}

func (l *Lexer) isCurrPunctuation() bool {
	switch l.read() {
	case '+', '-', '*', '/', '%', '=', '!', '<', '>', '&', '|', '^', '(', ')', '[', ']', '{', '}', ':', ';', '.', ',', '?':
		return true
	}
	return false
}

func (l *Lexer) isCurrNewline() bool {
	if l.read() == '\n' {
		return true
	}

	return false
}

func (l *Lexer) isCurrSkippable() bool {
	switch l.read() {
	case ' ', '\t', '\n', '\r':
		return true
	}

	return false
}

func (l *Lexer) processIdentifier() Token {
	identifierBuf := make([]byte, 0)
	identifierBuf = append(identifierBuf, l.read())
	l.advance()

	for l.hasChars() {
		if !l.isCurrIdentifier() && !l.isCurrDigit() {
			l.unread()
			break
		}

		identifierBuf = append(identifierBuf, l.read())
		l.advance()
	}
	identifier := string(identifierBuf)

	switch identifier {
	case "package":
		return Token{
			Kind:  PACKAGE,
			Value: identifier,
		}
	case "export":
		return Token{
			Kind:  EXPORT,
			Value: identifier,
		}
	case "extern":
		return Token{
			Kind:  EXTERN,
			Value: identifier,
		}
	case "static":
		return Token{
			Kind:  STATIC,
			Value: identifier,
		}
	case "type":
		return Token{
			Kind:  TYPE,
			Value: identifier,
		}
	case "public":
		return Token{
			Kind:  PUBLIC,
			Value: identifier,
		}
	case "private":
		return Token{
			Kind:  PRIVATE,
			Value: identifier,
		}
	case "ctor":
		return Token{
			Kind:  CTOR,
			Value: identifier,
		}
	case "dtor":
		return Token{
			Kind:  DTOR,
			Value: identifier,
		}
	case "fun":
		return Token{
			Kind:  FUN,
			Value: identifier,
		}
	case "let":
		return Token{
			Kind:  LET,
			Value: identifier,
		}
	case "const":
		return Token{
			Kind:  CONST,
			Value: identifier,
		}
	case "loop":
		return Token{
			Kind:  LOOP,
			Value: identifier,
		}
	case "while":
		return Token{
			Kind:  WHILE,
			Value: identifier,
		}
	case "do":
		return Token{
			Kind:  DO,
			Value: identifier,
		}
	case "for":
		return Token{
			Kind:  FOR,
			Value: identifier,
		}
	case "if":
		return Token{
			Kind:  IF,
			Value: identifier,
		}
	case "else":
		return Token{
			Kind:  ELSE,
			Value: identifier,
		}
	case "continue":
		return Token{
			Kind:  CONTINUE,
			Value: identifier,
		}
	case "break":
		return Token{
			Kind:  BREAK,
			Value: identifier,
		}
	case "breakall":
		return Token{
			Kind:  BREAKALL,
			Value: identifier,
		}
	case "return":
		return Token{
			Kind:  RETURN,
			Value: identifier,
		}
	case "true":
		return Token{
			Kind:  BOOL,
			Value: identifier,
		}
	case "false":
		return Token{
			Kind:  BOOL,
			Value: identifier,
		}
	}

	return Token{
		Kind:  IDENT,
		Value: identifier,
	}
}

func (l *Lexer) processNumber() Token {
	numberBuf := make([]byte, 0)
	numberBuf = append(numberBuf, l.read())
	l.advance()

	var isFloat bool
	for l.hasChars() {
		if !isFloat && l.read() == '.' {
			isFloat = true
			numberBuf = append(numberBuf, l.read())

			l.advance()
			if !l.hasChars() || !l.isCurrDigit() {
				isFloat = false
				l.unread()
				l.unread()
				numberBuf = numberBuf[:len(numberBuf)-1]
				break
			}
		}

		if !l.isCurrDigit() {
			l.unread()
			break
		}

		numberBuf = append(numberBuf, l.read())
		l.advance()
	}

	if isFloat {
		return Token{
			Kind:  FLOAT,
			Value: string(numberBuf),
		}
	}

	return Token{
		Kind:  INT,
		Value: string(numberBuf),
	}
}

func (l *Lexer) processStringLiteral() Token {
	l.advance()

	stringBuf := make([]byte, 0)
	var foundClosingQuote bool
	for l.hasChars() {
		if l.read() == '"' {
			foundClosingQuote = true
			break
		}

		if l.read() == '\n' {
			l.eh.AddError(newExpectedError('"'))
			l.eh.FailNow()
		}

		stringBuf = append(stringBuf, l.read())
		l.advance()
	}

	if !foundClosingQuote {
		l.eh.AddError(newExpectedError('"'))
		l.eh.FailNow()
	}

	return Token{
		Kind:  STRING,
		Value: string(stringBuf),
	}
}

func (l *Lexer) processCharLiteral() Token {
	var char byte

	l.advance()
	if !l.hasChars() {
		l.eh.AddError(newExpectedError('\''))
		l.eh.FailNow()
	}

	if l.read() == '\n' {
		l.eh.AddError(newExpectedError('\''))
		l.eh.FailNow()
	}
	char = l.read()

	l.advance()
	if !l.hasChars() || l.read() != '\'' {
		l.eh.AddError(newExpectedError('\''))
		l.eh.FailNow()
	}

	return Token{
		Kind:  CHAR,
		Value: string(char),
	}
}

func (l *Lexer) processPlus() Token {
	l.advance()
	if !l.hasChars() {
		return Token{
			Kind:  PLUS,
			Value: "+",
		}
	}

	if l.read() == '+' {
		return Token{
			Kind:  INC,
			Value: "++",
		}
	}

	if l.read() == '=' {
		return Token{
			Kind:  ADD_ASSIGN,
			Value: "+=",
		}
	}

	l.unread()
	return Token{
		Kind:  PLUS,
		Value: "+",
	}
}

func (l *Lexer) processMinus() Token {
	l.advance()
	if !l.hasChars() {
		return Token{
			Kind:  MINUS,
			Value: "-",
		}
	}

	if l.read() == '-' {
		return Token{
			Kind:  DEC,
			Value: "--",
		}
	}

	if l.read() == '=' {
		return Token{
			Kind:  SUB_ASSIGN,
			Value: "-=",
		}
	}

	l.unread()
	return Token{
		Kind:  MINUS,
		Value: "-",
	}
}

func (l *Lexer) processAsterisk() Token {
	l.advance()
	if !l.hasChars() {
		return Token{
			Kind:  ASTERISK,
			Value: "*",
		}
	}

	if l.read() == '=' {
		return Token{
			Kind:  MUL_ASSIGN,
			Value: "*=",
		}
	}

	l.unread()
	return Token{
		Kind:  ASTERISK,
		Value: "*",
	}
}

func (l *Lexer) processOneLineComment() Token {
	content := make([]byte, 0)

	l.advance()
	for l.hasChars() {
		if l.read() == '\n' {
			break
		}

		content = append(content, l.read())
		l.advance()
	}

	return Token{
		Kind:  ONELINE_COMMENT,
		Value: string(content),
	}
}

func (l *Lexer) processMultiLineComment() Token {
	content := make([]byte, 0)

	l.advance()
	for l.hasChars() {
		if l.read() == '*' {
			l.advance()
			if l.hasChars() && l.read() == '/' {
				l.advance()
				break
			}

			if !l.hasChars() {
				l.eh.AddError(newExpectedError('/'))
				l.eh.FailNow()
			}
		}

		content = append(content, l.read())
		l.advance()
	}

	return Token{
		Kind:  MULTILINE_COMMENT,
		Value: string(content),
	}
}

func (l *Lexer) processSlash() Token {
	l.advance()
	if !l.hasChars() {
		return Token{
			Kind:  SLASH,
			Value: "/",
		}
	}

	if l.read() == '=' {
		return Token{
			Kind:  DIV_ASSIGN,
			Value: "/=",
		}
	}

	if l.read() == '/' {
		return l.processOneLineComment()
	}

	if l.read() == '*' {
		return l.processMultiLineComment()
	}

	l.unread()
	return Token{
		Kind:  SLASH,
		Value: "/",
	}
}

func (l *Lexer) processPercent() Token {
	l.advance()
	if !l.hasChars() {
		return Token{
			Kind:  PERCENT,
			Value: "%",
		}
	}

	if l.read() == '=' {
		return Token{
			Kind:  MOD_ASSIGN,
			Value: "%=",
		}
	}

	l.unread()
	return Token{
		Kind:  PERCENT,
		Value: "%",
	}
}

func (l *Lexer) processEquals() Token {
	l.advance()
	if !l.hasChars() {
		return Token{
			Kind:  ASSIGN,
			Value: "=",
		}
	}

	if l.read() == '=' {
		return Token{
			Kind:  EQ,
			Value: "==",
		}
	}

	l.unread()
	return Token{
		Kind:  ASSIGN,
		Value: "=",
	}
}

func (l *Lexer) processAmpersand() Token {
	l.advance()
	if !l.hasChars() {
		return Token{
			Kind:  BAND,
			Value: "&",
		}
	}

	if l.read() == '&' {
		return Token{
			Kind:  LAND,
			Value: "&&",
		}
	}

	if l.read() == '=' {
		return Token{
			Kind:  BAND_ASSIGN,
			Value: "&=",
		}
	}

	l.unread()
	return Token{
		Kind:  BAND,
		Value: "&",
	}
}
func (l *Lexer) processCaret() Token {
	l.advance()
	if !l.hasChars() {
		return Token{
			Kind:  XOR,
			Value: "^",
		}
	}

	if l.read() == '=' {
		return Token{
			Kind:  XOR_ASSIGN,
			Value: "^=",
		}
	}

	l.unread()
	return Token{
		Kind:  XOR,
		Value: "^",
	}
}

func (l *Lexer) processPipe() Token {
	l.advance()
	if !l.hasChars() {
		return Token{
			Kind:  BOR,
			Value: "|",
		}
	}

	if l.read() == '|' {
		return Token{
			Kind:  LOR,
			Value: "||",
		}
	}

	if l.read() == '=' {
		return Token{
			Kind:  BOR_ASSIGN,
			Value: "|=",
		}
	}

	l.unread()
	return Token{
		Kind:  BOR,
		Value: "|",
	}
}

func (l *Lexer) processGreaterThan() Token {
	l.advance()
	if !l.hasChars() {
		return Token{
			Kind:  GT,
			Value: ">",
		}
	}

	if l.read() == '>' {
		if l.hasChars() && l.next() == '=' {
			l.advance()
			return Token{
				Kind:  SHR_ASSIGN,
				Value: ">>=",
			}
		}

		return Token{
			Kind:  SHR,
			Value: ">>",
		}
	}

	if l.read() == '=' {
		return Token{
			Kind:  GEQ,
			Value: ">=",
		}
	}

	if l.read() == '-' {
		return Token{
			Kind:  CAST,
			Value: ">-",
		}
	}

	l.unread()
	return Token{
		Kind:  GT,
		Value: ">",
	}
}

func (l *Lexer) processLessThan() Token {
	l.advance()
	if !l.hasChars() {
		return Token{
			Kind:  LT,
			Value: "<",
		}
	}

	if l.read() == '<' {
		if l.hasChars() && l.next() == '=' {
			l.advance()
			return Token{
				Kind:  SHL_ASSIGN,
				Value: "<<=",
			}
		}

		return Token{
			Kind:  SHL,
			Value: "<<",
		}
	}

	if l.read() == '=' {
		return Token{
			Kind:  LEQ,
			Value: "<=",
		}
	}

	l.unread()
	return Token{
		Kind:  LT,
		Value: "<",
	}
}

func (l *Lexer) processColon() Token {
	l.advance()
	if !l.hasChars() {
		return Token{
			Kind:  COLON,
			Value: ":",
		}
	}

	if l.read() == ':' {
		return Token{
			Kind:  COLONCOLON,
			Value: "::",
		}
	}

	l.unread()
	return Token{
		Kind:  COLON,
		Value: ":",
	}
}

func (l *Lexer) processExclamationMark() Token {
	l.advance()
	if !l.hasChars() {
		return Token{
			Kind:  XMARK,
			Value: "!",
		}
	}

	if l.read() == '=' {
		return Token{
			Kind:  NEQ,
			Value: "!=",
		}
	}

	l.unread()
	return Token{
		Kind:  XMARK,
		Value: "!",
	}
}

func (l *Lexer) processPunctuation() Token {
	switch l.read() {
	case '+':
		return l.processPlus()
	case '-':
		return l.processMinus()
	case '*':
		return l.processAsterisk()
	case '/':
		return l.processSlash()
	case '%':
		return l.processPercent()
	case '=':
		return l.processEquals()
	case '&':
		return l.processAmpersand()
	case '|':
		return l.processPipe()
	case '^':
		return l.processCaret()
	case '>':
		return l.processGreaterThan()
	case '<':
		return l.processLessThan()
	case '(':
		return Token{
			Kind:  LPAREN,
			Value: "(",
		}
	case '[':
		return Token{
			Kind:  LBRACKET,
			Value: "[",
		}
	case '{':
		return Token{
			Kind:  LBRACE,
			Value: "{",
		}
	case ')':
		return Token{
			Kind:  RPAREN,
			Value: ")",
		}
	case ']':
		return Token{
			Kind:  RBRACKET,
			Value: "]",
		}
	case '}':
		return Token{
			Kind:  RBRACE,
			Value: "}",
		}
	case ':':
		return l.processColon()
	case ';':
		return Token{
			Kind:  SEMICOLON,
			Value: ";",
		}
	case '.':
		return l.processDot()
	case ',':
		return Token{
			Kind:  COMMA,
			Value: ",",
		}
	case '!':
		return l.processExclamationMark()
	case '?':
		return Token{
			Kind:  QMARK,
			Value: "?",
		}
	}

	panic("unreachable")
}

func (l *Lexer) processDot() Token {
	l.advance()
	if !l.hasChars() || l.read() != '.' {
		l.unread()
		return Token{
			Kind:  DOT,
			Value: ".",
		}
	}

	l.advance()
	if !l.hasChars() || l.read() != '.' {
		l.unread()
		return Token{
			Kind:  RANGE,
			Value: "..",
		}
	}

	return Token{
		Kind:  ELLIPSIS,
		Value: "...",
	}
}

func (l *Lexer) hasChars() bool {
	return l.pos < len(l.buf)
}

func (l *Lexer) advance()   { l.pos++ }
func (l *Lexer) next() byte { return l.buf[l.pos+1] }
func (l *Lexer) read() byte { return l.buf[l.pos] }
func (l *Lexer) unread()    { l.pos-- }
