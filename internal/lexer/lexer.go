package lexer

import (
	"fmt"

	"github.com/kievzenit/ylang/internal/compiler_errors"
)

type LexerError struct {
	Message  string
	FileName string
	Line     int
	Col      int
}

func (e *LexerError) GetMessage() string {
	return e.Message
}

func (e *LexerError) GetLine() int {
	return e.Line
}

func (e *LexerError) GetColumn() int {
	return e.Col
}

func (e *LexerError) GetLength() int {
	return 1
}

func (e *LexerError) GetFileName() string {
	return e.FileName
}

func (l *Lexer) newUnexpectedError(unexpected byte) *LexerError {
	return &LexerError{
		Message:  fmt.Sprintf("unexpected character: '%s'", string(unexpected)),
		FileName: l.fileName,
		Line:     l.line,
		Col:      l.col,
	}
}

func (l *Lexer) newUnexpectedExpectedError(unexpected byte, expected byte) *LexerError {
	return &LexerError{
		Message: fmt.Sprintf(
			"expected '%s', but got: '%s', instead",
			string(expected),
			string(unexpected)),
		FileName: l.fileName,
		Line:     l.line,
		Col:      l.col,
	}
}

func (l *Lexer) newExpectedError(expected byte) *LexerError {
	return &LexerError{
		Message:  fmt.Sprintf("expected '%s'", string(expected)),
		FileName: l.fileName,
		Line:     l.line,
		Col:      l.col,
	}
}

type Lexer struct {
	fileName string

	buf []byte
	pos int

	line, col int

	eh compiler_errors.ErrorHandler
}

func NewLexer(fileName string, buf []byte, eh compiler_errors.ErrorHandler) *Lexer {
	return &Lexer{
		fileName: fileName,

		buf: buf,
		pos: 0,

		line: 1,
		col:  1,

		eh: eh,
	}
}

func (l *Lexer) Tokenize() []*Token {
	tokens := make([]*Token, 0)

	for l.hasChars() {
		switch {
		case l.isCurrSkippable():
			if l.isCurrNewline() {
				l.line++
				l.col = 0
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
			l.eh.AddError(l.newUnexpectedError(l.read()))
			l.eh.FailNow()
		}

		l.advance()
	}

	tokens = append(tokens, &Token{
		Kind:  EOF,
		Value: EOF.String(),
		Metadata: TokenMetadata{
			FileName: l.fileName,
			Line:     l.line + 1,
			Length:   0,
			Column:   0,
		},
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

func (l *Lexer) processIdentifier() *Token {
	columnStart := l.col

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
		return &Token{
			Kind:  PACKAGE,
			Value: identifier,
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	case "export":
		return &Token{
			Kind:  EXPORT,
			Value: identifier,
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	case "extern":
		return &Token{
			Kind:  EXTERN,
			Value: identifier,
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	case "static":
		return &Token{
			Kind:  STATIC,
			Value: identifier,
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	case "type":
		return &Token{
			Kind:  TYPE,
			Value: identifier,
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	case "public":
		return &Token{
			Kind:  PUBLIC,
			Value: identifier,
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	case "private":
		return &Token{
			Kind:  PRIVATE,
			Value: identifier,
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	case "ctor":
		return &Token{
			Kind:  CTOR,
			Value: identifier,
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	case "dtor":
		return &Token{
			Kind:  DTOR,
			Value: identifier,
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	case "fun":
		return &Token{
			Kind:  FUN,
			Value: identifier,
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	case "let":
		return &Token{
			Kind:  LET,
			Value: identifier,
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	case "const":
		return &Token{
			Kind:  CONST,
			Value: identifier,
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	case "loop":
		return &Token{
			Kind:  LOOP,
			Value: identifier,
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	case "while":
		return &Token{
			Kind:  WHILE,
			Value: identifier,
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	case "do":
		return &Token{
			Kind:  DO,
			Value: identifier,
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	case "for":
		return &Token{
			Kind:  FOR,
			Value: identifier,
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	case "if":
		return &Token{
			Kind:  IF,
			Value: identifier,
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	case "else":
		return &Token{
			Kind:  ELSE,
			Value: identifier,
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	case "continue":
		return &Token{
			Kind:  CONTINUE,
			Value: identifier,
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	case "break":
		return &Token{
			Kind:  BREAK,
			Value: identifier,
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	case "breakall":
		return &Token{
			Kind:  BREAKALL,
			Value: identifier,
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	case "return":
		return &Token{
			Kind:  RETURN,
			Value: identifier,
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	case "true":
		return &Token{
			Kind:  BOOL,
			Value: identifier,
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	case "false":
		return &Token{
			Kind:  BOOL,
			Value: identifier,
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	}

	return &Token{
		Kind:  IDENT,
		Value: identifier,
		Metadata: TokenMetadata{
			FileName: l.fileName,
			Line:     l.line,
			Length:   l.col - columnStart + 1,
			Column:   columnStart,
		},
	}
}

func (l *Lexer) processNumber() *Token {
	columnStart := l.col

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
				if !l.isCurrIdentifier() {
					l.eh.AddError(l.newUnexpectedError(l.read()))
					l.eh.FailNow()
				}

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
		return &Token{
			Kind:  FLOAT,
			Value: string(numberBuf),
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	}

	return &Token{
		Kind:  INT,
		Value: string(numberBuf),
		Metadata: TokenMetadata{
			FileName: l.fileName,
			Line:     l.line,
			Length:   l.col - columnStart + 1,
			Column:   columnStart,
		},
	}
}

func (l *Lexer) processStringLiteral() *Token {
	columnStart := l.col

	l.advance()

	stringBuf := make([]byte, 0)
	var foundClosingQuote bool
	for l.hasChars() {
		if l.read() == '"' {
			foundClosingQuote = true
			break
		}

		if l.read() == '\n' {
			l.eh.AddError(l.newExpectedError('"'))
			l.eh.FailNow()
		}

		stringBuf = append(stringBuf, l.read())
		l.advance()
	}

	if !foundClosingQuote {
		l.eh.AddError(l.newExpectedError('"'))
		l.eh.FailNow()
	}

	return &Token{
		Kind:  STRING,
		Value: string(stringBuf),
		Metadata: TokenMetadata{
			FileName: l.fileName,
			Line:     l.line,
			Length:   l.col - columnStart + 1,
			Column:   columnStart,
		},
	}
}

func (l *Lexer) processCharLiteral() *Token {
	columnStart := l.col

	var char byte

	l.advance()
	if !l.hasChars() {
		l.eh.AddError(l.newExpectedError('\''))
		l.eh.FailNow()
	}

	if l.read() == '\n' {
		l.eh.AddError(l.newExpectedError('\''))
		l.eh.FailNow()
	}
	char = l.read()

	l.advance()
	if !l.hasChars() || l.read() != '\'' {
		l.eh.AddError(l.newExpectedError('\''))
		l.eh.FailNow()
	}

	return &Token{
		Kind:  CHAR,
		Value: string(char),
		Metadata: TokenMetadata{
			FileName: l.fileName,
			Line:     l.line,
			Length:   l.col - columnStart + 1,
			Column:   columnStart,
		},
	}
}

func (l *Lexer) processPlus() *Token {
	columnStart := l.col

	l.advance()
	if !l.hasChars() {
		return &Token{
			Kind:  PLUS,
			Value: "+",
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	}

	if l.read() == '+' {
		return &Token{
			Kind:  INC,
			Value: "++",
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	}

	if l.read() == '=' {
		return &Token{
			Kind:  ADD_ASSIGN,
			Value: "+=",
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	}

	l.unread()
	return &Token{
		Kind:  PLUS,
		Value: "+",
		Metadata: TokenMetadata{
			FileName: l.fileName,
			Line:     l.line,
			Length:   l.col - columnStart + 1,
			Column:   columnStart,
		},
	}
}

func (l *Lexer) processMinus() *Token {
	columnStart := l.col
	l.advance()
	if !l.hasChars() {
		return &Token{
			Kind:  MINUS,
			Value: "-",
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	}

	if l.read() == '-' {
		return &Token{
			Kind:  DEC,
			Value: "--",
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	}

	if l.read() == '=' {
		return &Token{
			Kind:  SUB_ASSIGN,
			Value: "-=",
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	}

	l.unread()
	return &Token{
		Kind:  MINUS,
		Value: "-",
		Metadata: TokenMetadata{
			FileName: l.fileName,
			Line:     l.line,
			Length:   l.col - columnStart + 1,
			Column:   columnStart,
		},
	}
}

func (l *Lexer) processAsterisk() *Token {
	columnStart := l.col

	l.advance()
	if !l.hasChars() {
		return &Token{
			Kind:  ASTERISK,
			Value: "*",
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	}

	if l.read() == '=' {
		return &Token{
			Kind:  MUL_ASSIGN,
			Value: "*=",
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	}

	l.unread()
	return &Token{
		Kind:  ASTERISK,
		Value: "*",
		Metadata: TokenMetadata{
			FileName: l.fileName,
			Line:     l.line,
			Length:   l.col - columnStart + 1,
			Column:   columnStart,
		},
	}
}

func (l *Lexer) processOneLineComment() *Token {
	columnStart := l.col

	content := make([]byte, 0)

	l.advance()
	for l.hasChars() {
		if l.read() == '\n' {
			break
		}

		content = append(content, l.read())
		l.advance()
	}

	return &Token{
		Kind:  ONELINE_COMMENT,
		Value: string(content),
		Metadata: TokenMetadata{
			FileName: l.fileName,
			Line:     l.line,
			Length:   l.col - columnStart + 1,
			Column:   columnStart,
		},
	}
}

func (l *Lexer) processMultiLineComment() *Token {
	columnStart := l.col

	content := make([]byte, 0)

	l.advance()
	for l.hasChars() {
		if l.read() == '\n' {
			l.line++
			l.col = 0
		}

		if l.read() == '*' {
			l.advance()
			if l.hasChars() && l.read() == '/' {
				l.advance()
				break
			}

			if !l.hasChars() {
				l.eh.AddError(l.newExpectedError('/'))
				l.eh.FailNow()
			}
		}

		content = append(content, l.read())
		l.advance()
	}

	return &Token{
		Kind:  MULTILINE_COMMENT,
		Value: string(content),
		Metadata: TokenMetadata{
			FileName: l.fileName,
			Line:     l.line,
			Length:   l.col - columnStart + 1,
			Column:   columnStart,
		},
	}
}

func (l *Lexer) processSlash() *Token {
	columnStart := l.col

	l.advance()
	if !l.hasChars() {
		return &Token{
			Kind:  SLASH,
			Value: "/",
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	}

	if l.read() == '=' {
		return &Token{
			Kind:  DIV_ASSIGN,
			Value: "/=",
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	}

	if l.read() == '/' {
		return l.processOneLineComment()
	}

	if l.read() == '*' {
		return l.processMultiLineComment()
	}

	l.unread()
	return &Token{
		Kind:  SLASH,
		Value: "/",
		Metadata: TokenMetadata{
			FileName: l.fileName,
			Line:     l.line,
			Length:   l.col - columnStart + 1,
			Column:   columnStart,
		},
	}
}

func (l *Lexer) processPercent() *Token {
	columnStart := l.col

	l.advance()
	if !l.hasChars() {
		return &Token{
			Kind:  PERCENT,
			Value: "%",
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	}

	if l.read() == '=' {
		return &Token{
			Kind:  MOD_ASSIGN,
			Value: "%=",
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	}

	l.unread()
	return &Token{
		Kind:  PERCENT,
		Value: "%",
		Metadata: TokenMetadata{
			FileName: l.fileName,
			Line:     l.line,
			Length:   l.col - columnStart + 1,
			Column:   columnStart,
		},
	}
}

func (l *Lexer) processEquals() *Token {
	columnStart := l.col

	l.advance()
	if !l.hasChars() {
		return &Token{
			Kind:  ASSIGN,
			Value: "=",
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	}

	if l.read() == '=' {
		return &Token{
			Kind:  EQ,
			Value: "==",
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	}

	l.unread()
	return &Token{
		Kind:  ASSIGN,
		Value: "=",
		Metadata: TokenMetadata{
			FileName: l.fileName,
			Line:     l.line,
			Length:   l.col - columnStart + 1,
			Column:   columnStart,
		},
	}
}

func (l *Lexer) processAmpersand() *Token {
	columnStart := l.col

	l.advance()
	if !l.hasChars() {
		return &Token{
			Kind:  BAND,
			Value: "&",
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	}

	if l.read() == '&' {
		return &Token{
			Kind:  LAND,
			Value: "&&",
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	}

	if l.read() == '=' {
		return &Token{
			Kind:  BAND_ASSIGN,
			Value: "&=",
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	}

	l.unread()
	return &Token{
		Kind:  BAND,
		Value: "&",
		Metadata: TokenMetadata{
			FileName: l.fileName,
			Line:     l.line,
			Length:   l.col - columnStart + 1,
			Column:   columnStart,
		},
	}
}
func (l *Lexer) processCaret() *Token {
	columnStart := l.col

	l.advance()
	if !l.hasChars() {
		return &Token{
			Kind:  XOR,
			Value: "^",
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	}

	if l.read() == '=' {
		return &Token{
			Kind:  XOR_ASSIGN,
			Value: "^=",
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	}

	l.unread()
	return &Token{
		Kind:  XOR,
		Value: "^",
		Metadata: TokenMetadata{
			FileName: l.fileName,
			Line:     l.line,
			Length:   l.col - columnStart + 1,
			Column:   columnStart,
		},
	}
}

func (l *Lexer) processPipe() *Token {
	columnStart := l.col

	l.advance()
	if !l.hasChars() {
		return &Token{
			Kind:  BOR,
			Value: "|",
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	}

	if l.read() == '|' {
		return &Token{
			Kind:  LOR,
			Value: "||",
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	}

	if l.read() == '=' {
		return &Token{
			Kind:  BOR_ASSIGN,
			Value: "|=",
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	}

	l.unread()
	return &Token{
		Kind:  BOR,
		Value: "|",
		Metadata: TokenMetadata{
			FileName: l.fileName,
			Line:     l.line,
			Length:   l.col - columnStart + 1,
			Column:   columnStart,
		},
	}
}

func (l *Lexer) processGreaterThan() *Token {
	columnStart := l.col

	l.advance()
	if !l.hasChars() {
		return &Token{
			Kind:  GT,
			Value: ">",
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	}

	if l.read() == '>' {
		if l.hasChars() && l.next() == '=' {
			l.advance()
			return &Token{
				Kind:  SHR_ASSIGN,
				Value: ">>=",
				Metadata: TokenMetadata{
					FileName: l.fileName,
					Line:     l.line,
					Length:   l.col - columnStart + 1,
					Column:   columnStart,
				},
			}
		}

		return &Token{
			Kind:  SHR,
			Value: ">>",
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	}

	if l.read() == '=' {
		return &Token{
			Kind:  GEQ,
			Value: ">=",
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	}

	if l.read() == '-' {
		return &Token{
			Kind:  CAST,
			Value: ">-",
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	}

	l.unread()
	return &Token{
		Kind:  GT,
		Value: ">",
		Metadata: TokenMetadata{
			FileName: l.fileName,
			Line:     l.line,
			Length:   l.col - columnStart + 1,
			Column:   columnStart,
		},
	}
}

func (l *Lexer) processLessThan() *Token {
	columnStart := l.col

	l.advance()
	if !l.hasChars() {
		return &Token{
			Kind:  LT,
			Value: "<",
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	}

	if l.read() == '<' {
		if l.hasChars() && l.next() == '=' {
			l.advance()
			return &Token{
				Kind:  SHL_ASSIGN,
				Value: "<<=",
				Metadata: TokenMetadata{
					FileName: l.fileName,
					Line:     l.line,
					Length:   l.col - columnStart + 1,
					Column:   columnStart,
				},
			}
		}

		return &Token{
			Kind:  SHL,
			Value: "<<",
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	}

	if l.read() == '=' {
		return &Token{
			Kind:  LEQ,
			Value: "<=",
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	}

	l.unread()
	return &Token{
		Kind:  LT,
		Value: "<",
		Metadata: TokenMetadata{
			FileName: l.fileName,
			Line:     l.line,
			Length:   l.col - columnStart + 1,
			Column:   columnStart,
		},
	}
}

func (l *Lexer) processColon() *Token {
	columnStart := l.col

	l.advance()
	if !l.hasChars() {
		return &Token{
			Kind:  COLON,
			Value: ":",
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	}

	if l.read() == ':' {
		return &Token{
			Kind:  COLONCOLON,
			Value: "::",
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	}

	l.unread()
	return &Token{
		Kind:  COLON,
		Value: ":",
		Metadata: TokenMetadata{
			FileName: l.fileName,
			Line:     l.line,
			Length:   l.col - columnStart + 1,
			Column:   columnStart,
		},
	}
}

func (l *Lexer) processExclamationMark() *Token {
	columnStart := l.col

	l.advance()
	if !l.hasChars() {
		return &Token{
			Kind:  XMARK,
			Value: "!",
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	}

	if l.read() == '=' {
		return &Token{
			Kind:  NEQ,
			Value: "!=",
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	}

	l.unread()
	return &Token{
		Kind:  XMARK,
		Value: "!",
		Metadata: TokenMetadata{
			FileName: l.fileName,
			Line:     l.line,
			Length:   l.col - columnStart + 1,
			Column:   columnStart,
		},
	}
}

func (l *Lexer) processPunctuation() *Token {
	columnStart := l.col

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
		return &Token{
			Kind:  LPAREN,
			Value: "(",
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	case '[':
		return &Token{
			Kind:  LBRACKET,
			Value: "[",
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	case '{':
		return &Token{
			Kind:  LBRACE,
			Value: "{",
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	case ')':
		return &Token{
			Kind:  RPAREN,
			Value: ")",
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	case ']':
		return &Token{
			Kind:  RBRACKET,
			Value: "]",
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	case '}':
		return &Token{
			Kind:  RBRACE,
			Value: "}",
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	case ':':
		return l.processColon()
	case ';':
		return &Token{
			Kind:  SEMICOLON,
			Value: ";",
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	case '.':
		return l.processDot()
	case ',':
		return &Token{
			Kind:  COMMA,
			Value: ",",
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	case '!':
		return l.processExclamationMark()
	case '?':
		return &Token{
			Kind:  QMARK,
			Value: "?",
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	}

	panic("unreachable")
}

func (l *Lexer) processDot() *Token {
	columnStart := l.col

	l.advance()
	if !l.hasChars() || l.read() != '.' {
		l.unread()
		return &Token{
			Kind:  DOT,
			Value: ".",
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	}

	l.advance()
	if !l.hasChars() || l.read() != '.' {
		l.unread()
		return &Token{
			Kind:  RANGE,
			Value: "..",
			Metadata: TokenMetadata{
				FileName: l.fileName,
				Line:     l.line,
				Length:   l.col - columnStart + 1,
				Column:   columnStart,
			},
		}
	}

	return &Token{
		Kind:  ELLIPSIS,
		Value: "...",
		Metadata: TokenMetadata{
			FileName: l.fileName,
			Line:     l.line,
			Length:   l.col - columnStart + 1,
			Column:   columnStart,
		},
	}
}

func (l *Lexer) hasChars() bool {
	return l.pos < len(l.buf)
}

func (l *Lexer) advance()   { l.pos++; l.col++ }
func (l *Lexer) next() byte { return l.buf[l.pos+1] }
func (l *Lexer) read() byte { return l.buf[l.pos] }
func (l *Lexer) unread()    { l.pos--; l.col-- }
