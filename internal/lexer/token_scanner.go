package lexer

type TokenScanner interface {
	Read() Token
	Unread()
}

type SimpleTokenScanner struct {
	tokens []Token

	pos int
}

func NewTokenScanner(tokens []Token) TokenScanner {
	return &SimpleTokenScanner{
		tokens: tokens,
	}
}

func (s *SimpleTokenScanner) Read() Token {
	token := s.tokens[s.pos]
	s.pos++

	return token
}

func (s *SimpleTokenScanner) Unread() {
	s.pos--
}
