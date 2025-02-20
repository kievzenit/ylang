package lexer

type TokenScanner interface {
	HasTokens() bool
	Read() Token
	Unread() Token
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

func (s *SimpleTokenScanner) HasTokens() bool {
	return s.pos < len(s.tokens)
}

func (s *SimpleTokenScanner) Read() Token {
	token := s.tokens[s.pos]
	s.pos++

	return token
}

func (s *SimpleTokenScanner) Unread() Token {
	s.pos--
	return s.tokens[s.pos-1]
}
