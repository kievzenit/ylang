package ast

import "github.com/kievzenit/ylang/internal/lexer"

type ScopeStmt struct {
	StartToken *lexer.Token

	Stmts []Stmt
}

type TypeAccessModifier int

const (
	Private TypeAccessModifier = iota
	Public
)

type TypeMember struct {
	StartToken *lexer.Token

	Name           string
	Type           string
	AccessModifier *lexer.Token
}

type TypeFuncMember struct {
	*FuncDeclStmt
	AccessModifier *lexer.Token
}

type TypeConstructor struct {
	Args           []FuncArg
	Body           *ScopeStmt
	AccessModifier *lexer.Token
}

type TypeDestructor struct {
	Body           *ScopeStmt
	AccessModifier *lexer.Token
}

type TypeDeclStmt struct {
	StartToken *lexer.Token

	Name         string
	Constructors []TypeConstructor
	Destructors  []TypeDestructor
	Members      []TypeMember
	Funcs        []TypeFuncMember
	Extern       bool
}

type FuncDeclStmt struct {
	StartToken *lexer.Token

	Name       string
	ReturnType string
	Args       []FuncArg
	Body       *ScopeStmt
	Extern     bool
}

type FuncArg struct {
	Name string
	Type string
}

type VarDeclStmt struct {
	StartToken *lexer.Token

	Name         string
	ExplicitType string
	Value        Expr
	Static       bool
	Const        bool
}

type LoopStmt struct {
	StartToken *lexer.Token

	Body *ScopeStmt
}

type WhileStmt struct {
	StartToken *lexer.Token

	Cond Expr
	Body *ScopeStmt
}

type DoWhileStmt struct {
	StartToken *lexer.Token

	Cond Expr
	Body *ScopeStmt
}

type ElseIf struct {
	StartToken *lexer.Token

	Cond Expr
	Body *ScopeStmt
}

type IfStmt struct {
	StartToken *lexer.Token

	Cond   Expr
	Body   *ScopeStmt
	ElseIf []ElseIf
	Else   *ScopeStmt
}

type ExprStmt struct {
	Expr Expr
}

type ReturnStmt struct {
	StartToken *lexer.Token

	Expr Expr
}

type BreakStmt struct{
	StartToken *lexer.Token
}

type ContinueStmt struct{
	StartToken *lexer.Token
}

type BreakAllStmt struct{
	StartToken *lexer.Token
}

func (t *TypeDeclStmt) TopStmtNode() {}
func (f *FuncDeclStmt) TopStmtNode() {}
func (v *VarDeclStmt) TopStmtNode()  {}

func (s *ScopeStmt) AstNode()    {}
func (t *TypeDeclStmt) AstNode() {}
func (f *FuncDeclStmt) AstNode() {}
func (v *VarDeclStmt) AstNode()  {}
func (e *ExprStmt) AstNode()     {}
func (i *IfStmt) AstNode()       {}
func (l *LoopStmt) AstNode()     {}
func (w *WhileStmt) AstNode()    {}
func (d *DoWhileStmt) AstNode()  {}
func (r *ReturnStmt) AstNode()   {}
func (b *BreakStmt) AstNode()    {}
func (c *ContinueStmt) AstNode() {}
func (b *BreakAllStmt) AstNode() {}

func (s *ScopeStmt) FirstToken() *lexer.Token    { return s.StartToken }
func (t *TypeDeclStmt) FirstToken() *lexer.Token { return t.StartToken }
func (f *FuncDeclStmt) FirstToken() *lexer.Token { return f.StartToken }
func (v *VarDeclStmt) FirstToken() *lexer.Token  { return v.StartToken }
func (e *ExprStmt) FirstToken() *lexer.Token     { return e.Expr.FirstToken() }
func (i *IfStmt) FirstToken() *lexer.Token       { return i.StartToken }
func (l *LoopStmt) FirstToken() *lexer.Token     { return l.StartToken }
func (w *WhileStmt) FirstToken() *lexer.Token    { return w.StartToken }
func (d *DoWhileStmt) FirstToken() *lexer.Token  { return d.StartToken }
func (r *ReturnStmt) FirstToken() *lexer.Token   { return r.StartToken }
func (b *BreakStmt) FirstToken() *lexer.Token    { return b.StartToken }
func (c *ContinueStmt) FirstToken() *lexer.Token { return c.StartToken }
func (b *BreakAllStmt) FirstToken() *lexer.Token { return b.StartToken }

func (s *ScopeStmt) StmtNode()    {}
func (t *TypeDeclStmt) StmtNode() {}
func (f *FuncDeclStmt) StmtNode() {}
func (v *VarDeclStmt) StmtNode()  {}
func (e *ExprStmt) StmtNode()     {}
func (i *IfStmt) StmtNode()       {}
func (l *LoopStmt) StmtNode()     {}
func (w *WhileStmt) StmtNode()    {}
func (d *DoWhileStmt) StmtNode()  {}
func (r *ReturnStmt) StmtNode()   {}
func (b *BreakStmt) StmtNode()    {}
func (c *ContinueStmt) StmtNode() {}
func (b *BreakAllStmt) StmtNode() {}
