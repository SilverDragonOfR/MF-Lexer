package main

import (
	"bufio"
	"fmt"
	"os"
	"reflect"
	"strconv"
	"strings"

	"github.com/fatih/color"
)

// Setting global environment scope
func setUpGlobalEnvironmentScope(env Environment) {
	env.declareVar("true", MK_BOOL_VALUE(true), true)
	env.declareVar("false", MK_BOOL_VALUE(false), true)
	env.declareVar("nil", MK_NIL_VALUE(), true)
}

// making enum TokenType
type TokenType int

const (
	// single-character
	LEFT_PAREN TokenType = iota + 1
	RIGHT_PAREN
	LEFT_BRACE
	RIGHT_BRACE
	LEFT_SQUARE
	RIGHT_SQUARE
	COMMA
	COLON
	DOT
	MINUS
	PLUS
	SEMICOLON
	SLASH
	STAR

	// one or two character token
	BANG
	BANG_EQUAL
	EQUAL
	EQUAL_EQUAL
	GREATER
	GREATER_EQUAL
	LESS
	LESS_EQUAL

	// literals
	IDENTIFIER
	STRING
	NUMBER

	// keywords
	AND
	BREAK
	CLASS
	CONST
	ELSE
	FALSE
	FN
	FOR
	IF
	LOOP
	NIL
	OR
	PRINT
	RETURN
	SUPER
	THIS
	TRUE
	VAR
	WHILE

	// eof
	EOF
)

func (t TokenType) EnumIndex() int {
	return int(t)
}

func (t TokenType) String() string {
	return [...]string{"LEFT_PAREN", "RIGHT_PAREN", "LEFT_BRACE", "RIGHT_BRACE", "LEFT_SQUARE", "RIGHT_SQUARE", "COMMA", "COLON", "DOT", "MINUS", "PLUS", "SEMICOLON", "SLASH", "STAR", "BANG", "BANG_EQUAL", "EQUAL", "EQUAL_EQUAL", "GREATER", "GREATER_EQUAL", "LESS", "LESS_EQUAL", "IDENTIFIER", "STRING", "NUMBER", "AND", "BREAK", "CLASS", "CONST", "ELSE", "FALSE", "FN", "FOR", "IF", "LOOP", "NIL", "OR", "PRINT", "RETURN", "SUPER", "THIS", "TRUE", "VAR", "WHILE", "EOF"}[t-1]
}

var keywords map[string]TokenType = make(map[string]TokenType)

// making Token struct
type Token struct {
	tokenType TokenType
	lexeme    string
	literal   string
	line      int
}

func (t Token) String() string {
	return color.GreenString(t.tokenType.String()) + " " + t.lexeme + " " + t.literal
}

func main() {

	// keyword initialisation
	keywords["and"] = AND
	keywords["break"] = BREAK
	keywords["class"] = CLASS
	keywords["const"] = CONST
	keywords["else"] = ELSE
	keywords["false"] = FALSE
	keywords["for"] = FOR
	keywords["fn"] = FN
	keywords["if"] = IF
	keywords["loop"] = LOOP
	keywords["nil"] = NIL
	keywords["or"] = OR
	keywords["print"] = PRINT
	keywords["return"] = RETURN
	keywords["super"] = SUPER
	keywords["this"] = THIS
	keywords["true"] = TRUE
	keywords["var"] = VAR
	keywords["while"] = WHILE

	// Environment variables
	env := Environment{parent: nil, variables: map[string]any{}, constants: map[string]void{}}
	setUpGlobalEnvironmentScope(env)

	if len(os.Args) > 2 {
		fmt.Println()
		fmt.Println(color.YellowString("USAGE : lexer [script]"))
		fmt.Println()
		os.Exit(64)
	} else if len(os.Args) == 2 {
		runFile(os.Args[1], env)
	} else {
		runPrompt(env)
	}

}

// Error handling
func Error(line int, message string) {
	report(line, message)
}

func report(line int, message string) {
	fmt.Println(color.RedString("ERROR : [ Line"), color.RedString(strconv.Itoa(line)), color.RedString("]"))
	fmt.Println(color.RedString("      :"), color.RedString("->"), color.RedString(message))
	fmt.Println()
	os.Exit(1)
}

// Scanner object
type Scanner struct {
	source  string
	tokens  []Token
	start   int
	current int
	line    int
}

func (sc Scanner) isAtEnd() bool {
	return sc.current >= len(sc.source)
}

func (sc Scanner) scanTokens() []Token {

	for !sc.isAtEnd() {
		sc.start = sc.current
		sc.scanToken()
	}
	eofToken := Token{EOF, "", "null", sc.line}
	sc.tokens = append(sc.tokens, eofToken)

	return sc.tokens
}

func (sc *Scanner) advance() byte {
	ret := sc.source[sc.current]
	sc.current = sc.current + 1
	return ret
}

func (sc *Scanner) addToken(tokenType TokenType) {
	sc.addTokenWithLiteral(tokenType, "null")
}

func (sc *Scanner) addTokenWithLiteral(tokenType TokenType, literal string) {
	text := sc.source[sc.start:sc.current]
	newToken := Token{tokenType, text, "null", sc.line}
	sc.tokens = append(sc.tokens, newToken)
}

func (sc *Scanner) match(expected byte) bool {
	if sc.isAtEnd() {
		return false
	}
	if sc.source[sc.current] != expected {
		return false
	}
	sc.current = sc.current + 1
	return true
}

func (sc *Scanner) peek() byte {
	if sc.isAtEnd() {
		return '\x00'
	}
	return sc.source[sc.current]
}

func (sc *Scanner) peekNext() byte {
	if sc.current+1 >= len(sc.source) {
		return '\x00'
	}
	return sc.source[sc.current+1]
}

func (sc *Scanner) addString() {
	for sc.peek() != '"' && !sc.isAtEnd() {
		if sc.peek() == '\n' {
			sc.line = sc.line + 1
		}
		sc.advance()
	}

	if sc.isAtEnd() {
		Error(sc.line, "Unterminated string")
		return
	}
	sc.advance()
	value := sc.source[sc.start+1 : sc.current-1]
	sc.addTokenWithLiteral(STRING, value)
}

func (sc *Scanner) isDigit(c byte) bool {
	digit_list := []byte{'0', '1', '2', '3', '4', '5', '6', '7', '8', '9'}
	for _, digit := range digit_list {
		if digit == c {
			return true
		}
	}
	return false
}

func (sc *Scanner) isAlpha(c byte) bool {
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
}

func (sc *Scanner) isAlphaNumeric(c byte) bool {
	return sc.isAlpha(c) || sc.isDigit(c)
}

func (sc *Scanner) addWord() {
	for sc.isAlphaNumeric(sc.peek()) {
		sc.advance()
	}

	text := sc.source[sc.start:sc.current]
	gotType, isFound := keywords[text]
	if isFound {
		sc.addToken(gotType)
	} else {
		sc.addToken(IDENTIFIER)
	}
}

func (sc *Scanner) addNumber() {
	for sc.isDigit(sc.peek()) {
		sc.advance()
	}
	if sc.peek() == '.' && sc.isDigit(sc.peekNext()) {
		sc.advance()
	}
	for sc.isDigit(sc.peek()) {
		sc.advance()
	}
	value := sc.source[sc.start:sc.current]
	sc.addTokenWithLiteral(NUMBER, value)
}

func (sc *Scanner) scanToken() {
	var c byte = sc.advance()
	switch c {
	case '(':
		sc.addToken(LEFT_PAREN)
	case ')':
		sc.addToken(RIGHT_PAREN)
	case '{':
		sc.addToken(LEFT_BRACE)
	case '}':
		sc.addToken(RIGHT_BRACE)
	case '[':
		sc.addToken(LEFT_SQUARE)
	case ']':
		sc.addToken(RIGHT_SQUARE)
	case ',':
		sc.addToken(COMMA)
	case ':':
		sc.addToken(COLON)
	case '.':
		sc.addToken(DOT)
	case '-':
		sc.addToken(MINUS)
	case '+':
		sc.addToken(PLUS)
	case ';':
		sc.addToken(SEMICOLON)
	case '*':
		sc.addToken(STAR)
	case '!':
		if sc.match('=') {
			sc.addToken(BANG_EQUAL)
		} else {
			sc.addToken(BANG)
		}
	case '=':
		if sc.match('=') {
			sc.addToken(EQUAL_EQUAL)
		} else {
			sc.addToken(EQUAL)
		}
	case '<':
		if sc.match('=') {
			sc.addToken(LESS_EQUAL)
		} else {
			sc.addToken(LESS)
		}
	case '>':
		if sc.match('=') {
			sc.addToken(GREATER_EQUAL)
		} else {
			sc.addToken(GREATER)
		}
	case '/':
		if sc.match('/') {
			for sc.peek() != '\n' && !sc.isAtEnd() {
				sc.advance()
			}
		} else {
			sc.addToken(SLASH)
		}
	case ' ', '\t':
	case '\r':
		sc.advance()
	case '\n':
		sc.line += 1
	case '"':
		sc.addString()
	case '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
		sc.addNumber()
	default:
		if sc.isAlpha(c) {
			sc.addWord()
		} else {
			Error(sc.line, "Unexpected character "+string(c))
		}
	}
}

// run functions
func run(source string, env Environment) {
	// running code
	p := Parser{make([]Token, 0)}
	program := p.produceAST(source)
	result := evaluate(0, program, env)
	fmt.Printf(color.GreenString("%+v\n"), result)
}

func runFile(path string, env Environment) {
	fmt.Println()
	_, err := os.Stat(path)
	if err != nil {
		fmt.Println(color.RedString("ERROR : file not found at"), color.RedString(path))
		fmt.Println()
		return
	}
	data, err := os.ReadFile(path)
	if err != nil {
		fmt.Println(color.RedString("ERROR : file cannot be read at"), color.RedString(path))
		fmt.Println()
		return
	}
	fmt.Println(color.YellowString("Welcome to Toy Lang Source"))
	fmt.Println(color.YellowString("File " + path + " running ..."))
	fmt.Println()
	run(string(data), env)
	fmt.Println()
	fmt.Println(color.YellowString("Thanks for using Source - by Pratik"))
	fmt.Println()
}

func runPrompt(env Environment) {

	reader := bufio.NewReader(os.Stdin)
	fmt.Println()
	fmt.Println(color.YellowString("Welcome to Toy Lang REPL"))
	fmt.Println(color.YellowString("Enter 'quit' to exit"))

	for {
		fmt.Print(color.YellowString("\n>>> "))
		line, err := reader.ReadString('\n')
		fmt.Println()
		if err != nil {
			fmt.Println(color.RedString("ERROR : I/O error occured"))
		}
		line = line[:len(line)-2]
		if strings.TrimSpace(line) == "quit" {
			fmt.Println(color.YellowString("Thanks for using REPL - by Pratik"))
			fmt.Println()
			break
		}
		run(line, env)
	}

}

// PARSER

type NodeType int

const (
	// Statements
	_PROGRAM NodeType = iota + 1
	_VAR_DECLARE

	// Expressions
	_ASSIGN_EXPR
	_NUMERIC_LITERAL
	_BOOL_LITERAL
	_NIL_LITERAL
	_IDENTIFIER
	_BINARY_EXPR
)

func (n NodeType) EnumIndex() int {
	return int(n)
}

func (n NodeType) String() string {
	return [...]string{"PROGRAM", "VAR_DECLARE", "ASSIGN_EXPR", "NUMERIC_LITERAL", "BOOL_LITERAL", "NIL_LITERAL", "IDENTIFIER", "BINARY_EXPR"}[n-1]
}

type Stmt struct {
	kind string
}

type VarDeclaration struct {
	kind       string
	constant   bool
	identifier string
	value      any
}

type Program struct {
	kind string
	body []any
}

type Expr struct {
	kind NodeType
}

type AssignmentExpr struct {
	kind     string
	assignee any
	value    any
}

type BinaryExpr struct {
	kind     string
	left     any
	right    any
	operator string
}

type Identifier struct {
	kind   string
	symbol string
}

type NumericLiteral struct {
	kind  string
	value float64
}

type BoolLiteral struct {
	kind  string
	value bool
}

type NilLiteral struct {
	kind  string
	value string
}

type Parser struct {
	tokens []Token
}

func (p *Parser) not_eof() bool {
	return p.tokens[0].tokenType != EOF
}

func (p *Parser) at() Token {
	return p.tokens[0]
}

func (p *Parser) eat() Token {
	prev := p.tokens[0]
	p.tokens = p.tokens[1:]
	return prev
}

func (p *Parser) expect(tokenType TokenType, errMsg string) Token {
	prev := p.tokens[0]
	p.tokens = p.tokens[1:]
	if prev.tokenType == EOF || prev.tokenType != tokenType {
		Error(prev.line, errMsg)
		os.Exit(1)
	}
	return prev
}

func (p *Parser) produceAST(source string) Program {
	sc := Scanner{source, []Token{}, 0, 0, 1}
	p.tokens = sc.scanTokens()

	program := Program{
		kind: _PROGRAM.String(),
		body: make([]any, 0),
	}

	line := 1
	for p.not_eof() {
		program.body = append(program.body, p.parse_stmt(line))
		line += 1
	}

	return program
}

func (p *Parser) parse_stmt(line int) any {

	switch p.at().tokenType {
	case VAR, CONST:
		return p.parse_var_declare(line)
	default:
		return p.parse_expr()
	}
}

func (p *Parser) parse_var_declare(line int) any {
	isConstant := (p.eat().tokenType == CONST)
	identifier := p.expect(IDENTIFIER, "Expected identifier after var | const keyword").lexeme

	if p.at().tokenType == SEMICOLON {
		p.eat()
		if isConstant {
			Error(line, "Must assign value to const declared identifier "+identifier)
			os.Exit(1)
		}
		return VarDeclaration{
			kind:       _VAR_DECLARE.String(),
			constant:   false,
			identifier: identifier,
			value:      nil,
		}
	}

	if isConstant {
		p.expect(EQUAL, "Expected = following identifier "+identifier)
		declaration := VarDeclaration{
			kind:       _VAR_DECLARE.String(),
			constant:   isConstant,
			identifier: identifier,
			value:      p.parse_expr(),
		}
		if p.at().tokenType == SEMICOLON {
			p.eat()
		}
		return declaration
	} else {
		if p.at().tokenType == EQUAL {
			p.eat()
			declaration := VarDeclaration{
				kind:       _VAR_DECLARE.String(),
				constant:   isConstant,
				identifier: identifier,
				value:      p.parse_expr(),
			}
			if p.at().tokenType == SEMICOLON {
				p.eat()
			}
			return declaration
		} else {
			return VarDeclaration{
				kind:       _VAR_DECLARE.String(),
				constant:   isConstant,
				identifier: identifier,
				value:      nil,
			}
		}
	}
}

func (p *Parser) parse_expr() any {
	return p.parse_assignment_expr()
}

func (p *Parser) parse_assignment_expr() any {
	left := p.parse_additive_expr()
	if p.at().tokenType == EQUAL {
		p.eat()
		value := p.parse_assignment_expr()
		return AssignmentExpr{
			kind:     _ASSIGN_EXPR.String(),
			assignee: left,
			value:    value,
		}
	}

	return left
}

func (p *Parser) parse_additive_expr() any {
	var left = p.parse_multiplicative_expr()
	for p.at().tokenType == PLUS || p.at().tokenType == MINUS {
		var operator string = p.eat().lexeme
		var right = p.parse_multiplicative_expr()
		left = BinaryExpr{
			kind:     _BINARY_EXPR.String(),
			left:     left,
			right:    right,
			operator: operator,
		}
	}
	return left
}

func (p *Parser) parse_multiplicative_expr() any {
	var left = p.parse_primary_expr()
	for p.at().tokenType == STAR || p.at().tokenType == SLASH {
		var operator string = p.eat().lexeme
		var right = p.parse_multiplicative_expr()
		left = BinaryExpr{
			kind:     _BINARY_EXPR.String(),
			left:     left,
			right:    right,
			operator: operator,
		}
	}
	return left
}

func (p *Parser) parse_primary_expr() any {
	tk := p.at().tokenType

	switch tk {
	case IDENTIFIER:
		return Identifier{
			kind:   _IDENTIFIER.String(),
			symbol: p.eat().lexeme,
		}
	case NIL:
		p.eat()
		return NilLiteral{kind: _NIL_LITERAL.String(), value: "nil"}
	case TRUE:
		p.eat()
		return BoolLiteral{kind: _BOOL_LITERAL.String(), value: true}
	case FALSE:
		p.eat()
		return BoolLiteral{kind: _BOOL_LITERAL.String(), value: false}
	case NUMBER:
		val, err := strconv.ParseFloat(p.eat().lexeme, 64)
		if err != nil {
			Error(p.at().line, "Cannot convert "+p.at().lexeme+" to number")
			os.Exit(1)
		}
		return NumericLiteral{
			kind:  _NUMERIC_LITERAL.String(),
			value: val,
		}
	case LEFT_PAREN:
		p.eat()
		val := p.parse_expr()
		p.expect(RIGHT_PAREN, "Unexpected token "+p.at().lexeme+" found, Expected )")
		return val
	default:
		Error(p.tokens[0].line, "Unexpected token "+p.at().tokenType.String()+" found during parsing")
		os.Exit(1)
		return Expr{}
	}
}

// VALUES AT RUNTIME

func MK_NUMBER_VALUE(n float64) NumberValue {
	return NumberValue{valueType: _NUMBER_VALUE.String(), value: n}
}

func MK_NIL_VALUE() NilValue {
	return NilValue{valueType: _NIL_VALUE.String(), value: "nil"}
}

func MK_BOOL_VALUE(b bool) BoolValue {
	return BoolValue{valueType: _BOOL_VALUE.String(), value: b}
}

type ValueType int

const (
	_NIL_VALUE ValueType = iota + 1
	_BOOL_VALUE
	_NUMBER_VALUE
)

func (v ValueType) EnumIndex() int {
	return int(v)
}

func (v ValueType) String() string {
	return [...]string{"NIL_VALUE", "BOOL_VALUE", "NUMBER_VALUE"}[v-1]
}

type NilValue struct {
	valueType string
	value     string
}

type BoolValue struct {
	valueType string
	value     bool
}

type NumberValue struct {
	valueType string
	value     float64
}

// TREE WALK INTERPRETER

func evaluate(line int, astNode any, env Environment) any {

	kind := reflect.TypeOf(astNode).Name()

	switch kind {
	case reflect.TypeOf(NumericLiteral{}).Name():
		astNode_typed := astNode.(NumericLiteral)
		return MK_NUMBER_VALUE(astNode_typed.value)
	case reflect.TypeOf(NilLiteral{}).Name():
		return MK_NIL_VALUE()
	case reflect.TypeOf(BoolLiteral{}).Name():
		astNode_typed := astNode.(BoolLiteral)
		return MK_BOOL_VALUE(astNode_typed.value)
	case reflect.TypeOf(Identifier{}).Name():
		astNode_typed := astNode.(Identifier)
		return evaluate_identifier(line, astNode_typed, env)
	case reflect.TypeOf(AssignmentExpr{}).Name():
		astNode_typed := astNode.(AssignmentExpr)
		return evaluate_assignment(line, astNode_typed, env)
	case reflect.TypeOf(BinaryExpr{}).Name():
		astNode_typed := astNode.(BinaryExpr)
		return evaluate_binary_expr(line, astNode_typed, env)
	case reflect.TypeOf(Program{}).Name():
		astNode_typed := astNode.(Program)
		return evaluate_program(astNode_typed, env)
	case reflect.TypeOf(VarDeclaration{}).Name():
		astNode_typed := astNode.(VarDeclaration)
		return evaluate_var_declaration(line, astNode_typed, env)
	default:
		errMsg := fmt.Sprintf("Bad AST Setup. Unrecogised Node %T", astNode)
		Error(line, errMsg)
		os.Exit(1)
		return MK_NIL_VALUE()
	}
}

func evaluate_assignment(line int, node AssignmentExpr, env Environment) any {
	if reflect.TypeOf(node.assignee).Name() != reflect.TypeOf(Identifier{}).Name() {
		errMsg := "Cannot assign " + reflect.ValueOf(node.value).String() + " to " + reflect.ValueOf(node.assignee).String()
		Error(line, errMsg)
		os.Exit(1)
	}
	return env.assignVar(line, node.assignee.(Identifier).symbol, evaluate(line, node.value, env))
}

func evaluate_var_declaration(line int, declaration VarDeclaration, env Environment) any {
	val := evaluate(line, declaration.value, env)
	if val == nil {
		val = MK_NIL_VALUE()
	}
	return env.declareVar(declaration.identifier, val, declaration.constant)
}

func evaluate_identifier(line int, identifier Identifier, env Environment) any {
	val := env.lookupVar(line, identifier.symbol)
	return val
}

func evaluate_numeric_binary_expr(line int, lhs NumberValue, rhs NumberValue, operator string) NumberValue {
	result := 0.0

	if operator == "+" {
		result = lhs.value + rhs.value
	} else if operator == "-" {
		result = lhs.value - rhs.value
	} else if operator == "*" {
		result = lhs.value * rhs.value
	} else if operator == "/" {
		if rhs.value == 0.0 {
			Error(line, "Division by 0 not allowed")
			os.Exit(1)
		}
		result = lhs.value / rhs.value
	} else {
		Error(line, "Unknown operator "+operator)
		os.Exit(1)
	}

	return MK_NUMBER_VALUE(result)
}

func evaluate_binary_expr(line int, binop BinaryExpr, env Environment) any {
	lhs := evaluate(line, binop.left, env)
	rhs := evaluate(line, binop.right, env)
	if reflect.TypeOf(lhs).Name() == reflect.TypeOf(NumberValue{}).Name() && reflect.TypeOf(rhs).Name() == reflect.TypeOf(NumberValue{}).Name() {
		lhs_typed := lhs.(NumberValue)
		rhs_typed := rhs.(NumberValue)
		return evaluate_numeric_binary_expr(line, lhs_typed, rhs_typed, binop.operator)
	} else {
		return MK_NIL_VALUE()
	}
}

func evaluate_program(program Program, env Environment) any {
	var lastEvaluated any
	lastEvaluated = MK_NIL_VALUE()
	for i, stmt := range program.body {
		lastEvaluated = evaluate(i+1, stmt, env)
	}
	return lastEvaluated
}

// Environment Class for Scope

type void struct{}

type Environment struct {
	parent    *Environment
	variables map[string]any
	constants map[string]void
}

func (e *Environment) createEnvironment(parentEnv Environment) {
	e.parent = &parentEnv
	e.variables = make(map[string]any, 0)
	e.constants = make(map[string]void, 0)
}

func (e *Environment) declareVar(varname string, value any, constant bool) any {
	_, ok := e.variables[varname]
	if ok {
		Error(0, "Cannot redeclare identifier "+varname)
		os.Exit(1)
	}
	e.variables[varname] = value
	if constant {
		e.constants[varname] = void{}
	}
	return value
}

func (e *Environment) assignVar(line int, varname string, value any) any {
	env := e.resolve(line, varname)
	_, ok := e.constants[varname]
	if ok {
		Error(line, "Cannot assign to const identifier "+varname)
		os.Exit(1)
	}
	env.variables[varname] = value
	return value
}

func (e *Environment) resolve(line int, varname string) Environment {
	_, ok := e.variables[varname]
	if ok {
		return *e
	}

	if e.parent == nil {
		Error(line, "Cannot resolve identifier "+varname)
		os.Exit(1)
	}

	return e.parent.resolve(line, varname)
}

func (e *Environment) lookupVar(line int, varname string) any {
	env := e.resolve(line, varname)
	return env.variables[varname]
}
