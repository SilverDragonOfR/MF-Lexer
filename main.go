package main

import (
	"bufio"
	"fmt"
	"math"
	"os"
	"reflect"
	"strconv"
	"strings"
	"time"

	"github.com/fatih/color"
)

// loop stack overflow
const MAX_LOOPS = 50

// Setting global environment scope
func setUpGlobalEnvironmentScope(env Environment) {

	// Environment variables
	env.declareVar("true", MK_BOOL_VALUE(true), true)
	env.declareVar("false", MK_BOOL_VALUE(false), true)
	env.declareVar("nil", MK_NIL_VALUE(), true)

	// Native functions
	env.declareVar("print", MK_NATIVE_FN_VALUE(func(args []any, env Environment) any {
		fmt.Print(color.GreenString(fmt.Sprintln(args...)))
		return MK_NIL_VALUE()
	}), true)

	env.declareVar("getTime", MK_NATIVE_FN_VALUE(func(args []any, env Environment) any {
		return MK_NUMBER_VALUE(float64(time.Now().Unix()))
	}), true)
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
	PERCENT
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
	CONTINUE
	ELSE
	ELIF
	FALSE
	FN
	FOR
	IF
	LOOP
	NIL
	OR
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
	return [...]string{"LEFT_PAREN", "RIGHT_PAREN", "LEFT_BRACE", "RIGHT_BRACE", "LEFT_SQUARE", "RIGHT_SQUARE", "COMMA", "COLON", "DOT", "MINUS", "PLUS", "PERCENT", "SEMICOLON", "SLASH", "STAR", "BANG", "BANG_EQUAL", "EQUAL", "EQUAL_EQUAL", "GREATER", "GREATER_EQUAL", "LESS", "LESS_EQUAL", "IDENTIFIER", "STRING", "NUMBER", "AND", "BREAK", "CLASS", "CONST", "CONTINUE", "ELSE", "ELIF", "FALSE", "FN", "FOR", "IF", "LOOP", "NIL", "OR", "RETURN", "SUPER", "THIS", "TRUE", "VAR", "WHILE", "EOF"}[t-1]
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
	keywords["continue"] = CONTINUE
	keywords["else"] = ELSE
	keywords["elif"] = ELIF
	keywords["false"] = FALSE
	keywords["for"] = FOR
	keywords["fn"] = FN
	keywords["if"] = IF
	keywords["loop"] = LOOP
	keywords["nil"] = NIL
	keywords["or"] = OR
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
	fmt.Println(color.RedString("      :"), color.RedString(message))
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
	case '%':
		sc.addToken(PERCENT)
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
	_ = evaluate(0, program, env)
	// fmt.Printf(color.GreenString("%+v\n"), result)
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
	_IF
	_LOOP
	_BREAK
	_CONTINUE

	// Expressions
	_FUNCTION_EXPR
	_ASSIGN_EXPR
	_MEMBER_EXPR
	_CALL_EXPR

	//Literals
	_PROPERTY
	_ARRAY_LITERAL
	_STRING_LITERAL
	_OBJECT_LITERAL
	_NUMERIC_LITERAL
	_BOOL_LITERAL
	_NIL_LITERAL
	_IDENTIFIER
	_BINARY_EXPR
	_UNARY_EXPR
)

func (n NodeType) EnumIndex() int {
	return int(n)
}

func (n NodeType) String() string {
	return [...]string{"PROGRAM", "VAR_DECLARE", "IF", "LOOP", "BREAK", "CONTINUE", "FUNCTION_EXPR", "ASSIGN_EXPR", "MEMBER_EXPR", "CALL_EXPR", "PROPERTY", "ARRAY_LITERAL", "STRING_LITERAL", "OBJECT_LITERAL", "NUMERIC_LITERAL", "BOOL_LITERAL", "NIL_LITERAL", "IDENTIFIER", "BINARY_EXPR", "UNARY_EXPR"}[n-1]
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

type If struct {
	kind          string
	ifCondition   any
	ifBody        []any
	countElif     int
	elifCondition []any
	elifBody      [][]any
	isElse        bool
	elseBody      []any
}

type Loop struct {
	kind          string
	loopCondition any
	loopBody      []any
	maxLoops      int
}

type Break struct {
	kind string
}

type Continue struct {
	kind string
}

type Program struct {
	kind string
	body []any
}

type Expr struct {
	kind NodeType
}

type FunctionExpr struct {
	kind       string
	name       string
	parameters []string
	body       []any
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

type UnaryExpr struct {
	kind     string
	operator string
	operand  any
}

type CallExpr struct {
	kind   string
	args   []any
	caller any
}

type MemberExpr struct {
	kind     string
	object   any
	property any
	computed bool
}

type Identifier struct {
	kind   string
	symbol string
}

type NumericLiteral struct {
	kind  string
	value float64
}

type ArrayLiteral struct {
	kind  string
	value []any
}

type Property struct {
	kind  string
	key   string
	value any
}

type StringLiteral struct {
	kind  string
	value string
}

type ObjectLiteral struct {
	kind       string
	properties []Property
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

	// fmt.Printf("token: %+v\n", p.tokens)

	program := Program{
		kind: _PROGRAM.String(),
		body: make([]any, 0),
	}

	line := 1
	for p.not_eof() {
		program.body = append(program.body, p.parse_stmt(line))
		line += 1
	}

	// fmt.Printf("%+v\n", program.body)
	return program
}

func (p *Parser) parse_stmt(line int) any {

	switch p.at().tokenType {
	case VAR, CONST:
		return p.parse_var_declare(line)
	case IF:
		return p.parse_if(line)
	case LOOP:
		return p.parse_loop(line)
	case BREAK:
		p.eat()
		return Break{kind: _BREAK.String()}
	case CONTINUE:
		p.eat()
		return Continue{kind: _CONTINUE.String()}
	default:
		return p.parse_expr(line)
	}
}

func (p *Parser) parse_if(line int) any {
	p.eat()
	p.expect(LEFT_PAREN, "Expected ( after if")
	ifCondition := p.parse_expr(line)
	p.expect(RIGHT_PAREN, "Expected ) after expression")
	p.expect(LEFT_BRACE, "Expected { after expression")
	ifBody := make([]any, 0)
	for p.at().tokenType != EOF && p.at().tokenType != RIGHT_BRACE {
		ifBody = append(ifBody, p.parse_stmt(line))
	}
	p.expect(RIGHT_BRACE, "Expected } after if body")
	countElif := 0
	elifCondition := make([]any, 0)
	elifBody := make([][]any, 0)
	for p.at().tokenType == ELIF {
		p.eat()
		countElif += 1
		p.expect(LEFT_PAREN, "Expected ( after elif")
		elifCondition_i := p.parse_expr(line)
		p.expect(RIGHT_PAREN, "Expected ) after expression")
		p.expect(LEFT_BRACE, "Expected { after expression")
		elifBody_i := make([]any, 0)
		for p.at().tokenType != EOF && p.at().tokenType != RIGHT_BRACE {
			elifBody_i = append(elifBody_i, p.parse_stmt(line))
		}
		p.expect(RIGHT_BRACE, "Expected } after elif body")
		elifCondition = append(elifCondition, elifCondition_i)
		elifBody = append(elifBody, elifBody_i)
	}
	isElse := false
	elseBody := make([]any, 0)
	if p.at().tokenType == ELSE {
		p.eat()
		isElse = true
		p.expect(LEFT_BRACE, "Expected { after else")
		for p.at().tokenType != EOF && p.at().tokenType != RIGHT_BRACE {
			elseBody = append(elseBody, p.parse_stmt(line))
		}
		p.expect(RIGHT_BRACE, "Expected } after else body")
	}
	return If{
		kind:          _IF.String(),
		ifCondition:   ifCondition,
		ifBody:        ifBody,
		countElif:     countElif,
		elifCondition: elifCondition,
		elifBody:      elifBody,
		isElse:        isElse,
		elseBody:      elseBody,
	}
}

func (p *Parser) parse_loop(line int) any {
	p.eat()
	p.expect(LEFT_PAREN, "Expected ( after loop")
	loopCondition := p.parse_expr(line)
	p.expect(RIGHT_PAREN, "Expected ) after expression")
	p.expect(LEFT_BRACE, "Expected { after expression")
	loopBody := make([]any, 0)
	for p.at().tokenType != EOF && p.at().tokenType != RIGHT_BRACE {
		loopBody = append(loopBody, p.parse_stmt(line))
	}
	p.expect(RIGHT_BRACE, "Expected } after if body")
	return Loop{
		kind:          _LOOP.String(),
		loopCondition: loopCondition,
		loopBody:      loopBody,
		maxLoops:      MAX_LOOPS,
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
			value:      p.parse_expr(line),
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
				value:      p.parse_expr(line),
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
				value:      NilLiteral{kind: _NIL_LITERAL.String(), value: "nil"},
			}
		}
	}
}

func (p *Parser) parse_expr(line int) any {
	return p.parse_assignment_expr(line)
}

func (p *Parser) parse_assignment_expr(line int) any {
	left := p.parse_object_expr(line)
	if p.at().tokenType == EQUAL {
		p.eat()
		value := p.parse_assignment_expr(line)
		return AssignmentExpr{
			kind:     _ASSIGN_EXPR.String(),
			assignee: left,
			value:    value,
		}
	}

	return left
}

func (p *Parser) parse_object_expr(line int) any {
	if p.at().tokenType != LEFT_BRACE {
		return p.parse_array_expr(line)
	}
	p.eat()
	properties := make([]Property, 0)
	for p.not_eof() && p.at().tokenType != RIGHT_BRACE {
		key := p.expect(IDENTIFIER, "Object key expected").lexeme

		if p.at().tokenType == COMMA {
			p.eat()
			properties = append(properties, Property{kind: _PROPERTY.String(), key: key, value: nil})
			continue
		} else if p.at().tokenType == RIGHT_BRACE {
			properties = append(properties, Property{kind: _PROPERTY.String(), key: key, value: nil})
			continue
		}

		p.expect(COLON, "Object missing : after key")
		value := p.parse_expr(line)
		properties = append(properties, Property{kind: _PROPERTY.String(), key: key, value: value})
		if p.at().tokenType != RIGHT_BRACE {
			p.expect(COMMA, "Expected , or } after property")
		}
	}

	p.expect(RIGHT_BRACE, "Object missing }")
	return ObjectLiteral{
		kind:       _OBJECT_LITERAL.String(),
		properties: properties,
	}
}

func (p *Parser) parse_array_expr(line int) any {
	if p.at().tokenType != LEFT_SQUARE {
		return p.parse_function_expr(line)
	}
	p.eat()
	arr := make([]any, 0)
	for p.not_eof() && p.at().tokenType != RIGHT_SQUARE {
		value := p.parse_expr(line)
		arr = append(arr, value)
		if p.at().tokenType != RIGHT_SQUARE {
			p.expect(COMMA, "Expected , or ] after element")
		}
	}

	p.expect(RIGHT_SQUARE, "Array missing ]")
	return ArrayLiteral{
		kind:  _ARRAY_LITERAL.String(),
		value: arr,
	}
}

func (p *Parser) parse_function_expr(line int) any {
	if p.at().tokenType != FN {
		return p.parse_logical_not_expr(line)
	}
	p.eat()
	name := p.expect(IDENTIFIER, "Expected identifier following fn").lexeme
	args := p.parse_args(line)
	params := make([]string, 0)
	for _, arg := range args {
		if reflect.TypeOf(arg).Name() != reflect.TypeOf(Identifier{}).Name() {
			Error(line, "Inside function declaration expected parameter to be of type string")
			os.Exit(1)
		}
		arg_typed := arg.(Identifier)
		params = append(params, arg_typed.symbol)
	}

	p.expect(LEFT_BRACE, "Expected { after function indentifier")
	body := make([]any, 0)
	for p.at().tokenType != EOF && p.at().tokenType != RIGHT_BRACE {
		body = append(body, p.parse_stmt(line))
	}
	p.expect(RIGHT_BRACE, "Expected } after function body")
	fn := FunctionExpr{
		kind:       _FUNCTION_EXPR.String(),
		name:       name,
		parameters: params,
		body:       body,
	}
	return fn
}

func (p *Parser) parse_logical_not_expr(line int) any {
	if p.at().tokenType != BANG {
		return p.parse_logical_and_or_expr(line)
	}
	var operator = p.eat().lexeme
	var operand = p.parse_logical_and_or_expr(line)
	return UnaryExpr{
		kind:     _BINARY_EXPR.String(),
		operator: operator,
		operand:  operand,
	}
}

func (p *Parser) parse_logical_and_or_expr(line int) any {
	var left = p.parse_relational_equal_expr(line)
	for p.at().tokenType == AND || p.at().tokenType == OR {
		var operator string = p.eat().lexeme
		var right = p.parse_relational_equal_expr(line)
		left = BinaryExpr{
			kind:     _BINARY_EXPR.String(),
			left:     left,
			right:    right,
			operator: operator,
		}
	}
	return left
}

func (p *Parser) parse_relational_equal_expr(line int) any {
	var left = p.parse_relational_greater_less_expr(line)
	for p.at().tokenType == EQUAL_EQUAL || p.at().tokenType == BANG_EQUAL {
		var operator string = p.eat().lexeme
		var right = p.parse_relational_greater_less_expr(line)
		left = BinaryExpr{
			kind:     _BINARY_EXPR.String(),
			left:     left,
			right:    right,
			operator: operator,
		}
	}
	return left
}

func (p *Parser) parse_relational_greater_less_expr(line int) any {
	var left = p.parse_additive_expr(line)
	for p.at().tokenType == GREATER || p.at().tokenType == GREATER_EQUAL || p.at().tokenType == LESS || p.at().tokenType == LESS_EQUAL || p.at().tokenType == LESS || p.at().tokenType == LESS_EQUAL {
		var operator string = p.eat().lexeme
		var right = p.parse_additive_expr(line)
		left = BinaryExpr{
			kind:     _BINARY_EXPR.String(),
			left:     left,
			right:    right,
			operator: operator,
		}
	}
	return left
}

func (p *Parser) parse_additive_expr(line int) any {
	var left = p.parse_multiplicative_expr(line)
	for p.at().tokenType == PLUS || p.at().tokenType == MINUS {
		var operator string = p.eat().lexeme
		var right = p.parse_multiplicative_expr(line)
		left = BinaryExpr{
			kind:     _BINARY_EXPR.String(),
			left:     left,
			right:    right,
			operator: operator,
		}
	}
	return left
}

func (p *Parser) parse_multiplicative_expr(line int) any {
	var left = p.parse_call_member_expr(line)
	for p.at().tokenType == STAR || p.at().tokenType == SLASH || p.at().tokenType == PERCENT {
		var operator string = p.eat().lexeme
		var right = p.parse_call_member_expr(line)
		left = BinaryExpr{
			kind:     _BINARY_EXPR.String(),
			left:     left,
			right:    right,
			operator: operator,
		}
	}
	return left
}

func (p *Parser) parse_call_member_expr(line int) any {
	member := p.parse_member_expr(line)

	if p.at().tokenType == LEFT_PAREN {
		member = p.parse_call_expr(line, member)
	}
	return member
}

func (p *Parser) parse_call_expr(line int, caller any) any {
	var call_expr any
	call_expr = CallExpr{
		kind:   _CALL_EXPR.String(),
		args:   p.parse_args(line),
		caller: caller,
	}

	if p.at().tokenType == LEFT_PAREN {
		call_expr = p.parse_call_expr(line, call_expr)
	}
	return call_expr
}

func (p *Parser) parse_args(line int) []any {
	p.expect(LEFT_PAREN, "Expected (")
	var args []any
	if p.at().tokenType == RIGHT_PAREN {
		args = make([]any, 0)
	} else {
		args = p.parse_args_list(line)
	}
	p.expect(RIGHT_PAREN, "Expected )")
	return args
}

func (p *Parser) parse_args_list(line int) []any {
	args := make([]any, 0)
	args = append(args, p.parse_assignment_expr(line))
	for p.at().tokenType == COMMA {
		p.eat()
		args = append(args, p.parse_assignment_expr(line))
	}
	return args
}

func (p *Parser) parse_member_expr(line int) any {
	object := p.parse_primary_expr(line)

	for p.at().tokenType == DOT || p.at().tokenType == LEFT_SQUARE {
		operator := p.eat()
		var property any
		var computed bool

		if operator.tokenType == DOT {
			computed = false
			property = p.parse_primary_expr(line)

			if reflect.TypeOf(property).Name() != reflect.TypeOf(Identifier{}).Name() {
				Error(p.at().line, "Right side of . must be identifier")
				os.Exit(1)
			}
		} else {
			computed = true
			property = p.parse_expr(line)
			p.expect(RIGHT_SQUARE, "Missing ]")
		}

		object = MemberExpr{
			kind:     _MEMBER_EXPR.String(),
			object:   object,
			property: property,
			computed: computed,
		}
	}

	return object
}

func (p *Parser) parse_primary_expr(line int) any {
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
	case STRING:
		val := p.eat().lexeme
		return StringLiteral{
			kind:  _STRING_LITERAL.String(),
			value: val,
		}
	case LEFT_PAREN:
		p.eat()
		val := p.parse_expr(line)
		p.expect(RIGHT_PAREN, "Unexpected token "+p.at().lexeme+" found, Expected )")
		return val
	default:
		Error(p.tokens[0].line, "Unexpected token "+p.at().tokenType.String()+" found during parsing")
		os.Exit(1)
		return Expr{}
	}
}

// VALUES AT RUNTIME

func MK_BREAK_VALUE() BreakValue {
	return BreakValue{valueType: _BREAK_VALUE.String()}
}

func MK_CONTINUE_VALUE() ContinueValue {
	return ContinueValue{valueType: _CONTINUE_VALUE.String()}
}

func MK_STRING_VALUE(s string) StringValue {
	return StringValue{valueType: _STRING_VALUE.String(), value: s}
}

func MK_NUMBER_VALUE(n float64) NumberValue {
	return NumberValue{valueType: _NUMBER_VALUE.String(), value: n}
}

func MK_NIL_VALUE() NilValue {
	return NilValue{valueType: _NIL_VALUE.String(), value: "nil"}
}

func MK_BOOL_VALUE(b bool) BoolValue {
	return BoolValue{valueType: _BOOL_VALUE.String(), value: b}
}

func MK_NATIVE_FN_VALUE(call FunctionCall) NativeFnValue {
	return NativeFnValue{valueType: _NATIVE_FN_VALUE.String(), call: call}
}

type ValueType int

const (
	_NIL_VALUE ValueType = iota + 1
	_ARRAY_VALUE
	_STRING_VALUE
	_BOOL_VALUE
	_NUMBER_VALUE
	_OBJECT_VALUE
	_NATIVE_FN_VALUE
	_FN_VALUE
	_BREAK_VALUE
	_CONTINUE_VALUE
)

func (v ValueType) EnumIndex() int {
	return int(v)
}

func (v ValueType) String() string {
	return [...]string{"NIL_VALUE", "ARRAY_VALUE", "STRING_VALUE", "BOOL_VALUE", "NUMBER_VALUE", "OBJECT_VALUE", "NATIVE_FN_VALUE", "FN_VALUE", "BREAK_VALUE", "CONTINUE_VALUE"}[v-1]
}

type NilValue struct {
	valueType string
	value     string
}

type ArrayValue struct {
	valueType string
	value     []any
}

type StringValue struct {
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

type ObjectValue struct {
	valueType  string
	properties map[string]any
}

type FunctionCall func(args []any, env Environment) any

type NativeFnValue struct {
	valueType string
	call      FunctionCall
}

type FnValue struct {
	valueType  string
	name       string
	parameters []string
	body       []any
	declareEnv *Environment
}

type BreakValue struct {
	valueType string
}

type ContinueValue struct {
	valueType string
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
	case reflect.TypeOf(StringLiteral{}).Name():
		astNode_typed := astNode.(StringLiteral)
		return MK_STRING_VALUE(astNode_typed.value)
	case reflect.TypeOf(BoolLiteral{}).Name():
		astNode_typed := astNode.(BoolLiteral)
		return MK_BOOL_VALUE(astNode_typed.value)
	case reflect.TypeOf(Identifier{}).Name():
		astNode_typed := astNode.(Identifier)
		return evaluate_identifier(line, astNode_typed, env)
	case reflect.TypeOf(ArrayLiteral{}).Name():
		astNode_typed := astNode.(ArrayLiteral)
		return evaluate_array_expr(line, astNode_typed, env)
	case reflect.TypeOf(ObjectLiteral{}).Name():
		astNode_typed := astNode.(ObjectLiteral)
		return evaluate_object_expr(line, astNode_typed, env)
	case reflect.TypeOf(FunctionExpr{}).Name():
		astNode_typed := astNode.(FunctionExpr)
		return evaluate_function_expr(line, astNode_typed, env)
	case reflect.TypeOf(CallExpr{}).Name():
		astNode_typed := astNode.(CallExpr)
		return evaluate_call_expr(line, astNode_typed, env)
	case reflect.TypeOf(AssignmentExpr{}).Name():
		astNode_typed := astNode.(AssignmentExpr)
		return evaluate_assignment(line, astNode_typed, env)
	case reflect.TypeOf(MemberExpr{}).Name():
		astNode_typed := astNode.(MemberExpr)
		return evaluate_member_expr(line, astNode_typed, env)
	case reflect.TypeOf(BinaryExpr{}).Name():
		astNode_typed := astNode.(BinaryExpr)
		return evaluate_binary_expr(line, astNode_typed, env)
	case reflect.TypeOf(UnaryExpr{}).Name():
		astNode_typed := astNode.(UnaryExpr)
		return evaluate_unary_expr(line, astNode_typed, env)
	case reflect.TypeOf(Program{}).Name():
		astNode_typed := astNode.(Program)
		return evaluate_program(astNode_typed, env)
	case reflect.TypeOf(If{}).Name():
		astNode_typed := astNode.(If)
		return evaluate_if_stmt(line, astNode_typed, env)
	case reflect.TypeOf(Loop{}).Name():
		astNode_typed := astNode.(Loop)
		return evaluate_loop_stmt(line, astNode_typed, env)
	case reflect.TypeOf(Break{}).Name():
		return MK_BREAK_VALUE()
	case reflect.TypeOf(Continue{}).Name():
		return MK_CONTINUE_VALUE()
	case reflect.TypeOf(VarDeclaration{}).Name():
		astNode_typed := astNode.(VarDeclaration)
		return evaluate_var_declaration(line, astNode_typed, env)
	default:
		errMsg := fmt.Sprintf("Bad AST Setup. Unrecogised Node %+v", astNode)
		Error(line, errMsg)
		os.Exit(1)
		return MK_NIL_VALUE()
	}
}

func evaluate_function_expr(line int, expr FunctionExpr, env Environment) any {
	fn := FnValue{
		valueType:  _FN_VALUE.String(),
		name:       expr.name,
		parameters: expr.parameters,
		body:       expr.body,
		declareEnv: &env,
	}
	return env.declareVar(expr.name, fn, true)
}

func evaluate_member_expr(line int, node MemberExpr, env Environment) any {

	if !node.computed {
		lhs := evaluate(line, node.object, env)
		if !(reflect.TypeOf(lhs).Name() == reflect.TypeOf(ObjectValue{}).Name()) {
			Error(line, "Cannot use . on "+reflect.TypeOf(lhs).Name())
			os.Exit(1)
		}
		lhs_typed := lhs.(ObjectValue)
		node_property_typed := node.property.(Identifier)
		value, ok := lhs_typed.properties[node_property_typed.symbol]
		if !ok {
			Error(line, "Object "+reflect.ValueOf(lhs_typed.properties).String()+" does not have property "+node_property_typed.symbol)
			os.Exit(1)
		}
		return value
	} else {
		lhs := evaluate(line, node.object, env)
		if (reflect.TypeOf(lhs).Name() == reflect.TypeOf(ObjectValue{}).Name()) {
			lhs_typed := lhs.(ObjectValue)
			evaluated_node_property := evaluate(line, node.property, env)
			if reflect.TypeOf(evaluated_node_property).Name() != reflect.TypeOf(StringValue{}).Name() {
				Error(line, "Index identifier must evaluate to string")
				os.Exit(1)
			}
			evaluated_node_property_typed := evaluated_node_property.(StringValue)
			value, ok := lhs_typed.properties[evaluated_node_property_typed.value[1:len(evaluated_node_property_typed.value)-1]]
			if !ok {
				Error(line, "Object "+reflect.ValueOf(lhs_typed.properties).String()+" does not have property "+evaluated_node_property_typed.value)
				os.Exit(1)
			}
			return value
		} else if (reflect.TypeOf(lhs).Name() == reflect.TypeOf(ArrayValue{}).Name()) {
			lhs_typed := lhs.(ArrayValue)
			evaluated_node_property := evaluate(line, node.property, env)
			if reflect.TypeOf(evaluated_node_property).Name() != reflect.TypeOf(NumberValue{}).Name() {
				Error(line, "Index identifier must evaluate to an int number")
				os.Exit(1)
			}
			evaluated_node_property_typed := evaluated_node_property.(NumberValue)
			if !(evaluated_node_property_typed.value == math.Ceil(evaluated_node_property_typed.value)) {
				Error(line, "Index identifier must evaluate to an int not float")
				os.Exit(1)
			}
			var index int = int(evaluated_node_property_typed.value)
			if index >= len(lhs_typed.value) || index < 0 {
				Error(line, "Index "+fmt.Sprint(index)+" out of bounds for array size "+fmt.Sprint(len(lhs_typed.value)))
				os.Exit(1)
			}
			element := lhs_typed.value[index]
			return element
		} else {
			Error(line, "Cannot use . on "+reflect.TypeOf(lhs).Name())
			os.Exit(1)
			return 1
		}
	}
}

func evaluate_assignment(line int, node AssignmentExpr, env Environment) any {
	if reflect.TypeOf(node.assignee).Name() == reflect.TypeOf(Identifier{}).Name() {
		return env.assignVar(line, node.assignee.(Identifier).symbol, evaluate(line, node.value, env))
	} else if reflect.TypeOf(node.assignee).Name() == reflect.TypeOf(MemberExpr{}).Name() {
		node_assignee_typed := node.assignee.(MemberExpr)
		if !node_assignee_typed.computed {
			lhs := evaluate(line, node_assignee_typed.object, env)
			if !(reflect.TypeOf(lhs).Name() == reflect.TypeOf(ObjectValue{}).Name()) {
				Error(line, "Cannot use . on "+reflect.TypeOf(lhs).Name())
				os.Exit(1)
			}
			lhs_typed := lhs.(ObjectValue)
			node_property_typed := node_assignee_typed.property.(Identifier)
			lhs_typed.properties[node_property_typed.symbol] = evaluate(line, node.value, env)
			return lhs_typed.properties[node_property_typed.symbol]
		} else {
			lhs := evaluate(line, node_assignee_typed.object, env)
			if (reflect.TypeOf(lhs).Name() == reflect.TypeOf(ObjectValue{}).Name()) {
				lhs_typed := lhs.(ObjectValue)
				evaluated_node_property := evaluate(line, node_assignee_typed.property, env)
				if reflect.TypeOf(evaluated_node_property).Name() != reflect.TypeOf(StringValue{}).Name() {
					Error(line, "Index identifier must evaluate to string")
					os.Exit(1)
				}
				evaluated_node_property_typed := evaluated_node_property.(StringValue)
				lhs_typed.properties[evaluated_node_property_typed.value[1:len(evaluated_node_property_typed.value)-1]] = evaluate(line, node.value, env)
				return lhs_typed.properties[evaluated_node_property_typed.value[1:len(evaluated_node_property_typed.value)-1]]
			} else if (reflect.TypeOf(lhs).Name() == reflect.TypeOf(ArrayValue{}).Name()) {
				lhs_typed := lhs.(ArrayValue)
				evaluated_node_property := evaluate(line, node_assignee_typed.property, env)
				if reflect.TypeOf(evaluated_node_property).Name() != reflect.TypeOf(NumberValue{}).Name() {
					Error(line, "Index identifier must evaluate to an int number")
					os.Exit(1)
				}
				evaluated_node_property_typed := evaluated_node_property.(NumberValue)
				if !(evaluated_node_property_typed.value == math.Ceil(evaluated_node_property_typed.value)) {
					Error(line, "Index identifier must evaluate to an int not float")
					os.Exit(1)
				}
				var index int = int(evaluated_node_property_typed.value)
				if index >= len(lhs_typed.value) || index < 0 {
					Error(line, "Index "+fmt.Sprint(index)+" out of bounds for array size "+fmt.Sprint(len(lhs_typed.value)))
					os.Exit(1)
				}
				lhs_typed.value[index] = evaluate(line, node.value, env)
				return lhs_typed.value[index]
			} else {
				Error(line, "Cannot use . on "+reflect.TypeOf(lhs).Name())
				os.Exit(1)
				return 1
			}
		}

	} else {
		errMsg := "Cannot assign " + reflect.ValueOf(node.value).String() + " to " + reflect.ValueOf(node.assignee).String()
		Error(line, errMsg)
		os.Exit(1)
		return 1
	}
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

func evaluate_array_expr(line int, arr ArrayLiteral, env Environment) any {
	array := ArrayValue{valueType: _ARRAY_VALUE.String(), value: make([]any, 0)}
	for _, element := range arr.value {
		runtimeElement := evaluate(line, element, env)
		array.value = append(array.value, runtimeElement)
	}
	return array
}

func evaluate_object_expr(line int, obj ObjectLiteral, env Environment) any {
	object := ObjectValue{valueType: _OBJECT_VALUE.String(), properties: make(map[string]any, 0)}
	for _, prop := range obj.properties {
		key := prop.key
		value := prop.value
		if value == nil {
			runtimeVal := env.lookupVar(line, key)
			object.properties[key] = runtimeVal
		} else {
			runtimeVal := evaluate(line, value, env)
			object.properties[key] = runtimeVal
		}
	}
	return object
}

func evaluate_call_expr(line int, expr CallExpr, env Environment) any {
	args := make([]any, 0)
	for i, arg := range expr.args {
		args = append(args, evaluate(line+i, arg, env))
	}
	fn := evaluate(line, expr.caller, env)
	if reflect.TypeOf(fn).Name() == reflect.TypeOf(NativeFnValue{}).Name() {
		result := fn.(NativeFnValue).call(args, env)
		return result
	} else if reflect.TypeOf(fn).Name() == reflect.TypeOf(FnValue{}).Name() {
		fn_typed := fn.(FnValue)
		scope := Environment{
			parent:    fn_typed.declareEnv,
			variables: make(map[string]any, 0),
			constants: make(map[string]void, 0),
		}
		if len(args) != len(fn_typed.parameters) {
			Error(line, fmt.Sprint(len(args))+" arguments does not match "+fmt.Sprint(len(fn_typed.parameters))+" parameters")
			os.Exit(1)
		}
		for i := range fn_typed.parameters {
			varname := fn_typed.parameters[i]
			scope.declareVar(varname, args[i], false)
		}
		var result any
		result = MK_NIL_VALUE()
		for _, stmt := range fn_typed.body {
			result = evaluate(line, stmt, scope)
		}
		return result

	} else {
		Error(line, "Cannot call "+reflect.ValueOf(fn).Kind().String())
		os.Exit(1)
		return 1
	}
}

func evaluate_numeric_binary_expr(line int, lhs NumberValue, rhs NumberValue, operator string) any {

	if operator == "+" {
		result := lhs.value + rhs.value
		return MK_NUMBER_VALUE(result)
	} else if operator == "-" {
		result := lhs.value - rhs.value
		return MK_NUMBER_VALUE(result)
	} else if operator == "*" {
		result := lhs.value * rhs.value
		return MK_NUMBER_VALUE(result)
	} else if operator == "/" {
		if rhs.value == 0.0 {
			Error(line, "Division by 0 not allowed")
			os.Exit(1)
		}
		result := lhs.value / rhs.value
		return MK_NUMBER_VALUE(result)
	} else if operator == ">" {
		result := lhs.value > rhs.value
		return MK_BOOL_VALUE(result)
	} else if operator == ">=" {
		result := lhs.value >= rhs.value
		return MK_BOOL_VALUE(result)
	} else if operator == "<" {
		result := lhs.value < rhs.value
		return MK_BOOL_VALUE(result)
	} else if operator == "<=" {
		result := lhs.value <= rhs.value
		return MK_BOOL_VALUE(result)
	} else if operator == "==" {
		result := lhs.value == rhs.value
		return MK_BOOL_VALUE(result)
	} else if operator == "!=" {
		result := lhs.value != rhs.value
		return MK_BOOL_VALUE(result)
	} else {
		Error(line, "Unknown operator "+operator)
		os.Exit(1)
		return 1
	}
}

func evaluate_bool_binary_expr(line int, lhs BoolValue, rhs BoolValue, operator string) any {

	if operator == "==" {
		result := lhs.value == rhs.value
		return MK_BOOL_VALUE(result)
	} else if operator == "!=" {
		result := lhs.value != rhs.value
		return MK_BOOL_VALUE(result)
	} else if operator == "and" {
		result := lhs.value && rhs.value
		return MK_BOOL_VALUE(result)
	} else if operator == "or" {
		result := lhs.value || rhs.value
		return MK_BOOL_VALUE(result)
	} else {
		Error(line, "Unknown operator "+operator)
		os.Exit(1)
		return 1
	}
}

func evaluate_string_binary_expr(line int, lhs StringValue, rhs StringValue, operator string) any {

	if operator == "+" {
		result := lhs.value[:len(lhs.value)-1] + rhs.value[1:]
		return MK_STRING_VALUE(result)
	} else if operator == ">" {
		result := lhs.value > rhs.value
		return MK_BOOL_VALUE(result)
	} else if operator == ">=" {
		result := lhs.value >= rhs.value
		return MK_BOOL_VALUE(result)
	} else if operator == "<" {
		result := lhs.value < rhs.value
		return MK_BOOL_VALUE(result)
	} else if operator == "<=" {
		result := lhs.value <= rhs.value
		return MK_BOOL_VALUE(result)
	} else if operator == "==" {
		result := lhs.value == rhs.value
		return MK_BOOL_VALUE(result)
	} else if operator == "!=" {
		result := lhs.value != rhs.value
		return MK_BOOL_VALUE(result)
	} else {
		Error(line, "Unknown operator "+operator)
		os.Exit(1)
		return 1
	}
}

func evaluate_binary_expr(line int, binop BinaryExpr, env Environment) any {
	lhs := evaluate(line, binop.left, env)
	rhs := evaluate(line, binop.right, env)
	if reflect.TypeOf(lhs).Name() == reflect.TypeOf(NumberValue{}).Name() && reflect.TypeOf(rhs).Name() == reflect.TypeOf(NumberValue{}).Name() {
		lhs_typed := lhs.(NumberValue)
		rhs_typed := rhs.(NumberValue)
		return evaluate_numeric_binary_expr(line, lhs_typed, rhs_typed, binop.operator)
	} else if reflect.TypeOf(lhs).Name() == reflect.TypeOf(BoolValue{}).Name() && reflect.TypeOf(rhs).Name() == reflect.TypeOf(BoolValue{}).Name() {
		lhs_typed := lhs.(BoolValue)
		rhs_typed := rhs.(BoolValue)
		return evaluate_bool_binary_expr(line, lhs_typed, rhs_typed, binop.operator)
	} else if reflect.TypeOf(lhs).Name() == reflect.TypeOf(StringValue{}).Name() && reflect.TypeOf(rhs).Name() == reflect.TypeOf(StringValue{}).Name() {
		lhs_typed := lhs.(StringValue)
		rhs_typed := rhs.(StringValue)
		return evaluate_string_binary_expr(line, lhs_typed, rhs_typed, binop.operator)
	} else {
		return MK_NIL_VALUE()
	}
}

func evaluate_unary_expr(line int, unop UnaryExpr, env Environment) any {
	operand := evaluate(line, unop.operand, env)
	if unop.operator == "!" && reflect.TypeOf(operand).Name() == reflect.TypeOf(BoolValue{}).Name() {
		operand_typed := operand.(BoolValue)
		return BoolValue{valueType: operand_typed.valueType, value: !operand_typed.value}
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

func evaluate_if_stmt(line int, ifBlock If, env Environment) any {
	var lastEvaluated any
	lastEvaluated = MK_NIL_VALUE()
	scope := Environment{parent: &env, variables: make(map[string]any, 0), constants: make(map[string]void, 0)}
	ifCondition := evaluate(line, ifBlock.ifCondition, env)

	if reflect.TypeOf(ifCondition).Name() != reflect.TypeOf(BoolValue{}).Name() {
		Error(line, "if condition must evaluate to a bool")
		os.Exit(1)
	}
	ifCondition_typed := ifCondition.(BoolValue)
	if ifCondition_typed.value {
		for i, stmt := range ifBlock.ifBody {
			lastEvaluated = evaluate(line+i, stmt, scope)
		}
	} else if ifBlock.countElif > 0 {
		for i := 0; i < ifBlock.countElif; i++ {
			elifCondition_i := evaluate(line, ifBlock.elifCondition[i], scope)
			if reflect.TypeOf(elifCondition_i).Name() != reflect.TypeOf(BoolValue{}).Name() {
				Error(line, "elif condition must evaluate to a bool")
				os.Exit(1)
			}
			elifCondition_i_typed := elifCondition_i.(BoolValue)
			if elifCondition_i_typed.value {
				for i, stmt := range ifBlock.elifBody[i] {
					lastEvaluated = evaluate(line+i, stmt, scope)
				}
				break
			}
		}
	} else {
		if ifBlock.isElse {
			for i, stmt := range ifBlock.elseBody {
				lastEvaluated = evaluate(line+i, stmt, scope)
			}
		}
	}

	return lastEvaluated
}

func evaluate_loop_stmt(line int, loopBlock Loop, env Environment) any {
	var lastEvaluated any
	lastEvaluated = MK_NIL_VALUE()

out:
	for loopVar := 0; loopVar <= loopBlock.maxLoops; loopVar++ {

		if loopVar == loopBlock.maxLoops {
			Error(line, "Probably in Infinite loop, iteration "+fmt.Sprint(loopVar))
			os.Exit(2)
		}

		scope := Environment{parent: &env, variables: make(map[string]any, 0), constants: make(map[string]void, 0)}
		loopCondition := evaluate(line, loopBlock.loopCondition, env)

		if reflect.TypeOf(loopCondition).Name() != reflect.TypeOf(BoolValue{}).Name() {
			Error(line, "loop condition must evaluate to a bool")
			os.Exit(1)
		}
		loopCondition_typed := loopCondition.(BoolValue)
		if !loopCondition_typed.value {
			break
		}
		for i, stmt := range loopBlock.loopBody {
			lastEvaluated = evaluate(line+i, stmt, scope)
			if reflect.TypeOf(lastEvaluated).Name() == reflect.TypeOf(ContinueValue{}).Name() {
				lastEvaluated = MK_NIL_VALUE()
				break
			} else if reflect.TypeOf(lastEvaluated).Name() == reflect.TypeOf(BreakValue{}).Name() {
				lastEvaluated = MK_NIL_VALUE()
				break out
			}
		}
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

// MF has attained Turing complete :)
// So happy!
// ~ SilverDragonOfR
