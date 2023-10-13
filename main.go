package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"

	"github.com/fatih/color"
)

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

// global check variable
var hasError = false

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

	if len(os.Args) > 2 {
		fmt.Println()
		fmt.Println(color.YellowString("USAGE : lexer [script]"))
		fmt.Println()
		os.Exit(64)
	} else if len(os.Args) == 2 {
		runFile(os.Args[1])
	} else {
		runPrompt()
	}

}

// error handling
func error(line int, message string) {
	report(line, "", message)
}

func report(line int, where string, message string) {
	fmt.Println(color.RedString("ERROR : [ Line"), color.RedString(strconv.Itoa(line)), color.RedString("]"))
	fmt.Println(color.RedString("      :"), color.RedString(where), color.RedString("->"), color.RedString(message))
	hasError = true
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
		error(sc.line, "Unterminated string")
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
			error(sc.line, "Unexpected character "+string(c))
		}
	}
}

// run functions
func run(source string) {

	sc := Scanner{source, []Token{}, 0, 0, 1}
	var tokens []Token = sc.scanTokens()

	if !hasError {
		for _, token := range tokens {
			fmt.Println(token)
		}
	}
}

func runFile(path string) {
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
	run(string(data))
	if hasError {
		fmt.Println()
		os.Exit(1)
	}
	fmt.Println()
	fmt.Println(color.YellowString("Thanks for using Source - by Pratik"))
	fmt.Println()
}

func runPrompt() {

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
		run(line)
		hasError = false
	}

}
