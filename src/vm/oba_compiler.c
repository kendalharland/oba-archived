#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "oba_compiler.h"
#include "oba_memory.h"
#include "oba_opcodes.h"
#include "oba_token.h"
#include "oba_vm.h"

typedef struct {
  ObaVM* vm;
  Token current;
  Token previous;

  // Whether the parser encountered an error.
  // Code is not executed if this is true.
  bool hasError;

  const char* tokenStart;
  const char* currentChar;
  const char* source;

  int currentLine;
} Parser;

// Forward declarations.
static void ignoreNewlines(Parser*);
static void grouping(Parser*, bool);
static void infixOp(Parser*, bool);
static void literal(Parser*, bool);

// Bytecode -------------------------------------------------------------------

static void emitByte(Parser* parser, int byte) {
  writeChunk(parser->vm->chunk, byte);
}

static void emitOp(Parser* parser, OpCode code) { emitByte(parser, code); }

// Adds [value] the the Vm's constant pool.
// Returns the address of the new constant within the pool.
static int addConstant(Parser* parser, Value value) {
  writeValueArray(&parser->vm->chunk->constants, value);
  return parser->vm->chunk->constants.count - 1;
}

// Registers [value] as a constant value.
//
// Constants are OP_CONSTANT followed by a 16-bit argument which points to
// the constant's location in the constant pool.
static void emitConstant(Parser* parser, Value value) {
  // Register the constant in the VM's constant pool.
  int constant = addConstant(parser, value);
  emitOp(parser, OP_CONSTANT);
  emitByte(parser, constant);
}

// Grammar --------------------------------------------------------------------

// Parse precedence table.
// Greater value == greater precedence.
typedef enum {
  PREC_NONE,
  PREC_LOWEST,
  PREC_SUM,     // + -
  PREC_PRODUCT, // * /
} Precedence;

typedef void (*GrammarFn)(Parser*, bool canAssign);

// Oba grammar rules.
//
// The Pratt parser tutorial at stuffwithstuff describes these as "Parselets".
// The difference between this implementation and parselets is that the prefix
// and infix parselets for the same token are stored on this one struct instead
// in separate tables. This means the same rule implements different operations
// that share the same lexeme.
//
// Additionally, our parser stores tokens internally, so GrammarFn does not
// accept the previous token as an argument, it accesses it using
// parser->previous.
typedef struct {
  GrammarFn prefix;
  GrammarFn infix;
  Precedence precedence;
  const char* name;
} GrammarRule;

// clang-format off

// Pratt parser rules.
//
// See: http://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/
#define UNUSED                     { NULL, NULL, PREC_NONE, NULL }
#define PREFIX(fn)                 { fn, NULL, PREC_NONE, NULL }
#define INFIX_OPERATOR(prec, name) { NULL, infixOp, prec, name }

GrammarRule rules[] =  {
  /* TOK_LPAREN    */ PREFIX(grouping),  
  /* TOK_RPAREN    */ UNUSED, 
  /* TOK_PLUS      */ INFIX_OPERATOR(PREC_SUM, "+"),
  /* TOK_MINUS     */ INFIX_OPERATOR(PREC_SUM, "-"),
  /* TOK_MULTIPLY  */ INFIX_OPERATOR(PREC_PRODUCT, "*"),
  /* TOK_DIVIDE    */ INFIX_OPERATOR(PREC_PRODUCT, "/"),
  /* TOK_IDENT     */ UNUSED, // TODO(kendal): This is not correct.
  /* TOK_NUMBER    */ PREFIX(literal),
  /* TOK_STRING    */ UNUSED, // TODO(kendal): This is not correct.
  /* TOK_NEWLINE   */ UNUSED, 
  /* TOK_ERROR     */ UNUSED,  
  /* TOK_EOF       */ UNUSED,
};

// Gets the [GrammarRule] associated with tokens of [type].
static GrammarRule* getRule(TokenType type) {
  return &rules[type];
}

// clang-format on

// Lexing ---------------------------------------------------------------------

static void printError(Parser* parser, int line, const char* label,
                       const char* format, va_list args) {
  char message[1024];
  int length = sprintf(message, "%s: ", label);
  length += vsprintf(message + length, format, args);
  // TODO(kendal): Ensure length < 1024
  printf("%s\n", message);
}

static void lexError(Parser* parser, const char* format, ...) {
  parser->hasError = true;

  va_list args;
  va_start(args, format);
  printError(parser, parser->currentLine, "Error", format, args);
  va_end(args);
}

static void error(Parser* parser, const char* format, ...) {
  parser->hasError = true;

  // The lexer already reported this error.
  if (parser->previous.type == TOK_ERROR)
    return;

  va_list args;
  va_start(args, format);
  printError(parser, parser->currentLine, "Error", format, args);
  va_end(args);
}

// Parsing --------------------------------------------------------------------

static char peekChar(Parser* parser) { return *parser->currentChar; }

static char nextChar(Parser* parser) {
  char c = peekChar(parser);
  parser->currentChar++;
  if (c == '\n')
    parser->currentLine++;
  return c;
}

static bool matchChar(Parser* parser, char c) {
  if (peekChar(parser) != c)
    return false;
  nextChar(parser);
  return true;
}

// Returns the type of the current token.
static TokenType peek(Parser* parser) { return parser->current.type; }

static void makeToken(Parser* parser, TokenType type) {
  parser->current.type = type;
  parser->current.start = parser->tokenStart;
  parser->current.length = (int)(parser->currentChar - parser->tokenStart);
  parser->current.line = parser->currentLine;

  // Make line tokens appear on the line containing the "\n".
  if (type == TOK_NEWLINE)
    parser->current.line--;
}

static void makeNumber(Parser* parser) {
  parser->current.value = strtod(parser->tokenStart, NULL);
  makeToken(parser, TOK_NUMBER);
}

static bool isIdent(char c) { return isalpha(c) || c == '_'; }

static bool isNumber(char c) { return isdigit(c); }

// Finishes lexing an identifier.
static void readIdent(Parser* parser) {
  while (isIdent(peekChar(parser)) || isdigit(peekChar(parser))) {
    nextChar(parser);
  }
  makeToken(parser, TOK_IDENT);
  // TODO(kendal): Handle keywords.
}

static void readNumber(Parser* parser) {
  while (isNumber(peekChar(parser))) {
    nextChar(parser);
  }
  makeNumber(parser);
}

static void skipLineComment(Parser* parser) {
  // A comment goes until the end of the line.
  while (peekChar(parser) != '\n' && peekChar(parser) != '\0') {
    nextChar(parser);
  }
}

// Lexes the next token and stores it in [parser.current].
static void nextToken(Parser* parser) {
  parser->previous = parser->current;

  if (parser->current.type == TOK_EOF)
    return;

  while (peekChar(parser) != '\0') {
    parser->tokenStart = parser->currentChar;
    char c = nextChar(parser);
    switch (c) {
    case ' ':
    case '\r':
    case '\t':
      break;
    case '\n':
      makeToken(parser, TOK_NEWLINE);
      return;
    case '(':
      makeToken(parser, TOK_LPAREN);
      return;
    case ')':
      makeToken(parser, TOK_RPAREN);
      return;
    case '+':
      makeToken(parser, TOK_PLUS);
      return;
    case '-':
      makeToken(parser, TOK_MINUS);
      return;
    case '*':
      makeToken(parser, TOK_MULTIPLY);
      return;
    case '/':
      if (matchChar(parser, '/')) {
        skipLineComment(parser);
        break;
      }

      makeToken(parser, TOK_DIVIDE);
      return;
    default:
      if (isIdent(c)) {
        readIdent(parser);
        return;
      }
      if (isNumber(c)) {
        readNumber(parser);
        return;
      }
      lexError(parser, "Invalid character '%c'.", c);
      parser->current.type = TOK_ERROR;
      parser->current.length = 0;
      return;
    }
  }

  // No more source left.
  parser->tokenStart = parser->currentChar;
  makeToken(parser, TOK_EOF);
}

// Returns true iff the next token has the [expected] Type.
static bool match(Parser* parser, TokenType expected) {
  if (peek(parser) != expected)
    return false;
  nextToken(parser);
  return true;
}

static bool matchLine(Parser* parser) {
  if (!match(parser, TOK_NEWLINE))
    return false;
  while (match(parser, TOK_NEWLINE))
    ;
  return true;
}

static void ignoreNewlines(Parser* parser) { matchLine(parser); }

// Moves past the next token which must have the [expected] type.
// If the type is not as expected, this emits an error and attempts to continue
// parsing at the next token.
static void consume(Parser* parser, TokenType expected,
                    const char* errorMessage) {
  nextToken(parser);
  if (parser->previous.type != expected) {
    error(parser, errorMessage);
    if (parser->current.type == expected) {
      nextToken(parser);
    }
  }
}

// AST ------------------------------------------------------------------------

static void parse(Parser* parser, int precedence) {
  nextToken(parser);
  Token token = parser->previous;

  GrammarFn prefix = rules[token.type].prefix;
  if (prefix == NULL) {
    error(parser, "Parse error");
    return;
  }

  bool canAssign = false;
  prefix(parser, canAssign);

  while (precedence < rules[parser->current.type].precedence) {
    nextToken(parser);
    GrammarFn infix = rules[parser->previous.type].infix;
    infix(parser, canAssign);
  }
}

static void expression(Parser* parser) { parse(parser, PREC_LOWEST); }

// A parenthesized expression.
static void grouping(Parser* parser, bool canAssign) {
  expression(parser);
  consume(parser, TOK_RPAREN, "Expected ')' after expression.");
}

static void literal(Parser* parser, bool canAssign) {
  emitConstant(parser, parser->previous.value);
}

static void infixOp(Parser* parser, bool canAssign) {
  GrammarRule* rule = getRule(parser->previous.type);
  ignoreNewlines(parser);

  // Compile the right hand side. precedence + 1 makes this left-associative.
  parse(parser, rule->precedence);
  if (strcmp(rule->name, "+") == 0)
    emitOp(parser, OP_ADD);
  else if (strcmp(rule->name, "-") == 0)
    emitOp(parser, OP_MINUS);
  else if (strcmp(rule->name, "*") == 0)
    emitOp(parser, OP_MULTIPLY);
  else if (strcmp(rule->name, "/") == 0)
    emitOp(parser, OP_DIVIDE);
  else
    error(parser, "Invalid operator %s", rule->name);
}

// Compiling ------------------------------------------------------------------

struct sCompiler {
  Parser* parser;
};

void initCompiler(Compiler* compiler, Parser* parser) {
  compiler->parser = parser;
  compiler->parser->vm->compiler = compiler;

  compiler->parser->vm->chunk = (Chunk*)reallocate(NULL, 0, sizeof(Chunk));
  initChunk(compiler->parser->vm->chunk);
  compiler->parser->vm->ip = compiler->parser->vm->chunk->code;
}

// TODO(kendal): Fix the type instead of using 'int'.
int obaCompile(ObaVM* vm, const char* source) {
  // Skip the UTF-8 BOM if there is one.
  if (strncmp(source, "\xEF\xBB\xBF", 3) == 0)
    source += 3;

  Parser parser;
  parser.vm = vm;
  parser.source = source;
  parser.tokenStart = source;
  parser.currentChar = source;
  parser.currentLine = 1;
  parser.current.type = TOK_ERROR;
  parser.current.start = source;
  parser.current.length = 0;
  parser.current.line = 0;
  parser.current.value = 0;
  parser.hasError = false;

  nextToken(&parser);

  Compiler compiler;
  initCompiler(&compiler, &parser);
  ignoreNewlines(&parser);

  while (!match(compiler.parser, TOK_EOF)) {
    expression(compiler.parser);
    // If no newline, the file must end on this line.
    if (!matchLine(compiler.parser)) {
      consume(compiler.parser, TOK_EOF, "Expected end of file.");
      break;
    }
  }

  emitOp(&parser, OP_EXIT);
  // TODO(kendal): Emit instr signalling the end of input source.
  return 0;
}
