#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "oba_compiler.h"
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

// Grammar --------------------------------------------------------------------

typedef enum {
  PREC_NONE,
  PREC_LOWEST,
  PREC_TERM, // + -
} Precedence;

// TODO(kendal): Replace this with the appropriate type.
typedef void Signature;

typedef void (*GrammarFn)(Parser*, bool canAssign);
typedef void (*SignatureFn)(Parser* parser, Signature* signature);

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
  SignatureFn method;
  Precedence precedence;
  const char* name;
} GrammarRule;

// clang-format off

// Pratt parser rules.
//
// See: http://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/
#define UNUSED                     { NULL, NULL, NULL, PREC_NONE, NULL }
#define PREFIX(fn)                 { fn, NULL, NULL, PREC_NONE, NULL }
#define INFIX_OPERATOR(prec, name) { NULL, infixOp, infixSignature, prec, name }
#define OPERATOR(name)             { unaryOp, infixOp, mixedSignature, PREC_TERM, name }

// Forward declarations.
static void grouping(Parser*, bool);
static void literal(Parser*, bool);

GrammarRule rules[] =  {
  /* TOK_LPAREN  */ PREFIX(grouping),  
  /* TOK_RPAREN  */ UNUSED, 
  /* TOK_IDENT   */ UNUSED, // TODO(kendal): This is not correct.
  /* TOK_NUMBER  */ PREFIX(literal),
  /* TOK_STRING  */ UNUSED, // TODO(kendal): This is not correct.
  /* TOK_NEWLINE */ UNUSED, 
  /* TOK_ERROR   */ UNUSED,  
  /* TOK_EOF     */ UNUSED,
};

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
    ;
  return;

  va_list args;
  va_start(args, format);
  printError(parser, parser->currentLine, "Error", format, args);
  va_end(args);
}

static void printTokenType(TokenType type) {
  switch (type) {
  case TOK_LPAREN:
    printf("TOK_LPAREN");
    break;
  case TOK_RPAREN:
    printf("TOK_RPAREN");
    break;
  case TOK_EOF:
    printf("TOK_EOF");
    break;
  case TOK_NEWLINE:
    printf("TOK_NEWLINE");
    break;
  default:
    printf("TOK_ERROR");
  }
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

static bool isIdent(char c) { return isalpha(c) || c == '_'; }

// Finishes lexing an identifier.
static void readIdent(Parser* parser) {
  while (isIdent(peekChar(parser)) || isdigit(peekChar(parser))) {
    nextChar(parser);
  }
  makeToken(parser, TOK_IDENT);
  // TODO(kendal): Handle keywords.
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
    case '(':
      makeToken(parser, TOK_LPAREN);
      return;
    case ')':
      makeToken(parser, TOK_RPAREN);
      return;
    case '\n':
      makeToken(parser, TOK_NEWLINE);
      return;
    default:
      if (isIdent(c)) {
        readIdent(parser);
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
  Token token = parser->current;

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
  perror("unimplemented: literal");
  exit(1);
}

// Compiling ------------------------------------------------------------------

struct sCompiler {
  Parser* parser;
};

void initCompiler(Compiler* compiler, Parser* parser) {
  compiler->parser = parser;
  parser->vm->compiler = compiler;
}

// TODO(kjharland): Fix the type instead of using 'int'.
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
  // TODO(kendal): parser.current.value
  parser.hasError = false;

  // Read the first token. (Why?)
  nextToken(&parser);

  Compiler compiler;
  initCompiler(&compiler, &parser);

  while (!match(compiler.parser, TOK_EOF)) {
    printTokenType(compiler.parser->current.type);
    expression(compiler.parser);
    printf("\n");
  }

  return 0;
}
