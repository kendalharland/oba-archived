#include <stdbool.h>
#include <stdio.h>
#include <string.h>

#include "oba_compiler.h"
#include "oba_token.h"
#include "oba_vm.h"

// Parsing --------------------------------------------------------------------

typedef struct {
	ObaVM* vm;
	Token current;
	Token previous;
	
	const char* tokenStart;
	const char* currentChar;

	int currentLine;
} Parser;

// Returns the type of the current token.
static TokenType peek(Parser* parser) {
  return parser->current.type;
}

static char peekChar(Parser* parser) {
	return *parser->currentChar;
}

static char nextChar(Parser* parser) {
	char c = peekChar(parser);
	parser->currentChar++;
	if (c == '\n') parser->currentLine++;
	return c;
}

// Lexes the next token and stores it in [parser.current].
static void nextToken(Parser* parser) {
	parser->previous = parser->current;

	if (parser->current.type == TOK_EOF) return;

	// TODO(kendal): Delete and actually update the token on parser.
	Token token;
	parser->current = token;

	while(peekChar(parser) != '\0') {
		parser->tokenStart = parser->currentChar;
		char c = nextChar(parser);
		switch(c) {
			case '(':
				printf("got (");
				return;
			case ')':
				printf("got )");
				return;
			case '\n':
				printf("got newline");
				return;
			default:
				printf("got unkown char");
				return;
		}
	}
}

static bool match(Parser* parser, TokenType expected) {
  if (peek(parser) != expected) return false;
  nextToken(parser);
  return true;
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
int obaCompile(ObaVM* vm, const char *source) {
   // Skip the UTF-8 BOM if there is one.
  if (strncmp(source, "\xEF\xBB\xBF", 3) == 0) source += 3;
	
  Parser parser;
  Compiler compiler;
  initCompiler(&compiler, &parser);

  while (!match(compiler.parser, TOK_EOF)) {
	nextToken(compiler.parser);
  }

  return 0;
}

