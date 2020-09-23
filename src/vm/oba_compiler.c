#include <stdbool.h>
#include <string.h>

#include "oba_compiler.h"
#include "oba_token.h"
#include "oba_vm.h"

// Parsing --------------------------------------------------------------------

typedef struct {
	ObaVM* vm;
	Token current;
} Parser;

// Returns the type of the current token.
static TokenType peek(Parser* parser) {
  return parser->current.type;
}

// Lexes the next token and stores it in [parser.current].
static void nextToken(Parser* parser) {}

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

  }

  return 0;
}

