#ifndef oba_token_h
#define oba_token_h

#include "oba_value.h"

// Oba Tokens.
//
// WARNING: When updating this table, also update the table of GrammarRules in
// oba_compiler.c.
typedef enum {
  TOK_LPAREN,
  TOK_RPAREN,
  TOK_PLUS,

  TOK_IDENT,
  TOK_NUMBER,

  // A String literal.
  TOK_STRING,
  TOK_NEWLINE,

  TOK_ERROR,
  TOK_EOF,
} TokenType;

typedef struct {
  TokenType type;

  // The beginning of the token, pointing directly at the source.
  const char* start;

  // The number of characters in the token.
  int length;

  // The 1-based line where the token appears.
  int line;

  // The parsed value if the token is a literal.
  // TODO(kendal): Define this instead of using 'int'.
  Value value;
} Token;

#endif
