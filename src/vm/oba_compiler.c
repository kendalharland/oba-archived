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

typedef struct {
  Token token;
  int depth;
} Local;

struct sCompiler {
  Local locals[MAX_LOCALS];
  int localCount;
  int currentDepth;
  Parser* parser;
};

void initCompiler(Compiler* compiler, Parser* parser) {
  compiler->parser = parser;
  compiler->parser->vm->compiler = compiler;
  compiler->localCount = 0;
  compiler->currentDepth = 0;

  compiler->parser->vm->chunk = (Chunk*)reallocate(NULL, 0, sizeof(Chunk));
  initChunk(compiler->parser->vm->chunk);
  compiler->parser->vm->ip = compiler->parser->vm->chunk->code;
}

static void printError(Compiler* compiler, int line, const char* label,
                       const char* format, va_list args) {
  char message[1024];
  int length = sprintf(message, "%s: ", label);
  length += vsprintf(message + length, format, args);
  // TODO(kendal): Ensure length < 1024
  printf("%s\n", message);
}

static void lexError(Compiler* compiler, const char* format, ...) {
  compiler->parser->hasError = true;

  va_list args;
  va_start(args, format);
  printError(compiler, compiler->parser->currentLine, "Error", format, args);
  va_end(args);
}

static void error(Compiler* compiler, const char* format, ...) {
  compiler->parser->hasError = true;

  // The lexer already reported this error.
  if (compiler->parser->previous.type == TOK_ERROR)
    return;

  va_list args;
  va_start(args, format);
  printError(compiler, compiler->parser->currentLine, "Error", format, args);
  va_end(args);
}
// Forward declarations because the grammar is recursive.
static void ignoreNewlines(Compiler*);
static void statement(Compiler*);

static void grouping(Compiler*, bool);
static void unaryOp(Compiler*, bool);
static void infixOp(Compiler*, bool);
static void identifier(Compiler*, bool);
static void literal(Compiler*, bool);
static void string(Compiler*, bool);
static void matchExpr(Compiler*, bool);

static void declaration(Compiler*);

// Bytecode -------------------------------------------------------------------

static void emitByte(Compiler* compiler, int byte) {
  writeChunk(compiler->parser->vm->chunk, byte);
}

static void emitOp(Compiler* compiler, OpCode code) {
  emitByte(compiler, code);
}

// Adds [value] the the Vm's constant pool.
// Returns the address of the new constant within the pool.
static int addConstant(Compiler* compiler, Value value) {
  writeValueArray(&compiler->parser->vm->chunk->constants, value);
  return compiler->parser->vm->chunk->constants.count - 1;
}

// Registers [value] as a constant value.
//
// Constants are OP_CONSTANT followed by a 16-bit argument which points to
// the constant's location in the constant pool.
static void emitConstant(Compiler* compiler, Value value) {
  // Register the constant in the VM's constant pool.
  int constant = addConstant(compiler, value);
  emitOp(compiler, OP_CONSTANT);
  emitByte(compiler, constant);
}

static void emitBool(Compiler* compiler, Value value) {
  AS_BOOL(value) ? emitOp(compiler, OP_TRUE) : emitOp(compiler, OP_FALSE);
}

static void patchJump(Compiler* compiler, int offset) {
  Chunk* chunk = compiler->parser->vm->chunk;

  // -2 to account for the placeholder bytes.
  int jump = chunk->count - offset - 2;
  if (jump > UINT16_MAX) {
    error(compiler, "Too much code to jump over");
    return;
  }

  chunk->code[offset] = (jump >> 8) & 0xff;
  chunk->code[offset + 1] = jump & 0xff;
}

static int emitJump(Compiler* compiler, OpCode op) {
  emitOp(compiler, op);
  emitByte(compiler, 0xff);
  emitByte(compiler, 0xff);
  return compiler->parser->vm->chunk->count - 2;
}

static void emitLoop(Compiler* compiler, int start) {
  emitOp(compiler, OP_LOOP);

  int jump = compiler->parser->vm->chunk->count - start - 2;
  if (jump > UINT16_MAX) {
    error(compiler, "Loop body too large");
    return;
  }

  // Store the exact index of the loop start instruction as the operand.
  emitByte(compiler, (start >> 8) & 0xff);
  emitByte(compiler, start & 0xff);
}

static int declareGlobal(Compiler* compiler, Value name) {
  return addConstant(compiler, name);
}

static void defineGlobal(Compiler* compiler, int global) {
  emitOp(compiler, OP_DEFINE_GLOBAL);
  emitByte(compiler, global);
}

static void setLocal(Compiler* compiler, int slot) {
  // The local's value is already at the top of the stack.
  emitOp(compiler, OP_SET_LOCAL);
  emitByte(compiler, slot);
}

// Declares a new local in an uninitialized state.
// Any attempt to use the local before it is initialized is an error.
static void addLocal(Compiler* compiler, Token name) {
  Local* local = &compiler->locals[compiler->localCount++];
  local->token = name;
  local->depth = -1;
}

static void defineLocal(Compiler* compiler) {
  // We cannot have declared any new locals before defining this one, because
  // assignments do not nest inside expressions. The local we're defining is
  // the most recent one.
  Local* local = &compiler->locals[compiler->localCount - 1];
  local->depth = compiler->currentDepth;

  setLocal(compiler, compiler->localCount - 1);
}

static bool identifiersMatch(Token a, Token b) {
  return a.length == b.length && memcmp(a.start, b.start, a.length) == 0;
}

static int declareVariable(Compiler* compiler, Token name) {
  if (compiler->currentDepth == 0) {
    return declareGlobal(compiler,
                         OBJ_VAL(copyString(name.start, name.length)));
  }

  int slot = compiler->localCount;

  // Ensure the variable is is not already declared in this scope.
  for (int slot = compiler->localCount - 1; slot >= 0; slot--) {
    Local local = compiler->locals[slot];
    if (local.depth < compiler->currentDepth) {
      break;
    }
    if (identifiersMatch(name, local.token)) {
      error(compiler, "Variable with this name already declared in this scope");
      return 0;
    }
  }

  addLocal(compiler, name);
  return 0;
}

static void defineVariable(Compiler* compiler, int variable) {
  // Local variables live on the stack, so we don't need to define anything.
  if (compiler->currentDepth > 0) {
    defineLocal(compiler);
    return;
  }

  defineGlobal(compiler, variable);
}

// TODO(kendal): Handle the case where an existing variable is being re-set to
// some value, rather than declared or defined for the first time.

static void getGlobal(Compiler* compiler, Value name) {
  int global = addConstant(compiler, name);
  emitOp(compiler, OP_GET_GLOBAL);
  emitByte(compiler, global);
}

static void getLocal(Compiler* compiler, int slot) {
  Local local = compiler->locals[slot];
  if (local.depth < 0) {
    error(compiler, "Cannot read local variable in its own initializer");
    return;
  }
  emitOp(compiler, OP_GET_LOCAL);
  emitByte(compiler, (uint8_t)slot);
}

// Finds a local variabled named [name] in the current scope.
// Returns a negative number if it is not found.
static int lookupLocal(Compiler* compiler, Token name) {
  // Find the first local whose depth is gte the current scope and whose
  // token matches `name`.
  for (int i = compiler->localCount - 1; i >= 0; i--) {
    Local local = compiler->locals[i];
    if (local.token.length == name.length &&
        identifiersMatch(local.token, name)) {
      return i;
    }
  }
  return -1;
}

static void getVariable(Compiler* compiler) {
  Token name = compiler->parser->previous;
  int local = lookupLocal(compiler, name);
  if (local >= 0) {
    getLocal(compiler, local);
  } else {
    Value value = OBJ_VAL(copyString(name.start, name.length));
    getGlobal(compiler, value);
  }
}

static void setVariable(Compiler* compiler, Token name) {
  int local = lookupLocal(compiler, name);
  if (local >= 0) {
    setLocal(compiler, local);
    // The local already has a slot, so we don't need to keep the new value on
    // the stack.
    emitOp(compiler, OP_POP);
    return;
  }
  // Globals are constant.
  // TODO(kendal): Lookup the global so that we can print whether it's defined
  // at all.
  error(compiler, "Cannot reassign global variable");
}

// Grammar --------------------------------------------------------------------

// Parse precedence table.
// Greater value == greater precedence.
typedef enum {
  PREC_NONE,
  PREC_LOWEST,
  PREC_ASSIGN,  // =
  PREC_COND,    // < > <= >= != ==
  PREC_SUM,     // + -
  PREC_PRODUCT, // * /
} Precedence;

typedef void (*GrammarFn)(Compiler*, bool canAssign);

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
  /* TOK_NOT       */ PREFIX(unaryOp),
  /* TOK_ASSIGN    */ INFIX_OPERATOR(PREC_ASSIGN, "="),
  /* TOK_GT        */ INFIX_OPERATOR(PREC_COND, ">"),
  /* TOK_LT        */ INFIX_OPERATOR(PREC_COND, "<"),
  /* TOK_GTE       */ INFIX_OPERATOR(PREC_COND, ">="),
  /* TOK_LTE       */ INFIX_OPERATOR(PREC_COND, "<="),
  /* TOK_EQ        */ INFIX_OPERATOR(PREC_COND, "=="),
  /* TOK_NEQ       */ INFIX_OPERATOR(PREC_COND, "!="),
  /* TOK_GUARD     */ UNUSED,
  /* TOK_LPAREN    */ PREFIX(grouping),  
  /* TOK_RPAREN    */ UNUSED, 
  /* TOK_LBRACK    */ UNUSED,  
  /* TOK_RBRACK    */ UNUSED, 
  /* TOK_PLUS      */ INFIX_OPERATOR(PREC_SUM, "+"),
  /* TOK_MINUS     */ INFIX_OPERATOR(PREC_SUM, "-"),
  /* TOK_MULTIPLY  */ INFIX_OPERATOR(PREC_PRODUCT, "*"),
  /* TOK_DIVIDE    */ INFIX_OPERATOR(PREC_PRODUCT, "/"),
  /* TOK_IDENT     */ PREFIX(identifier),
  /* TOK_NUMBER    */ PREFIX(literal),
  /* TOK_STRING    */ PREFIX(string),
  /* TOK_NEWLINE   */ UNUSED, 
  /* TOK_DEBUG     */ UNUSED,
  /* TOK_LET       */ UNUSED,
  /* TOK_TRUE      */ PREFIX(literal),
  /* TOK_FALSE     */ PREFIX(literal),
  /* TOK_IF        */ UNUSED,  
  /* TOK_ELSE      */ UNUSED,  
  /* TOK_WHILE     */ UNUSED,  
  /* TOK_MATCH     */ PREFIX(matchExpr),
  /* TOK_ERROR     */ UNUSED,  
  /* TOK_EOF       */ UNUSED,
};

// Gets the [GrammarRule] associated with tokens of [type].
static GrammarRule* getRule(TokenType type) {
  return &rules[type];
}

typedef struct {
  const char* lexeme;
  size_t length;
  TokenType type;
} Keyword;

static Keyword keywords[] = {
    {"debug", 5, TOK_DEBUG},
    {"false", 5, TOK_FALSE},
    {"let",   3, TOK_LET},
    {"true",  4, TOK_TRUE},
    {"if",    2, TOK_IF},
    {"else",  4, TOK_ELSE},
    {"while", 5, TOK_WHILE},
    {"match", 5, TOK_MATCH},
    {NULL,    0, TOK_EOF}, // Sentinel to mark the end of the array.
};

// clang-format on

// Lexing ---------------------------------------------------------------------

// Parsing --------------------------------------------------------------------

static char peekChar(Compiler* compiler) {
  return *compiler->parser->currentChar;
}

static char nextChar(Compiler* compiler) {
  char c = peekChar(compiler);
  compiler->parser->currentChar++;
  if (c == '\n')
    compiler->parser->currentLine++;
  return c;
}

static bool matchChar(Compiler* compiler, char c) {
  if (peekChar(compiler) != c)
    return false;
  nextChar(compiler);
  return true;
}

// Returns the type of the current token.
static TokenType peek(Compiler* compiler) {
  return compiler->parser->current.type;
}

static void makeToken(Compiler* compiler, TokenType type) {
  compiler->parser->current.type = type;
  compiler->parser->current.start = compiler->parser->tokenStart;
  compiler->parser->current.length =
      (int)(compiler->parser->currentChar - compiler->parser->tokenStart);
  compiler->parser->current.line = compiler->parser->currentLine;

  // Make line tokens appear on the line containing the "\n".
  if (type == TOK_NEWLINE)
    compiler->parser->current.line--;
}

static void makeNumber(Compiler* compiler) {
  double value = strtod(compiler->parser->tokenStart, NULL);
  compiler->parser->current.value = OBA_NUMBER(value);
  makeToken(compiler, TOK_NUMBER);
}

static void makeString(Compiler* compiler) { makeToken(compiler, TOK_STRING); }

static bool isName(char c) { return isalpha(c) || c == '_'; }

static bool isNumber(char c) { return isdigit(c); }

// Finishes lexing a string.
static void readString(Compiler* compiler) {
  // TODO(kendal): Handle strings with escaped quotes.
  while (peekChar(compiler) != '"') {
    nextChar(compiler);
  }
  nextChar(compiler);
  makeString(compiler);
}

// Finishes lexing an identifier.
static void readName(Compiler* compiler) {
  while (isName(peekChar(compiler)) || isdigit(peekChar(compiler))) {
    nextChar(compiler);
  }

  size_t length = compiler->parser->currentChar - compiler->parser->tokenStart;
  for (int i = 0; keywords[i].lexeme != NULL; i++) {
    if (length == keywords[i].length &&
        memcmp(compiler->parser->tokenStart, keywords[i].lexeme, length) == 0) {
      makeToken(compiler, keywords[i].type);
      return;
    }
  }
  makeToken(compiler, TOK_IDENT);
}

static void readNumber(Compiler* compiler) {
  while (isNumber(peekChar(compiler))) {
    nextChar(compiler);
  }
  makeNumber(compiler);
}

static void skipLineComment(Compiler* compiler) {
  // A comment goes until the end of the line.
  while (peekChar(compiler) != '\n' && peekChar(compiler) != '\0') {
    nextChar(compiler);
  }
}

// Lexes the next token and stores it in [parser.current].
static void nextToken(Compiler* compiler) {
  compiler->parser->previous = compiler->parser->current;

  if (compiler->parser->current.type == TOK_EOF)
    return;

#define IF_MATCH_NEXT(next, matched, unmatched)                                \
  do {                                                                         \
    if (matchChar(compiler, next))                                             \
      makeToken(compiler, matched);                                            \
    else                                                                       \
      makeToken(compiler, unmatched);                                          \
  } while (0)

  while (peekChar(compiler) != '\0') {
    compiler->parser->tokenStart = compiler->parser->currentChar;
    char c = nextChar(compiler);
    switch (c) {
    case ' ':
    case '\r':
    case '\t':
      break;
    case '\n':
      makeToken(compiler, TOK_NEWLINE);
      return;
    case '|':
      makeToken(compiler, TOK_GUARD);
      return;
    case '(':
      makeToken(compiler, TOK_LPAREN);
      return;
    case ')':
      makeToken(compiler, TOK_RPAREN);
      return;
    case '{':
      makeToken(compiler, TOK_LBRACK);
      return;
    case '}':
      makeToken(compiler, TOK_RBRACK);
      return;
    case '+':
      makeToken(compiler, TOK_PLUS);
      return;
    case '-':
      makeToken(compiler, TOK_MINUS);
      return;
    case '*':
      makeToken(compiler, TOK_MULTIPLY);
      return;
    case '!':
      IF_MATCH_NEXT('=', TOK_NEQ, TOK_NOT);
      return;
    case '>':
      IF_MATCH_NEXT('=', TOK_GTE, TOK_GT);
      return;
    case '<':
      IF_MATCH_NEXT('=', TOK_LTE, TOK_LT);
      return;
    case '=':
      IF_MATCH_NEXT('=', TOK_EQ, TOK_ASSIGN);
      return;
    case '/':
      if (matchChar(compiler, '/')) {
        skipLineComment(compiler);
        break;
      }

      makeToken(compiler, TOK_DIVIDE);
      return;
    case '"':
      readString(compiler);
      return;
    default:
      if (isName(c)) {
        readName(compiler);
        return;
      }
      if (isNumber(c)) {
        readNumber(compiler);
        return;
      }
      lexError(compiler, "Invalid character '%c'.", c);
      compiler->parser->current.type = TOK_ERROR;
      compiler->parser->current.length = 0;
      return;
    }
  }

#undef IF_MATCH_NEXT

  // No more source left.
  compiler->parser->tokenStart = compiler->parser->currentChar;
  makeToken(compiler, TOK_EOF);
}

// Returns true iff the next token has the [expected] Type.
static bool match(Compiler* compiler, TokenType expected) {
  if (peek(compiler) != expected)
    return false;
  nextToken(compiler);
  return true;
}

static bool matchLine(Compiler* compiler) {
  if (!match(compiler, TOK_NEWLINE))
    return false;
  while (match(compiler, TOK_NEWLINE))
    ;
  return true;
}

static void ignoreNewlines(Compiler* compiler) { matchLine(compiler); }

// Moves past the next token which must have the [expected] type.
// If the type is not as expected, this emits an error and attempts to continue
// parsing at the next token.
static void consume(Compiler* compiler, TokenType expected,
                    const char* errorMessage) {
  nextToken(compiler);
  if (compiler->parser->previous.type != expected) {
    error(compiler, errorMessage);
    if (compiler->parser->current.type == expected) {
      nextToken(compiler);
    }
  }
}

// AST ------------------------------------------------------------------------

static void parse(Compiler* compiler, int precedence) {
  nextToken(compiler);
  Token token = compiler->parser->previous;

  GrammarFn prefix = rules[token.type].prefix;
  if (prefix == NULL) {
    error(compiler, "Parse error %d", token.type);
    return;
  }

  bool canAssign = precedence < PREC_COND;
  prefix(compiler, canAssign);

  while (precedence < rules[compiler->parser->current.type].precedence) {
    nextToken(compiler);
    GrammarFn infix = rules[compiler->parser->previous.type].infix;
    infix(compiler, canAssign);
  }
}

static void expression(Compiler* compiler) { parse(compiler, PREC_LOWEST); }

static void assignStmt(Compiler* compiler) {
  consume(compiler, TOK_IDENT, "Expected an identifier.");
  // Get the name, but don't declare it yet; A variable should not be in scope
  // in its own initializer.
  Token name = compiler->parser->previous;
  int variable = declareVariable(compiler, name);

  // Compile the initializer.
  consume(compiler, TOK_ASSIGN, "Expected '='");
  expression(compiler);

  // Now define the variable.
  defineVariable(compiler, variable);
}

static void debugStmt(Compiler* compiler) {
  expression(compiler);
  emitOp(compiler, OP_DEBUG);
}

static void enterScope(Compiler* compiler) { compiler->currentDepth++; }

static void exitScope(Compiler* compiler) {
  compiler->currentDepth--;
  for (int i = compiler->localCount - 1; i >= 0; i--) {
    Local local = compiler->locals[i];
    if (local.depth > compiler->currentDepth) {
      compiler->localCount--;
      emitOp(compiler, OP_POP);
    } else {
      break;
    }
  }
}

static void blockStmt(Compiler* compiler) {
  enterScope(compiler);

  ignoreNewlines(compiler);

  do {
    declaration(compiler);
    ignoreNewlines(compiler);
  } while (peek(compiler) != TOK_RBRACK && peek(compiler) != TOK_EOF);

  ignoreNewlines(compiler);
  consume(compiler, TOK_RBRACK, "Expected '}' at the end of block");
  exitScope(compiler);
}

static void ifStmt(Compiler* compiler) {
  // Compile the conditional.
  expression(compiler);

  // Emit the jump instruction.
  // When the VM reaches this, the value of the conditional is on the top of the
  // stack, and it will jump based on that value's truthiness.
  int offset = emitJump(compiler, OP_JUMP_IF_FALSE);
  // Compile the "then" branch.
  statement(compiler);
  patchJump(compiler, offset);

  // Compile the "else" branch.
  if (match(compiler, TOK_ELSE)) {
    // At this point the value of the conditional is still on the stack. Skip
    // the else clause if it was truthy.
    offset = emitJump(compiler, OP_JUMP_IF_TRUE);
    statement(compiler);
    patchJump(compiler, offset);
  }

  // Don't forget to pop the conditional
  emitOp(compiler, OP_POP);
}

static void whileStmt(Compiler* compiler) {
  int loopStart = compiler->parser->vm->chunk->count;

  // Compile the conditional.
  expression(compiler);

  int offset = emitJump(compiler, OP_JUMP_IF_FALSE);
  statement(compiler);

  // Pop the conditional before looping, since the value is recompiled each time
  // based on the new stack contents.
  emitOp(compiler, OP_POP);
  emitLoop(compiler, loopStart);
  patchJump(compiler, offset);
}

static void statement(Compiler* compiler) {
  if (match(compiler, TOK_DEBUG)) {
    debugStmt(compiler);
  } else if (match(compiler, TOK_LBRACK)) {
    blockStmt(compiler);
  } else if (match(compiler, TOK_IF)) {
    ifStmt(compiler);
  } else if (match(compiler, TOK_WHILE)) {
    whileStmt(compiler);
  } else {
    expression(compiler);
  }
}

static void declaration(Compiler* compiler) {
  if (match(compiler, TOK_LET)) {
    assignStmt(compiler);
  } else {
    statement(compiler);
  }
}

// A parenthesized expression.
static void grouping(Compiler* compiler, bool canAssign) {
  expression(compiler);
  consume(compiler, TOK_RPAREN, "Expected ')' after expression.");
}

static void string(Compiler* compiler, bool canAssign) {
  // +1 and -2 to omit the leading and traling '"'.
  emitConstant(compiler,
               OBJ_VAL(copyString(compiler->parser->previous.start + 1,
                                  compiler->parser->previous.length - 2)));
}

static void assignment(Compiler* compiler, bool canAssign) {
  if (!canAssign) {
    error(compiler, "Cannot assign variable in this scope");
    return;
  }

  Token name = compiler->parser->previous;

  // Compile the initializer.
  consume(compiler, TOK_ASSIGN, "Expected '='");
  expression(compiler);

  setVariable(compiler, name);
}

static void identifier(Compiler* compiler, bool canAssign) {
  if (peek(compiler) == TOK_ASSIGN) {
    assignment(compiler, canAssign);
    return;
  }
  getVariable(compiler);
}

// TODO(kjharland): Support variable patterns.
static void pattern(Compiler* compiler) {
  nextToken(compiler);

  Token token = compiler->parser->previous;
  switch (token.type) {
  case TOK_TRUE:
    emitConstant(compiler, OBA_BOOL(true));
    break;
  case TOK_FALSE:
    emitConstant(compiler, OBA_BOOL(false));
    break;
  case TOK_NUMBER:
    emitConstant(compiler, token.value);
    break;
  case TOK_STRING:
    emitConstant(compiler,
                 OBJ_VAL(copyString(token.start + 1, token.length - 2)));
    break;
  default:
    error(compiler, "Expected a constant value.");
  }
}

static void matchExprCase(Compiler* compiler) {
  pattern(compiler);

  int offset = emitJump(compiler, OP_JUMP_IF_NOT_MATCH);
  if (!match(compiler, TOK_ASSIGN)) {
    error(compiler, "Expected '=' after pattern");
    return;
  }

  // Compile the body, which only gets evaluated if the pattern above matched.
  expression(compiler);
  // If the expression was evaluated, its value is sitting on top of the stack
  // above the original match value, and should be the return value of the
  // entire expression. Swap them so that this expressions value is returned.
  emitOp(compiler, OP_SWAP_STACK_TOP);
  patchJump(compiler, offset);
}

static void matchExpr(Compiler* compiler, bool canAssign) {
  // Compile the expression to push the value to match onto the stack.
  expression(compiler);
  ignoreNewlines(compiler);

  if (!match(compiler, TOK_GUARD)) {
    error(compiler, "Expected guard after match expression");
    return;
  }

  do {
    matchExprCase(compiler);
    ignoreNewlines(compiler);
  } while (match(compiler, TOK_GUARD));

  // Clean up the stack.
  emitOp(compiler, OP_POP);
}

static void literal(Compiler* compiler, bool canAssign) {
  switch (compiler->parser->previous.type) {
  case TOK_TRUE:
    emitBool(compiler, OBA_BOOL(true));
    break;
  case TOK_FALSE:
    emitBool(compiler, OBA_BOOL(false));
    break;
  case TOK_NUMBER:
    emitConstant(compiler, compiler->parser->previous.value);
    break;
  default:
    error(compiler, "Expected a boolean or number value.");
  }
}

static void unaryOp(Compiler* compiler, bool canAssign) {
  GrammarRule* rule = getRule(compiler->parser->previous.type);
  TokenType opType = compiler->parser->previous.type;

  ignoreNewlines(compiler);

  // Compile the right hand side (right-associative).
  parse(compiler, rule->precedence);

  switch (opType) {
  case TOK_NOT:
    emitOp(compiler, OP_NOT);
    break;
  default:
    error(compiler, "Invalid operator %s", rule->name);
  }
}

static void infixOp(Compiler* compiler, bool canAssign) {
  GrammarRule* rule = getRule(compiler->parser->previous.type);
  TokenType opType = compiler->parser->previous.type;

  ignoreNewlines(compiler);

  // Compile the right hand side (right-associative).
  parse(compiler, rule->precedence);

  switch (opType) {
  case TOK_PLUS:
    emitOp(compiler, OP_ADD);
    return;
  case TOK_MINUS:
    emitOp(compiler, OP_MINUS);
    return;
  case TOK_MULTIPLY:
    emitOp(compiler, OP_MULTIPLY);
    return;
  case TOK_DIVIDE:
    emitOp(compiler, OP_DIVIDE);
    return;
  case TOK_GT:
    emitOp(compiler, OP_GT);
    return;
  case TOK_LT:
    emitOp(compiler, OP_LT);
    return;
  case TOK_GTE:
    emitOp(compiler, OP_GTE);
    return;
  case TOK_LTE:
    emitOp(compiler, OP_LTE);
    return;
  case TOK_EQ:
    emitOp(compiler, OP_EQ);
    return;
  case TOK_NEQ:
    emitOp(compiler, OP_NEQ);
    return;
  case TOK_ASSIGN:
    emitOp(compiler, OP_ASSIGN);
    return;
  default:
    error(compiler, "Invalid operator %s", rule->name);
    return;
  }
}

// Compiling ------------------------------------------------------------------

bool obaCompile(ObaVM* vm, const char* source) {
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
  parser.hasError = false;

  Compiler compiler;
  initCompiler(&compiler, &parser);

  nextToken(&compiler);
  ignoreNewlines(&compiler);

  while (!match(&compiler, TOK_EOF)) {
    declaration(&compiler);
    // If no newline, the file must end on this line.
    if (!matchLine(&compiler)) {
      consume(&compiler, TOK_EOF, "Expected end of file.");
      break;
    }
  }

  emitOp(&compiler, OP_EXIT);
  return parser.hasError;
}
