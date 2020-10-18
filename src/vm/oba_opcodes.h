// This defines the bytecode instructions used by the VM.

#ifndef oba_opcodes_h
#define oba_opcodes_h

#include "oba_value.h"
#include <stdint.h>

typedef enum {
  OP_CONSTANT,
  OP_ERROR,
  OP_ADD,
  OP_MINUS,
  OP_MULTIPLY,
  OP_DIVIDE,
  OP_TRUE,
  OP_FALSE,
  OP_NOT,
  OP_GT,
  OP_LT,
  OP_GTE,
  OP_LTE,
  OP_EQ,
  OP_NEQ,
  OP_ASSIGN,
  OP_POP,
  OP_DEBUG,
  OP_DEFINE_GLOBAL,
  OP_GET_GLOBAL,
  OP_GET_LOCAL,
  OP_GET_UPVALUE,
  OP_SET_UPVALUE,
  OP_SET_LOCAL,
  OP_JUMP,
  OP_JUMP_IF_FALSE,
  OP_JUMP_IF_TRUE,
  OP_JUMP_IF_NOT_MATCH,
  OP_LOOP,
  OP_CALL,
  OP_CLOSURE,
  OP_CLOSE_UPVALUE,
  OP_RETURN,

  OP_EXIT,
} OpCode;

// Chunk is a dynamic array of Oba bytecode instructions.
typedef struct {
  int capacity;
  int count;
  uint8_t* code;
  ValueArray constants;
} Chunk;

void initChunk(Chunk*);

// Frees the memory held by a [Chunk] previously allocate with [initChunk].
void freeChunk(Chunk*);

// Writes a byte to the given [Chunk], allocating if necessary.
void writeChunk(Chunk*, uint8_t);

#endif
