// This defines the bytecode instructions used by the VM.

#ifndef oba_opcodes_h
#define oba_opcodes_h

#include <stdint.h>

#include "oba_value.h"

typedef enum {
  OP_CONSTANT,
  OP_ADD,
  OP_MINUS,
  OP_MULTIPLY,
  OP_DIVIDE,

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
