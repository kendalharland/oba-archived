// This defines the bytecode instructions used by the VM.

#ifndef oba_opcodes_h
#define oba_opcodes_h

#include <stdint.h>

typedef enum {
  OP_CONSTANT,
} OpCode;

// Chunk is a dynamic array of Oba bytecode instructions.
typedef struct {
  int capacity;
  int count;
  uint8_t* code;
} Chunk;

void initChunk(Chunk*);

// Frees the memory held by a [Chunk] previously allocate with [initChunk].
void freeChunk(Chunk*);

// Writes a byte to the given [Chunk], allocating if necessary.
void writeChunk(Chunk*, uint8_t);

#endif
