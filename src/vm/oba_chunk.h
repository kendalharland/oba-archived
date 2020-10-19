#ifndef oba_chunk_h
#define oba_chunk_h

#include "oba_value.h"
#include <stdint.h>

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
