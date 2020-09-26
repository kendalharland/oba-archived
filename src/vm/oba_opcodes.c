#include <stdint.h>
#include <stdlib.h>

#include "oba_memory.h"
#include "oba_opcodes.h"

void initChunk(Chunk* chunk) {
  chunk->capacity = 0;
  chunk->count = 0;
  chunk->code = NULL;
}

void freeChunk(Chunk* chunk) {
  FREE_ARRAY(uint8_t, chunk, chunk->capacity);
  initChunk(chunk);
}

void writeChunk(Chunk* chunk, uint8_t byte) {
  if (chunk->capacity <= chunk->count) {
    int oldCap = chunk->capacity;
    chunk->capacity = GROW_CAPACITY(oldCap);
    chunk->code = GROW_ARRAY(uint8_t, chunk->code, oldCap, chunk->capacity);
  }

  chunk->code[chunk->count] = byte;
  chunk->count++;
}
