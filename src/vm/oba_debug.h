#ifndef oba_debug_h
#define oba_debug_h

#include "oba_chunk.h"

int disassembleInstruction(Chunk*, int);
int disassemble(Chunk*, const char*);

#endif
