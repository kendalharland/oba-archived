#include <stdio.h>

#include "oba_debug.h"
#include "oba_value.h"

static int constantInstruction(const char* name, Chunk* chunk, int offset) {
  uint8_t constant = chunk->code[offset + 1];
  printf("%-16s %4d '", name, constant);
  printValue(chunk->constants.values[constant]);
  printf("'\n");
  return offset + 2;
}

static int simpleInstruction(const char* name, Chunk* chunk, int offset) {
  printf("%s\n", name);
  return offset + 1;
}

int disassemble(Chunk* chunk, const char* name) {
  printf("== %s ==\n", name);
  for (int offset = 0; offset < chunk->count;) {
    offset = disassembleInstruction(chunk, offset);
  }
}

int disassembleInstruction(Chunk* chunk, int offset) {
  printf("%04d ", offset);

  uint8_t instr = chunk->code[offset];
  switch (instr) {
  case OP_CONSTANT:
    return constantInstruction("OP_CONSTANT", chunk, offset);
  case OP_ADD:
    return simpleInstruction("OP_ADD", chunk, offset);
  case OP_MINUS:
    return simpleInstruction("OP_MINUS", chunk, offset);
  case OP_MULTIPLY:
    return simpleInstruction("OP_MULTIPLY", chunk, offset);
  case OP_DIVIDE:
    return simpleInstruction("OP_DIVIDE", chunk, offset);
  case OP_TRUE:
    return simpleInstruction("OP_TRUE", chunk, offset);
  case OP_FALSE:
    return simpleInstruction("OP_FALSE", chunk, offset);
  case OP_NOT:
    return simpleInstruction("OP_NOT", chunk, offset);
  case OP_GT:
    return simpleInstruction("OP_GT", chunk, offset);
  case OP_LT:
    return simpleInstruction("OP_LT", chunk, offset);
  case OP_GTE:
    return simpleInstruction("OP_GTE", chunk, offset);
  case OP_LTE:
    return simpleInstruction("OP_LTE", chunk, offset);
  case OP_EQ:
    return simpleInstruction("OP_EQ", chunk, offset);
  case OP_NEQ:
    return simpleInstruction("OP_NEQ", chunk, offset);
  case OP_ASSIGN:
    return simpleInstruction("OP_ASSIGN", chunk, offset);
  case OP_DEFINE_GLOBAL:
    return constantInstruction("OP_DEFINE_GLOBAL", chunk, offset);
  case OP_GET_GLOBAL:
    return constantInstruction("OP_GET_GLOBAL", chunk, offset);
  case OP_DEBUG:
    return simpleInstruction("OP_DEBUG", chunk, offset);
  case OP_EXIT:
    return simpleInstruction("OP_EXIT", chunk, offset);
  default:
    printf("Unknown opcode %d\n", instr);
    return offset + 1;
  }
}
