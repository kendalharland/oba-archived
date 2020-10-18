#include <stdio.h>

#include "oba_debug.h"
#include "oba_function.h"
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

static int byteInstruction(const char* name, Chunk* chunk, int offset) {
  uint8_t operand = chunk->code[offset + 1];
  printf("%-16s %4d\n", name, operand);
  return offset + 2;
}

static int jumpInstruction(const char* name, int sign, Chunk* chunk,
                           int offset) {
  uint16_t jump = (uint16_t)(chunk->code[offset + 1] << 8);
  jump |= chunk->code[offset + 2];
  printf("%-16s %4d -> %d\n", name, offset, offset + 3 + sign * jump);
  return offset + 3;
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
  case OP_ERROR:
    return constantInstruction("OP_ERROR", chunk, offset);
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
  case OP_SET_LOCAL:
    return byteInstruction("OP_SET_LOCAL", chunk, offset);
  case OP_GET_LOCAL:
    return byteInstruction("OP_GET_LOCAL", chunk, offset);
  case OP_SET_UPVALUE:
    return byteInstruction("OP_SET_UPVALUE", chunk, offset);
  case OP_GET_UPVALUE:
    return byteInstruction("OP_GET_UPVALUE", chunk, offset);
  case OP_CLOSE_UPVALUE:
    return simpleInstruction("OP_CLOSE_UPVALUE", chunk, offset);
  case OP_POP:
    return simpleInstruction("OP_POP", chunk, offset);
  case OP_JUMP:
    return jumpInstruction("OP_JUMP", 1, chunk, offset);
  case OP_JUMP_IF_FALSE:
    return jumpInstruction("OP_JUMP_IF_FALSE", 1, chunk, offset);
  case OP_JUMP_IF_TRUE:
    return jumpInstruction("OP_JUMP_IF_TRUE", 1, chunk, offset);
  case OP_JUMP_IF_NOT_MATCH:
    return jumpInstruction("OP_JUMP_IF_NOT_MATCH", 1, chunk, offset);
  case OP_LOOP:
    return jumpInstruction("OP_LOOP", -1, chunk, offset);
  case OP_CALL:
    return constantInstruction("OP_CALL", chunk, offset);
  case OP_CLOSURE: {
    offset++;
    uint8_t constant = chunk->code[offset++];
    printf("%-16s %4d ", "OP_CLOSURE", constant);
    printValue(chunk->constants.values[constant]);
    printf("\n");

    ObjFunction* function = AS_FUNCTION(chunk->constants.values[constant]);
    for (int j = 0; j < function->upvalueCount; j++) {
      int isLocal = chunk->code[offset++];
      int slot = chunk->code[offset++];
      printf("%04d      |              %s %d \n", offset - 2,
             isLocal ? "local" : "upvalue", slot);
    }
    return offset;
  }
  case OP_RETURN:
    return simpleInstruction("OP_RETURN", chunk, offset);
  case OP_DEBUG:
    return simpleInstruction("OP_DEBUG", chunk, offset);
  case OP_EXIT:
    return simpleInstruction("OP_EXIT", chunk, offset);
  default:
    printf("Unknown opcode %d\n", instr);
    return offset + 1;
  }
}
