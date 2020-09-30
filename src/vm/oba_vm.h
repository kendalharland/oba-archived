#ifndef oba_vm_h
#define oba_vm_h

#include "oba_compiler.h"
#include "oba_opcodes.h"
#include "oba_token.h"

// The maximum size of the VM stack in bytes.
// TODO(kendal): Support dynamically resizing the stack.
#define STACK_MAX 256

#define TABLE_MAX_LOAD 0.75

typedef struct sTable Table;
typedef struct sEntry Entry;

struct ObaVM {
  Compiler* compiler;

  // The bytecode to execute.
  Chunk* chunk;

  // Points to the current instruction in chunk.
  uint8_t* ip;

  Value stack[STACK_MAX];
  Value* stackTop;
  Table* globals;
};

#endif
