#ifndef oba_vm_h
#define oba_vm_h

#include "oba_compiler.h"
#include "oba_function.h"
#include "oba_opcodes.h"
#include "oba_token.h"

// The maximum size of the VM stack in bytes.
// TODO(kendal): Support dynamically resizing the stack.
#define STACK_MAX 256
#define FRAMES_MAX 256

#define TABLE_MAX_LOAD 0.75

typedef struct sTable Table;
typedef struct sEntry Entry;

struct ObaVM {
  CallFrame frames[FRAMES_MAX];
  CallFrame* frame;

  Value stack[STACK_MAX];
  Value* stackTop;
  Table* globals;
};

#endif
