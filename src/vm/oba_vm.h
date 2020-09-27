#ifndef oba_vm_h
#define oba_vm_h

#include "oba_compiler.h"
#include "oba_opcodes.h"
#include "oba_token.h"

// The maximum size of the VM stack in bytes.
// TODO(kendal): Support dynamically resizing the stack.
#define STACK_MAX 256

// TODO(kendal): List:
// - Create a chunk type to represent compiled bytecode.
// - Wirte up the compiler to feed instructions to the VM.
// - Create disassembler to format compiled bytecode instructions.
// - Write up the VM to decode the compiled chunks.
struct ObaVM {
  Compiler* compiler;
  Chunk* chunk;

  Value stack[STACK_MAX];
  Value* stackTop;

  uint8_t* ip;
};

#endif
