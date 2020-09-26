#ifndef oba_vm_h
#define oba_vm_h

#include "oba_compiler.h"
#include "oba_opcodes.h"
#include "oba_token.h"

// TODO(kendal): List:
// - Create a chunk type to represent compiled bytecode.
// - Wirte up the compiler to feed instructions to the VM.
// - Create disassembler to format compiled bytecode instructions.
// - Write up the VM to decode the compiled chunks.
struct ObaVM {
  Compiler* compiler;
  // The chunk of compiled bytecode.
  Chunk* chunk;
};

#endif
