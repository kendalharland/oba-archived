#include <stdlib.h>
#include <string.h>

#include "oba.h"
#include "oba_vm.h"

#ifdef DEBUG_TRACE_EXECUTION
#include "oba_debug.h"
#endif

ObaVM* obaNewVM() {
  // TODO(kendal): sizeof(ObaVM) here instead?
  ObaVM* vm = (ObaVM*)realloc(NULL, sizeof(*vm));
  memset(vm, 0, sizeof(ObaVM));
  return vm;
}

void obaFreeVM(ObaVM* vm) {
  freeChunk(vm->chunk);
  free(vm);
  vm = NULL;
}

static void run(ObaVM* vm) {
#define READ_BYTE() (*vm->ip++)
  for (;;) {
    uint8_t instruction;
    switch (instruction = READ_BYTE()) {
    case OP_ADD:
      // Pop the last two args off the stack.
      // Add them, push the result onto the stack.
      break;
    case OP_MINUS:
      // Pop the last two args off the stack.
      // Subtract them, push the result onto the stack.
      break;
    case OP_MULTIPLY:
      // Pop the last two args off the stack.
      // Mul them, push the result onto the stack.
      break;
    case OP_DIVIDE:
      // Pop the last two args off the stack.
      // Divide them, push the result onto the stack.
      break;
    }
  }
#undef READ_BYTE
}

static ObaInterpretResult interpret(ObaVM* vm) {
  if (vm->chunk == NULL || vm->chunk->code == NULL)
    return OBA_RESULT_SUCCESS;

  vm->ip = vm->chunk->code;
  run(vm);

  return OBA_RESULT_SUCCESS;
}

ObaInterpretResult obaInterpret(ObaVM* vm, const char* source) {
  obaCompile(vm, source);
#ifdef DEBUG_TRACE_EXECUTION
  disassemble(vm->chunk, "interpret");
#endif
  return interpret(vm);
}
