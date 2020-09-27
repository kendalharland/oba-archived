#include <stdlib.h>
#include <string.h>

#include "oba.h"
#include "oba_vm.h"

#ifdef DEBUG_TRACE_EXECUTION
#include "oba_debug.h"
#endif

static void resetStack(ObaVM* vm) { vm->stackTop = vm->stack; }

ObaVM* obaNewVM() {
  // TODO(kendal): sizeof(ObaVM) here instead?
  ObaVM* vm = (ObaVM*)realloc(NULL, sizeof(*vm));
  memset(vm, 0, sizeof(ObaVM));
  resetStack(vm);
  return vm;
}

void obaFreeVM(ObaVM* vm) {
  freeChunk(vm->chunk);
  vm->stackTop = NULL;
  free(vm);
  vm = NULL;
}

static void push(ObaVM* vm, Value value) {
  *vm->stackTop = value;
  vm->stackTop++;
}

static Value pop(ObaVM* vm) {
  // TODO(kendal): Handle vm->stackTop == vm->stack?
  vm->stackTop--;
  return *vm->stackTop;
}

static void run(ObaVM* vm) {

  // clang-format off

#define READ_BYTE() (*vm->ip++)
#define READ_CONSTANT() (vm->chunk->constants.values[READ_BYTE()])
#define BINARY_OP(op)                                                          \
do {                                                                           \
  double b = pop(vm);                                                          \
  double a = pop(vm);                                                          \
  push(vm, a op b);                                                            \
} while (0)

  // clang-format on

  for (;;) {
#ifdef DEBUG_TRACE_EXECUTION
    disassembleInstruction(vm->chunk, (int)(vm->ip - vm->chunk->code));
#endif
    uint8_t instruction;
    switch (instruction = READ_BYTE()) {
    case OP_CONSTANT:
      push(vm, READ_CONSTANT());
      break;
    case OP_ADD:
      BINARY_OP(+);
      break;
    case OP_MINUS:
      BINARY_OP(-);
      break;
    case OP_MULTIPLY:
      BINARY_OP(*);
      break;
    case OP_DIVIDE:
      BINARY_OP(/);
      break;
    case OP_EXIT:
      return;
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
  return interpret(vm);
}
