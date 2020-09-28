#include <stdarg.h>
#include <stdio.h>
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

static Value peek(ObaVM* vm, int lookahead) {
  return *(vm->stackTop + lookahead);
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

static void runtimeError(ObaVM* vm, const char* format, ...) {
  va_list args;
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
  fputs("\n", stderr);

  // TODO(kendal): Capture op line info
  /*
  size_t instruction = vm->ip - vm->chunk->code - 1;
  int line = vm->chunk->lines[instruction];
  fprintf(stderr, "[line %d] in script\n", line);
  */
  resetStack(vm);
}

static ObaInterpretResult run(ObaVM* vm) {

  // clang-format off

#define READ_BYTE() (*vm->ip++)
#define READ_CONSTANT() (vm->chunk->constants.values[READ_BYTE()])

// TODO(kendal): how do we handle non-numeric ops?
#define BINARY_OP(type, op)                                                    \
do {                                                                           \
  if (!IS_NUMBER(peek(vm, 0)) || !IS_NUMBER(peek(vm, 1))) {                    \
    runtimeError(vm, "Expected numeric operands to (op)");                     \
    return OBA_RESULT_RUNTIME_ERROR;                                           \
  }                                                                            \
  double b = AS_NUMBER(pop(vm));                                               \
  double a = AS_NUMBER(pop(vm));                                               \
  push(vm, type(a op b));                                                      \
} while (0)

  // clang-format on

  for (;;) {
#ifdef DEBUG_TRACE_EXECUTION
    disassembleInstruction(vm->chunk, (int)(vm->ip - vm->chunk->code));
    printf("          ");
    for (Value* slot = vm->stack; slot < vm->stackTop; slot++) {
      printf("[ ");
      printValue(*slot);
      printf(" ]");
    }
    printf("\n");

#endif
    uint8_t instruction;
    switch (instruction = READ_BYTE()) {
    case OP_CONSTANT:
      push(vm, READ_CONSTANT());
      break;
    case OP_ADD:
      BINARY_OP(OBA_NUMBER, +);
      break;
    case OP_MINUS:
      BINARY_OP(OBA_NUMBER, -);
      break;
    case OP_MULTIPLY:
      BINARY_OP(OBA_NUMBER, *);
      break;
    case OP_DIVIDE:
      BINARY_OP(OBA_NUMBER, /);
      break;
    case OP_TRUE:
      push(vm, OBA_BOOL(true));
      break;
    case OP_FALSE:
      push(vm, OBA_BOOL(false));
      break;
    case OP_EXIT:
      return OBA_RESULT_SUCCESS;
    }
  }
#undef READ_BYTE
}

static ObaInterpretResult interpret(ObaVM* vm) {
  if (vm->chunk == NULL || vm->chunk->code == NULL)
    return OBA_RESULT_SUCCESS;

  vm->ip = vm->chunk->code;
  return run(vm);
}

ObaInterpretResult obaInterpret(ObaVM* vm, const char* source) {
  if (obaCompile(vm, source)) {
    return OBA_RESULT_COMPILE_ERROR;
  }
  return interpret(vm);
}
