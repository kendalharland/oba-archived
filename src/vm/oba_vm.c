#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "oba.h"
#include "oba_builtins.h"
#include "oba_function.h"
#include "oba_memory.h"
#include "oba_vm.h"

#ifdef DEBUG_TRACE_EXECUTION
#include "oba_debug.h"
#endif

// VM -------------------------------------------------------------------------

static Value peek(ObaVM* vm, int lookahead) {
  return *(vm->stackTop - lookahead);
}

static void push(ObaVM* vm, Value value) {
  *vm->stackTop = value;
  vm->stackTop++;
}

static Value pop(ObaVM* vm) {
  vm->stackTop--;
  return *vm->stackTop;
}

static void defineNative(ObaVM* vm, const char* name, NativeFn function) {
  push(vm, OBJ_VAL(copyString(vm, name, (int)strlen(name))));
  push(vm, OBJ_VAL(newNative(vm, function)));
  tableSet(vm->globals, AS_STRING(vm->stack[0]), vm->stack[1]);
  pop(vm);
  pop(vm);
}

static void resetStack(ObaVM* vm) { vm->stackTop = vm->stack; }

static void registerBuiltins(ObaVM* vm, Builtin* builtins, int builtinsLength) {
  Builtin* builtin = __builtins__;

  // Original builtins.
  while (builtin->name != NULL) {
    defineNative(vm, builtin->name, builtin->function);
    builtin++;
  }

  // User builtins, registered last so they can override the originals.
  for (int i = 0; i < builtinsLength; i++) {
    defineNative(vm, builtins[i].name, builtins[i].function);
  }
}

static void runtimeError(ObaVM* vm, const char* format, ...) {
  va_list args;
  va_start(args, format);
  fprintf(stderr, "Runtime error: ");
  vfprintf(stderr, format, args);
  va_end(args);
  fputs("\n", stderr);

  // TODO(kendal): Capture op line info
  /*
  size_t instruction = vm->frame->ip - vm->frame->closure->function->chunk.code
  - 1; int line = vm->frame->closure->function->chunk.lines[instruction];
  fprintf(stderr, "[line %d] in script\n", line);
  */
  resetStack(vm);
}

static bool call(ObaVM* vm, ObjClosure* closure, int arity) {
  if (arity != closure->function->arity) {
    runtimeError(vm, "Expected %d arguments but got %d",
                 closure->function->arity, arity);
    return false;
  }

  vm->frame++;
  if (vm->frame - vm->frames > FRAMES_MAX) {
    runtimeError(vm, "Too many nested function calls");
    return false;
  }
  vm->frame->closure = closure;
  vm->frame->ip = closure->function->chunk.code;
  vm->frame->slots = vm->stackTop - arity;
  return true;
}

static bool callNative(ObaVM* vm, NativeFn native, int arity) {
  Value result = native(arity, vm->stackTop - arity);
  vm->stackTop -= arity;
  push(vm, result);
  return true;
}

static bool callValue(ObaVM* vm, Value value, int arity) {
  if (IS_OBJ(value)) {
    switch (OBJ_TYPE(value)) {
    case OBJ_CLOSURE:
      return call(vm, AS_CLOSURE(value), arity);
    case OBJ_NATIVE:
      return callNative(vm, AS_NATIVE(value), arity);
    default:
      // Non-callable
      break;
    }
  }

  runtimeError(vm, "Can only call functions");
  return false;
}

// Captures the local value in an upvalue.
// If an existing upvalue already closes over the local, it is returned.
// Otherwise a new one is created.
static ObjUpvalue* captureUpvalue(ObaVM* vm, Value* local) {
  ObjUpvalue* prev = NULL;
  ObjUpvalue* upvalue = vm->openUpvalues;

  while (upvalue != NULL && upvalue->location > local) {
    prev = upvalue;
    upvalue = upvalue->next;
  }

  if (upvalue != NULL && upvalue->location == local) {
    return upvalue;
  }

  ObjUpvalue* createdUpvalue = newUpvalue(vm, local);
  createdUpvalue->next = upvalue;

  if (prev == NULL) {
    vm->openUpvalues = createdUpvalue;
  } else {
    prev->next = createdUpvalue;
  }

  return createdUpvalue;
}

static void closeUpvalue(ObaVM* vm, Value* last) {
  while (vm->openUpvalues != NULL && vm->openUpvalues->location >= last) {
    vm->openUpvalues->closed = *vm->openUpvalues->location;
    vm->openUpvalues->location = &vm->openUpvalues->closed;
    vm->openUpvalues = vm->openUpvalues->next;
  }
}

char* readFile(ObaVM* vm, const char* path) {
  FILE* fp = fopen(path, "rb");
  if (!fp) {
    perror("failed to read file");
    exit(1);
  }

  // Get the file size.
  fseek(fp, 0L, SEEK_END);
  long size = ftell(fp);
  rewind(fp);

  // Read the contents.
  char* contents = malloc(size + 1);
  if (!contents) {
    fclose(fp);
    perror("failed to allocate memory for file");
    exit(1);
  }
  if (1 != fread(contents, size, 1, fp)) {
    fclose(fp);
    free(contents);
    perror("failed to read file");
    exit(1);
  }
  contents[size] = '\0';

  fclose(fp);
  return contents;
}

// TODO(kendal): Allow the host application to resolve modules in its own way.
static char* resolveModule(ObaVM* vm, Value name) {
  // TODO(kendal): Precompile core modules as part of the VM.
  ObjString* path = AS_STRING(name);

  int pathLength = path->length + strlen("mod/.oba") + 1;
  char* fullpath = malloc(pathLength);
  sprintf(fullpath, "mod/%s.oba", path->chars);
  fullpath[pathLength] = '\0';
  return fullpath;
}

ObjClosure* compileInModule(ObaVM* vm, Value value, const char* source) {
  ObjString* name = AS_STRING(value);
  ObjModule* module = newModule(vm, name);
  ObjFunction* function = obaCompile(vm, module, source);
  if (function == NULL) {
    return NULL;
  }

  // Store the module as a global variable of the current module.
  tableSet(vm->frame->closure->function->module->variables, module->name,
           OBJ_VAL(module));
  return newClosure(vm, function);
}

// TODO(kendal): If the module is already loaded, bail early.
// TODO(kendal): Handle circular imports.
static ObjClosure* importModule(ObaVM* vm, Value name) {
  char* path = resolveModule(vm, name);
  char* source = readFile(vm, path);

  ObjClosure* moduleClosure = compileInModule(vm, name, source);
  if (moduleClosure == NULL) {
    FREE_ARRAY(char, source, strlen(source));
    return NULL;
  }

  FREE_ARRAY(char, path, strlen(path));
  FREE_ARRAY(char, source, strlen(source));

  return moduleClosure;
}

static void return_(ObaVM* vm) {
  Value value = pop(vm);
  closeUpvalue(vm, vm->frame->slots);

  // -1 because the function itself is right before the slot pointer.
  vm->stackTop = vm->frame->slots - 1;
  push(vm, value);
  vm->frame->closure = NULL;
  vm->frame->ip = NULL;
  vm->frame->slots = NULL;
  vm->frame--;
}

static void concatenate(ObaVM* vm) {
  ObjString* b = AS_STRING(pop(vm));
  ObjString* a = AS_STRING(pop(vm));

  char* chars = ALLOCATE(char, b->length + a->length);
  int length = b->length + a->length;

  memcpy(chars, a->chars, a->length);
  memcpy(chars + a->length, b->chars, b->length);
  chars[length] = '\0';

  ObjString* result = takeString(vm, chars, length);
  push(vm, OBJ_VAL(result));
}

ObaVM* obaNewVM(Builtin* builtins, int builtinsLength) {
  ObaVM* vm = (ObaVM*)realloc(NULL, sizeof(*vm));
  memset(vm, 0, sizeof(ObaVM));

  vm->openUpvalues = NULL;
  vm->objects = NULL;
  vm->frame = vm->frames;

  vm->globals = (Table*)realloc(NULL, sizeof(Table));
  initTable(vm->globals);

  resetStack(vm);
  registerBuiltins(vm, builtins, builtinsLength);
  return vm;
}

static void freeObjects(ObaVM* vm) {
  Obj* obj = vm->objects;
  while (obj != NULL) {
    Obj* next = obj->next;
    freeObject(obj);
    obj = next;
  }
}

void obaFreeVM(ObaVM* vm) {
  // The closure is unset if an error occurred during compilation.
  if (vm->frame->closure != NULL) {
    freeChunk(&vm->frame->closure->function->chunk);
  }
  freeTable(vm->frame->closure->function->module->variables);
  freeObjects(vm);
  free(vm);
}

static ObaInterpretResult run(ObaVM* vm) {

  // clang-format off

#define READ_BYTE() (*vm->frame->ip++)

#define READ_SHORT() \
  (vm->frame->ip += 2, (uint16_t)((vm->frame->ip[-2] << 8) | vm->frame->ip[-1]))

#define READ_CONSTANT() \
  (vm->frame->closure->function->chunk.constants.values[READ_BYTE()])

#define READ_STRING() AS_STRING(READ_CONSTANT())

#define BINARY_OP(type, op)                                                    \
do {                                                                           \
  if (IS_NUMBER(peek(vm, 1)) && IS_NUMBER(peek(vm, 2))) {                      \
    double b = AS_NUMBER(pop(vm));                                             \
    double a = AS_NUMBER(pop(vm));                                             \
    push(vm, type(a op b));                                                    \
  } else {                                                                     \
    runtimeError(vm, "Expected numeric or string operands");                   \
    return OBA_RESULT_RUNTIME_ERROR;                                           \
  }                                                                            \
} while (0)

// Debug output

#ifdef DEBUG_TRACE_EXECUTION

#define DEBUG_TRACE_INSTRUCTIONS()                                             \
  disassembleInstruction(                                                      \
        &vm->frame->closure->function->chunk,                                  \
        (int)(vm->frame->ip - vm->frame->closure->function->chunk.code));      \
    printf("          ");                                                      \
    for (Value* slot = vm->stack; slot < vm->stackTop; slot++) {               \
      printf("[ ");                                                            \
      printValue(*slot);                                                       \
      printf(" ]");                                                            \
    }                                                                          \
    printf("\n");

  // clang-format on

#else

#define DEBUG_TRACE_INSTRUCTIONS() ;

#endif

  // Optimizations

#ifdef OBA_COMPUTED_GOTO

#define DISPATCH()                                                             \
  do {                                                                         \
    DEBUG_TRACE_INSTRUCTIONS();                                                \
    goto* dispatchTable[READ_BYTE()];                                          \
  } while (true)

#define INTERPRET_LOOP DISPATCH();

  // Computed goto dispatch table.
  // eli.thegreenplace.net/2012/07/12/computed-goto-for-efficient-dispatch-tables
  static void* dispatchTable[] = {
#define OPCODE(name) &&op_##name,
#include "oba_opcodes.h"
#undef OPCODE
  };

#define CASE_OP(name) op_##name

#else

#define OPCODE(name) OP_##name
#define CASE_OP(name) case OPCODE(name)

#define INTERPRET_LOOP                                                         \
  loop:                                                                        \
  DEBUG_TRACE_INSTRUCTIONS();                                                  \
  switch ((OpCode)READ_BYTE())

#define DISPATCH() goto loop

#endif

  INTERPRET_LOOP {

    CASE_OP(CONSTANT) : {
      push(vm, READ_CONSTANT());
      DISPATCH();
    }

    CASE_OP(ERROR) : {
      runtimeError(vm, AS_CSTRING(READ_CONSTANT()));
      return OBA_RESULT_RUNTIME_ERROR;
    }

    CASE_OP(ADD) : {
      if (IS_STRING(peek(vm, 1)) && IS_STRING(peek(vm, 2))) {
        concatenate(vm);
      } else {
        BINARY_OP(OBA_NUMBER, +);
      }
      DISPATCH();
    }

    CASE_OP(MINUS) : {
      BINARY_OP(OBA_NUMBER, -);
      DISPATCH();
    }

    CASE_OP(MULTIPLY) : {
      BINARY_OP(OBA_NUMBER, *);
      DISPATCH();
    }

    CASE_OP(DIVIDE) : {
      BINARY_OP(OBA_NUMBER, /);
      DISPATCH();
    }

    CASE_OP(NOT) : {
      if (!IS_BOOL(peek(vm, 1))) {
        runtimeError(vm, "Expected boolean value");
        return OBA_RESULT_RUNTIME_ERROR;
      }
      push(vm, OBA_BOOL(!AS_BOOL(pop(vm))));
      DISPATCH();
    }

    CASE_OP(GT) : {
      BINARY_OP(OBA_BOOL, >);
      DISPATCH();
    }

    CASE_OP(LT) : {
      BINARY_OP(OBA_BOOL, <);
      DISPATCH();
    }

    CASE_OP(GTE) : {
      BINARY_OP(OBA_BOOL, >=);
      DISPATCH();
    }

    CASE_OP(LTE) : {
      BINARY_OP(OBA_BOOL, <=);
      DISPATCH();
    }

    CASE_OP(EQ) : {
      Value b = pop(vm);
      Value a = pop(vm);
      push(vm, OBA_BOOL(valuesEqual(a, b)));
      DISPATCH();
    }

    CASE_OP(NEQ) : {
      Value b = pop(vm);
      Value a = pop(vm);
      push(vm, OBA_BOOL(!valuesEqual(a, b)));
      DISPATCH();
    }

    CASE_OP(TRUE) : {
      push(vm, OBA_BOOL(true));
      DISPATCH();
    }

    CASE_OP(FALSE) : {
      push(vm, OBA_BOOL(false));
      DISPATCH();
    }

    CASE_OP(JUMP) : {
      vm->frame->ip += READ_SHORT();
      DISPATCH();
    }

    CASE_OP(JUMP_IF_FALSE) : {
      if (!IS_BOOL(peek(vm, 1))) {
        runtimeError(vm, "Expected a boolean expression");
        return OBA_RESULT_RUNTIME_ERROR;
      }
      int jump = READ_SHORT();
      bool cond = AS_BOOL(peek(vm, 1));
      if (!cond)
        vm->frame->ip += jump;
      DISPATCH();
    }

    CASE_OP(JUMP_IF_TRUE) : {
      if (!IS_BOOL(peek(vm, 1))) {
        runtimeError(vm, "Expected a boolean expression");
        return OBA_RESULT_RUNTIME_ERROR;
      }
      int jump = READ_SHORT();
      bool cond = AS_BOOL(peek(vm, 1));
      if (cond)
        vm->frame->ip += jump;
      DISPATCH();
    }

    CASE_OP(JUMP_IF_NOT_MATCH) : {
      // TODO(kjharland): Support variable matches.
      int jump = READ_SHORT();
      Value a = peek(vm, 2);
      Value b = pop(vm);
      if (!valuesEqual(b, a)) {
        vm->frame->ip += jump;
        DISPATCH();
      }
      pop(vm);
      DISPATCH();
    }

    CASE_OP(LOOP) : {
      vm->frame->ip = vm->frame->closure->function->chunk.code + READ_SHORT();
      DISPATCH();
    }

    CASE_OP(DEFINE_GLOBAL) : {
      ObjString* name = READ_STRING();
      tableSet(vm->frame->closure->function->module->variables, name,
               peek(vm, 1));
      pop(vm);
      DISPATCH();
    }

    CASE_OP(GET_GLOBAL) : {
      ObjString* name = READ_STRING();
      Value value;

      if (!tableGet(vm->frame->closure->function->module->variables, name,
                    &value)) {
        if (!tableGet(vm->globals, name, &value)) {
          runtimeError(vm, "Undefined variable: %s", name->chars);
          return OBA_RESULT_RUNTIME_ERROR;
        }
      }
      push(vm, value);
      DISPATCH();
    }

    CASE_OP(SET_LOCAL) : {
      uint8_t slot = READ_BYTE();
      vm->frame->slots[slot] = peek(vm, 1);
      DISPATCH();
    }

    CASE_OP(GET_LOCAL) : {
      // Locals live on the top of the stack.
      uint8_t slot = READ_BYTE();
      push(vm, vm->frame->slots[slot]);
      DISPATCH();
    }

    CASE_OP(SET_UPVALUE) : {
      uint8_t slot = READ_BYTE();
      *vm->frame->closure->upvalues[slot]->location = peek(vm, 1);
      DISPATCH();
    }

    CASE_OP(GET_UPVALUE) : {
      uint8_t slot = READ_BYTE();
      ObjUpvalue* upvalue = vm->frame->closure->upvalues[slot];
      // The user can never get an upvalue directly. Push its captured value
      // onto the stack instead.
      push(vm, *upvalue->location);
      DISPATCH();
    }

    CASE_OP(CLOSE_UPVALUE) : {
      closeUpvalue(vm, vm->stackTop - 1);
      pop(vm);
      DISPATCH();
    }

    CASE_OP(GET_IMPORTED_VARIABLE) : {
      Value receiver = pop(vm);
      if (!IS_MODULE(receiver)) {
        runtimeError(vm, "Expected a module");
        return OBA_RESULT_RUNTIME_ERROR;
      }

      ObjModule* module = AS_MODULE(receiver);
      ObjString* name = READ_STRING();
      Value value;
      if (!tableGet(module->variables, name, &value)) {
        runtimeError(vm, "Variable '%s' not found in module '%s'", name->chars,
                     module->name);
        return OBA_RESULT_RUNTIME_ERROR;
      }
      push(vm, value);
      DISPATCH();
    }

    CASE_OP(CALL) : {
      uint8_t argCount = READ_BYTE();
      if (!callValue(vm, peek(vm, argCount + 1), argCount)) {
        return OBA_RESULT_RUNTIME_ERROR;
      }
      DISPATCH();
    }

    CASE_OP(CLOSURE) : {
      ObjFunction* function = AS_FUNCTION(READ_CONSTANT());
      ObjClosure* closure = newClosure(vm, function);
      push(vm, OBJ_VAL(closure));

      for (int j = 0; j < closure->upvalueCount; j++) {
        uint8_t isLocal = READ_BYTE();
        uint8_t slot = READ_BYTE();
        if (isLocal) {
          closure->upvalues[j] = captureUpvalue(vm, vm->frame->slots + slot);
        } else {
          closure->upvalues[j] = vm->frame->closure->upvalues[slot];
        }
      }
      DISPATCH();
    }

    CASE_OP(RETURN) : {
      return_(vm);
      DISPATCH();
    }

    CASE_OP(POP) : {
      pop(vm);
      DISPATCH();
    }

    CASE_OP(DEBUG) : {
      Value value = pop(vm);
      printValue(value);
      printf("\n");
      DISPATCH();
    }

    CASE_OP(IMPORT_MODULE) : {
      ObjClosure* moduleClosure = importModule(vm, READ_CONSTANT());
      push(vm, OBJ_VAL(moduleClosure));
      callValue(vm, OBJ_VAL(moduleClosure), 0);
      DISPATCH();
    }

    CASE_OP(END_MODULE) : {
      if (vm->frame - vm->frames > 1) {
        return_(vm);
      }
      DISPATCH();
    }

    CASE_OP(EXIT) : {
      // Pop the root closure off the stack.
      pop(vm);
      return OBA_RESULT_SUCCESS;
    }
  }

#undef READ_BYTE
#undef READ_SHORT
#undef READ_CONSTANT
#undef READ_STRING
#undef BINARY_OP
#undef CASE_OP
#undef DISPATCH
#undef INTERPRET_LOOP
#undef DEBUG_TRACE_INSTRUCTIONS
}

ObaInterpretResult obaInterpret(ObaVM* vm, const char* source) {
  ObjModule* module = newModule(vm, copyString(vm, "main", 4));
  ObjFunction* function = obaCompile(vm, module, source);
  if (function == NULL) {
    return OBA_RESULT_COMPILE_ERROR;
  }
  if (function->chunk.code == NULL) {
    return OBA_RESULT_SUCCESS;
  }

  push(vm, OBJ_VAL(function));
  ObjClosure* closure = newClosure(vm, function);
  pop(vm);
  push(vm, OBJ_VAL(closure));
  callValue(vm, OBJ_VAL(closure), 0);
  return run(vm);
}
