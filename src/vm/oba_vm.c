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

// Hash Table -----------------------------------------------------------------

struct sTable {
  int count;
  int capacity;
  Entry* entries;
};

struct sEntry {
  ObjString* key;
  Value value;
};

static void initTable(Table* table) {
  table->count = 0;
  table->capacity = 0;
  table->entries = NULL;
}

static void freeTable(Table* table) {
  FREE_ARRAY(Entry, table->entries, table->capacity);
  initTable(table);
}

static Entry* findEntry(Entry* entries, int capacity, ObjString* key) {
  uint32_t index = key->hash % capacity;
  for (;;) {
    Entry* entry = &entries[index];

    // TODO(kendal): Use string interning instead of comparing hashes?
    if (entry->key == NULL || entry->key->hash == key->hash) {
      return entry;
    }

    index = (index + 1) % capacity;
  }
}

static void adjustCapacity(Table* table, int capacity) {
  Entry* entries = ALLOCATE(Entry, capacity);
  for (int i = 0; i < capacity; i++) {
    entries[i].key = NULL;
    // TODO(kendal): Set the value to some zero value.
  }

  for (int i = 0; i < table->capacity; i++) {
    Entry* entry = &table->entries[i];
    if (entry->key == NULL)
      continue;

    Entry* dest = findEntry(entries, capacity, entry->key);
    dest->key = entry->key;
    dest->value = entry->value;
  }

  FREE_ARRAY(Entry, table->entries, table->capacity);
  table->entries = entries;
  table->capacity = capacity;
}

bool tableGet(Table* table, ObjString* key, Value* value) {
  if (table->count == 0)
    return false;

  Entry* entry = findEntry(table->entries, table->capacity, key);
  if (entry->key == NULL) {
    return false;
  }

  *value = entry->value;
  return true;
}

static bool tableSet(Table* table, ObjString* key, Value value) {
  if (table->count <= table->capacity * TABLE_MAX_LOAD) {
    int capacity = GROW_CAPACITY(table->capacity);
    adjustCapacity(table, capacity);
  }

  Entry* entry = findEntry(table->entries, table->capacity, key);

  bool isNewKey = entry->key == NULL;
  if (isNewKey)
    table->count++;

  entry->key = key;
  entry->value = value;
  return isNewKey;
}

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

ObaVM* obaNewVM(Builtin* builtins, int builtinsLength) {
  ObaVM* vm = (ObaVM*)realloc(NULL, sizeof(*vm));
  memset(vm, 0, sizeof(ObaVM));

  vm->openUpvalues = NULL;
  vm->objects = NULL;
  vm->globals = (Table*)realloc(NULL, sizeof(Table));
  initTable(vm->globals);
  resetStack(vm);
  vm->frame = vm->frames;

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
  vm->objects = NULL;
}

void obaFreeVM(ObaVM* vm) {
  freeChunk(&vm->frame->closure->function->chunk);
  freeTable(vm->globals);
  freeObjects(vm);
  free(vm);
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

static ObaInterpretResult run(ObaVM* vm) {

  // clang-format off

#define READ_BYTE() (*vm->frame->ip++)
#define READ_SHORT() \
  (vm->frame->ip += 2, (uint16_t)((vm->frame->ip[-2] << 8) | vm->frame->ip[-1]))
#define READ_CONSTANT() (vm->frame->closure->function->chunk.constants.values[READ_BYTE()])
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

  // clang-format on

  for (;;) {
#ifdef DEBUG_TRACE_EXECUTION
    disassembleInstruction(
        &vm->frame->closure->function->chunk,
        (int)(vm->frame->ip - vm->frame->closure->function->chunk.code));
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
    case OP_ERROR:
      runtimeError(vm, AS_CSTRING(READ_CONSTANT()));
      return OBA_RESULT_RUNTIME_ERROR;
    case OP_ADD: {
      if (IS_STRING(peek(vm, 1)) && IS_STRING(peek(vm, 2))) {
        concatenate(vm);
      } else {
        BINARY_OP(OBA_NUMBER, +);
      }
      break;
    }
    case OP_MINUS:
      BINARY_OP(OBA_NUMBER, -);
      break;
    case OP_MULTIPLY:
      BINARY_OP(OBA_NUMBER, *);
      break;
    case OP_DIVIDE:
      BINARY_OP(OBA_NUMBER, /);
      break;
    case OP_NOT: {
      if (!IS_BOOL(peek(vm, 1))) {
        runtimeError(vm, "Expected boolean value");
        return OBA_RESULT_RUNTIME_ERROR;
      }
      push(vm, OBA_BOOL(!AS_BOOL(pop(vm))));
      break;
    }
    case OP_GT:
      BINARY_OP(OBA_BOOL, >);
      break;
    case OP_LT:
      BINARY_OP(OBA_BOOL, <);
      break;
    case OP_GTE:
      BINARY_OP(OBA_BOOL, >=);
      break;
    case OP_LTE:
      BINARY_OP(OBA_BOOL, <=);
      break;
    case OP_EQ: {
      Value b = pop(vm);
      Value a = pop(vm);
      push(vm, OBA_BOOL(valuesEqual(a, b)));
      break;
    }
    case OP_NEQ: {
      Value b = pop(vm);
      Value a = pop(vm);
      push(vm, OBA_BOOL(!valuesEqual(a, b)));
      break;
    }
    case OP_TRUE:
      push(vm, OBA_BOOL(true));
      break;
    case OP_FALSE:
      push(vm, OBA_BOOL(false));
      break;
    case OP_JUMP:
      vm->frame->ip += READ_SHORT();
      break;
    case OP_JUMP_IF_FALSE: {
      if (!IS_BOOL(peek(vm, 1))) {
        runtimeError(vm, "Expected a boolean expression");
        return OBA_RESULT_RUNTIME_ERROR;
      }
      int jump = READ_SHORT();
      bool cond = AS_BOOL(peek(vm, 1));
      if (!cond)
        vm->frame->ip += jump;
      break;
    }
    case OP_JUMP_IF_TRUE: {
      if (!IS_BOOL(peek(vm, 1))) {
        runtimeError(vm, "Expected a boolean expression");
        return OBA_RESULT_RUNTIME_ERROR;
      }
      int jump = READ_SHORT();
      bool cond = AS_BOOL(peek(vm, 1));
      if (cond)
        vm->frame->ip += jump;
      break;
    }
    case OP_JUMP_IF_NOT_MATCH: {
      // TODO(kjharland): Support variable matches.
      int jump = READ_SHORT();
      Value a = peek(vm, 2);
      Value b = pop(vm);
      if (!valuesEqual(b, a)) {
        vm->frame->ip += jump;
        break;
      }
      pop(vm);
      break;
    }
    case OP_LOOP: {
      vm->frame->ip = vm->frame->closure->function->chunk.code + READ_SHORT();
      break;
    }
    case OP_DEFINE_GLOBAL: {
      ObjString* name = READ_STRING();
      tableSet(vm->globals, name, peek(vm, 1));
      pop(vm);
      break;
    }
    case OP_GET_GLOBAL: {
      ObjString* name = READ_STRING();
      Value value;
      if (!tableGet(vm->globals, name, &value)) {
        runtimeError(vm, "Undefined variable: %s", name->chars);
        return OBA_RESULT_RUNTIME_ERROR;
      }
      push(vm, value);
      break;
    }
    case OP_SET_LOCAL: {
      uint8_t slot = READ_BYTE();
      vm->frame->slots[slot] = peek(vm, 1);
      break;
    }
    case OP_GET_LOCAL: {
      // Locals live on the top of the stack.
      uint8_t slot = READ_BYTE();
      push(vm, vm->frame->slots[slot]);
      break;
    }
    case OP_SET_UPVALUE: {
      uint8_t slot = READ_BYTE();
      *vm->frame->closure->upvalues[slot]->location = peek(vm, 1);
      break;
    }
    case OP_GET_UPVALUE: {
      uint8_t slot = READ_BYTE();
      ObjUpvalue* upvalue = vm->frame->closure->upvalues[slot];
      // The user can never get an upvalue directly. Push its captured value
      // onto the stack instead.
      push(vm, *upvalue->location);
      break;
    }
    case OP_CLOSE_UPVALUE:
      closeUpvalue(vm, vm->stackTop - 1);
      pop(vm);
      break;
    case OP_CALL: {
      uint8_t argCount = READ_BYTE();
      if (!callValue(vm, peek(vm, argCount + 1), argCount)) {
        return OBA_RESULT_RUNTIME_ERROR;
      }
      break;
    }
    case OP_CLOSURE: {
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
      break;
    }
    case OP_RETURN:
      return_(vm);
      break;
    case OP_POP:
      pop(vm);
      break;
    case OP_DEBUG: {
      Value value = pop(vm);
      printValue(value);
      printf("\n");
      break;
    }
    case OP_EXIT:
      // Pop the root closure off the stack.
      pop(vm);
      return OBA_RESULT_SUCCESS;
    }
  }
#undef READ_BYTE
}

ObaInterpretResult obaInterpret(ObaVM* vm, const char* source) {
  ObjFunction* function = obaCompile(vm, source);
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
