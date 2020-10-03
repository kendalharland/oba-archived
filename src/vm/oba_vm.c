#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "oba.h"
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

static void resetStack(ObaVM* vm) { vm->stackTop = vm->stack; }

ObaVM* obaNewVM() {
  // TODO(kendal): sizeof(ObaVM) here instead?
  ObaVM* vm = (ObaVM*)realloc(NULL, sizeof(*vm));
  memset(vm, 0, sizeof(ObaVM));

  vm->globals = (Table*)realloc(NULL, sizeof(Table));
  initTable(vm->globals);
  resetStack(vm);
  return vm;
}

void obaFreeVM(ObaVM* vm) {
  freeChunk(vm->chunk);
  freeTable(vm->globals);
  vm->stackTop = NULL;
  free(vm);
  vm = NULL;
}

static Value peek(ObaVM* vm, int lookahead) {
  return *(vm->stackTop - lookahead);
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

static void concatenate(ObaVM* vm) {
  ObjString* b = AS_STRING(pop(vm));
  ObjString* a = AS_STRING(pop(vm));

  char* chars = ALLOCATE(char, b->length + a->length);
  int length = b->length + a->length;

  memcpy(chars, a->chars, a->length);
  memcpy(chars + a->length, b->chars, b->length);
  chars[length] = '\0';

  ObjString* result = takeString(chars, length);
  push(vm, OBJ_VAL(result));
}

static ObaInterpretResult run(ObaVM* vm) {

  // clang-format off

#define READ_BYTE() (*vm->ip++)
#define READ_SHORT() \
  (vm->ip += 2, (uint16_t)((vm->ip[-2] << 8) | vm->ip[-1]))
#define READ_CONSTANT() (vm->chunk->constants.values[READ_BYTE()])
#define READ_STRING() AS_STRING(READ_CONSTANT())

#define BINARY_OP(type, op)                                                    \
do {                                                                           \
  if (IS_NUMBER(peek(vm, 1)) && IS_NUMBER(peek(vm, 2))) {                      \
    double b = AS_NUMBER(pop(vm));                                             \
    double a = AS_NUMBER(pop(vm));                                             \
    push(vm, type(a op b));                                                    \
  } else if (IS_STRING(peek(vm, 1)) && IS_STRING(peek(vm, 2))) {               \
    concatenate(vm);                                                           \
  } else {                                                                     \
    runtimeError(vm, "Expected numeric or string operands");                   \
    return OBA_RESULT_RUNTIME_ERROR;                                           \
  }                                                                            \
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
    // TODO(kendal): Bug! This will concatenate if the operands are strings.
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
    case OP_JUMP_IF_FALSE: {
      if (!IS_BOOL(peek(vm, 1))) {
        runtimeError(vm, "Expected a boolean expression");
        return OBA_RESULT_RUNTIME_ERROR;
      }
      int jump = READ_SHORT();
      bool cond = AS_BOOL(peek(vm, 1));
      if (!cond)
        vm->ip += jump;
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
        vm->ip += jump;
      break;
    }
    case OP_JUMP_IF_NOT_MATCH: {
      // TODO(kjharland): Support variable matches.
      int jump = READ_SHORT();
      Value a = peek(vm, 2);
      Value b = pop(vm);
      if (!valuesEqual(b, a)) {
        vm->ip += jump;
      }
      break;
    }
    case OP_LOOP: {
      vm->ip = vm->chunk->code + READ_SHORT();
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
      vm->stack[slot] = peek(vm, 1);
      break;
    }
    case OP_GET_LOCAL: {
      // Locals live on the top of the stack.
      uint8_t slot = READ_BYTE();
      push(vm, vm->stack[slot]);
      break;
    }
    case OP_SWAP_STACK_TOP: {
      Value top = pop(vm);
      Value next = pop(vm);
      push(vm, top);
      push(vm, next);
      break;
    }
    case OP_POP:
      pop(vm);
      break;
    case OP_DEBUG: {
      Value value = pop(vm);
      printf("DEBUG: ");
      printValue(value);
      printf("\n");
      break;
    }
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
