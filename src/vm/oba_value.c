#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "oba_memory.h"
#include "oba_value.h"
#include "oba_vm.h"

void printFunction(ObjFunction* function) {
  if (function->name != NULL) {
    printf("<fn %s::%s>", function->module->name->chars, function->name->chars);
  } else {
    // The function is still being compiled and this value is being printed
    // while the function is on the stack, probably as a debugging message.
    printf("<fn>");
  }
}

void printObject(Value value) {
  Obj* obj = AS_OBJ(value);
  switch (obj->type) {
  case OBJ_CLOSURE:
    printFunction(AS_CLOSURE(value)->function);
    break;
  case OBJ_FUNCTION:
    printFunction(AS_FUNCTION(value));
    break;
  case OBJ_STRING:
    printf("%s", AS_CSTRING(value));
    break;
  case OBJ_NATIVE:
    printf("<native fn>");
    break;
  case OBJ_UPVALUE:
    printValue(*(AS_UPVALUE(value)->location));
    break;
  case OBJ_MODULE: {
    ObjModule* module = (ObjModule*)obj;
    printf("<module %s>", module->name->chars);
    break;
  }
  default:
    break; // Unreachable
  }
}

Obj* allocateObject(ObaVM* vm, size_t size, ObjType type) {
#ifdef DEBUG_TRACE_EXECUTION
  printf("allocate object type: %d size %d\n", type, size);
#endif
  Obj* object = (Obj*)reallocate(NULL, 0, size);
  object->type = type;
  object->next = vm->objects;
  vm->objects = object;
  return object;
}

void freeObject(Obj* obj) {
#ifdef DEBUG_TRACE_EXECUTION
  printf("free object type: %d\n", obj->type);
  printObject(OBJ_VAL(obj));
  printf(" @ %p\n", obj);
#endif

  switch (obj->type) {
  case OBJ_STRING: {
    ObjString* string = (ObjString*)obj;
    FREE_ARRAY(char, string->chars, string->length + 1);
    FREE(ObjString, obj);
    break;
  }
  case OBJ_NATIVE:
    FREE(ObjNative, obj);
    break;
  case OBJ_FUNCTION: {
    ObjFunction* function = (ObjFunction*)obj;
    freeChunk(&function->chunk);
    FREE(ObjFunction, obj);
    break;
  }
  case OBJ_CLOSURE: {
    ObjClosure* closure = (ObjClosure*)obj;
    FREE_ARRAY(ObjUpvalue*, closure->upvalues, closure->upvalueCount);
    FREE(ObjClosure, obj);
    break;
  }
  case OBJ_UPVALUE:
    FREE(ObjUpvalue, obj);
    break;
  case OBJ_MODULE: {
    ObjModule* module = (ObjModule*)obj;
    freeTable(module->variables);
    FREE(ObjModule, obj);
    break;
  }
  }
}

void initValueArray(ValueArray* array) {
  array->capacity = 0;
  array->count = 0;
  array->values = NULL;
}

void freeValueArray(ValueArray* array) {
  FREE_ARRAY(Value, array->values, array->capacity);
  initValueArray(array);
}

void writeValueArray(ValueArray* array, Value value) {
  if (array->capacity <= array->count) {
    int oldCap = array->capacity;
    array->capacity = GROW_CAPACITY(oldCap);
    array->values = GROW_ARRAY(Value, array->values, oldCap, array->capacity);
  }

  array->values[array->count] = value;
  array->count++;
}

// FNV-1a hash function
// https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function.
ObjString* allocateString(ObaVM* vm, char* chars, int length, uint32_t hash) {
  ObjString* string = ALLOCATE_OBJ(vm, ObjString, OBJ_STRING);
  string->length = length;
  string->chars = chars;
  string->hash = hash;

  return string;
}

static uint32_t hashString(const char* key, int length) {
  uint32_t hash = 2166136261u;

  for (int i = 0; i < length; i++) {
    hash ^= key[i];
    hash *= 16777619;
  }

  return hash;
}

ObjString* copyString(ObaVM* vm, const char* chars, int length) {
  char* heapChars = ALLOCATE(char, length + 1);
  memcpy(heapChars, chars, length);
  heapChars[length] = '\0';
  uint32_t hash = hashString(heapChars, length);

  return allocateString(vm, heapChars, length, hash);
}

ObjString* takeString(ObaVM* vm, char* chars, int length) {
  uint32_t hash = hashString(chars, length);
  return allocateString(vm, chars, length, hash);
}

ObjNative* newNative(ObaVM* vm, NativeFn function) {
  ObjNative* native = ALLOCATE_OBJ(vm, ObjNative, OBJ_NATIVE);
  native->function = function;
  return native;
}

ObjModule* newModule(ObaVM* vm, ObjString* name) {
  ObjModule* module = ALLOCATE_OBJ(vm, ObjModule, OBJ_MODULE);
  module->name = name;
  module->variables = (Table*)reallocate(NULL, 0, sizeof(Table));
  initTable(module->variables);
  return module;
}

bool objectsEqual(Value ao, Value bo) {
  if (OBJ_TYPE(ao) != OBJ_TYPE(bo))
    return false;

  switch (OBJ_TYPE(ao)) {
  case OBJ_STRING: {
    ObjString* a = AS_STRING(ao);
    ObjString* b = AS_STRING(bo);
    return a->length == b->length && strcmp(a->chars, b->chars) == 0;
  }
  case OBJ_FUNCTION: {
    ObjFunction* a = AS_FUNCTION(ao);
    ObjFunction* b = AS_FUNCTION(bo);
    return a == b;
  }
  case OBJ_NATIVE: {
    NativeFn a = AS_NATIVE(ao);
    NativeFn b = AS_NATIVE(bo);
    return a == b;
  }
  default:
    return false; // Unreachable.
  }
}

bool valuesEqual(Value a, Value b) {
  if (a.type != b.type)
    return false;

  switch (a.type) {
  case VAL_BOOL:
    return AS_BOOL(a) == AS_BOOL(b);
  case VAL_NUMBER:
    return AS_NUMBER(a) == AS_NUMBER(b);
  case VAL_OBJ:
    return objectsEqual(a, b);
  default:
    return false; // Unreachable.
  }
}

void printValue(Value value) {
  switch (value.type) {
  case VAL_NUMBER:
    printf("%g", AS_NUMBER(value));
    break;
  case VAL_BOOL:
    printf(AS_BOOL(value) ? "true" : "false");
    break;
  case VAL_OBJ:
    printObject(value);
    break;
  }
}

void initTable(Table* table) {
  table->count = 0;
  table->capacity = 0;
  table->entries = NULL;
}

void freeTable(Table* table) {
  FREE_ARRAY(Entry, table->entries, table->capacity);
  initTable(table);
}

Entry* findEntry(Entry* entries, int capacity, ObjString* key) {
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

void adjustCapacity(Table* table, int capacity) {
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

bool tableSet(Table* table, ObjString* key, Value value) {
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
