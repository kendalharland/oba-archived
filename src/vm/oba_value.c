#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "oba_memory.h"
#include "oba_value.h"
#include "oba_vm.h"

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

Obj* allocateObject(ObaVM* vm, size_t size, ObjType type) {
  Obj* object = (Obj*)reallocate(NULL, 0, size);
  object->type = type;

  object->next = vm->objects;
  vm->objects = object;
  return object;
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

ObjNative* newNative(ObaVM* vm, NativeFn function) {
  ObjNative* native = ALLOCATE_OBJ(vm, ObjNative, OBJ_NATIVE);
  native->function = function;
  return native;
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

void freeObject(Obj* obj) {
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
  }
}

bool objectsEqual(Value ao, Value bo) {
  if (OBJ_TYPE(ao) != OBJ_TYPE(bo))
    return false;

  // TODO(kendal): Closures.
  switch (OBJ_TYPE(ao)) {
  case OBJ_STRING: {
    ObjString* a = AS_STRING(ao);
    ObjString* b = AS_STRING(bo);
    return a->length == b->length && strcmp(a->chars, b->chars) == 0;
  }
  case OBJ_FUNCTION: {
    // TODO(kendal): Test this.
    ObjFunction* a = AS_FUNCTION(ao);
    ObjFunction* b = AS_FUNCTION(bo);
    return a == b;
  }
  case OBJ_NATIVE: {
    // TODO(kendal): Test this.
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

void printFunction(ObjFunction* function) { printf("%s", function->name); }

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
