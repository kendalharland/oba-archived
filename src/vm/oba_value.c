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

static Obj* allocateObject(size_t size, ObjType type) {
  Obj* object = (Obj*)reallocate(NULL, 0, size);
  object->type = type;
  return object;
}

static ObjString* allocateString(char* chars, int length) {
  ObjString* string = ALLOCATE_OBJ(ObjString, OBJ_STRING);
  string->length = length;
  string->chars = chars;

  return string;
}

ObjString* copyString(const char* chars, int length) {
  char* heapChars = ALLOCATE(char, length + 1);
  memcpy(heapChars, chars, length);
  heapChars[length] = '\0';

  return allocateString(heapChars, length);
}

ObjString* takeString(char* chars, int length) {
  return allocateString(chars, length);
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

void printObject(Value value) {
  Obj* obj = AS_OBJ(value);
  switch (obj->type) {
  case OBJ_STRING:
    printf("%s", AS_CSTRING(value));
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
