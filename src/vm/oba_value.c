#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "oba_memory.h"
#include "oba_value.h"

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

void printValue(Value value) {
  if (IS_NUMBER(value)) {
    printf("%g", AS_NUMBER(value));
  } else if (IS_BOOL(value)) {
    bool v = AS_BOOL(value);
    printf(v ? "true" : "false");
  }
}
