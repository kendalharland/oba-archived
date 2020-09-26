#ifndef oba_value_h
#define oba_value_h

#include <stdlib.h>

// TODO(kendal): Replace this with the appropriate type.
typedef double Value;

// ValueArray is a dynamic array of oba values.
typedef struct {
  int capacity;
  int count;
  Value* values;
} ValueArray;

void initValueArray(ValueArray*);

// Frees the memory held by a [ValueArray] previously allocate with
// [initValueArray].
void freeValueArray(ValueArray*);

// Writes a byte to the given [ValueArray], allocating if necessary.
void writeValueArray(ValueArray*, Value);

#endif
