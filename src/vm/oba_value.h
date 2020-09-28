#ifndef oba_value_h
#define oba_value_h

#include <stdlib.h>

// TODO(kendal):
// - VM stack holds doubles; needs to hold Values.
// - Compiler emits double consts; needs to emit Values (strings too).
// -

// Helper macros for coverting to and from Oba values -------------------------

#define OBA_NUMBER(value) ((Value){VAL_NUMBER, {.number = value}})

#define IS_NUMBER(value) ((value).type == VAL_NUMBER)

#define AS_NUMBER(value) ((value).as.number)

// A tagged-union representing Oba values.
typedef enum {
  VAL_NUMBER
  // TODO(kendal): Bools, objects, strings.
} ValueType;

typedef struct {
  ValueType type;
  union {
    double number;
  } as;
} Value;

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

void printValue(Value value);

#endif
