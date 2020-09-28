#ifndef oba_value_h
#define oba_value_h

#include <stdbool.h>
#include <stdlib.h>

// Helper macros for coverting to and from Oba values -------------------------

#define OBA_NUMBER(value) ((Value){VAL_NUMBER, {.number = value}})
#define OBA_BOOL(value) ((Value){VAL_BOOL, {.boolean = value}})

#define IS_NUMBER(value) ((value).type == VAL_NUMBER)
#define IS_BOOL(value) ((value).type == VAL_BOOL)

#define AS_NUMBER(value) ((value).as.number)
#define AS_BOOL(value) ((value).as.boolean)

// A tagged-union representing Oba values.
typedef enum {
  VAL_NUMBER,
  VAL_BOOL,
  // TODO(kendal): objects.
  // TODO(kendal): strings.
} ValueType;

typedef struct {
  ValueType type;
  union {
    bool boolean;
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
