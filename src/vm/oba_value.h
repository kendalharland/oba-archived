#ifndef oba_value_h
#define oba_value_h

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

// Helper macros for coverting to and from Oba values -------------------------

// Macros for converting from C to Oba.
#define OBA_BOOL(value) ((Value){VAL_BOOL, {.boolean = value}})
#define OBA_NUMBER(value) ((Value){VAL_NUMBER, {.number = value}})
#define OBJ_VAL(object) ((Value){VAL_OBJ, {.obj = (Obj*)object}})

// Macros for type-checking.
#define IS_BOOL(value) ((value).type == VAL_BOOL)
#define IS_NUMBER(value) ((value).type == VAL_NUMBER)
#define IS_OBJ(value) ((value).type == VAL_OBJ)
#define IS_STRING(value) isObjType(value, OBJ_STRING)
#define IS_FUNCTION(value) isObjType(value, OBJ_FUNCTION)
#define OBJ_TYPE(value) (AS_OBJ(value)->type)

// Macros for converting from Oba to C.
#define AS_BOOL(value) ((value).as.boolean)
#define AS_NUMBER(value) ((value).as.number)
#define AS_OBJ(value) ((value).as.obj)
#define AS_FUNCTION(value) ((ObjFunction*)AS_OBJ(value))
#define AS_STRING(value) ((ObjString*)AS_OBJ(value))
#define AS_CSTRING(value) (((ObjString*)AS_OBJ(value))->chars)

// An Oba object in heap memory.
typedef enum {
  OBJ_STRING,
  OBJ_FUNCTION,
} ObjType;

typedef struct {
  ObjType type;
} Obj;

typedef struct {
  Obj obj;
  int length;
  char* chars;
  uint32_t hash;
} ObjString;

typedef enum { TYPE_FUNCTION, TYPE_SCRIPT } FunctionType;

// A tagged-union representing Oba values.
typedef enum {
  VAL_BOOL,
  VAL_NUMBER,
  VAL_OBJ,
} ValueType;

typedef struct {
  ValueType type;
  union {
    bool boolean;
    double number;
    Obj* obj;
  } as;
} Value;

// ValueArray is a dynamic array of oba values.
typedef struct {
  int capacity;
  int count;
  Value* values;
} ValueArray;

static inline bool isObjType(Value value, ObjType type) {
  return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

void initValueArray(ValueArray*);

// Frees the memory held by a [ValueArray] previously allocate with
// [initValueArray].
void freeValueArray(ValueArray*);

// Writes a byte to the given [ValueArray], allocating if necessary.
void writeValueArray(ValueArray*, Value);

void printValue(Value value);

ObjString* copyString(const char* chars, int length);
Obj* allocateObject(size_t size, ObjType type);
ObjString* allocateString(char* chars, int length, uint32_t hash);
ObjString* takeString(char* chars, int length);
bool valuesEqual(Value a, Value b);

#endif
