#ifndef oba_value_h
#define oba_value_h

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#include "oba.h"

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
#define IS_CLOSURE(value) isObjType(value, OBJ_CLOSURE)
#define IS_NATIVE(value) isObjType(value, OBJ_NATIVE)
#define OBJ_TYPE(value) (AS_OBJ(value)->type)

// Macros for converting from Oba to C.
#define AS_BOOL(value) ((value).as.boolean)
#define AS_NUMBER(value) ((value).as.number)
#define AS_OBJ(value) ((value).as.obj)
#define AS_FUNCTION(value) ((ObjFunction*)AS_OBJ(value))
#define AS_CLOSURE(value) ((ObjClosure*)AS_OBJ(value))
#define AS_STRING(value) ((ObjString*)AS_OBJ(value))
#define AS_CSTRING(value) (((ObjString*)AS_OBJ(value))->chars)
#define AS_NATIVE(value) (((ObjNative*)AS_OBJ(value))->function)
#define AS_UPVALUE(value) ((ObjUpvalue*)AS_OBJ(value))

#define TABLE_MAX_LOAD 0.75

// An Oba object in heap memory.
typedef enum {
  OBJ_STRING,
  OBJ_FUNCTION,
  OBJ_CLOSURE,
  OBJ_NATIVE,
  OBJ_UPVALUE,
} ObjType;

typedef struct Obj {
  ObjType type;
  struct Obj* next;
} Obj;

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

typedef struct {
  Obj obj;
  int length;
  char* chars;
  uint32_t hash;
} ObjString;

typedef Value (*NativeFn)(int argc, Value* argv);

struct Builtin {
  const char* name;
  NativeFn function;
};

typedef struct {
  Obj obj;
  NativeFn function;
} ObjNative;

typedef struct {
  ObjString* key;
  Value value;
} Entry;

typedef struct {
  int count;
  int capacity;
  Entry* entries;
} Table;

typedef struct {
  Obj obj;
  Table* variables;
  char* name;
} ObjModule;

static inline bool isObjType(Value value, ObjType type) {
  return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

void initValueArray(ValueArray*);

// Frees the memory held by a [ValueArray] previously allocate with
// [initValueArray].
void freeValueArray(ValueArray*);

// Writes a byte to the given [ValueArray], allocating if necessary.
void writeValueArray(ValueArray*, Value);
bool valuesEqual(Value a, Value b);
void printValue(Value value);

ObjString* copyString(ObaVM* vm, const char* chars, int length);
Obj* allocateObject(ObaVM* vm, size_t size, ObjType type);
void freeObject(Obj*);

ObjString* allocateString(ObaVM* vm, char* chars, int length, uint32_t hash);
ObjString* takeString(ObaVM* vm, char* chars, int length);

ObjNative* newNative(ObaVM*, NativeFn);

void initTable(Table* table);
void freeTable(Table* table);
Entry* findEntry(Entry* entries, int capacity, ObjString* key);
void adjustCapacity(Table* table, int capacity);
bool tableGet(Table* table, ObjString* key, Value* value);
bool tableSet(Table* table, ObjString* key, Value value);

#endif
