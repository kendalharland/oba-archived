#ifndef oba_function_h
#define oba_function_h

#include <stdint.h>
#include <stdlib.h>

#include "oba_opcodes.h"
#include "oba_value.h"

typedef struct {
  Obj obj;
  Chunk chunk;
  int arity;
  ObjString* name;
} ObjFunction;

typedef struct {
  ObjFunction* function;
  uint8_t* ip;
  Value* slots;
} CallFrame;

ObjFunction* newFunction();
void freeFunction(ObjFunction*);

#endif
