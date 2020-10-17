#ifndef oba_function_h
#define oba_function_h

#include <stdint.h>
#include <stdlib.h>

#include "oba_opcodes.h"
#include "oba_value.h"

typedef struct {
  Obj obj;
  Value* location;
} ObjUpvalue;

typedef struct {
  Obj obj;
  Chunk chunk;
  int arity;
  ObjString* name;
  int upvalueCount;
} ObjFunction;

// An instance of ObjFunction which captures the values in the function's
// lexical scope at runtime.
typedef struct {
  Obj obj;
  ObjFunction* function;
  ObjUpvalue** upvalues;
  int upvalueCount;
} ObjClosure;

typedef struct {
  ObjClosure* closure;
  uint8_t* ip;
  Value* slots;
} CallFrame;

ObjFunction* newFunction();
void freeFunction(ObjFunction*);

ObjClosure* newClosure();
void freeClosure(ObjClosure*);

ObjUpvalue* newUpvalue(Value*);
void freeUpvalue(ObjUpvalue*);

#endif
