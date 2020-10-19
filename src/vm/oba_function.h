#ifndef oba_function_h
#define oba_function_h

#include <stdint.h>
#include <stdlib.h>

#include "oba.h"
#include "oba_opcodes.h"
#include "oba_value.h"

typedef struct ObjUpvalue {
  Obj obj;

  // A pointer to the stack location of the local captured by this upvalue.
  Value* location;

  // The value of the local captured by this upvalue.
  // This is uninitialized until the upvalue is closed.
  Value closed;

  struct ObjUpvalue* next;
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

ObjFunction* newFunction(ObaVM*);
void freeFunction(ObjFunction*);

ObjClosure* newClosure(ObaVM*, ObjFunction*);
void freeClosure(ObjClosure*);

ObjUpvalue* newUpvalue(ObaVM*, Value*);
void freeUpvalue(ObjUpvalue*);

#endif
