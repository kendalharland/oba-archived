#ifndef oba_function_h
#define oba_function_h

#include <stdint.h>
#include <stdlib.h>

#include "oba.h"
#include "oba_chunk.h"
#include "oba_value.h"

// ObjUpvalue represents a value that is closed over by a function.
//
// Most functions point to values that live on the stack. A closure may outlive
// the values that it closes over. When such values are popped off the stack,
// the VM hoists them into this object to ensure they live as long as the
// closures that reference them.
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
  ObjString* name;

  // The bytecode that executes this function.
  Chunk chunk;

  // The number of arguments this function accepts.
  int arity;

  // The number of upvalues this function closes over.
  int upvalueCount;

  // The module where this function is defined.
  ObjModule* module;
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

ObjFunction* newFunction(ObaVM*, ObjModule*);
void freeFunction(ObjFunction*);

ObjClosure* newClosure(ObaVM*, ObjFunction*);
void freeClosure(ObjClosure*);

ObjUpvalue* newUpvalue(ObaVM*, Value*);
void freeUpvalue(ObjUpvalue*);

#endif
