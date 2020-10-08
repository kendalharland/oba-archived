#include "oba_function.h"
#include "oba_memory.h"
#include "oba_value.h"

ObjFunction* newFunction() {
  ObjFunction* function = ALLOCATE_OBJ(ObjFunction, OBJ_FUNCTION);
  initChunk(&function->chunk);
  function->arity = 0;
  function->name = NULL;
}

void freeFunction(ObjFunction* function) {
  function->arity = 0;
  function->name = NULL;
  freeChunk(&function->chunk);
  reallocate(function, sizeof(ObjFunction), 0);
}
