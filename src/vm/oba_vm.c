#include <stdlib.h>
#include <string.h>

#include "oba.h"
#include "oba_vm.h"

#ifdef DEBUG_TRACE_EXECUTION
#include "oba_debug.h"
#endif

ObaVM* obaNewVM() {
  // TODO(kendal): sizeof(ObaVM) here instead?
  ObaVM* vm = (ObaVM*)realloc(NULL, sizeof(*vm));
  memset(vm, 0, sizeof(ObaVM));
  return vm;
}

void obaFreeVM(ObaVM* vm) {
  freeChunk(vm->chunk);
  free(vm);
  vm = NULL;
}

ObaInterpretResult obaInterpret(ObaVM* vm, const char* source) {
  obaCompile(vm, source);
#ifdef DEBUG_TRACE_EXECUTION
  disassemble(vm->chunk, "interpret");
#endif
  return OBA_RESULT_SUCCESS;
}
