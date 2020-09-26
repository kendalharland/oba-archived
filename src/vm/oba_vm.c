#include <stdlib.h>
#include <string.h>

#include "oba.h"
#include "oba_vm.h"

ObaVM* obaNewVM() {
  // TODO(kendal): sizeof(ObaVM) here instead?
  ObaVM* vm = (ObaVM*)realloc(NULL, sizeof(*vm));
  memset(vm, 0, sizeof(ObaVM));
	initChunk(vm->chunk);
  return vm;
}

void obaFreeVM(ObaVM* vm) {
	freeChunk(vm->chunk);
  free(vm);
  vm = NULL;
}

ObaInterpretResult obaInterpret(ObaVM* vm, const char* source) {
  obaCompile(vm, source);
  // TODO(kjharland): Interpret the compiled chunks.
  return OBA_RESULT_SUCCESS;
}
