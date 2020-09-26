#include <stdlib.h>
#include <string.h>

#include "oba.h"
#include "oba_vm.h"

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
	Chunk chunk;
	initChunk(&chunk);
	vm->chunk = &chunk;

  obaCompile(vm, source);
	// TODO(kendal): Disassemble the compiled chunks.
  // TODO(kendal): Interpret the compiled chunks.
  return OBA_RESULT_SUCCESS;
}
