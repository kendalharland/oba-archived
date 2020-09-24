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
	
}

ObaInterpretResult obaInterpret(ObaVM* vm, const char* source) {
	return OBA_RESULT_SUCCESS;
}

