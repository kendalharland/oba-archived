#ifndef oba_compiler_h
#define oba_compiler_h

#include "oba.h"

typedef struct sCompiler Compiler;

// Compiles [source], a string of Oba source code.
int obaCompile(ObaVM* vm, const char* source);

#endif
