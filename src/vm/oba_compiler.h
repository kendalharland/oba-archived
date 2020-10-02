#ifndef oba_compiler_h
#define oba_compiler_h

#include <stdbool.h>

#include "oba.h"

#define MAX_LOCALS 256

typedef struct sCompiler Compiler;

// Compiles [source], a string of Oba source code.
// Returns true iff an error was encountered while compiling. Code should not
// be executed if so.
bool obaCompile(ObaVM* vm, const char* source);

#endif
