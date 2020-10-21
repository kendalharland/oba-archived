#ifndef oba_builtin_h
#define oba_builtin_h

#include <time.h>
#include <unistd.h>

#include "oba.h"
#include "oba_value.h"

// TODO(kendal): Support error reporting in natives.

Value sleepNative(ObaVM* vm, int argc, Value* argv) {
  // Assume argc == 1 and argv != NULL
  Value seconds = argv[0];
  unsigned int remaining = (unsigned int)AS_NUMBER(seconds);
  while (remaining > 0)
    remaining = sleep(remaining);
  return OBA_NUMBER(0);
}

Value nowNative(ObaVM* vm, int argc, Value* argv) {
  return OBA_NUMBER((double)clock() / CLOCKS_PER_SEC);
}

Value readByteNative(ObaVM* vm, int argc, Value* argv) {
  int c;
  if ((c = getchar()) == EOF) {
    return NIL_VAL;
  }
  const char byte = (const char)c;
  return OBJ_VAL(copyString(vm, &byte, 1));
}

Value readLineNative(ObaVM* vm, int argc, Value* argv) {
  char* line = NULL;
  size_t length;
  if (getline(&line, &length, stdin) == -1) {
    return NIL_VAL;
    // TODO: if (feof(stdin)) { /* nil */ }  else { /* error */ }
  }
  Value value = OBJ_VAL(copyString(vm, line, length));
  free(line);
  return value;
}

Builtin __builtins__[] = {
    {"__native_sleep", &sleepNative},
    {"__native_now", &nowNative},
    {"__native_read_byte", &readByteNative},
    {"__native_read_line", &readLineNative},
    {NULL, NULL}, // Sentinel to mark the end of the array.
};

#endif
