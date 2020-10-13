#ifndef oba_builtin_h
#define oba_builtin_h

#include <unistd.h>

#include "oba.h"
#include "oba_value.h"

// TODO(kendal): Support error reporting in natives.

Value sleepNative(int argc, Value* argv) {
  // Assume argc == 1 and argv != NULL
  Value seconds = argv[0];
  unsigned int remaining = (unsigned int)AS_NUMBER(seconds);
  while (remaining > 0)
    remaining = sleep(remaining);
  return OBA_NUMBER(0);
}

Builtin __builtins__[] = {
    {"sleep", &sleepNative},
    {NULL, NULL}, // Sentinel to mark the end of the array.
};

#endif
