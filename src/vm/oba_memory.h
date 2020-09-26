#ifndef oba_memory_h
#define oba_memory_h

#include <stdlib.h>

#define GROW_CAPACITY(cap)                                                     \
  ((cap) < 8 ? 8 : (cap)*2)

#define GROW_ARRAY(type, pointer, oldCount, newCount)                          \
  (type*)reallocate(pointer, sizeof(type) * (oldCount),                        \
                    sizeof(type) * (newCount))

#define FREE_ARRAY(type, pointer, oldCount)                                    \
  reallocate(pointer, sizeof(type) * oldCount, 0)

// Reallocates [pointer] from [oldSize] to [newSize].
// If [newSize] is 0, [pointer] is freed.
void* reallocate(void* pointer, size_t oldSize, size_t newSize);

#endif
