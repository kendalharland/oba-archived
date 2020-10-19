#ifndef oba_memory_h
#define oba_memory_h

#include <stdlib.h>

#define GROW_CAPACITY(cap) ((cap) < 8 ? 8 : (cap)*2)

#define GROW_ARRAY(type, pointer, oldCount, newCount)                          \
  (type*)reallocate(pointer, sizeof(type) * (oldCount),                        \
                    sizeof(type) * (newCount))

#define FREE_ARRAY(type, pointer, oldCount)                                    \
  reallocate(pointer, sizeof(type) * oldCount, 0)

#define ALLOCATE(type, count) (type*)reallocate(NULL, 0, sizeof(type) * (count))

#define ALLOCATE_OBJ(vm, type, objectType)                                     \
  (type*)allocateObject(vm, sizeof(type), objectType)

#define FREE(type, pointer) reallocate(pointer, sizeof(type), 0)

// Reallocates [pointer] from [oldSize] to [newSize].
// If [newSize] is 0, [pointer] is freed.
void* reallocate(void* pointer, size_t oldSize, size_t newSize);

#endif
