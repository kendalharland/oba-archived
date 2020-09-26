#ifndef oba_memory_h
#define oba_memory_h

#include <stdlib.h>

#define MIN_CAPACITY 8
#define CAPACITY_RESIZE_RATE 2

#define GROW_CAPACITY(cap)                                                     \
  ((cap) < MIN_CAPACITY ? MIN_CAPACITY : (cap)*CAPACITY_RESIZE_RATE)

#define GROW_ARRAY(type, pointer, oldCount, newCount)                          \
  (type*)reallocate(pointer, sizeof(type) * (oldCount),                        \
                    sizeof(type) * (newCount))

#define FREE_ARRAY(type, pointer, oldCount)                                    \
  (type*)reallocate(pointer, sizeof(type) * oldCount, 0)

// Reallocates [pointer] from [oldSize] to [newSize].
// If [newSize] is 0, [pointer] is freed.
void* reallocate(void* pointer, size_t oldSize, size_t newSize);

#endif
