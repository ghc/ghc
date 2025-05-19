#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>

// Excerpted from ProddableBlocks.h
typedef struct {
    size_t size;
    size_t capacity;
    // sorted list of disjoint (start,end) pairs
    struct _ProddableBlock *data;
} ProddableBlockSet;

void initProddableBlockSet ( ProddableBlockSet* set );
void addProddableBlock ( ProddableBlockSet* set, void* start, size_t size );
bool containsSpan ( const ProddableBlockSet *set, uintptr_t start, uintptr_t end );

int main () {
  ProddableBlockSet set;
  initProddableBlockSet(&set);
  addProddableBlock(&set, (void*) 0x20, 0x10);
  addProddableBlock(&set, (void*) 0x30, 0x10);
  addProddableBlock(&set, (void*) 0x100, 0x10);

  assert( containsSpan(&set, 0x20, 0x30));
  assert( containsSpan(&set, 0x30, 0x29));
  assert(!containsSpan(&set, 0x30, 0x49));
  assert(!containsSpan(&set, 0x60, 0x70));
  assert(!containsSpan(&set, 0x90, 0x110));
  assert( containsSpan(&set, 0x100, 0x101));
  return 0;
}

