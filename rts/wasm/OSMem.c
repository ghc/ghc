// Note [Megablock allocator on wasm]
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Wasm modules operate on a continuous linear memory, and one can use
// the memory.size/memory.grow opcodes to query the current memory
// size and extend it, both of which operates in the units of 64KiB
// pages. Compared to typical unix processes, the Wasm memory address
// space doesn't have page-level read/write access restriction,
// doesn't have holes that segfaults when accessed, and there's no
// native mmap().

// It's possible to have userland mmap() emulation in Wasm that only
// allows mapping anonymous pages by calling malloc(). But the way our
// unix megablock allocator uses mmap() doesn't work well with
// existing emulations: we over-allocate then trim to guarantee
// alignment, and we do a lot of partial munmap()s. So we might as
// well implement our own OSMem.c logic for wasm.
//
// To allocate megablocks, it's possible to call aligned_alloc()
// directly. But there's still significant space waste: when
// aligned_alloc() is called consecutively to allocate multiple
// megablocks, there are gaps no smaller than 1 megablock between
// returned spaces. This is natural, since malloc() implementations
// tend to use extra header/footer around allocated space to maintain
// internal metadata (tested with dlmalloc/emmalloc/mimalloc). It's
// possible to mitigate the space waste issue by adding custom pooling
// logic, but there's a better solution here.
//
// wasi-libc uses dlmalloc, which calls sbrk() to grab memory from the
// system, which calls memory.grow under the hood. In unix programs,
// it's typically a bad practice to call both malloc() and sbrk()
// within the same address space, since sbrk() calls may violate the
// libc allocator's certain invariants. But dlmalloc permits this
// behavior!
//
// Therefore, we bypass dlmalloc, and directly call sbrk() to
// allocate megablocks.
//
// One remaining question is how to free a megablock. Wasm spec
// doesn't allow shrinking the linear memory, so the logic of
// returning memory to the OS doesn't make sense here. Given the
// megablock allocator already has its internal free-list, we avoid
// writing our own pooling logic, and simply avoid returning any
// megablock on Wasm.

#include "Rts.h"
#include "sm/OSMem.h"

#include <unistd.h>

#define PAGESIZE (0x10000)

GHC_STATIC_ASSERT(MBLOCK_SIZE == PAGESIZE, "MBLOCK_SIZE must be equal to wasm page size");

void osMemInit(void)
{
}

void *
osGetMBlocks(uint32_t n)
{
  return sbrk(PAGESIZE * n);
}

void osBindMBlocksToNode(
    void *addr STG_UNUSED,
    StgWord size STG_UNUSED,
    uint32_t node STG_UNUSED)
{
}

StgWord64 getPhysicalMemorySize (void)
{
    return 1ULL << 32;
}

uint32_t osNumaNodes(void)
{
    return 1;
}

uint64_t osNumaMask(void)
{
    return 1;
}
