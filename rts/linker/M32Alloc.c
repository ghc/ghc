/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2000-2012
 *
 * RTS Object Linker
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "sm/OSMem.h"
#include "linker/M32Alloc.h"
#include "LinkerInternals.h"

#include <inttypes.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/*

Note [Compile Time Trickery]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This file implements two versions of each of the `m32_*` functions. At the top
of the file there is the real implementation (compiled in when
`RTS_LINKER_USE_MMAP` is true) and a dummy implementation that exists only to
satisfy the compiler and which hould never be called. If any of these dummy
implementations are called the program will abort.

The rationale for this is to allow the calling code to be written without using
the C pre-processor (CPP) `#if` hackery. The value of `RTS_LINKER_USE_MMAP` is
known at compile time, code like:

    if (RTS_LINKER_USE_MMAP)
        m32_allocator_init();

will be compiled to call to `m32_allocator_init` if  `RTS_LINKER_USE_MMAP` is
true and will be optimised awat to nothing if `RTS_LINKER_USE_MMAP` is false.
However, regardless of the value of `RTS_LINKER_USE_MMAP` the compiler will
still check the call for syntax and correct function parameter types.

*/

#if RTS_LINKER_USE_MMAP == 1

/*

Note [M32 Allocator]
~~~~~~~~~~~~~~~~~~~~

A memory allocator that allocates only pages in the 32-bit range (lower 2GB).
This is useful on 64-bit platforms to ensure that addresses of allocated
objects can be referenced with a 32-bit relative offset.

Initially, the linker used `mmap` to allocate a page per object. Hence it
wasted a lot of space for small objects (see #9314). With this allocator, we
try to fill pages as much as we can for small objects.

How does it work?
-----------------

For small objects, a Word64 counter is added at the beginning of the page they
are stored in. It indicates the number of objects that are still alive in the
page. When the counter drops down to zero, the page is freed. The counter is
atomically decremented, hence the deallocation is thread-safe.

During the allocation phase, the allocator keeps track of some pages that are
not totally filled: the number of pages in the "filling" list is configurable
with M32_MAX_PAGES. Allocation consists in finding some place in one of these
pages or starting a new one, then increasing the page counter. If none of the
pages in the "filling" list has enough free space, the most filled one is
flushed (see below) and a new one is allocated.

The allocator holds a reference on pages in the "filling" list: the counter in
these pages is 1+n where n is the current number of objects allocated in the
page. Hence allocated objects can be freed while the allocator is using
(filling) the page. Flushing a page consists in decreasing its counter and
removing it from the "filling" list. By extension, flushing the allocator
consists in flushing all the pages in the "filling" list.  Don't forget to
flush the allocator at the end of the allocation phase in order to avoid space
leaks!

Large objects are objects that are larger than a page (minus the bytes required
for the counter and the optional padding). These objects are allocated into
their own set of pages.  We can differentiate large and small objects from
their address: large objects are aligned on page size while small objects never
are (because of the space reserved for the page's object counter).

For large objects, the remaining space at the end of the last page is left
unused by the allocator. It can be used with care as it will be freed with the
associated large object. GHC linker uses this feature/hack, hence changing the
implementation of the M32 allocator must be done with care (i.e. do not try to
improve the allocator to avoid wasting this space without modifying the linker
code accordingly).

Object allocation is *not* thread-safe (however it could be done easily with a
lock in the allocator structure). Object deallocation is thread-safe.

*/

#define ROUND_UP(x,size) ((x + size - 1) & ~(size - 1))
#define ROUND_DOWN(x,size) (x & ~(size - 1))

/****************************************************************************
 * M32 ALLOCATOR (see Note [M32 Allocator]
 ***************************************************************************/

#define M32_MAX_PAGES 32
#define M32_REFCOUNT_BYTES 8


/**
 * An allocated page being filled by the allocator
 */
struct m32_alloc_t {
   void * base_addr;             // Page address
   size_t current_size;          // Number of bytes already reserved
};

/**
 * Allocator
 *
 * Currently an allocator is just a set of pages being filled. The maximum
 * number of pages can be configured with M32_MAX_PAGES.
 */
typedef struct m32_allocator_t {
   struct m32_alloc_t pages[M32_MAX_PAGES];
} m32_allocator;

// We use a global memory allocator
static struct m32_allocator_t alloc;

/**
 * Wrapper for `unmap` that handles error cases.
 * This is the real implementation. There is another dummy implementation below.
 * See the note titled "Compile Time Trickery" at the top of this file.
 */
static void
munmapForLinker (void * addr, size_t size)
{
   int r = munmap(addr,size);
   if (r == -1) {
      // Should we abort here?
      sysErrorBelch("munmap");
   }
}

/**
 * Initialize the allocator structure
 * This is the real implementation. There is another dummy implementation below.
 * See the note titled "Compile Time Trickery" at the top of this file.
 */
void
m32_allocator_init(void)
{
   memset(&alloc, 0, sizeof(struct m32_allocator_t));
   // Preallocate the initial M32_MAX_PAGES to ensure that they don't
   // fragment the memory.
   size_t pgsz = getPageSize();
   char* bigchunk = mmapForLinker(pgsz * M32_MAX_PAGES,MAP_ANONYMOUS,-1,0);
   if (bigchunk == NULL)
       barf("m32_allocator_init: Failed to map");

   int i;
   for (i=0; i<M32_MAX_PAGES; i++) {
      alloc.pages[i].base_addr = bigchunk + i*pgsz;
      *((uintptr_t*)alloc.pages[i].base_addr) = 1;
      alloc.pages[i].current_size = M32_REFCOUNT_BYTES;
   }
}

/**
 * Atomically decrement the object counter on the given page and release the
 * page if necessary. The given address must be the *base address* of the page.
 *
 * You shouldn't have to use this method. Use `m32_free` instead.
 */
static void
m32_free_internal(void * addr) {
   uintptr_t c = __sync_sub_and_fetch((uintptr_t*)addr, 1);
   if (c == 0) {
      munmapForLinker(addr, getPageSize());
   }
}

/**
 * Release the allocator's reference to pages on the "filling" list. This
 * should be called when it is believed that no more allocations will be needed
 * from the allocator to ensure that empty pages waiting to be filled aren't
 * unnecessarily held.
 *
 * This is the real implementation. There is another dummy implementation below.
 * See the note titled "Compile Time Trickery" at the top of this file.
 */
void
m32_allocator_flush(void) {
   int i;
   for (i=0; i<M32_MAX_PAGES; i++) {
      void * addr =  __sync_fetch_and_and(&alloc.pages[i].base_addr, 0x0);
      if (addr != 0) {
         m32_free_internal(addr);
      }
   }
}

// Return true if the object has its own dedicated set of pages
#define m32_is_large_object(size,alignment) \
   (size >= getPageSize() - ROUND_UP(M32_REFCOUNT_BYTES,alignment))

// Return true if the object has its own dedicated set of pages
#define m32_is_large_object_addr(addr) \
   ((uintptr_t) addr % getPageSize() == 0)

/**
 * Free the memory associated with an object.
 *
 * If the object is "small", the object counter of the page it is allocated in
 * is decremented and the page is not freed until all of its objects are freed.
 *
 * This is the real implementation. There is another dummy implementation below.
 * See the note titled "Compile Time Trickery" at the top of this file.
 */
void
m32_free(void *addr, size_t size)
{
   uintptr_t m = (uintptr_t) addr % getPageSize();

   if (m == 0) {
      // large object
      munmapForLinker(addr,roundUpToPage(size));
   }
   else {
      // small object
      void * page_addr = (void*)((uintptr_t)addr - m);
      m32_free_internal(page_addr);
   }
}

/**
 * Allocate `size` bytes of memory with the given alignment.
 *
 * This is the real implementation. There is another dummy implementation below.
 * See the note titled "Compile Time Trickery" at the top of this file.
 */
void *
m32_alloc(size_t size, size_t alignment)
{
   size_t pgsz = getPageSize();

   if (m32_is_large_object(size,alignment)) {
       // large object
       return mmapForLinker(size,MAP_ANONYMOUS,-1,0);
   }

   // small object
   // Try to find a page that can contain it
   int empty = -1;
   int most_filled = -1;
   int i;
   for (i=0; i<M32_MAX_PAGES; i++) {
      // empty page
      if (alloc.pages[i].base_addr == 0) {
         empty = empty == -1 ? i : empty;
         continue;
      }
      // If the page is referenced only by the allocator, we can reuse it.
      // If we don't then we'll be left with a bunch of pages that have a
      // few bytes left to allocate and we don't get to use or free them
      // until we use up all the "filling" pages. This will unnecessarily
      // allocate new pages and fragment the address space.
      if (*((uintptr_t*)(alloc.pages[i].base_addr)) == 1) {
         alloc.pages[i].current_size = M32_REFCOUNT_BYTES;
      }
      // page can contain the buffer?
      size_t alsize = ROUND_UP(alloc.pages[i].current_size, alignment);
      if (size <= pgsz - alsize) {
         void * addr = (char*)alloc.pages[i].base_addr + alsize;
         alloc.pages[i].current_size = alsize + size;
         // increment the counter atomically
         __sync_fetch_and_add((uintptr_t*)alloc.pages[i].base_addr, 1);
         return addr;
      }
      // most filled?
      if (most_filled == -1
       || alloc.pages[most_filled].current_size < alloc.pages[i].current_size)
      {
         most_filled = i;
      }
   }

   // If we haven't found an empty page, flush the most filled one
   if (empty == -1) {
      m32_free_internal(alloc.pages[most_filled].base_addr);
      alloc.pages[most_filled].base_addr    = 0;
      alloc.pages[most_filled].current_size = 0;
      empty = most_filled;
   }

   // Allocate a new page
   void * addr = mmapForLinker(pgsz,MAP_ANONYMOUS,-1,0);
   if (addr == NULL) {
      return NULL;
   }
   alloc.pages[empty].base_addr    = addr;
   // Add M32_REFCOUNT_BYTES bytes for the counter + padding
   alloc.pages[empty].current_size =
       size+ROUND_UP(M32_REFCOUNT_BYTES,alignment);
   // Initialize the counter:
   // 1 for the allocator + 1 for the returned allocated memory
   *((uintptr_t*)addr)            = 2;
   return (char*)addr + ROUND_UP(M32_REFCOUNT_BYTES,alignment);
}

#elif RTS_LINKER_USE_MMAP == 0

// The following implementations of these functions should never be called. If
// they are, there is a bug at the call site.
// See the note titled "Compile Time Trickery" at the top of this file.

void
m32_allocator_init(void)
{
    barf("%s: RTS_LINKER_USE_MMAP is %d", __func__, RTS_LINKER_USE_MMAP);
}

void
m32_allocator_flush(void)
{
    barf("%s: RTS_LINKER_USE_MMAP is %d", __func__, RTS_LINKER_USE_MMAP);
}

void
m32_free(void *addr STG_UNUSED, size_t size STG_UNUSED)
{
    barf("%s: RTS_LINKER_USE_MMAP is %d", __func__, RTS_LINKER_USE_MMAP);
}

void *
m32_alloc(size_t size STG_UNUSED, size_t alignment STG_UNUSED)
{
    barf("%s: RTS_LINKER_USE_MMAP is %d", __func__, RTS_LINKER_USE_MMAP);
}

#else

#error RTS_LINKER_USE_MMAP should be either `0` or `1`.

#endif
