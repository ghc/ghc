/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2000-2012
 *
 * RTS Object Linker
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "sm/OSMem.h"
#include "RtsUtils.h"
#include "linker/M32Alloc.h"
#include "linker/MMap.h"
#include "ReportMemoryMap.h"

#include <inttypes.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/*
Note [Compile Time Trickery]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This file implements two versions of each of the `m32_*` functions. At the top
of the file there is the real implementation (compiled in when
`NEED_M32` is true) and a dummy implementation that exists only to
satisfy the compiler and which should never be called. If any of these dummy
implementations are called the program will abort.

The rationale for this is to allow the calling code to be written without using
the C pre-processor (CPP) `#if` hackery. The value of `NEED_M32` is
known at compile time, allowing code like:

    if (NEED_M32)
        m32_allocator_init();

will be compiled to call to `m32_allocator_init` if  `NEED_M32` is
true and will be optimised away to nothing if `NEED_M32` is false.
However, regardless of the value of `NEED_M32` the compiler will
still check the call for syntax and correct function parameter types.

*/

#if defined(NEED_M32)

/*
Note [M32 Allocator]
~~~~~~~~~~~~~~~~~~~~
A memory allocator that allocates only pages in the 32-bit range (lower 2GB).
This is useful on 64-bit platforms to ensure that addresses of allocated
objects can be referenced with a 32-bit relative offset.

Initially, the linker used `mmap` to allocate a page per object. Hence it
wasted a lot of space for small objects (see #9314). With this allocator, we
try to fill pages as much as we can for small objects.

The interface
-------------

The allocator exposes three operations:

 * m32_allocator_new creates a new allocator; an allocator may be configured
   to allocate code (readable/executable) pages or data (readable/writeable)
   pages.

 * m32_alloc uses an allocator to allocated a (aligned) bit of memory
 *
 * m32_allocator_flush is used to indicate that the pages allocated thusfar
   have been filled. This will protect the pages.

 * m32_allocator_free is used to free the allocator and the pages it has
   allocated.

How does it work?
-----------------

The allocator manages two kinds of allocations:

 * small allocations, which are allocated into a set of "nursery" pages
   (recorded in m32_allocator_t.pages; the size of the set is <= M32_MAX_PAGES)

 * large allocations are those larger than a page and are mapped directly

Each page (or the first page of a large allocation) begins with a m32_page_t
header, which records various information depending upon what stage of its
life-cycle it is in:

 * in the case of a page in the small-allocation nursery: the number of bytes
   allocated into the page thusfar

 * in the case of a filled page:
    * the size of the mapping (PAGE_SIZE in the case
      of a nursery page, or greater in the case of a page arising from a large
      allocation)

Allocation (in the case of a small request) consists of walking the nursery to
find a page that will accommodate the request. If none exists then we allocate a
new nursery page (flushing an existing one to the filled list if the nursery is
full).

The allocator maintains two linked lists of filled pages, both linked together
with m32_page_t.link:

 * unprotected_pages records pages that have been filled but have not yet been
   protected (e.g. due to a call to m32_allocator_flush)

 * protect_pages records pages that have been filled and protected

m32_allocator_flush does two things:

 * it flushes all pages from the nursery

 * it protects the pages in m32_allocator_t.unprotected_list (and formerly in
   the nursery) and moves them
   to protected_list.

For large objects, the remaining space at the end of the last page is left
unused by the allocator. It can be used with care as it will be freed with the
associated large object. GHC linker uses this feature/hack, hence changing the
implementation of the M32 allocator must be done with care (i.e. do not try to
improve the allocator to avoid wasting this space without modifying the linker
code accordingly).

To avoid unnecessary mapping/unmapping we maintain a global list of free pages
(which can grow up to M32_MAX_FREE_PAGE_POOL_SIZE long). Pages on this list
have the usual m32_page_t header and are linked together with
m32_page_t.free_page.next. When run out of free pages we allocate a chunk of
M32_MAP_PAGES to both avoid fragmenting our address space and amortize the
runtime cost of the mapping.

The allocator is *not* thread-safe.

*/

// Enable internal consistency checking
#if defined(DEBUG)
#define M32_DEBUG
#endif

#define ROUND_UP(x,size) ((x + size - 1) & ~(size - 1))
#define ROUND_DOWN(x,size) (x & ~(size - 1))

/****************************************************************************
 * M32 ALLOCATOR (see Note [M32 Allocator]
 ***************************************************************************/

/* How many open pages each allocator will keep around? */
#define M32_MAX_PAGES 32
/* How many pages should we map at once when re-filling the free page pool? */
#define M32_MAP_PAGES 32
/* Upper bound on the number of pages to keep in the free page pool */
#define M32_MAX_FREE_PAGE_POOL_SIZE 256

/* A utility to verify that a given address is "acceptable" for use by m32. */
static bool
is_okay_address(void *p) {
  int8_t *here = LINKER_LOAD_BASE;
  ssize_t displacement = (int8_t *) p - here;
  // if we assume -fPIC, we don't care where we load code.
  // But we still want to use the m32 allocator to avoid fragmentation (#24432)
  return RtsFlags.MiscFlags.linkerAlwaysPic
         || ((displacement > -0x7fffffff) && (displacement < 0x7fffffff));
}

enum m32_page_type {
  FREE_PAGE,    // a page in the free page pool
  NURSERY_PAGE, // a nursery page
  FILLED_PAGE,  // a page on the filled list
};

/**
 * Page header
 *
 * Every page (or large allocation) allocated with m32 has one of these at its
 * start.
 */
struct m32_page_t {
  union {
    // Pages (or large allocations) that have been filled and are in either the
    // unprotected_list or protected_list are linked together with this field.
    struct {
      uint32_t size;
      struct m32_page_t *next;
    } filled_page;

    // Pages in the small-allocation nursery encode their current allocation
    // offset here.
    size_t current_size;

    // Pages in the global free page pool are linked via this field.
    struct {
      struct m32_page_t *next;
    } free_page;
  };
#if defined(M32_DEBUG)
  enum m32_page_type type;
#endif
  uint8_t contents[];
};

/* Consistency-checking infrastructure */
#if defined(M32_DEBUG)
static void ASSERT_PAGE_ALIGNED(void *page) {
  const size_t pgsz = getPageSize();
  if ((((uintptr_t) page) & (pgsz-1)) != 0) {
    barf("m32: invalid page alignment");
  }
}
static void ASSERT_VALID_PAGE(struct m32_page_t *page) {
  ASSERT_PAGE_ALIGNED(page);
  switch (page->type) {
  case FREE_PAGE:
  case NURSERY_PAGE:
  case FILLED_PAGE:
    break;
  default:
    barf("m32: invalid page state\n");
  }
}
static void ASSERT_PAGE_TYPE(struct m32_page_t *page, enum m32_page_type ty) {
  if (page->type != ty) { barf("m32: unexpected page type"); }
}
static void ASSERT_PAGE_NOT_FREE(struct m32_page_t *page) {
  if (page->type == FREE_PAGE) { barf("m32: unexpected free page"); }
}
static void SET_PAGE_TYPE(struct m32_page_t *page, enum m32_page_type ty) {
  page->type = ty;
}
#else
#define ASSERT_PAGE_ALIGNED(page)
#define ASSERT_VALID_PAGE(page)
#define ASSERT_PAGE_NOT_FREE(page)
#define ASSERT_PAGE_TYPE(page, ty)
#define SET_PAGE_TYPE(page, ty)
#endif

/* Accessors */
static void
m32_filled_page_set_next(struct m32_page_t *page, struct m32_page_t *next)
{
  ASSERT_PAGE_TYPE(page, FILLED_PAGE);
  if (next != NULL && ! is_okay_address(next)) {
    barf("m32_filled_page_set_next: Page %p not within 4GB of program text", next);
  }
  page->filled_page.next = next;
}

static struct m32_page_t *
m32_filled_page_get_next(struct m32_page_t *page)
{
  ASSERT_PAGE_TYPE(page, FILLED_PAGE);
  return (struct m32_page_t *) (uintptr_t) page->filled_page.next;
}

/**
 * Allocator
 *
 * Currently an allocator is just a set of pages being filled. The maximum
 * number of pages can be configured with M32_MAX_PAGES.
 */
struct m32_allocator_t {
   bool executable;
   // List of pages that have been filled but not yet protected.
   struct m32_page_t *unprotected_list;
   // List of pages that have been filled and protected.
   struct m32_page_t *protected_list;
   // Pages in the small-allocation nursery
   struct m32_page_t *pages[M32_MAX_PAGES];
};

/**
 * Global free page pool
 *
 * We keep a small pool of free pages around to avoid fragmentation.
 */
struct m32_page_t *m32_free_page_pool = NULL;
/** Number of pages in free page pool */
unsigned int m32_free_page_pool_size = 0;

/**
 * Free a filled page or, if possible, place it in the free page pool.
 */
static void
m32_release_page(struct m32_page_t *page)
{
  // Some sanity-checking
  ASSERT_VALID_PAGE(page);
  ASSERT_PAGE_NOT_FREE(page);

  const size_t pgsz = getPageSize();
  ssize_t sz = page->filled_page.size;
  IF_DEBUG(sanity, memset(page, 0xaa, sz));

  // Break the page, which may be a large multi-page allocation, into
  // individual pages for the page pool
  while (sz > 0) {
    if (m32_free_page_pool_size < M32_MAX_FREE_PAGE_POOL_SIZE) {
      mprotectForLinker(page, pgsz, MEM_READ_WRITE);
      SET_PAGE_TYPE(page, FREE_PAGE);
      page->free_page.next = m32_free_page_pool;
      m32_free_page_pool = page;
      m32_free_page_pool_size ++;
    } else {
      break;
    }
    page = (struct m32_page_t *) ((uint8_t *) page + pgsz);
    sz -= pgsz;
  }

  // The free page pool is full, release the rest back to the system
  if (sz > 0) {
    munmapForLinker((void *) page, ROUND_UP(sz, pgsz), "m32_release_page");
  }
}

/**
 * Allocate a page from the free page pool or operating system. No guarantee is
 * made regarding the state of the m32_page_t fields.
 */
static struct m32_page_t *
m32_alloc_page(void)
{
  if (m32_free_page_pool_size == 0) {
    /*
     * Free page pool is empty; refill it with a new batch of M32_MAP_PAGES
     * pages.
     */
    const size_t pgsz = getPageSize();
    const size_t map_sz = pgsz * M32_MAP_PAGES;
    uint8_t *chunk = mmapAnonForLinker(map_sz);
    if (! is_okay_address(chunk + map_sz)) {
      reportMemoryMap();
      barf("m32_alloc_page: failed to allocate pages within 4GB of program text (got %p)", chunk);
    }
    IF_DEBUG(sanity, memset(chunk, 0xaa, map_sz));

#define GET_PAGE(i) ((struct m32_page_t *) (chunk + (i) * pgsz))
    for (int i=0; i < M32_MAP_PAGES; i++) {
      struct m32_page_t *page = GET_PAGE(i);
      SET_PAGE_TYPE(page, FREE_PAGE);
      page->free_page.next = GET_PAGE(i+1);
    }

    GET_PAGE(M32_MAP_PAGES-1)->free_page.next = m32_free_page_pool;
    m32_free_page_pool = (struct m32_page_t *) chunk;
    m32_free_page_pool_size += M32_MAP_PAGES;
#undef GET_PAGE
  }

  struct m32_page_t *page = m32_free_page_pool;
  m32_free_page_pool = page->free_page.next;
  m32_free_page_pool_size --;
  ASSERT_PAGE_TYPE(page, FREE_PAGE);
  return page;
}

/**
 * Initialize the allocator structure
 * This is the real implementation. There is another dummy implementation below.
 * See the note titled "Compile Time Trickery" at the top of this file.
 */
m32_allocator *
m32_allocator_new(bool executable)
{
  m32_allocator *alloc =
    stgMallocBytes(sizeof(m32_allocator), "m32_new_allocator");
  memset(alloc, 0, sizeof(struct m32_allocator_t));
  alloc->executable = executable;
  return alloc;
}

/**
 * Unmap all pages on the given list.
 */
static void
m32_allocator_unmap_list(struct m32_page_t *head)
{
  while (head != NULL) {
    ASSERT_VALID_PAGE(head);
    struct m32_page_t *next = m32_filled_page_get_next(head);
    m32_release_page(head);
    head = next;
  }
}

/**
 * Free an m32_allocator and the pages that it has allocated.
 */
void m32_allocator_free(m32_allocator *alloc)
{
  /* free filled pages */
  m32_allocator_unmap_list(alloc->unprotected_list);
  m32_allocator_unmap_list(alloc->protected_list);

  /* free partially-filled pages */
  for (int i=0; i < M32_MAX_PAGES; i++) {
    if (alloc->pages[i]) {
      m32_release_page(alloc->pages[i]);
    }
  }

  stgFree(alloc);
}

/**
 * Push a page onto the given filled page list.
 */
static void
m32_allocator_push_filled_list(struct m32_page_t **head, struct m32_page_t *page)
{
  ASSERT_PAGE_TYPE(page, FILLED_PAGE);
    // N.B. it's the caller's responsibility to set the pagetype to FILLED_PAGE
  m32_filled_page_set_next(page, *head);
  *head = page;
}

/**
 * Release the allocator's reference to pages on the "filling" list. This
 * should be called when it is believed that no more allocations will be needed
 * from the allocator to ensure that empty pages waiting to be filled aren't
 * unnecessarily held.
 *
 * If this allocator is for executable allocations this is where we mark the
 * filled pages as executable (and no longer writable).
 *
 * This is the real implementation. There is another dummy implementation below.
 * See the note titled "Compile Time Trickery" at the top of this file.
 */
void
m32_allocator_flush(m32_allocator *alloc) {
   for (int i=0; i<M32_MAX_PAGES; i++) {
     if (alloc->pages[i] == NULL) {
       continue;
     } else if (alloc->pages[i]->current_size == sizeof(struct m32_page_t)) {
       // the page is empty, free it
       m32_release_page(alloc->pages[i]);
     } else {
       // the page contains data, move it to the unprotected list
       SET_PAGE_TYPE(alloc->pages[i], FILLED_PAGE);
       m32_allocator_push_filled_list(&alloc->unprotected_list, alloc->pages[i]);
     }
     alloc->pages[i] = NULL;
   }

   // Write-protect pages if this is an executable-page allocator.
   if (alloc->executable) {
     struct m32_page_t *page = alloc->unprotected_list;
     while (page != NULL) {
       ASSERT_PAGE_TYPE(page, FILLED_PAGE);
       struct m32_page_t *next = m32_filled_page_get_next(page);
       m32_allocator_push_filled_list(&alloc->protected_list, page);
       mprotectForLinker(page, page->filled_page.size, MEM_READ_EXECUTE);
       page = next;
     }
     alloc->unprotected_list = NULL;
   }
}

/**
 * Return true if the allocation request should be considered "large".
 */
static bool
m32_is_large_object(size_t size, size_t alignment)
{
   return size >= getPageSize() - ROUND_UP(sizeof(struct m32_page_t), alignment);
}

static void
m32_report_allocation(struct m32_allocator_t *alloc STG_UNUSED, void *addr STG_UNUSED, size_t size STG_UNUSED)
{
    IF_DEBUG(linker_verbose, debugBelch(
      "m32_allocated(%p:%s): %p - %p\n",
      alloc, alloc->executable ? "RX": "RW",
      addr, (uint8_t*) addr + size));
}

/**
 * Allocate `size` bytes of memory with the given alignment.
 *
 * This is the real implementation. There is another dummy implementation below.
 * See the note titled "Compile Time Trickery" at the top of this file.
 */
void *
m32_alloc(struct m32_allocator_t *alloc, size_t size, size_t alignment)
{
   size_t pgsz = getPageSize();

   if (m32_is_large_object(size,alignment)) {
      // large object
      size_t alsize = ROUND_UP(sizeof(struct m32_page_t), alignment);
      // TODO: lower-bound allocation size to allocation granularity and return
      // remainder to free pool.
      struct m32_page_t *page = mmapAnonForLinker(alsize+size);
      if (page == NULL) {
          sysErrorBelch("m32_alloc: Failed to map pages for %zd bytes", size);
          return NULL;
      } else if (! is_okay_address(page)) {
          reportMemoryMap();
          barf("m32_alloc: warning: Allocation of %zd bytes resulted in pages above 4GB (%p)",
               size, page);
      }
      SET_PAGE_TYPE(page, FILLED_PAGE);
      page->filled_page.size = alsize + size;
      m32_allocator_push_filled_list(&alloc->unprotected_list, (struct m32_page_t *) page);
      uint8_t *res = (uint8_t *) page + alsize;
      m32_report_allocation(alloc, res, size);
      return res;
   }

   // small object
   // Try to find a page that can contain it
   int empty = -1;
   int most_filled = -1;
   int i;
   for (i=0; i<M32_MAX_PAGES; i++) {
      // empty page
      if (alloc->pages[i] == NULL) {
         empty = empty == -1 ? i : empty;
         continue;
      }

      // page can contain the buffer?
      ASSERT_VALID_PAGE(alloc->pages[i]);
      ASSERT_PAGE_TYPE(alloc->pages[i], NURSERY_PAGE);
      size_t alsize = ROUND_UP(alloc->pages[i]->current_size, alignment);
      if (size <= pgsz - alsize) {
         void * addr = (char*)alloc->pages[i] + alsize;
         alloc->pages[i]->current_size = alsize + size;
         m32_report_allocation(alloc, addr, size);
         return addr;
      }

      // is this the most filled page we've seen so far?
      if (most_filled == -1
       || alloc->pages[most_filled]->current_size < alloc->pages[i]->current_size)
      {
         most_filled = i;
      }
   }

   // If we haven't found an empty page, flush the most filled one
   if (empty == -1) {
      SET_PAGE_TYPE(alloc->pages[most_filled], FILLED_PAGE);
      m32_allocator_push_filled_list(&alloc->unprotected_list, alloc->pages[most_filled]);
      alloc->pages[most_filled] = NULL;
      empty = most_filled;
   }

   // Allocate a new page
   struct m32_page_t *page = m32_alloc_page();
   if (page == NULL) {
      return NULL;
   }
   SET_PAGE_TYPE(page, NURSERY_PAGE);
   alloc->pages[empty]               = page;
   // Add header size and padding
   alloc->pages[empty]->current_size = size + ROUND_UP(sizeof(struct m32_page_t),alignment);
   uint8_t *res = (uint8_t *) page + ROUND_UP(sizeof(struct m32_page_t), alignment);
   m32_report_allocation(alloc, res, size);
   return res;
}

#else

// The following implementations of these functions should never be called. If
// they are, there is a bug at the call site.
// See the note titled "Compile Time Trickery" at the top of this file.

m32_allocator *
m32_allocator_new(bool executable STG_UNUSED)
{
    barf("%s: RTS_LINKER_USE_MMAP is %d", __func__, RTS_LINKER_USE_MMAP);
}

void m32_allocator_free(m32_allocator *alloc STG_UNUSED)
{
    barf("%s: RTS_LINKER_USE_MMAP is %d", __func__, RTS_LINKER_USE_MMAP);
}

void
m32_allocator_flush(m32_allocator *alloc STG_UNUSED)
{
    barf("%s: RTS_LINKER_USE_MMAP is %d", __func__, RTS_LINKER_USE_MMAP);
}

void *
m32_alloc(m32_allocator *alloc STG_UNUSED,
          size_t size STG_UNUSED,
          size_t alignment STG_UNUSED)
{
    barf("%s: RTS_LINKER_USE_MMAP is %d", __func__, RTS_LINKER_USE_MMAP);
}

#endif
