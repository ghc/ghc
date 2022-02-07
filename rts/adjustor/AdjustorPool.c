#include "rts/PosixSource.h"
#include "Rts.h"

#include "sm/OSMem.h"
#include "RtsUtils.h"
#include "linker/MMap.h"
#include "AdjustorPool.h"

#include <string.h>

/*
 * Note [Adjustor pools]
 * ~~~~~~~~~~~~~~~~~~~~~
 * Memory management for adjustors is somewhat complicated by the fact that
 * modern operating systems highly discourage users from simultaneously
 * mapping memory as both writable and executable. This means that when GHC
 * creates an adjustor, it must immediately re-map it to be non-writable,
 * thus precluding the possibility of adding further adjustors to the same
 * page.
 *
 * For a long time we would simply allocate a fresh page for each adjustor.
 * Given that the adjustor code is quite small (on the order of a dozen
 * bytes), this incurred significant space overhead. This was particularly
 * bad on Windows, where address space can only be allocated in units of 16
 * pages (64 kBytes). Consequently, even allocating a couple of thousand
 * adjustors can consume gigabytes of memory.
 *
 * AdjustorPool is a specialized allocator for managing adjustors.
 * Specifically, it exploits the fact that an adjustor's code can be
 * constructed even if we don't know the location of closure to which the
 * adjustor will eventually refer. Consequently, we can allocate an entire
 * page (a "chunk") of adjustors at a time, populate it with code, and remap
 * it as non-writable/executable. Each adjustor's code is constructed to take
 * its writable "context" (typically an StgStablePtr and StgFunPtr) from
 * another region of memory that we leave as writable.
 *
 * To construct an AdjustorPool one uses newAdjustorPool and provide:
 *
 *  - the size of the adjustor code, in bytes
 *  - a function to construct the adjustor code
 *
 * After a pool has been constructed, new adjustors can be allocated from it
 * using alloc_adjustor and freed using free_adjustor. The pool maintains a
 * free list and will reallocate into free adjustor slots when possible.
 * We currently make no attempt at freeing AdjustorChunks which contain no
 * live adjustors.
 *
 * The AdjustorPool module also exposes a high-level interface,
 * new_adjustor_pool_from_template, capturing the common case where the
 * adjustor code can be simply copied from a "template" and the adjustor fixed
 * up.
 *
 *   ┌───────────────────────────────────────┬─────────────────────────┐
 *   │                                       │                         │
 *   ▼AdjustorPool         AdjustorChunk     │      AdjustorChunk      │
 *   ┌────────────┐   ┌──►┌───────────────┐  │ ┌──►┌────────────────┐  │
 *   │code_size   │   │   │owner          ├──┘ │   │owner           ├──┘
 *   │free_list   ├───┤ ┌─┤exec_page      │    │   │exec_page_next  │
 *   └────────────┘   │ │ │free_list_next ├────┘   │free_list_next  ├───► ...
 *                    │ │ │first_free     │        │first_free      │
 *                    │ │ ├───────────────┤        ├────────────────┤
 *                    │ │ │slot_bitmap    │        │slot_bitmap     │
 *                    │ │ │               │        │                │
 *                    │ │ │               │        │                │
 *                    │ │ ├───────────────┤        ├────────────────┤
 *                    │ │ │context[0]     │        │context[0]      │
 *                    │ │ │context[1]     │        │context[1]      │
 *                    │ │ │...            │        │...             │
 *                    │ │ └───────────────┘        └────────────────┘
 *                    │ │
 *                    │ │
 *                    │ │
 *                    │ └─┐
 *                    │   ▼AdjustorExecPage
 *                    │   ┌──────────────────┐
 *                    └───┤owner             │
 *                        ├──────────────────┤
 *                        │adjustor 0 code   │   N.B. the code of each adjustor
 *                        │                  │   contains a reference back to
 *                        ├──────────────────┤   its slot's chunk->context
 *                        │adjustor 1 code   │   entry.
 *                        │                  │
 *                        ├──────────────────┤
 *                        │adjustor 3 code   │
 *                        │                  │
 *                        ├──────────────────┤
 *                        │...               │
 *                        │                  │
 *                        └──────────────────┘
 *
 */

// Round up the N to the nearest multiple of s
#define ROUND_UP(n, s) ((((n) + (s) - 1) / (s)) * (s))

// Forward declarations
struct AdjustorExecPage;
struct AdjustorChunk;
struct AdjustorPool;

static struct AdjustorChunk *alloc_adjustor_chunk(struct AdjustorPool *owner);

#define ADJUSTOR_EXEC_PAGE_MAGIC 0xddeeffaabbcc0011ULL

struct AdjustorExecPage {
    uint64_t magic;
      /* since FFI is ripe for human error, we include a magic number at the
       * beginning of the page to ensure that free_adjustor can catch when it
       * is passed an invalid pointer.
       */
    struct AdjustorChunk *owner;
    uint8_t adjustor_code[];
};

struct AdjustorPool {
    mk_adjustor_code_fn make_code;
    void *user_data; /* user data to be passed to make_code */

    size_t adjustor_code_size; /* how many bytes of code does each adjustor require?  */
    size_t context_size; /* how large is the context associated with each adjustor? */
    size_t chunk_slots; /* how many adjustors per chunk? */
    struct AdjustorChunk *free_list;
#if defined(THREADED_RTS)
    Mutex lock;
#endif
};

struct AdjustorChunk {
    size_t first_free;
      /* index of the first free adjustor slot.
       * Invariant: this must be the index of a slot with unset bit in slot_bitmap
       * if the chunk is on its owning AdjustorPool's free_list. If the chunk is not
       * on free_list then first_free == pool->chunk_slots.
       */
    struct AdjustorPool *owner;
      /* the pool which owns this chunk */
    struct AdjustorChunk *free_list_next;
      /* the next chunk in the pool's free list */
    struct AdjustorExecPage *exec_page;
      /* an AdjustorExecPage containing the code for each adjustor */
    void *contexts;
      /* an context for each adjustor slot. This points to the contexts
       * array which lives after slot_bitmap */
    uint8_t slot_bitmap[];
      /* a bit for each adjustor slot; bit is set if the slot is allocated */
};

struct AdjustorPool *
new_adjustor_pool(
        size_t context_size,
        size_t code_size,
        mk_adjustor_code_fn make_code,
        void *user_data)
{
    struct AdjustorPool *pool = stgMallocBytes(sizeof(struct AdjustorPool), "newAdjustorPool");
    const size_t code_alignment = 16;
    pool->make_code = make_code;
    pool->user_data = user_data;
    pool->context_size = context_size;
    pool->adjustor_code_size = code_size;
    size_t usable_exec_page_sz = getPageSize() - ROUND_UP(sizeof(struct AdjustorExecPage), code_alignment);
    pool->chunk_slots = usable_exec_page_sz / ROUND_UP(code_size, code_alignment);
    pool->free_list = NULL;
#if defined(THREADED_RTS)
    initMutex(&pool->lock);
#endif
    return pool;
}

/* Return the index of the first unset bit of the given bitmap or
 * length_in_bits. */
static size_t
bitmap_first_unset(uint8_t *bitmap, size_t length_in_bits, size_t start_idx)
{
    for (size_t i = start_idx; i < length_in_bits; i += 8) {
        uint8_t x = bitmap[i / 8];
        if (x != 0xff) {
            return i + __builtin_clz(~x);
        }
    }
    return length_in_bits;
}

static void
bitmap_set(uint8_t *bitmap, size_t idx, bool value)
{
    size_t word_n = idx / 8;
    uint8_t bit = 1 << (idx % 8);
    if (value) {
        bitmap[word_n] |= bit;
    } else {
        bitmap[word_n] &= ~bit;
    }
}

// N.B. this is unused in non-DEBUG compilers
static bool STG_UNUSED
bitmap_get(uint8_t *bitmap, size_t idx)
{
    size_t word_n = idx / 8;
    uint8_t bit = 1 << (idx % 8);
    return bitmap[word_n] & bit;
}

static void *
get_context(struct AdjustorChunk *chunk, size_t slot_idx)
{
    uint8_t *contexts = (uint8_t *) chunk->contexts;
    return contexts + chunk->owner->context_size * slot_idx;
}

void *
alloc_adjustor(struct AdjustorPool *pool, void *context)
{
    size_t slot_idx;
    struct AdjustorChunk *chunk;

    ACQUIRE_LOCK(&pool->lock);
    // allocate a new chunk if free_list is empty.
    if (pool->free_list == NULL) {
        pool->free_list = alloc_adjustor_chunk(pool);
    }

    chunk = pool->free_list;
    slot_idx = chunk->first_free;
    ASSERT(slot_idx < pool->chunk_slots);
    bitmap_set(chunk->slot_bitmap, slot_idx, 1);

    // advance first_free
    chunk->first_free = bitmap_first_unset(chunk->slot_bitmap, pool->chunk_slots, slot_idx+1);
    if (chunk->first_free == pool->chunk_slots) {
        // there are no free slots left in this chunk; remove it from
        // free_list.
        pool->free_list = chunk->free_list_next;
        chunk->free_list_next = NULL;
    }

    // update next_free and update bitmap
    ASSERT(bitmap_get(chunk->slot_bitmap, slot_idx));
    bitmap_set(chunk->slot_bitmap, slot_idx, true);

    // fill in the context
    memcpy(get_context(chunk, slot_idx), context, pool->context_size);
    void *adjustor = &chunk->exec_page->adjustor_code[pool->adjustor_code_size * slot_idx];
    RELEASE_LOCK(&pool->lock);

    return adjustor;
}

/* Free an adjustor previously allocated with alloc_adjustor, returning its
 * context
 */
void
free_adjustor(void *adjustor, void *context) {
    uintptr_t exec_page_mask = ~(getPageSize() - 1ULL);
    struct AdjustorExecPage *exec_page = (struct AdjustorExecPage *) ((uintptr_t) adjustor & exec_page_mask);
    if (exec_page->magic != ADJUSTOR_EXEC_PAGE_MAGIC) {
        barf("free_adjustor was passed an invalid adjustor");
    }
    struct AdjustorChunk *chunk = exec_page->owner;
    struct AdjustorPool *pool = chunk->owner;

    size_t slot_off = (uint8_t *) adjustor - exec_page->adjustor_code;
    size_t slot_idx = slot_off / pool->adjustor_code_size;
    // ensure that the slot is aligned as we would expect.
    ASSERT(slot_off % pool->adjustor_code_size == 0);

    ACQUIRE_LOCK(&pool->lock);

    // ensure that the slot is in fact allocated.
    ASSERT(bitmap_get(chunk->slot_bitmap, slot_idx));
    // mark it as free.
    bitmap_set(chunk->slot_bitmap, slot_idx, false);
    // add the chunk to the pool's free_list if necessary.
    if (chunk->first_free == pool->chunk_slots) {
        chunk->free_list_next = pool->free_list;
        pool->free_list = chunk;
    }

    // update first_free
    if (chunk->first_free > slot_idx) {
        chunk->first_free = slot_idx;
    }

    memcpy(context, get_context(chunk, slot_idx), pool->context_size);
    memset(get_context(chunk, slot_idx), 0, pool->context_size);

    RELEASE_LOCK(&pool->lock);
}

/* Must hold owner->lock */
static struct AdjustorChunk *
alloc_adjustor_chunk(struct AdjustorPool *owner) {
    size_t pg_sz = getPageSize();
    struct AdjustorExecPage *exec_page = mmapAnonForLinker(pg_sz);
    if (exec_page == NULL) {
        barf("alloc_adjustor_chunk: failed to allocate");
    }
    exec_page->magic = ADJUSTOR_EXEC_PAGE_MAGIC;

    // N.B. pad bitmap to ensure that .contexts is aligned.
    size_t bitmap_sz = ROUND_UP(owner->chunk_slots, 8*sizeof(void*)) / 8;
    size_t contexts_sz = owner->context_size * owner->chunk_slots;
    size_t alloc_sz = sizeof(struct AdjustorChunk) + bitmap_sz + contexts_sz;
    struct AdjustorChunk *chunk = stgMallocBytes(alloc_sz, "allocAdjustorChunk");
    chunk->owner = owner;
    chunk->first_free = 0;
    chunk->contexts = (struct AdjustorContext *) (chunk->slot_bitmap + bitmap_sz);
    chunk->free_list_next = NULL;
    chunk->exec_page = exec_page;
    chunk->exec_page->owner = chunk;

    // initialize the slot bitmap
    memset(chunk->slot_bitmap, 0, bitmap_sz);
    memset(chunk->contexts, 0, contexts_sz);

    size_t code_sz = owner->adjustor_code_size;
    for (size_t i = 0; i < owner->chunk_slots; i++) {
        owner->make_code(
                &exec_page->adjustor_code[i*code_sz],
                get_context(chunk, i),
                owner->user_data);
    }

    // Remap the executable page as executable
    mprotectForLinker(exec_page, pg_sz, MEM_READ_EXECUTE);

    return chunk;
}

static void
mk_adjustor_from_template(
        uint8_t *exec_code,
        const void *context,
        void *user_data)
{
    const struct AdjustorContext *adjustor_context = context;
    struct AdjustorTemplate *tmpl = (struct AdjustorTemplate *) user_data;

    // Copy the code
    memcpy(exec_code, tmpl->code_start, tmpl->code_end - tmpl->code_start);

    // Fix up the context pointer
    size_t context_off = (uint8_t *) tmpl->context_ptr - tmpl->code_start;
    const struct AdjustorContext **slot_context_ptr =
        (const struct AdjustorContext **) (exec_code + context_off);
    *slot_context_ptr = adjustor_context;
}

struct AdjustorPool *
new_adjustor_pool_from_template(const struct AdjustorTemplate *tmpl)
{
    size_t code_size = tmpl->code_end - tmpl->code_start;
    return new_adjustor_pool(
            sizeof(struct AdjustorContext),
            code_size,
            mk_adjustor_from_template,
            (void *) tmpl);
}

