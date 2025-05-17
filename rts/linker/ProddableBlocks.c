/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2025
 *
 * RTS Object Linker
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "RtsUtils.h"
#include "linker/ProddableBlocks.h"

struct _ProddableBlock {
  void* start;
  int   size;
  struct _ProddableBlock* next;
};

typedef struct _ProddableBlock ProddableBlock;

void
initProddableBlockSet ( ProddableBlockSet* set )
{
    set->head = NULL;
}

/* -----------------------------------------------------------------------------
 * Sanity checking.  For each ObjectCode, maintain a list of address ranges
 * which may be prodded during relocation, and abort if we try and write
 * outside any of these.
 */
void
addProddableBlock ( ProddableBlockSet* set, void* start, int size )
{
    ProddableBlock* pb = stgMallocBytes(sizeof(ProddableBlock), "addProddableBlock");

    IF_DEBUG(linker, debugBelch("addProddableBlock: %p %d\n", start, size));
    ASSERT(size > 0);
    pb->start = start;
    pb->size  = size;
    pb->next  = set->head;
    set->head = pb;
}

void
checkProddableBlock (ProddableBlockSet *set, void *addr, size_t size )
{
    ProddableBlock* pb;

    for (pb = set->head; pb != NULL; pb = pb->next) {
        char* s = (char*)(pb->start);
        char* e = s + pb->size;
        char* a = (char*)addr;
        if (a >= s && (a+size) <= e)
            return;
    }
    barf("checkProddableBlock: invalid fixup in runtime linker: %p", addr);
}

void freeProddableBlocks (ProddableBlockSet *set)
{
    ProddableBlock *pb, *next;

    for (pb = set->head; pb != NULL; pb = next) {
        next = pb->next;
        stgFree(pb);
    }
    set->head = NULL;
}

