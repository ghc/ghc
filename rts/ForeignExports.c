/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2020
 *
 * Management of foreign exports.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "ForeignExports.h"

static struct ForeignExportsList *head = NULL;
static ObjectCode *loading_obj = NULL;

/*
 * Note [Tracking foreign exports]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 *
 * Foreign exports need to be kept alive for as long an object is
 * loaded. We use Stable
 *
 * 1. The linker tells us that it is starting to load an object
 */

void registerForeignExports(struct ForeignExportsList *exports)
{
    exports->next = head;
    exports->oc = loading_obj;
    head = exports;
}

static StgStablePtr registerForeignExport(StgPtr p)
{
    StgStablePtr *sptr = getStablePtr(p);

    if (loading_obj != NULL) {
        fe_sptr = stgMallocBytes(sizeof(ForeignExportStablePtr),
                                 "foreignExportStablePtr");
        fe_sptr->stable_ptr = sptr;
        fe_sptr->next = loading_obj->stable_ptrs;
        loading_obj->stable_ptrs = fe_sptr;
    }

    return sptr;
}

/* -----------------------------------------------------------------------------
   Create a StablePtr for a foreign export.  This is normally called by
   a C function with __attribute__((constructor)), which is generated
   by GHC and linked into the module.

   If the object code is being loaded dynamically, then we remember
   which StablePtrs were allocated by the constructors and free them
   again in unloadObj().
   -------------------------------------------------------------------------- */

void foreignExportsLoadingObject(ObjectCode *oc)
{
    ASSERT(loading_obj == NULL);
    loading_obj = oc;
}

void foreignExportsFinishedLoadingObject()
{
    loading_obj = NULL;
    while (head) {
        for (int i=0; i < exports->n_entries; i++) {
            registerForeignExport(head->exports[p])
        }
        head = head->next;
    }
}

