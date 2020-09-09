/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2020
 *
 * Management of foreign exports.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "ForeignExports.h"

static struct ForeignExportsList *pending = NULL;
static ObjectCode *loading_obj = NULL;

/*
 * Note [Tracking foreign exports]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 *
 * Things (e.g. CAFs) depended upon by `foreign export`s need to be kept alive
 * for as long an object is loaded. To ensure this we create a stable pointer
 * to each `foreign export`'d closure. This works as follows:
 *
 * 1. The compiler  (namely GHC.HsToCore.Foreign.Decl.foreignExports)
 *    inserts a C-stub into each object containing a `foreign export`. This
 *    stub contains two things:
 *
 *    - A `ForeignExportsList` object listing all of the exported closures, and
 *    - An initializer which calls `registerForeignExports` with a reference to
 *      the `ForeignExportsList`.
 *
 * 2. When the object is loaded its initializer is called by the linker (this
 *    might be the system's dynamic linker or GHC's own static linker). The
 *    `registerForeignExports` places the new `ForeignExportsList` on
 *    `pending` list.
 *
 * 3. When loading has finished (e.g. during RTS initialization or at the end
 *    of `Linker.c:ocTryLoad`) `processForeignExports` is called. Here we
 *    traverse the `pending` list and create a `StablePtr` for each.
 *
 * The reason for this two-step process is that in general we are very
 * restricted in what we can do in an initializer function. For instance, we
 * cannot necessarily call `malloc`  since the `libc`'s own initializer may not
 * have run yet.
 *
 * Another consideration here is that the linker needs to know which
 * `StablePtr`s belong to each `ObjectCode` it loads for the sake of unloading.
 * For this reason, the linker informs us when it is loading an object by calling
 * `foreignExportsLoadingObjects` and
 * `foreignExportsFinishedLoadingObject`. We take note of the
 * `ObjectCode*` we are loading in `loading_obj` such that we can
 * associate the `StablePtr` with the `ObjectCode` in
 * `processForeignExports`.
 *
 */

void registerForeignExports(struct ForeignExportsList *exports)
{
    exports->next = pending;
    exports->oc = loading_obj;
    pending = exports;
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
    ASSERT(loading_obj != NULL);
    loading_obj = NULL;
    processForeignExports();
}

void processForeignExports()
{
    while (pending) {
        for (int i=0; i < exports->n_entries; i++) {
            StgPtr = pending->exports[i];
            StgStablePtr *sptr = getStablePtr(p);

            if (loading_obj != NULL) {
                fe_sptr = stgMallocBytes(sizeof(ForeignExportStablePtr),
                                         "foreignExportStablePtr");
                fe_sptr->stable_ptr = sptr;
                fe_sptr->next = loading_obj->stable_ptrs;
                pending->oc->stable_ptrs = fe_sptr;
            }
        }

        pending = pending->next;
    }
}
