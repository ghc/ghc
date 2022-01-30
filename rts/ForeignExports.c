/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2020
 *
 * Management of foreign exports.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "RtsUtils.h"
#include "ForeignExports.h"

/* protected by linker_mutex after start-up */
static struct ForeignExportsList *pending = NULL;
static ObjectCode *loading_obj = NULL;

/*
 * Note [Tracking foreign exports]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Foreign exports are garbage collection roots. That is, things (e.g. CAFs)
 * depended upon by a module's `foreign export`s need to be kept alive for as
 * long an module is loaded. To ensure this we create a stable pointer to each
 * `foreign export`'d closure. This works as follows:
 *
 * 1. The compiler  (namely GHC.HsToCore.Foreign.Decl.foreignExports)
 *    inserts a C-stub into each module containing a `foreign export`. This
 *    stub contains two things:
 *
 *    - A `ForeignExportsList` listing all of the exported closures, and
 *
 *    - An initializer which calls `registerForeignExports` with a reference to
 *      the `ForeignExportsList`.
 *
 * 2. When the module's object code is loaded, its initializer is called by the
 *    linker (this might be the system's dynamic linker or GHC's own static
 *    linker). `registerForeignExports` then places the module's
 *    `ForeignExportsList` on `pending` list.
 *
 * 3. When loading has finished (e.g. during RTS initialization or at the end
 *    of `Linker.c:ocTryLoad`) `processForeignExports` is called. Here we
 *    traverse the `pending` list and create a `StablePtr` for each export
 *    therein.
 *
 * The reason for this two-step process is that we are very restricted in what
 * we can do in an initializer function. For instance, we cannot necessarily
 * call `malloc`  since the `libc`'s own initializer may not have run yet.
 * For instance, doing exactly this resulted in #18548.
 *
 * Another consideration here is that the linker needs to know which
 * `StablePtr`s belong to each `ObjectCode` so it can free them when the module is
 * unloaded.  For this reason, the linker informs us when it is loading an
 * object by calling `foreignExportsLoadingObject` and
 * `foreignExportsFinishedLoadingObject`. We take note of the `ObjectCode*` we
 * are loading in `loading_obj` such that we can associate the `ForeignExportsList` with
 * the `ObjectCode` in `processForeignExports`. We then record each of the
 * StablePtrs we create in the ->stable_ptrs array of ForeignExportsList so
 * they can be enumerated during unloading.
 *
 */

void registerForeignExports(struct ForeignExportsList *exports)
{
    ASSERT(exports->next == NULL);
    ASSERT(exports->oc == NULL);
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

/* Caller must own linker_mutex so that we can safely modify
 * oc->stable_ptrs. */
void processForeignExports()
{
    while (pending) {
        struct ForeignExportsList *cur = pending;
        pending = cur->next;

        /* sanity check */
        ASSERT(cur->stable_ptrs == NULL);

        /* N.B. We only need to populate the ->stable_ptrs
         * array if the object might later be unloaded.
         */
        if (cur->oc != NULL) {
            cur->stable_ptrs =
              stgMallocBytes(sizeof(StgStablePtr*) * cur->n_entries,
                             "foreignExportStablePtr");

            for (int i=0; i < cur->n_entries; i++) {
                StgStablePtr *sptr = getStablePtr(cur->exports[i]);

                if (cur->oc != NULL) {
                    cur->stable_ptrs[i] = sptr;
                }
            }
            cur->next = cur->oc->foreign_exports;
            cur->oc->foreign_exports = cur;
        } else {
            /* can't be unloaded, don't bother populating
             * ->stable_ptrs array. */
            for (int i=0; i < cur->n_entries; i++) {
                getStablePtr(cur->exports[i]);
            }
        }
    }
}
