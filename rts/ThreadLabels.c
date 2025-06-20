/* -----------------------------------------------------------------------------
 * ThreadLabels.c
 *
 * (c) The GHC Team 2002-2003
 *
 * Table of thread labels.
 *
 * ---------------------------------------------------------------------------*/

#include "rts/PosixSource.h"
#include "Rts.h"

#include "ThreadLabels.h"
#include "RtsUtils.h"
#include "RtsFlags.h"
#include "Hash.h"
#include "Trace.h"
#include "AllocArray.h"

#include <stdlib.h>
#include <string.h>

/*
 * Note [Thread Labels]
 * ~~~~~~~~~~~~~~~~~~~~
 * The user may assign a textual label to a thread using the labelThread#
 * primop to help identify the thread. This label is represented by StgTSO's
 * label field which contains a pointer to a ByteArray# containing a
 * UTF-8 string.
 *
 * Note that this string isn't necessary NULL terminated; rather, its length is
 * determined by the ByteArray# length.
 */

void
setThreadLabel(Capability  *cap,
               StgTSO      *tso,
               char *label)
{
    int len = strlen(label);
    StgArrBytes *arr = allocateArrBytes(cap, len, cap->r.rCCCS);
    // On allocation failure don't perform the effect. It's not convenient to
    // propagate failure from here since there are multiple callers in the RTS.
    if (RTS_UNLIKELY(arr == NULL)) return;
    memcpy(&arr->payload, label, len);
    labelThread(cap, tso, arr);
}

void
labelThread(Capability  *cap,
            StgTSO      *tso,
            StgArrBytes *label)
{
    if (tso->label) {
        IF_NONMOVING_WRITE_BARRIER_ENABLED {
            updateRemembSetPushClosure(cap, (StgClosure *) tso->label);
        }
    }
    recordClosureMutated(cap, (StgClosure*)tso);
    RELEASE_STORE(&tso->label, label);
    traceThreadLabel(cap, tso, (char *) label->payload, label->bytes);
}
