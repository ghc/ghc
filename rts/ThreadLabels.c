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
#include "Hash.h"
#include "Trace.h"

#include <stdlib.h>
#include <string.h>

/*
 * Note [Thread Labels]
 * ~~~~~~~~~~~~~~~~~~~~
 * The user may assign a textual label to a thread using the labelThread#
 * primop to help identify the thread. This label is represented by StgTSO's
 * label field which contains a pointer to a ByteArray# containing a
 * UTF-8 string. We mandate that this string must be null-terminated
 * to ensure that it is convenient to use from the RTS.
 */

char *
lookupThreadLabel(StgTSO *tso)
{
    if (tso->label) {
        return (char*) tso->label->payload;
    } else {
        return NULL;
    }
}

static StgArrBytes *
allocateArrBytes(Capability *cap, size_t size_in_bytes)
{
    uint32_t data_size_in_words, total_size_in_words;

    /* round up to a whole number of words */
    data_size_in_words  = ROUNDUP_BYTES_TO_WDS(size_in_bytes);
    total_size_in_words = sizeofW(StgArrBytes) + data_size_in_words;

    StgArrBytes *arr = (StgArrBytes *) allocate(cap, total_size_in_words);
    SET_ARR_HDR(arr, &stg_ARR_WORDS_info, cap->r.rCCCS, size_in_bytes);
    arr->bytes = size_in_bytes;
    return arr;
}

void
labelThread(Capability  *cap   STG_UNUSED,
            StgTSO      *tso   STG_UNUSED,
            char        *label STG_UNUSED)
{
    int len = strlen(label);
    StgArrBytes *arr = allocateArrBytes(cap, len+1);
    memcpy(&arr->payload, label, len);
    arr->payload[len] = '\0';
    setThreadLabel(cap, tso, arr);
}

void
setThreadLabel(Capability  *cap   STG_UNUSED,
               StgTSO      *tso   STG_UNUSED,
               StgArrBytes *label STG_UNUSED)
{
    if (tso->label) {
        IF_NONMOVING_WRITE_BARRIER_ENABLED {
            updateRemembSetPushClosure(cap, (StgClosure *) tso->label);
        }
        recordClosureMutated(cap, (StgClosure*)tso);
    }
    tso->label = label;
    traceThreadLabel(cap, tso, (char *) label->payload);
}
