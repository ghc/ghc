/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2014-2015
 *
 * Producing stacktraces with DWARF unwinding using libdw..
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/source-tree/includes
 *
 * -------------------------------------------------------------------------- */

#pragma once

#include "BeginPrivate.h"

#if USE_LIBDW

/* Begin a libdw session. A session is tied to a particular capability */
LibdwSession *libdwInit(void);

/* Free a session */
void libdwFree(LibdwSession *session);

// Traverse backtrace in order of outer-most to inner-most frame
#define FOREACH_FRAME_INWARDS(pc, bt)                                 \
    BacktraceChunk *_chunk;                                           \
    unsigned int _frame_idx;                                          \
    for (_chunk = &bt->frames; _chunk != NULL; _chunk = _chunk->next) \
        for (_frame_idx=0;                                            \
             pc = _chunk->frames[_frame_idx], _frame_idx < _chunk->n_frames; \
             _frame_idx++)

// Traverse a backtrace in order of inner-most to outer-most frame
int libdwForEachFrameOutwards(Backtrace *bt,
                              int (*cb)(StgPtr, void*),
                              void *user_data);

#endif /* USE_LIBDW */

#include "EndPrivate.h"
