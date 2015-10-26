/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2014-2015
 *
 * Producing stacktraces with DWARF unwinding using libdw..
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
 *
 * -------------------------------------------------------------------------- */

#ifndef LIBDW_H
#define LIBDW_H

#include "BeginPrivate.h"

#ifdef USE_LIBDW

struct LibDwSession_;
typedef struct LibDwSession_ LibDwSession;

/* Begin a libdw session. A session is tied to a particular capability */
LibDwSession *libdw_init(void);

/* Free a session */
void libdw_free(LibDwSession *session);

/* Request a backtrace of the current stack state */
Backtrace *libdw_get_backtrace(LibDwSession *session);

/* Lookup Location information for the given address.
 * Returns 0 if successful, 1 if address could not be found. */
int libdw_lookup_location(LibDwSession *session, Location *loc, StgPtr pc);

/* Pretty-print a backtrace to std*/
void libdw_print_backtrace(LibDwSession *session, FILE *file, Backtrace *bt);

// Traverse backtrace in order of outer-most to inner-most frame
#define FOREACH_FRAME_INWARDS(pc, bt)                                 \
    BacktraceChunk *_chunk;                                           \
    unsigned int _frame_idx;                                          \
    for (_chunk = &bt->frames; _chunk != NULL; _chunk = _chunk->next) \
        for (_frame_idx=0;                                            \
             pc = _chunk->frames[_frame_idx], _frame_idx < _chunk->n_frames; \
             _frame_idx++)

// Traverse a backtrace in order of inner-most to outer-most frame
int foreach_frame_outwards(Backtrace *bt,
                           int (*cb)(StgPtr, void*),
                           void *user_data);

#endif /* USE_LIBDW */

#include "EndPrivate.h"

#endif /* LIBDW_H */
