/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2009
 *
 * User-overridable RTS hooks.
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   http://hackage.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
 *
 * ---------------------------------------------------------------------------*/

#ifndef RTS_HOOKS_H
#define RTS_HOOKS_H

extern char *ghc_rts_opts;

extern void OnExitHook (void);
extern int  NoRunnableThreadsHook (void);
extern void StackOverflowHook (W_ stack_size);
extern void OutOfHeapHook (W_ request_size, W_ heap_size);
extern void MallocFailHook (W_ request_size /* in bytes */, char *msg);
extern void defaultsHook (void);

#endif /* RTS_HOOKS_H */
