/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2009
 *
 * RTS signal handling 
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
 *
 * ---------------------------------------------------------------------------*/

#ifndef RTS_BLOCKSIGNALS_H
#define RTS_BLOCKSIGNALS_H

/* Used by runProcess() in the process package
 */

/*
 * Function: blockUserSignals()
 *
 * Temporarily block the delivery of further console events. Needed to
 * avoid race conditions when GCing the queue of outstanding handlers or
 * when emptying the queue by running the handlers.
 * 
 */
void blockUserSignals(void);

/*
 * Function: unblockUserSignals()
 *
 * The inverse of blockUserSignals(); re-enable the deliver of console events.
 */
void unblockUserSignals(void);

#endif /* RTS_BLOCKSIGNALS_H */
