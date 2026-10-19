/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2009
 *
 * RTS signal handling
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/source-tree/includes
 *
 * ---------------------------------------------------------------------------*/

#pragma once

/* Used by runProcess() in the process package, but this is almost certainly
 * a bad idea.
 *
 * See also:
 *  + https://github.com/haskell/process/issues/384
 *  + https://github.com/haskell/process/issues/385
 *  + GHC issue #27968
 *  + See Note [pthread_sigmask and the ineffectiveness of masking signals]
 */

/*
 * Function: blockUserSignals()
 *
 * Temporarily block the delivery of further user-registered posix signals.
 *
 * This is exported from the RTS but should no longer be used. It is likely to
 * be removed in future.
 */
void blockUserSignals(void);

/*
 * Function: unblockUserSignals()
 *
 * The inverse of blockUserSignals(); re-enable the deliver of console events.
 */
void unblockUserSignals(void);
