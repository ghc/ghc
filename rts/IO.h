/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2020
 *
 * Prototypes for functions in IO.c
 *
 * Hooks for the I/O subsystem(s) that are called from other parts of the RTS.
 *
 * There are several different I/O subsystem implementations, for different
 * platforms (notably Windows vs others), and for the threaded vs non-threaded
 * RTS. These implementations all need hooks into other parts of the RTS, such
 * as startup/shutdown, the scheduler and other special features.
 *
 * To keep things comprehensible, all the hooks used by all the different I/O
 * subsystem implementations are centralised here. Not all implementations use
 * all hooks.
 *
 * -------------------------------------------------------------------------*/

#pragma once

#include "BeginPrivate.h"

/* Init hook: called from hs_init_ghc.
 */
RTS_PRIVATE void initIOManager(void);


/* Shutdown hooks: called from hs_exit_ before and after the scheduler exits.
 */
RTS_PRIVATE void stopIOManager(void);
RTS_PRIVATE void exitIOManager(bool wait_threads);

#include "EndPrivate.h"

