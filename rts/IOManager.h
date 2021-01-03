/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2020
 *
 * Prototypes for functions in IOManager.c and elsewhere
 *
 * Hooks for the I/O subsystem(s) that are called from other parts of the RTS.
 *
 * There are several different I/O subsystem implementations (aka I/O managers),
 * for different platforms (notably Windows vs others), and for the threaded vs
 * non-threaded RTS. These implementations all need hooks into other parts of
 * the RTS, such as startup/shutdown, the scheduler and other special features.
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
void initIOManager(void);

/* Init hook: called from forkProcess in the child process on the surviving
 * capability.
 *
 * Note that this is synchronous and can run Haskell code, so can change the
 * given cap.
 */
void initIOManagerAfterFork(/* inout */ Capability **pcap);

/* TODO: rationalise initIOManager and initIOManagerAfterFork into a single
         per-capability init function.
 */


/* Shutdown hooks: called from hs_exit_ before and after the scheduler exits.
 */
void stopIOManager(void);
void exitIOManager(bool wait_threads);

/*
 * Communicating with the IO manager thread (see GHC.Conc).
 * Posix implementation in posix/Signals.c
 * Win32 implementation in win32/ThrIOManager.c, Windows's WINIO has the same
 * interfaces for Threaded and Non-threaded I/O, so these methods are always
 * available for WINIO.
*/
void ioManagerWakeup (void);
#if defined(THREADED_RTS) || defined(mingw32_HOST_OS)
void ioManagerDie (void);
void ioManagerStart (void);
#endif

/* Pedantic warning cleanliness
 */
#if !defined(THREADED_RTS) && defined(mingw32_HOST_OS)
#define USED_IF_NOT_THREADS_AND_MINGW32
#else
#define USED_IF_NOT_THREADS_AND_MINGW32 STG_UNUSED
#endif

#if defined(THREADED_RTS) && !defined(mingw32_HOST_OS)
#define USED_IF_THREADS_AND_NOT_MINGW32
#else
#define USED_IF_THREADS_AND_NOT_MINGW32 STG_UNUSED
#endif


#include "EndPrivate.h"
