/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2009
 *
 * IO Manager functionality in the RTS
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
 *
 * -------------------------------------------------------------------------- */

#ifndef RTS_IOMANAGER_H
#define RTS_IOMANAGER_H

#if defined(mingw32_HOST_OS)

int  rts_InstallConsoleEvent ( int action, StgStablePtr *handler );
void rts_ConsoleHandlerDone  ( int ev );
extern StgInt console_handler;

void *   getIOManagerEvent  (void);
HsWord32 readIOManagerEvent (void);
void     sendIOManagerEvent (HsWord32 event);

#else

void     setIOManagerControlFd   (int fd);
void     setIOManagerWakeupFd   (int fd);

#endif

//
// Communicating with the IO manager thread (see GHC.Conc).
// Posix implementation in posix/Signals.c
// Win32 implementation in win32/ThrIOManager.c
//
void ioManagerWakeup (void);
#if defined(THREADED_RTS)
void ioManagerDie (void);
void ioManagerStart (void);
#endif

#endif /* RTS_IOMANAGER_H */
