/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2009
 *
 * IO Manager functionality in the RTS
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/source-tree/includes
 *
 * -------------------------------------------------------------------------- */

#pragma once

#ifndef RTS_EXPORT
# define RTS_EXPORT
#endif

#if defined(mingw32_HOST_OS)

#define IO_MANAGER_WAKEUP 0xffffffff
#define IO_MANAGER_DIE    0xfffffffe
/* spurious wakeups are returned as zero.  */
/* console events are ((event<<1) | 1).  */

RTS_EXPORT int  rts_InstallConsoleEvent ( int action, StgStablePtr *handler );
RTS_EXPORT void rts_ConsoleHandlerDone  ( int ev );
extern RTS_EXPORT StgInt console_handler;

RTS_EXPORT void *   getIOManagerEvent  (void);
RTS_EXPORT HsWord32 readIOManagerEvent (void);
RTS_EXPORT void     sendIOManagerEvent (HsWord32 event);
RTS_EXPORT void     ioManagerFinished  (void);

#else

RTS_EXPORT void     setIOManagerControlFd   (uint32_t cap_no, int fd);
RTS_EXPORT void     setTimerManagerControlFd(int fd);
RTS_EXPORT void     setIOManagerWakeupFd   (int fd);

#endif

