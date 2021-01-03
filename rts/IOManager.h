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

#include "EndPrivate.h"
