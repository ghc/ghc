/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005, 2026
 *
 * Shared utilities for I/O managers: signal handling functionality.
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#if defined(RTS_USER_SIGNALS)

#include <signal.h>

#include "BeginPrivate.h"

/*
 * Function: initUserSignals()
 *
 * Initialize the console handling substrate.
 */
void initUserSignals(void);

/* Finalise signal and console events handling.
 */
void finiUserSignals(void);

void startSignalHandler(Capability *cap, int sig, siginfo_t *info);

typedef void (*SignalHandler)(int sig, siginfo_t *info, void *p);

int installSignalHandler(int sig, int spi, sigset_t *mask, bool nocldstop,
                         SignalHandler handler);

/* Utilities to help with the self-pipe trick for signal handling. */
void newNonBlockingPipe(int *pipe_fd_r, int *pipe_fd_w);
void writeSignalInfoToPipe (int  sig, siginfo_t *info, int fd);
bool readSignalInfoFromPipe(int *sig, siginfo_t *info, int fd);

#endif

#include "EndPrivate.h"

