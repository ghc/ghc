/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2025
 *
 * Utilities for a simple fd-based cross-thread wakeup mechanism.
 *
 * This is used in I/O managers, to provide a mechanism to wake them when they
 * are blocked waiting on fds and timeouts. The mechanism works by including
 * the read end fd into the set of fds the I/O manager waits on, and when a
 * wake up is needed, the write end fd is used.
 *
 * Prototypes for functions in FdWakeup.c
 *
 * -------------------------------------------------------------------------*/

#pragma once

#include "BeginPrivate.h"

void newFdWakeup(int *fd_r, int *fd_w);
void closeFdWakeup(int fd_r, int fd_w);

void sendFdWakeup(int fd_w);
void collectFdWakeup(int fd_r);

#include "EndPrivate.h"

