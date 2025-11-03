/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2025
 *
 * Utilities for a simple fd-based cross-thread wakeup mechanism.
 *
 * It provides a mechanism for a thread that block on fds to add a simple
 * wakeup/notification feature.
 *
 * Start with newFdWakeup, and pass the fd_r to the thread that needs the
 * wakeup feature. The thread that needs to be woken should include the fd_r
 * into the set of fds that the thread waits on (e.g. using poll or similar).
 * If this fd becomes ready for read, the thread must call collectFdWakeup,
 * and when a wake up is needed, the write end fd is used. In any other thread
 * (or in a signal handler), call sendFdWakeup(fd_w) to (asynchronously) cause
 * the wakeup.
 *
 * There is no message payload. Multiple wakeups may be combined (if they're
 * sent multiple times before the notified thread can wake and call
 * collectFdWakeup).
 *
 * The implementation uses pipe() or eventfd() on supported OSs.
 *
 * Prototypes for functions in FdWakeup.c
 *
 * -------------------------------------------------------------------------*/

#pragma once

#include "BeginPrivate.h"

void newFdWakeup(int *fd_r, int *fd_w);
void closeFdWakeup(int fd_r, int fd_w);

/* This is safe to use from a signal handler */
void sendFdWakeup(int fd_w);
void collectFdWakeup(int fd_r);

#include "EndPrivate.h"

