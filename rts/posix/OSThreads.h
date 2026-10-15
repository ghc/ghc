/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2001-2009
 *
 * Extra posix-specific thread functions
 *
 * See rts/include/rts/OSThreads.h for the OS-independent API
 *
 * --------------------------------------------------------------------------*/

#pragma once

int createPThread( OSThreadId *tid, const char *name,
                   OSThreadProc *startProc, void *param,
                   pthread_attr_t *attr);
