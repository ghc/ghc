/* AsyncIO.h
 *
 * Integrating Win32 asynchronous IOCP with the GHC RTS.
 *
 * (c) Tamar Christina, 2018
 *
 * NOTE: This is the WinIO manager, only used for --io-manager=native.
 *       For the MIO manager see AsyncIO.h.
 */

#pragma once

#include "Rts.h"
#include <stdbool.h>
#include <windows.h>

RTS_PUBLIC extern bool startupAsyncWinIO(void);
RTS_PUBLIC extern void shutdownAsyncWinIO(bool wait_threads);
RTS_PUBLIC extern void awaitAsyncRequests(bool wait);
RTS_PUBLIC extern void registerIOCPHandle (HANDLE port);
RTS_PUBLIC extern void registerAlertableWait (bool has_timeout, DWORD mssec);

RTS_PUBLIC extern OVERLAPPED_ENTRY* getOverlappedEntries (uint32_t *num);
RTS_PUBLIC extern void completeSynchronousRequest (void);
RTS_PUBLIC extern bool queueIOThread(void);
