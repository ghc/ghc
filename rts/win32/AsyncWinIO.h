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

extern bool startupAsyncWinIO(void);
extern void shutdownAsyncWinIO(bool wait_threads);
extern void awaitAsyncRequests(bool wait);
extern void registerIOCPHandle (HANDLE port);
extern void registerAlertableWait (bool has_timeout, DWORD mssec);

extern OVERLAPPED_ENTRY* getOverlappedEntries (uint32_t *num);
extern void completeSynchronousRequest (void);
extern bool queueIOThread(void);
