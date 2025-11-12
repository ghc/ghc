/*
 * Console control handler support.
 *
 * NOTE: This is the MIO manager, only used for --io-manager=posix.
 *       For the WINIO manager see base in the GHC.Event modules.
 */

#pragma once

/*
 * Console control handlers lets an application handle Ctrl+C, Ctrl+Break etc.
 * in Haskell under Win32. Akin to the Unix signal SIGINT.
 *
 * The API offered by ConsoleHandler.h is identical to that of the
 * signal handling code (which isn't supported under win32.)
 * Unsurprisingly, the underlying impl is derived from the signal
 * handling code also.
 */

#if !defined(THREADED_RTS)
/*
 * under THREADED_RTS, console events are passed to the IO manager
 * thread, which starts up the handler.  See ThrIOManager.c.
 */

/*
 * Function: rts_waitConsoleHandlerCompletion()
 *
 * Esoteric entry point used by worker thread that got woken
 * up as part Ctrl-C delivery.
 */
extern int rts_waitConsoleHandlerCompletion(void);

#endif /* THREADED_RTS */
