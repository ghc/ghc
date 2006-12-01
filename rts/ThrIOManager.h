/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2006
 *
 * Communicating with the IO manager thread (see GHC.Conc).
 * Posix implementation in posix/Signals.c
 * Win32 implementation in win32/ThrIOManager.c
 *
 * -------------------------------------------------------------------------*/

#if defined(THREADED_RTS)
void ioManagerWakeup (void);
void ioManagerDie (void);
void ioManagerStart (void);
#endif
