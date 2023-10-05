/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2006
 *
 * The IO manager thread in THREADED_RTS.
 * See also libraries/base/GHC/Conc.hs.
 *
 * NOTE: This is used by both MIO and WINIO
 * ---------------------------------------------------------------------------*/

#pragma once

/* Communicating with the IO manager thread (see GHC.Conc).
 */
void ioManagerWakeup (void);
void ioManagerDie (void);
void ioManagerStart (void);
void ioManagerFinished (void);
