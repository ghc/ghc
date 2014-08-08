/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-2005
 *
 * Prototypes for functions in Select.c
 *
 * -------------------------------------------------------------------------*/

#ifndef POSIX_SELECT_H
#define POSIX_SELECT_H

// An absolute time value in units of 10ms.
typedef StgWord LowResTime;

RTS_PRIVATE LowResTime getDelayTarget (HsInt us);

#endif /* POSIX_SELECT_H */

// Local Variables:
// mode: C
// fill-column: 80
// indent-tabs-mode: nil
// c-basic-offset: 4
// buffer-file-coding-system: utf-8-unix
// End:
