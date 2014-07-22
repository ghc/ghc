/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Profiling interval timer
 *
 * ---------------------------------------------------------------------------*/

#ifndef PROFTIMER_H
#define PROFTIMER_H

#include "BeginPrivate.h"

void initProfTimer      ( void );
void handleProfTick     ( void );

void stopHeapProfTimer  ( void );
void startHeapProfTimer ( void );

extern rtsBool performHeapProfile;

#include "EndPrivate.h"

#endif /* PROFTIMER_H */

// Local Variables:
// mode: C
// fill-column: 80
// indent-tabs-mode: nil
// c-basic-offset: 4
// buffer-file-coding-system: utf-8-unix
// End:
