/* -----------------------------------------------------------------------------
 * $Id: InitEachPE.c,v 1.2 1998/12/02 13:29:12 simonm Exp $
 *
 * User-overridable RTS hooks.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"

#ifdef PAR
void
initEachPEHook (void)
{ /* in a GUM setup this is called on each
     PE immediately before SynchroniseSystem
     it can be used to read in static data 
     to each PE which has to be available to
     each PE

     This version is the one specialised 
     for Lolita, calling the LoadAllData stuff.
     The default version probably should do 
     nothing -- HWL
  */
}
#endif
