/* -----------------------------------------------------------------------------
 *
 * User-overridable RTS hooks.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"

#ifdef PAR
void
InitEachPEHook (void)
{ /* In a GUM setup this is called on each
     PE immediately before SynchroniseSystem.
     It can be used to read in static data 
     to each PE which has to be available to
     each PE. See GPH-Maple as an example how to
     use this in combination with foreign language
     code:
       http://www.risc.uni-linz.ac.at/software/ghc-maple/
     -- HWL
  */
}
#endif
