/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2005
 *
 * Machine-dependent process statistics functions
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "OSStats.h"

long getPageFaults(void)
{
  /* ToDo (on NT): better, get this via the performance data
     that's stored in the registry. */
    return 0;
}
