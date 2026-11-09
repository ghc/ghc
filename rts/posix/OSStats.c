/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2005
 *
 * Machine-dependent process statistics functions
 *
 * ---------------------------------------------------------------------------*/

// Not POSIX, due to use of ru_majflt in getPageFaults()
// #include "rts/PosixSource.h"

#include "Rts.h"
#include "OSStats.h"

#include <unistd.h>

#if defined(HAVE_SYS_RESOURCE_H)
# include <sys/resource.h>
#endif

long getPageFaults(void)
{
#if !defined(HAVE_GETRUSAGE) || defined(haiku_HOST_OS)
    return 0;
#else
    struct rusage t;
    getrusage(RUSAGE_SELF, &t);
    return(t.ru_majflt);
#endif
}
