#include "HsBaseConfig.h"

/* For _SC_CLK_TCK */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif

/* for CLK_TCK */
#if HAVE_TIME_H
#include <time.h>
#endif

long clk_tck(void) {
#if defined(CLK_TCK)
    return (CLK_TCK);
#else
    return sysconf(_SC_CLK_TCK);
#endif
}
