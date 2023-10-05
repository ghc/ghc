#include "HsBaseConfig.h"

/* For _SC_CLK_TCK */
#if HAVE_UNISTD_H
#include <unistd.h>
#endif

/* for CLK_TCK */
#include <time.h>

long clk_tck(void) {
#if defined(CLK_TCK)
    return (CLK_TCK);
#else
    return sysconf(_SC_CLK_TCK);
#endif
}
