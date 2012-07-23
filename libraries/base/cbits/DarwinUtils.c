#include "HsBase.h"

#ifdef darwin_HOST_OS

static double scaling_factor = 0.0;

void initialize_timer()
{
    mach_timebase_info_data_t info;
    (void) mach_timebase_info(&info);
    scaling_factor = (double)info.numer / (double)info.denom;
    scaling_factor *= 1e-9;
}

void absolute_time(double *result)
{
    uint64_t time = mach_absolute_time();
    *result = (double)time * scaling_factor;
}

#endif
