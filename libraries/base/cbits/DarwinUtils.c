#include "HsBase.h"

#ifdef darwin_HOST_OS

void absolute_time(double *result)
{
    uint64_t time = mach_absolute_time();
    static double scaling_factor = 0.0;

    if (scaling_factor == 0.0)
    {
        mach_timebase_info_data_t info;
        (void) mach_timebase_info(&info);
        scaling_factor = (double)info.numer / (double)info.denom;
        scaling_factor *= 1e-9;
    }

    *result = (double)time * scaling_factor;
}

#endif
