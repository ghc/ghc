// glibc will only expose the POSIX strerror_r if this is defined.
#define _POSIX_C_SOURCE 200112L

#include <string.h>
#include <errno.h>

// This must be included after <string.h> lest _GNU_SOURCE may be defined.
#include "HsBaseConfig.h"

// returns zero on success
int base_strerror_r(int errnum, char *ptr, size_t buflen)
{
#if defined(HAVE_STRERROR_R)
    int ret = strerror_r(errnum, ptr, buflen);
    if (ret == ERANGE) {
        // Ellipsize the error
        ptr[buflen-4] = '.';
        ptr[buflen-3] = '.';
        ptr[buflen-2] = '.';
        ret = 0;
    }
    return ret;
#elif defined(HAVE_STRERROR_S)
    strerror_s(ptr, buflen, errnum);
    return 0;
#else
#error neither strerror_r nor strerror_s are supported
#endif
}
