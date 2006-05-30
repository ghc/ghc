/* -----------------------------------------------------------------------------
 *
 * (c) The University of Glasgow 2006
 *
 * OS-specific memory management
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "OSMem.h"

#include <unistd.h>
#include <sys/types.h>
#include <sys/mman.h>

/* no C99 header stdint.h on OpenBSD? */
#if defined(openbsd_HOST_OS)
typedef unsigned long my_uintptr_t;
#else
#include <stdint.h>
typedef uintptr_t my_uintptr_t;
#endif

lnat getPageSize (void)
{
    static lnat pageSize = 0;
    if (pageSize) {
	return pageSize;
    } else {
	long ret;
	ret = sysconf(_SC_PAGESIZE);
	if (ret == -1) {
	    barf("getPageSize: cannot get page size");
	}
	return ret;
    }
}

void setExecutable (void *p, lnat len, rtsBool exec)
{
    my_uintptr_t pageSize = getPageSize();

    /* malloced memory isn't executable by default on OpenBSD */
    my_uintptr_t mask             = ~(pageSize - 1);
    my_uintptr_t startOfFirstPage = ((my_uintptr_t)p          ) & mask;
    my_uintptr_t startOfLastPage  = ((my_uintptr_t)p + len - 1) & mask;
    my_uintptr_t size             = startOfLastPage - startOfFirstPage + pageSize;
    if (mprotect((void*)startOfFirstPage, (size_t)size, 
		 (exec ? PROT_EXEC : 0) | PROT_READ | PROT_WRITE) != 0) {
	barf("makeExecutable: failed to protect 0x%p\n", p);
    }
}
