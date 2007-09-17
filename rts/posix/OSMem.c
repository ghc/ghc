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
    StgWord pageSize = getPageSize();

    /* malloced memory isn't executable by default on OpenBSD */
    StgWord mask             = ~(pageSize - 1);
    StgWord startOfFirstPage = ((StgWord)p          ) & mask;
    StgWord startOfLastPage  = ((StgWord)p + len - 1) & mask;
    StgWord size             = startOfLastPage - startOfFirstPage + pageSize;
    if (mprotect((void*)startOfFirstPage, (size_t)size, 
		 (exec ? PROT_EXEC : 0) | PROT_READ | PROT_WRITE) != 0) {
	barf("makeExecutable: failed to protect 0x%p\n", p);
    }
}
