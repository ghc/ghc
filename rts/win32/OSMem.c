/* -----------------------------------------------------------------------------
 *
 * (c) The University of Glasgow 2006
 *
 * OS-specific memory management
 *
 * ---------------------------------------------------------------------------*/

#include <windows.h>
#include "Rts.h"
#include "OSMem.h"

lnat getPageSize (void)
{
    static lnat pagesize = 0;
    if (pagesize) {
	return pagesize;
    } else {
	SYSTEM_INFO sSysInfo;
	GetSystemInfo(&sSysInfo);
	pagesize = sSysInfo.dwPageSize;
	return pagesize;
    }
}

void setExecutable (void *p, lnat len, rtsBool exec)
{
    DWORD dwOldProtect = 0;
    if (VirtualProtect (p, len, 
			exec ? PAGE_EXECUTE_READWRITE : PAGE_READWRITE, 
			&dwOldProtect) == 0)
    {
	barf("makeExecutable: failed to protect 0x%p; error=%lu; old protection: %lu\n",
	     p, (unsigned long)GetLastError(), (unsigned long)dwOldProtect);
    }
}
