/* -----------------------------------------------------------------------------
 *
 * (c) The University of Glasgow 2006-2008
 *
 * OS-specific memory management
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "BeginPrivate.h"

void osMemInit(void);
void *osGetMBlocks(uint32_t n);
void osFreeMBlocks(void *addr, uint32_t n);
void osReleaseFreeMemory(void);
void osFreeAllMBlocks(void);
size_t getPageSize (void);
StgWord64 getPhysicalMemorySize (void);
void setExecutable (void *p, W_ len, bool exec);
bool osBuiltWithNumaSupport(void); // See #14956
bool osNumaAvailable(void);
uint32_t osNumaNodes(void);
uint64_t osNumaMask(void);
void osBindMBlocksToNode(void *addr, StgWord size, uint32_t node);

INLINE_HEADER size_t
roundDownToPage (size_t x)
{
    size_t size = getPageSize();
    return (x & ~(size - 1));
}

INLINE_HEADER size_t
roundUpToPage (size_t x)
{
    size_t size = getPageSize();
    return ((x + size - 1) & ~(size - 1));
}


#if defined(USE_LARGE_ADDRESS_SPACE)

/*
  If "large address space" is enabled, we allocate memory in two
  steps: first we request some address space, and then we request some
  memory in it. This allows us to ask for much more address space that
  we will ever need, which keeps everything nice and consecutive.
*/

// Reserve the large address space blob of the given size, and return the
// address that the OS has chosen for it. It is not safe to access the memory
// pointed to by the return value, until that memory is committed using
// osCommitMemory().
//
// The value pointed to by len will be filled by the caller with an upper
// bound on the amount of memory to reserve. On return this will be set
// to the amount of memory actually reserved.
//
// This function is called once when the block allocator is initialized.
//
// startAddress must be greater or equal than 8 * (1 << 30), and can be
// NULL, in which case a default will be picked by the RTS.
void *osReserveHeapMemory(void *startAddress, W_ *len);

// Commit (allocate memory for) a piece of address space, which must
// be within the previously reserved space After this call, it is safe
// to access @p up to @len bytes.
//
// There is no guarantee on the contents of the memory pointed to by
// @p, in particular it must not be assumed to contain all zeros.
void osCommitMemory(void *p, W_ len);

// Decommit (release backing memory for) a piece of address space,
// which must be within the previously reserve space and must have
// been previously committed After this call, it is again unsafe to
// access @p (up to @len bytes), but there is no guarantee that the
// memory will be released to the system (as far as eg. RSS statistics
// from top are concerned).
void osDecommitMemory(void *p, W_ len);

// Release the address space previously obtained and undo the effects of
// osReserveHeapMemory
//
// This function is called once, when the block allocator is deinitialized
// before the program terminates.
void osReleaseHeapMemory(void);
#endif

#include "EndPrivate.h"
