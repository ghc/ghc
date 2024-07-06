#include "Rts.h"

#include "sm/OSMem.h"
#include "linker/MMap.h"
#include "Trace.h"
#include "ReportMemoryMap.h"

#if RTS_LINKER_USE_MMAP
#include <sys/mman.h>
#endif

/* Link objects into the lower 2Gb on x86_64 and AArch64.  GHC assumes the
 * small memory model on this architecture (see gcc docs,
 * -mcmodel=small).
 *
 * MAP_32BIT not available on OpenBSD/amd64
 */
#if defined(MAP_32BIT) && (defined(x86_64_HOST_ARCH) || (defined(aarch64_TARGET_ARCH) || defined(aarch64_HOST_ARCH)))
#define MAP_LOW_MEM
#define TRY_MAP_32BIT MAP_32BIT
#else
#define TRY_MAP_32BIT 0
#endif

/* MAP_ANONYMOUS is MAP_ANON on some systems,
   e.g. OS X (before Sierra), OpenBSD etc */
#if !defined(MAP_ANONYMOUS) && defined(MAP_ANON)
#define MAP_ANONYMOUS MAP_ANON
#endif

/* In order to simplify control flow a bit, some references to mmap-related
   definitions are blocked off by a C-level if statement rather than a CPP-level
   #if statement. Since those are dead branches when !RTS_LINKER_USE_MMAP, we
   just stub out the relevant symbols here
*/
#if !RTS_LINKER_USE_MMAP
#define munmap(x,y) /* nothing */
#define MAP_ANONYMOUS 0
#endif

void *mmap_32bit_base = LINKER_LOAD_BASE;

void initLinkerMMap(void) {
    if (RtsFlags.MiscFlags.linkerMemBase != 0) {
        // User-override for mmap_32bit_base
        mmap_32bit_base = (void*)RtsFlags.MiscFlags.linkerMemBase;
    }
}

static const char *memoryAccessDescription(MemoryAccess mode)
{
  switch (mode) {
  case MEM_NO_ACCESS:    return "no-access";
  case MEM_READ_ONLY:    return "read-only";
  case MEM_READ_WRITE:   return "read-write";
  case MEM_READ_WRITE_THEN_READ_EXECUTE:
                         return "read-write-then-read-execute";
  case MEM_READ_EXECUTE: return "read-execute";
  case MEM_READ_WRITE_EXECUTE:
                         return "read-write-execute";
  default: barf("invalid MemoryAccess");
  }
}

/* A region of memory that we might map into. */
struct MemoryRegion {
    void *start;
    void *end;
    void *last;
      /* the end of the last mapping which we made into this region.
       * this is where we will start searching next time we need to allocate.
       */
};

#if defined(mingw32_HOST_OS)

/* A wrapper for VirtualQuery() providing useful debug output */
static int virtualQuery(void *baseAddr, PMEMORY_BASIC_INFORMATION info)
{
    int res = VirtualQuery (baseAddr, info, sizeof (*info));
    IF_DEBUG(linker_verbose,
        debugBelch("Probing region 0x%p (0x%p) - 0x%p (%" FMT_SizeT ") [%ld] with base 0x%p\n",
                   baseAddr,
                   info->BaseAddress,
                   (uint8_t *) info->BaseAddress + info->RegionSize,
                   info->RegionSize, info->State,
                   info->AllocationBase));
    if (!res) {
        IF_DEBUG(linker_verbose, debugBelch("Querying 0x%p failed. Aborting..\n", baseAddr));
        return 1;
    }
    return 0;
}

static inline uintptr_t round_up(uintptr_t num, uint64_t factor)
{
    return num + factor - 1 - (num + factor - 1) % factor;
}

/*
 * Try and find a location in the VMMAP to allocate SZ bytes starting at
 * BASEADDR.  If successful then location to use is returned and the amount of
 * bytes you *must* allocate is returned in REQ.  You are free to use less but
 * you must allocate the amount given in REQ.  If not successful NULL.
 */
static void *allocateBytes(void* baseAddr, void *endAddr, size_t sz, size_t *req)
{
    SYSTEM_INFO sys;
    GetSystemInfo(&sys);

    IF_DEBUG(linker_verbose, debugBelch("Requesting mapping of %" FMT_SizeT " bytes between %p and %p\n",
                                sz, baseAddr, endAddr));

    MEMORY_BASIC_INFORMATION info;
    uint8_t *initialAddr = baseAddr;
    uint8_t *region = NULL;
    while (!region
           && initialAddr <= (uint8_t *) endAddr
           && (void *) initialAddr < sys.lpMaximumApplicationAddress)
    {
        int res = virtualQuery(initialAddr, &info);
        if (res) {
            return NULL;
        }

        if ((info.State & MEM_FREE) == MEM_FREE) {
            IF_DEBUG(linker_verbose, debugBelch("Free range at 0x%p of %zu bytes\n",
                                        info.BaseAddress, info.RegionSize));

            if (info.RegionSize >= sz) {
                if (info.AllocationBase == 0) {
                    size_t needed_sz = round_up (sz, sys.dwAllocationGranularity);
                    if (info.RegionSize >= needed_sz) {
                        IF_DEBUG(linker_verbose, debugBelch("Range is unmapped, Allocation "
                                                    "required by granule...\n"));
                        *req = needed_sz;
                        region
                        = (void*)(uintptr_t)round_up ((uintptr_t)initialAddr,
                                                        sys.dwAllocationGranularity);
                        IF_DEBUG(linker_verbose, debugBelch("Requested %" PRId64 ", rounded: %"
                                                    PRId64 ".\n", sz, *req));
                        IF_DEBUG(linker_verbose, debugBelch("Aligned region claimed 0x%p -> "
                                                    "0x%p.\n", initialAddr, region));
                    }
                } else {
                    IF_DEBUG(linker_verbose, debugBelch("Range is usable for us, claiming...\n"));
                    *req = sz;
                    region = initialAddr;
                }
            }
        }
        initialAddr = (uint8_t *) info.BaseAddress + info.RegionSize;
    }

    return region;
}

/* Find free address space for mapping anonymous memory. */
static void *allocateLocalBytes(size_t sz, size_t *req)
{
    // We currently don't attempt to take address space from the region below
    // the image as malloc() tends to like to use this space, but we could do if
    // necessary.
    size_t max_range = 0x7fffffff - sz;

    static void *base_addr = NULL;
    if (base_addr == NULL) {
        base_addr = GetModuleHandleW(NULL);
    }
    uint8_t *end_addr = (uint8_t *) base_addr + max_range;

    // We track the location of the last allocation to avoid having to
    // do a linear search of address space looking for space on every allocation
    // as this can easily devolve into quadratic complexity.
    static void *last_alloca = NULL;
    if (last_alloca == NULL) {
        // Start the search at the image base
        last_alloca = base_addr;
    }

    void *result = NULL;
    result = allocateBytes (last_alloca, end_addr, sz, req);
    if (result == NULL) {
        // We failed to find suitable address space; restart the search at base_addr.
        result = allocateBytes (base_addr, end_addr, sz, req);
    }

    if (result != NULL) {
        last_alloca = (uint8_t *) result + *req;
    }
    return result;
}

static DWORD
memoryAccessToProt(MemoryAccess access)
{
  switch (access) {
  case MEM_NO_ACCESS:    return PAGE_NOACCESS;
  case MEM_READ_ONLY:    return PAGE_READONLY;
  case MEM_READ_WRITE:   return PAGE_READWRITE;
  case MEM_READ_WRITE_THEN_READ_EXECUTE:
                         return PAGE_READWRITE;
  case MEM_READ_EXECUTE: return PAGE_EXECUTE_READ;
  case MEM_READ_WRITE_EXECUTE:
                         return PAGE_EXECUTE_READWRITE;
  default: barf("invalid MemoryAccess");
  }
}

//
// Returns NULL on failure.
//
void *
mmapAnonForLinker (size_t bytes)
{
  size_t size = 0;
  /* For linking purposes we want to load code within a 4GB range from the
     load address of the application.  As such we need to find a location to
     allocate at.   */
  void* region = allocateLocalBytes (bytes, &size);
  if (region == NULL) {
      return NULL;
  }
  return VirtualAlloc(region, size, MEM_COMMIT | MEM_RESERVE, PAGE_READWRITE);
}

void
munmapForLinker (void *addr, size_t bytes, const char *caller)
{
  if (VirtualFree(addr, 0, MEM_RELEASE) == 0) {
    sysErrorBelch("munmapForLinker: %s: Failed to unmap %zd bytes at %p",
                  caller, bytes, addr);
  }
}

/**
 * Change the allowed access modes of a region of memory previously allocated
 * with mmapAnonForLinker.
 */
void
mprotectForLinker(void *start, size_t len, MemoryAccess mode)
{
  DWORD old;
  if (len == 0) {
    return;
  }
  DWORD prot = memoryAccessToProt(mode);

  if (VirtualProtect(start, len, prot, &old) == 0) {
    sysErrorBelch("mprotectForLinker: failed to protect %zd bytes at %p as %s",
                  len, start, memoryAccessDescription(mode));
    ASSERT(false);
  }
}

#elif RTS_LINKER_USE_MMAP

static int
memoryAccessToProt(MemoryAccess access)
{
    switch (access) {
    case MEM_NO_ACCESS:    return 0;
    case MEM_READ_ONLY:    return PROT_READ;
    case MEM_READ_WRITE:   return PROT_READ | PROT_WRITE;
    case MEM_READ_WRITE_THEN_READ_EXECUTE:
#  if defined(netbsd_HOST_OS)
        /* PROT_MPROTECT(PROT_EXEC) means that the pages are going to be
         * marked as executable in the future. On NetBSD requesting
         * additional permissions with mprotect(2) only succeeds when
         * permissions were initially requested in this manner.
         */
                           return PROT_READ | PROT_WRITE | PROT_MPROTECT(PROT_EXEC);
#  else
                           return PROT_READ | PROT_WRITE;
#  endif
    case MEM_READ_EXECUTE: return PROT_READ | PROT_EXEC;
    case MEM_READ_WRITE_EXECUTE:
                           return PROT_READ | PROT_WRITE | PROT_EXEC;
    default: barf("invalid MemoryAccess");
    }
}

static void *
doMmap(void *map_addr, size_t bytes, int prot, uint32_t flags, int fd, int offset)
{
    flags |= MAP_PRIVATE;

    IF_DEBUG(linker_verbose,
             debugBelch("mmapForLinker: \tprotection %#0x\n", prot));
    IF_DEBUG(linker_verbose,
             debugBelch("mmapForLinker: \tflags      %#0x\n", flags));
    IF_DEBUG(linker_verbose,
             debugBelch("mmapForLinker: \tsize       %#0zx\n", bytes));
    IF_DEBUG(linker_verbose,
             debugBelch("mmapForLinker: \tmap_addr   %p\n", map_addr));

    void * result = mmap(map_addr, bytes, prot, flags, fd, offset);
    if (result == MAP_FAILED) {
        sysErrorBelch("mmap %zx bytes at %p", bytes, map_addr);
        reportMemoryMap();
        errorBelch("Try specifying an address with +RTS -xm<addr> -RTS");
        return NULL;
    }
    return result;
}


static struct MemoryRegion *
nearImage(void) {
    static struct MemoryRegion region = { NULL, NULL, NULL };
    if (region.end == NULL) {
        region.start = mmap_32bit_base;
        region.end = (uint8_t *) region.start + 0x80000000;
        region.last = region.start;
    }
    return &region;
}

static void *
mmapAnywhere (
        size_t bytes,
        MemoryAccess access,
        uint32_t flags,
        int fd,
        int offset)
{
    int prot = memoryAccessToProt(access);
    return doMmap(NULL, bytes, prot, flags, fd, offset);
}

static void *
mmapInRegion (
        struct MemoryRegion *region,
        size_t bytes,
        MemoryAccess access,
        uint32_t flags,
        int fd,
        int offset)
{
    bool wrapped = false;
    int prot = memoryAccessToProt(access);
    void *p = region->last;
    while (1) {
        void *result = doMmap(p, bytes, prot, flags, fd, offset);
        if (result == NULL) {
            // The mapping failed
            return NULL;
        } else if (result < region->start) {
            // Uh oh, we assume that mmap() will only give us a
            // an address at or after the requested address.
            // Try again.
            p = (uint8_t *) result + bytes;
        } else if (result < region->end) {
            // Success!
            region->last = (uint8_t *) result + bytes;
            return result;
        } else if (wrapped) {
            // We failed to find a suitable mapping
            munmap(result, bytes);
            reportMemoryMap();
            errorBelch("mmapForLinker: failed to mmap() memory below 2Gb; "
                       "asked for %zu bytes at %p. "
                       "Try specifying an address with +RTS -xm<addr> -RTS",
                       bytes, p);
            return NULL;
        }

        // mmap() gave us too high an address; wrap around and try again
        munmap(result, bytes);
        wrapped = true;
        p = region->start;
    }
}

/*
 * Map memory for code.
 * Returns NULL on failure.
 */
void *
mmapForLinker (size_t bytes, MemoryAccess access, uint32_t flags, int fd, int offset)
{
    bytes = roundUpToPage(bytes);
    struct MemoryRegion *region;

    IF_DEBUG(linker_verbose, debugBelch("mmapForLinker: start\n"));
    if (RtsFlags.MiscFlags.linkerAlwaysPic) {
        /* make no attempt at mapping low memory if we are assuming PIC */
        region = NULL;
    } else {
        region = nearImage();
    }

    /* Use MAP_32BIT if appropriate */
    if (region && region->end <= (void *) 0xffffffff) {
        flags |= TRY_MAP_32BIT;
    }

    void *result;
    if (region) {
        result = mmapInRegion(region, bytes, access, flags, fd, offset);
    }
    else {
        result = mmapAnywhere(bytes, access, flags, fd, offset);
    }
    IF_DEBUG(linker_verbose,
             debugBelch("mmapForLinker: mapped %zd bytes starting at %p\n",
                        bytes, result));
    IF_DEBUG(linker_verbose,
             debugBelch("mmapForLinker: done\n"));
    return result;
}

/*
 * Map read/write pages in low memory. Returns NULL on failure.
 */
void *
mmapAnonForLinker (size_t bytes)
{
    return mmapForLinker (bytes, MEM_READ_WRITE_THEN_READ_EXECUTE, MAP_ANONYMOUS, -1, 0);
}

void munmapForLinker (void *addr, size_t bytes, const char *caller)
{
    int r = munmap(addr, bytes);
    if (r == -1) {
        // Should we abort here?
        sysErrorBelch("munmap: %s", caller);
    }
}

/* Note [Memory protection in the linker]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * For many years the linker would simply map all of its memory
 * with PROT_READ|PROT_WRITE|PROT_EXEC. However operating systems have been
 * becoming increasingly reluctant to accept this practice (e.g. #17353,
 * #12657) and for good reason: writable code is ripe for exploitation.
 *
 * Consequently mmapForLinker now maps its memory with PROT_READ|PROT_WRITE.
 * After the linker has finished filling/relocating the mapping it must then
 * call mprotectForLinker on the sections of the mapping which
 * contain executable code.
 *
 * Note that the m32 allocator handles protection of its allocations. For this
 * reason the caller to m32_alloc() must tell the allocator whether the
 * allocation needs to be executable. The caller must then ensure that they
 * call m32_allocator_flush() after they are finished filling the region, which
 * will cause the allocator to change the protection bits to
 * PROT_READ|PROT_EXEC.
 *
 */

/*
 * Mark an portion of a mapping previously reserved by mmapForLinker
 * as executable (but not writable).
 */
void mprotectForLinker(void *start, size_t len, MemoryAccess mode)
{
    if (len == 0) {
        return;
    }
    IF_DEBUG(linker_verbose,
             debugBelch("mprotectForLinker: protecting %" FMT_Word
                        " bytes starting at %p as %s\n",
                        (W_)len, start, memoryAccessDescription(mode)));

    int prot = memoryAccessToProt(mode);

    if (mprotect(start, len, prot) == -1) {
        sysErrorBelch("mprotectForLinker: failed to protect %zd bytes at %p as %s",
                      len, start, memoryAccessDescription(mode));
    }
}
#endif
