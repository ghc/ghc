/* -----------------------------------------------------------------------------
 *
 * (c) The University of Glasgow 2006-2007
 *
 * OS-specific memory management
 *
 * ---------------------------------------------------------------------------*/

// This is non-posix compliant.
// #include "rts/PosixSource.h"

#include "Rts.h"

#include "RtsUtils.h"
#include "sm/OSMem.h"
#include "sm/HeapAlloc.h"

#if defined(HAVE_UNISTD_H)
#include <unistd.h>
#endif
#if defined(HAVE_SYS_TYPES_H)
#include <sys/types.h>
#endif
#if defined(HAVE_SYS_MMAN_H)
#include <sys/mman.h>
#endif
#if defined(HAVE_STRING_H)
#include <string.h>
#endif
#if defined(HAVE_FCNTL_H)
#include <fcntl.h>
#endif
#if defined(HAVE_NUMA_H)
#include <numa.h>
#endif
#if defined(HAVE_NUMAIF_H)
#include <numaif.h>
#endif
#if defined(HAVE_SYS_RESOURCE_H) && defined(HAVE_SYS_TIME_H)
#include <sys/time.h>
#include <sys/resource.h>
#include <pthread.h>
#endif

#include <errno.h>

#if defined(darwin_HOST_OS) || defined(ios_HOST_OS)
#include <mach/mach.h>
#include <mach/vm_map.h>
#include <sys/sysctl.h>
#endif

#if !defined(MAP_FAILED)
# define MAP_FAILED ((void *)-1)
#endif

#if defined(hpux_HOST_OS)
# if !defined(MAP_ANON)
#  define MAP_ANON MAP_ANONYMOUS
# endif
#endif

#define HUGEPAGE_SIZE (2*1024*1024)
// This constant is from linux/mman.h
#define MAP_HUGE_2MB (21 << MAP_HUGE_SHIFT)
#define HUGEPAGE_FLAGS MAP_HUGETLB

#if !defined(darwin_HOST_OS)
# undef RESERVE_FLAGS
# if defined(MAP_GUARD)
#  define RESERVE_FLAGS  MAP_GUARD /* FreeBSD */
# elif defined(MAP_NORESERVE)
#  define RESERVE_FLAGS  MAP_NORESERVE | MAP_ANON | MAP_PRIVATE;
# else
#  if defined(USE_LARGE_ADDRESS_SPACE)
#   error USE_LARGE_ADDRESS_SPACE needs MAP_NORESERVE or MAP_GUARD
#  endif
# endif
#endif

int huge_tried = 0;
int huge_failed = 0;

static void *next_request = 0;

void osMemInit(void)
{
    next_request = (void *)RtsFlags.GcFlags.heapBase;
}

/* -----------------------------------------------------------------------------
   The mmap() method

   On Unix-like systems, we use mmap() to allocate our memory.  We
   want memory in chunks of MBLOCK_SIZE, and aligned on an MBLOCK_SIZE
   boundary.  The mmap() interface doesn't give us this level of
   control, so we have to use some heuristics.

   In the general case, if we want a block of n megablocks, then we
   allocate n+1 and trim off the slop from either side (using
   munmap()) to get an aligned chunk of size n.  However, the next
   time we'll try to allocate directly after the previously allocated
   chunk, on the grounds that this is aligned and likely to be free.
   If it turns out that we were wrong, we have to munmap() and try
   again using the general method.

   Note on posix_memalign(): this interface is available on recent
   systems and appears to provide exactly what we want.  However, it
   turns out not to be as good as our mmap() implementation, because
   it wastes extra space (using double the address space, in a test on
   x86_64/Linux).  The problem seems to be that posix_memalign()
   returns memory that can be free()'d, so the library must store
   extra information along with the allocated block, thus messing up
   the alignment.  Hence, we don't use posix_memalign() for now.

   -------------------------------------------------------------------------- */

/*
 A wrapper around mmap(), to abstract away from OS differences in
 the mmap() interface.

 It supports the following operations:
 - reserve: find a new chunk of available address space, and make it so
            that we own it (no other library will get it), but don't actually
            allocate memory for it
            the addr is a hint for where to place the memory (and most
            of the time the OS happily ignores!)
 - commit: given a chunk of address space that we know we own, make sure
           there is some memory backing it
           the addr is not a hint, it must point into previously reserved
           address space, or bad things happen
 - reserve&commit: do both at the same time

 The naming is chosen from the Win32 API (VirtualAlloc) which does the
 same thing and has done so forever, while support for this in Unix systems
 has only been added recently and is hidden in the posix portability mess.
 The Linux manpage suggests that mmap must be passed MAP_NORESERVE in order
 to get reservation-only behavior. It is confusing because to get the reserve
 behavior we need MAP_NORESERVE (which tells the kernel not to allocate backing
 space), but heh...
*/
enum
{
    MEM_RESERVE = 1,
    MEM_COMMIT = 2,
    MEM_RESERVE_AND_COMMIT = MEM_RESERVE | MEM_COMMIT
};

#if defined(linux_HOST_OS)
static void *
linux_retry_mmap(int operation, W_ size, void *ret, void *addr, int prot, int flags)
{
    if (addr != 0 && (operation & MEM_RESERVE)) {
        // Try again with no hint address.
        // It's not clear that this can ever actually help,
        // but since our alternative is to abort, we may as well try.
        ret = mmap(0, size, prot, flags, -1, 0);
    }
    if (ret == MAP_FAILED && errno == EPERM) {
        // Linux is not willing to give us any mapping,
        // so treat this as an out-of-memory condition
        // (really out of virtual address space).
        errno = ENOMEM;
    }
    return ret;
}
#endif /* defined(linux_HOST_OS) */

static void
post_mmap_madvise(int operation, W_ size, void *ret)
{
#if defined(MADV_WILLNEED)
    if (operation & MEM_COMMIT) {
        madvise(ret, size, MADV_WILLNEED);
# if defined(MADV_DODUMP)
        madvise(ret, size, MADV_DODUMP);
# endif
    } else {
        madvise(ret, size, MADV_DONTNEED);
# if defined(MADV_DONTDUMP)
        madvise(ret, size, MADV_DONTDUMP);
# endif
    }
#endif
}

/* Returns NULL on failure; errno set */
static void *
my_mmap (void *addr, W_ size, int operation)
{
    void *ret;

#if defined(darwin_HOST_OS)
    // Without MAP_FIXED, Apple's mmap ignores addr.
    // With MAP_FIXED, it overwrites already mapped regions, which
    // mmap(0, ... MAP_FIXED ...) is worst of all: It unmaps the program text
    // and replaces it with zeroes, causing instant death.
    // This behaviour seems to be conformant with IEEE Std 1003.1-2001.
    // Let's just use the underlying Mach Microkernel calls directly,
    // they're much nicer.

    kern_return_t err = 0;
    ret = addr;

    if(operation & MEM_RESERVE)
    {
        if(addr)    // try to allocate at address
            err = vm_allocate(mach_task_self(),(vm_address_t*) &ret,
                              size, false);
        if(!addr || err)    // try to allocate anywhere
            err = vm_allocate(mach_task_self(),(vm_address_t*) &ret,
                              size, true);
    }

    if(err) {
        // don't know what the error codes mean exactly, assume it's
        // not our problem though.
        errorBelch("memory allocation failed (requested %" FMT_Word " bytes)",
                   size);
        stg_exit(EXIT_FAILURE);
    }

    if(operation & MEM_COMMIT) {
        vm_protect(mach_task_self(), (vm_address_t)ret, size, false,
                   VM_PROT_READ|VM_PROT_WRITE);
    }

#else /* defined(darwin_HOST_OS) */

    int prot, flags;
    if (operation & MEM_COMMIT) {
        prot = PROT_READ | PROT_WRITE;
    } else {
        prot = PROT_NONE;
    }

    if (operation == MEM_RESERVE) {
# if defined(RESERVE_FLAGS)
        flags = RESERVE_FLAGS;
# else
        errorBelch("my_mmap(,,MEM_RESERVE) not supported on this platform");
# endif
    } else if (operation == MEM_COMMIT) {
        flags = MAP_FIXED | MAP_ANONYMOUS | MAP_PRIVATE;
        if ( RtsFlags.GcFlags.hugepages &&
            (size & (HUGEPAGE_SIZE - 1)) == 0) {
          huge_tried += 1;
          flags |= HUGEPAGE_FLAGS;
        }
    } else {
        flags = MAP_ANON | MAP_PRIVATE;
    }

    ret = mmap(addr, size, prot, flags, -1, 0);
    // If the mmap failed, and we tried with HUGEPAGE_FLAGS
    // then retry without.
    if (ret == MAP_FAILED && flags & HUGEPAGE_FLAGS){
      huge_failed += 1;
      flags &= ~HUGEPAGE_FLAGS;
      ret = mmap(addr, size, prot, flags, -1, 0);
    }
# if defined(linux_HOST_OS)
    if (ret == MAP_FAILED && errno == EPERM) {
        // Linux may return EPERM if it tried to give us
        // a chunk of address space below mmap_min_addr,
        // See #7500.
        ret = linux_retry_mmap(operation, size, ret, addr, prot, flags);
    }
# endif
    if (ret == MAP_FAILED) {
        return NULL;
    }
#endif /* defined(darwin_HOST_OS) */

    // Map in committed pages rather than take a fault for each chunk.
    // Also arrange to include them in core-dump files.
    post_mmap_madvise(operation, size, ret);

    return ret;
}

/* Variant of my_mmap which aborts in the case of an error */
static void *
my_mmap_or_barf (void *addr, W_ size, int operation)
{
    void *ret = my_mmap(addr, size, operation);

    if (ret == NULL) {
        if (errno == ENOMEM ||
            (errno == EINVAL && sizeof(void*)==4 && size >= 0xc0000000)) {
            // If we request more than 3Gig, then we get EINVAL
            // instead of ENOMEM (at least on Linux).
            errorBelch("out of memory (requested %" FMT_Word " bytes)", size);
            stg_exit(EXIT_HEAPOVERFLOW);
        } else {
            barf("getMBlock: mmap: %s", strerror(errno));
        }
    }

    return ret;
}

// Implements the general case: allocate a chunk of memory of 'size'
// mblocks.

static void *
gen_map_mblocks (W_ size)
{
    int slop;
    StgWord8 *ret;

    // Try to map a larger block, and take the aligned portion from
    // it (unmap the rest).
    size += MBLOCK_SIZE;
    ret = my_mmap_or_barf(0, size, MEM_RESERVE_AND_COMMIT);

    // unmap the slop bits around the chunk we allocated
    slop = (W_)ret & MBLOCK_MASK;

    if (munmap((void*)ret, MBLOCK_SIZE - slop) == -1) {
      barf("gen_map_mblocks: munmap failed");
    }
    if (slop > 0 && munmap((void*)(ret+size-slop), slop) == -1) {
      barf("gen_map_mblocks: munmap failed");
    }

    // ToDo: if we happened to get an aligned block, then don't
    // unmap the excess, just use it. For this to work, you
    // need to keep in mind the following:
    //     * Calling my_mmap() with an 'addr' arg pointing to
    //       already my_mmap()ed space is OK and won't fail.
    //     * If my_mmap() can't satisfy the request at the
    //       given 'next_request' address in getMBlocks(), that
    //       you unmap the extra mblock mmap()ed here (or simply
    //       satisfy yourself that the slop introduced isn't worth
    //       salvaging.)
    //

    // next time, try after the block we just got.
    ret += MBLOCK_SIZE - slop;
    return ret;
}

void *
osGetMBlocks(uint32_t n)
{
  void *ret;
  W_ size = MBLOCK_SIZE * (W_)n;

  if (next_request == 0) {
      // use gen_map_mblocks the first time.
      ret = gen_map_mblocks(size);
  } else {
      ret = my_mmap_or_barf(next_request, size, MEM_RESERVE_AND_COMMIT);

      if (((W_)ret & MBLOCK_MASK) != 0) {
          // misaligned block!
#if 0 // defined(DEBUG)
          errorBelch("warning: getMBlock: misaligned block %p returned "
                     "when allocating %d megablock(s) at %p",
                     ret, n, next_request);
#endif

          // unmap this block...
          if (munmap(ret, size) == -1) {
              barf("getMBlock: munmap failed");
          }
          // and do it the hard way
          ret = gen_map_mblocks(size);
      }
  }

  // Next time, we'll try to allocate right after the block we just got.
  // ToDo: check that we haven't already grabbed the memory at next_request
  next_request = (char *)ret + size;

  return ret;
}

void osBindMBlocksToNode(
    void *addr STG_UNUSED,
    StgWord size STG_UNUSED,
    uint32_t node STG_UNUSED)
{
#if HAVE_LIBNUMA
    int ret;
    unsigned long mask = 0;
    mask |= 1 << node;
    if (RtsFlags.GcFlags.numa) {
        ret = mbind(addr, (unsigned long)size,
                    MPOL_BIND, &mask, sizeof(StgWord)*8, MPOL_MF_STRICT);
        // paranoia: MPOL_BIND guarantees memory on the correct node;
        // MPOL_MF_STRICT will tell us if it didn't work.  We might want to
        // relax these in due course, but I want to be sure it's doing what we
        // want first.
        if (ret != 0) {
            sysErrorBelch("mbind");
            stg_exit(EXIT_FAILURE);
        }
    }
#endif
}


void osFreeMBlocks(void *addr, uint32_t n)
{
    munmap(addr, n * MBLOCK_SIZE);
}

void osReleaseFreeMemory(void) {
    /* Nothing to do on POSIX */
}

void osFreeAllMBlocks(void)
{
    void *mblock;
    void *state;

    for (mblock = getFirstMBlock(&state);
         mblock != NULL;
         mblock = getNextMBlock(&state, mblock)) {
        munmap(mblock, MBLOCK_SIZE);
    }
}

size_t getPageSize (void)
{
    static size_t pageSize = 0;

    if (pageSize == 0) {
        long ret;
        ret = sysconf(_SC_PAGESIZE);
        if (ret == -1) {
           barf("getPageSize: cannot get page size");
        }
        pageSize = ret;
    }

    return pageSize;
}

/* Returns 0 if physical memory size cannot be identified */
StgWord64 getPhysicalMemorySize (void)
{
    static StgWord64 physMemSize = 0;
    if (!physMemSize) {
#if defined(darwin_HOST_OS) || defined(ios_HOST_OS)
        /* So, darwin doesn't support _SC_PHYS_PAGES, but it does
           support getting the raw memory size in bytes through
           sysctlbyname(hw.memsize); */
        size_t len = sizeof(physMemSize);
        int ret = -1;

        /* Note hw.memsize is in bytes, so no need to multiply by page size. */
        ret = sysctlbyname("hw.memsize", &physMemSize, &len, NULL, 0);
        if (ret == -1) {
            physMemSize = 0;
            return 0;
        }
#else
        /* We'll politely assume we have a system supporting _SC_PHYS_PAGES
         * otherwise.  */
        W_ pageSize = getPageSize();
        long ret = sysconf(_SC_PHYS_PAGES);
        if (ret == -1) {
#if defined(DEBUG)
            errorBelch("warning: getPhysicalMemorySize: cannot get "
                       "physical memory size");
#endif
            return 0;
        }
        physMemSize = ret * pageSize;
#endif /* darwin_HOST_OS */
    }
    return physMemSize;
}

#if defined(USE_LARGE_ADDRESS_SPACE)


static void *
osTryReserveHeapMemory (W_ len, void *hint)
{
    void *base, *top;
    void *start, *end;

    ASSERT((len & ~MBLOCK_MASK) == len);

    /* We try to allocate len + MBLOCK_SIZE,
       because we need memory which is MBLOCK_SIZE aligned,
       and then we discard what we don't need */

    base = my_mmap(hint, len + MBLOCK_SIZE, MEM_RESERVE);

    if (base == NULL)
        return NULL;

    top = (void*)((W_)base + len + MBLOCK_SIZE);

    if (((W_)base & MBLOCK_MASK) != 0) {
        start = MBLOCK_ROUND_UP(base);
        end = MBLOCK_ROUND_DOWN(top);
        ASSERT(((W_)end - (W_)start) == len);

        if (munmap(base, (W_)start-(W_)base) < 0) {
            sysErrorBelch("unable to release slop before heap");
        }
        if (munmap(end, (W_)top-(W_)end) < 0) {
            sysErrorBelch("unable to release slop after heap");
        }
    } else {
        start = base;
    }

    return start;
}

void *osReserveHeapMemory(void *startAddressPtr, W_ *len)
{
    int attempt;
    void *at;

    /* We want to ensure the heap starts at least 8 GB inside the address space,
       since we want to reserve the address space below that address for code.
       Specifically, we need to make sure that any dynamically loaded code will
       be close enough to the original code so that short relocations will work.
       This is in particular important on Darwin/Mach-O, because object files
       not compiled as shared libraries are position independent but cannot be
       loaded above 4GB.

       We do so with a hint to the mmap, and we verify the OS satisfied our
       hint. We loop, shifting our hint by 1 BLOCK_SIZE every time, in case
       there is already something allocated there.

       Some systems impose resource limits restricting the amount of memory we
       can request (see, e.g. #10877). If mmap fails we halve our allocation
       request and try again. If our request size gets absurdly small we simply
       give up.

    */

    W_ minimumAddress = (W_)8 * (1 << 30);
    // We don't use minimumAddress (0x200000000) as default because we know
    // it can clash with third-party libraries. See ticket #12573.
    W_ startAddress = 0x4200000000;
    if (startAddressPtr) {
        startAddress = (W_)startAddressPtr;
    }
    if (startAddress < minimumAddress) {
        errorBelch(
            "Provided heap start address %p is lower than minimum address %p",
            (void*)startAddress, (void*)minimumAddress);
    }

#if defined(HAVE_SYS_RESOURCE_H) && defined(HAVE_SYS_TIME_H)
    struct rlimit asLimit;
    /* rlim_t is signed on some platforms, including FreeBSD;
     * explicitly cast to avoid sign compare error */
    if (!getrlimit(RLIMIT_AS, &asLimit)
        && asLimit.rlim_cur > 0
        && *len > (W_) asLimit.rlim_cur) {

        /* In case address space/virtual memory was limited by rlimit (ulimit),
           we try to reserver 2/3 of that limit. If we take all, there'll be
           nothing left for spawning system threads etc. and we'll crash
           (See #18623)
        */

        pthread_attr_t threadAttr;
        if (pthread_attr_init(&threadAttr)) {
            // Never fails on Linux
            sysErrorBelch("failed to initialize thread attributes");
            stg_exit(EXIT_FAILURE);
        }

        size_t stacksz = 0;
        if (pthread_attr_getstacksize(&threadAttr, &stacksz)) {
            // Should never fail
            sysErrorBelch("failed to read default thread stack size");
            stg_exit(EXIT_FAILURE);
        }

        // Cleanup
        if (pthread_attr_destroy(&threadAttr)) {
            // Should never fail
            sysErrorBelch("failed to destroy thread attributes");
            stg_exit(EXIT_FAILURE);
        }

        size_t pageSize = getPageSize();
        // 2/3rds of limit, round down to multiple of PAGE_SIZE
        *len = (W_) (asLimit.rlim_cur * 0.666) & ~(pageSize - 1);

        // debugBelch("New len: %zu, pageSize: %zu\n", *len, pageSize);

        /* Make sure we leave enough vmem for at least three threads.
           This number was found through trial and error. We're at least launching
           that many threads (e.g., itimer/ticker). We can't know for sure how much we need,
           but at least we can fail early and give a useful error message in this case. */
        if (((W_) (asLimit.rlim_cur - *len )) < ((W_) (stacksz * 3))) {
            // Three stacks is 1/3 of needed, then convert to Megabyte
            size_t needed = (stacksz * 3 * 3) / (1024 * 1024);
            errorBelch("the current resource limit for virtual memory ('ulimit -v' or RLIMIT_AS) is too low.\n"
                "Please make sure that at least %zuMiB of virtual memory are available.", needed);
            stg_exit(EXIT_FAILURE);
        }
    }
#endif

    attempt = 0;
    while (1) {
        *len &= ~MBLOCK_MASK;

        if (*len < MBLOCK_SIZE) {
            // Give up if the system won't even give us 16 blocks worth of heap
            barf("osReserveHeapMemory: Failed to allocate heap storage");
        }

        void *hint = (void*)(startAddress + attempt * BLOCK_SIZE);
        at = osTryReserveHeapMemory(*len, hint);
        if (at == NULL) {
            // This means that mmap failed which we take to mean that we asked
            // for too much memory. This can happen due to POSIX resource
            // limits. In this case we reduce our allocation request by a
            // fraction of the current size and try again.
            //
            // Note that the previously would instead decrease the request size
            // by a factor of two; however, this meant that significant amounts
            // of memory will be wasted (e.g. imagine a machine with 512GB of
            // physical memory but a 511GB ulimit). See #14492.
            *len -= *len / 8;
            // debugBelch("Limit hit, reduced len: %zu\n", *len);
        } else if ((W_)at >= minimumAddress) {
            // Success! We were given a block of memory starting above the 8 GB
            // mark, which is what we were looking for.

            break;
        } else {
            // We got addressing space but it wasn't above the 8GB mark.
            // Try again.
            if (munmap(at, *len) < 0) {
                sysErrorBelch("unable to release reserved heap");
            }
        }
        attempt++;
    }

    return at;
}

void osCommitMemory(void *at, W_ size)
{
    void *r = my_mmap(at, size, MEM_COMMIT);
    if (r == NULL) {
        errorBelch("Unable to commit %" FMT_Word " bytes of memory", size);
        errorBelch("Exiting. The system might be out of memory.");
        stg_exit(EXIT_FAILURE);
    }
}

/* Note [MADV_FREE and MADV_DONTNEED]
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * madvise() provides flags with which one can release no longer needed pages
 * back to the kernel without having to munmap() (which is expensive).
 *
 * On Linux, MADV_FREE is newer and faster because it can avoid zeroing
 * pages if they are re-used by the process later (see `man 2 madvise`),
 * but for the trade-off that memory inspection tools like `top` will
 * not immediately reflect the freeing in their display of resident memory
 * (RSS column): Only under memory pressure will Linux actually remove
 * the freed pages from the process and update its RSS statistics.
 * Until then, the pages show up as `LazyFree` in `/proc/PID/smaps`
 * (see `man 5 proc`).
 * The delayed RSS update can confuse programmers debugging memory issues,
 * production memory monitoring tools, and end users who may complain about
 * undue memory usage shown in reporting tools, so with
 * `disableDelayedOsMemoryReturn` we provide an RTS flag that allows forcing
 * usage of MADV_DONTNEED instead of MADV_FREE.
 */

void osDecommitMemory(void *at, W_ size)
{
    int r;

    // First make the memory unaccessible (so that we get a segfault
    // at the next attempt to touch it)
    // We only do this in DEBUG because it forces the OS to remove
    // all MMU entries for this page range, and there is no reason
    // to do so unless there is memory pressure
#if defined(DEBUG)
    r = mprotect(at, size, PROT_NONE);
    if(r < 0)
        sysErrorBelch("unable to make released memory unaccessible");
#endif

#if defined(MADV_FREE)
    // See Note [MADV_FREE and MADV_DONTNEED].
    // If MADV_FREE is disabled, fall-through to MADV_DONTNEED.
    if (!RtsFlags.MiscFlags.disableDelayedOsMemoryReturn) {
        // Try MADV_FREE first, FreeBSD has both and MADV_DONTNEED
        // just swaps memory out. Linux >= 4.5 has both DONTNEED and FREE; either
        // will work as they both allow the system to free anonymous pages.
        // It is important that we try both methods as the kernel which we were
        // built on may differ from the kernel we are now running on.
        r = madvise(at, size, MADV_FREE);
        if(r < 0) {
            if (errno == EINVAL) {
                // Perhaps the system doesn't support MADV_FREE; fall-through and
                // try MADV_DONTNEED.
            } else {
                sysErrorBelch("unable to decommit memory");
            }
        } else {
            return;
        }
    }
#endif

    r = madvise(at, size, MADV_DONTNEED);
    if(r < 0)
        sysErrorBelch("unable to decommit memory");
}

void osReleaseHeapMemory(void)
{
    int r;

    r = munmap((void*)mblock_address_space.begin,
               mblock_address_space.end - mblock_address_space.begin);
    if(r < 0)
        sysErrorBelch("unable to release address space");
}

#endif

bool osBuiltWithNumaSupport(void)
{
#if HAVE_LIBNUMA
    return true;
#else
    return false;
#endif
}

bool osNumaAvailable(void)
{
#if HAVE_LIBNUMA
    return (numa_available() != -1);
#else
    return false;
#endif
}

uint32_t osNumaNodes(void)
{
#if HAVE_LIBNUMA
    return numa_num_configured_nodes();
#else
    return 1;
#endif
}

uint64_t osNumaMask(void)
{
#if HAVE_LIBNUMA
    struct bitmask *mask;
    mask = numa_get_mems_allowed();
    if (osNumaNodes() > sizeof(StgWord)*8) {
        barf("osNumaMask: too many NUMA nodes (%d)", osNumaNodes());
    }
    uint64_t r = mask->maskp[0];
    numa_bitmask_free(mask);
    return r;
#else
    return 1;
#endif
}
