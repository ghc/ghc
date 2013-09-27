/* -----------------------------------------------------------------------------
 *
 * (c) The University of Glasgow 2006-2007
 *
 * OS-specific memory management
 *
 * ---------------------------------------------------------------------------*/

// This is non-posix compliant.
// #include "PosixSource.h"

#include "Rts.h"

#include "RtsUtils.h"
#include "sm/OSMem.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#include <errno.h>

#if darwin_HOST_OS
#include <mach/mach.h>
#include <mach/vm_map.h>
#endif

static caddr_t next_request = 0;

void osMemInit(void)
{
    next_request = (caddr_t)RtsFlags.GcFlags.heapBase;
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

// A wrapper around mmap(), to abstract away from OS differences in
// the mmap() interface.

static void *
my_mmap (void *addr, W_ size)
{
    void *ret;

#if defined(solaris2_HOST_OS) || defined(irix_HOST_OS)
    { 
        int fd = open("/dev/zero",O_RDONLY);
        ret = mmap(addr, size, PROT_READ | PROT_WRITE, MAP_PRIVATE, fd, 0);
        close(fd);
    }
#elif hpux_HOST_OS
    ret = mmap(addr, size, PROT_READ | PROT_WRITE, 
               MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
#elif darwin_HOST_OS
    // Without MAP_FIXED, Apple's mmap ignores addr.
    // With MAP_FIXED, it overwrites already mapped regions, whic
    // mmap(0, ... MAP_FIXED ...) is worst of all: It unmaps the program text
    // and replaces it with zeroes, causing instant death.
    // This behaviour seems to be conformant with IEEE Std 1003.1-2001.
    // Let's just use the underlying Mach Microkernel calls directly,
    // they're much nicer.
    
    kern_return_t err = 0;
    ret = addr;
    if(addr)    // try to allocate at address
        err = vm_allocate(mach_task_self(),(vm_address_t*) &ret, size, FALSE);
    if(!addr || err)    // try to allocate anywhere
        err = vm_allocate(mach_task_self(),(vm_address_t*) &ret, size, TRUE);
        
    if(err) {
        // don't know what the error codes mean exactly, assume it's
        // not our problem though.
        errorBelch("memory allocation failed (requested %" FMT_Word " bytes)", size);
        stg_exit(EXIT_FAILURE);
    } else {
        vm_protect(mach_task_self(),(vm_address_t)ret,size,FALSE,VM_PROT_READ|VM_PROT_WRITE);
    }
#elif linux_HOST_OS
    ret = mmap(addr, size, PROT_READ | PROT_WRITE,
               MAP_ANON | MAP_PRIVATE, -1, 0);
    if (ret == (void *)-1 && errno == EPERM) {
        // Linux may return EPERM if it tried to give us
        // a chunk of address space below mmap_min_addr,
        // See Trac #7500.
        if (addr != 0) {
            // Try again with no hint address.
            // It's not clear that this can ever actually help,
            // but since our alternative is to abort, we may as well try.
            ret = mmap(0, size, PROT_READ | PROT_WRITE,
                       MAP_ANON | MAP_PRIVATE, -1, 0);
        }
        if (ret == (void *)-1 && errno == EPERM) {
            // Linux is not willing to give us any mapping,
            // so treat this as an out-of-memory condition
            // (really out of virtual address space).
            errno = ENOMEM;
        }
    }
#else
    ret = mmap(addr, size, PROT_READ | PROT_WRITE, 
               MAP_ANON | MAP_PRIVATE, -1, 0);
#endif

    if (ret == (void *)-1) {
        if (errno == ENOMEM || 
            (errno == EINVAL && sizeof(void*)==4 && size >= 0xc0000000)) {
            // If we request more than 3Gig, then we get EINVAL
            // instead of ENOMEM (at least on Linux).
            errorBelch("out of memory (requested %" FMT_Word " bytes)", size);
            stg_exit(EXIT_FAILURE);
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
    ret = my_mmap(0, size);
    
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
osGetMBlocks(nat n)
{
  caddr_t ret;
  W_ size = MBLOCK_SIZE * (W_)n;

  if (next_request == 0) {
      // use gen_map_mblocks the first time.
      ret = gen_map_mblocks(size);
  } else {
      ret = my_mmap(next_request, size);

      if (((W_)ret & MBLOCK_MASK) != 0) {
          // misaligned block!
#if 0 // defined(DEBUG)
          errorBelch("warning: getMBlock: misaligned block %p returned when allocating %d megablock(s) at %p", ret, n, next_request);
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
  next_request = ret + size;

  return ret;
}

void osFreeMBlocks(char *addr, nat n)
{
    munmap(addr, n * MBLOCK_SIZE);
}

void osReleaseFreeMemory(void) {
    /* Nothing to do on POSIX */
}

void osFreeAllMBlocks(void)
{
    void *mblock;

    for (mblock = getFirstMBlock();
         mblock != NULL;
         mblock = getNextMBlock(mblock)) {
        munmap(mblock, MBLOCK_SIZE);
    }
}

W_ getPageSize (void)
{
    static W_ pageSize = 0;
    if (pageSize) {
        return pageSize;
    } else {
        long ret;
        ret = sysconf(_SC_PAGESIZE);
        if (ret == -1) {
           barf("getPageSize: cannot get page size");
        }
        pageSize = ret;
        return ret;
    }
}

void setExecutable (void *p, W_ len, rtsBool exec)
{
    StgWord pageSize = getPageSize();

    /* malloced memory isn't executable by default on OpenBSD */
    StgWord mask             = ~(pageSize - 1);
    StgWord startOfFirstPage = ((StgWord)p          ) & mask;
    StgWord startOfLastPage  = ((StgWord)p + len - 1) & mask;
    StgWord size             = startOfLastPage - startOfFirstPage + pageSize;
    if (mprotect((void*)startOfFirstPage, (size_t)size, 
                 (exec ? PROT_EXEC : 0) | PROT_READ | PROT_WRITE) != 0) {
        barf("setExecutable: failed to protect 0x%p\n", p);
    }
}
