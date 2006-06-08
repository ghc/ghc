/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1998-1999
 *
 * MegaBlock Allocator Interface.  This file contains all the dirty
 * architecture-dependent hackery required to get a chunk of aligned
 * memory from the operating system.
 *
 * ---------------------------------------------------------------------------*/

/* This is non-posix compliant. */
/* #include "PosixSource.h" */

#include "Rts.h"
#include "RtsUtils.h"
#include "RtsFlags.h"
#include "MBlock.h"
#include "BlockAlloc.h"
#include "Trace.h"

#ifdef HAVE_STDLIB_H
#include <stdlib.h>
#endif
#ifdef HAVE_STRING_H
#include <string.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifndef mingw32_HOST_OS
# ifdef HAVE_SYS_MMAN_H
# include <sys/mman.h>
# endif
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#if HAVE_WINDOWS_H
#include <windows.h>
#endif
#if darwin_HOST_OS
#include <mach/vm_map.h>
#endif

#include <errno.h>

lnat mblocks_allocated = 0;

/* -----------------------------------------------------------------------------
   The MBlock Map: provides our implementation of HEAP_ALLOCED()
   -------------------------------------------------------------------------- */

#if SIZEOF_VOID_P == 4
StgWord8 mblock_map[MBLOCK_MAP_SIZE]; // initially all zeros
#elif SIZEOF_VOID_P == 8
static MBlockMap dummy_mblock_map;
MBlockMap *mblock_cache = &dummy_mblock_map;
int mblock_map_count = 0;
MBlockMap **mblock_maps = NULL;

static MBlockMap *
findMBlockMap(void *p)
{
    int i;
    StgWord32 hi = (StgWord32) (((StgWord)p) >> 32);
    for( i = 0; i < mblock_map_count; i++ )
    {
        if(mblock_maps[i]->addrHigh32 == hi)
        {
	    return mblock_maps[i];
	}
    }
    return NULL;
}

StgBool
slowIsHeapAlloced(void *p)
{
    MBlockMap *map = findMBlockMap(p);
    if(map)
    {
    	mblock_cache = map;
	return map->mblocks[MBLOCK_MAP_ENTRY(p)];
    }
    else
    	return 0;
}
#endif

static void
markHeapAlloced(void *p)
{
#if SIZEOF_VOID_P == 4
    mblock_map[MBLOCK_MAP_ENTRY(p)] = 1;
#elif SIZEOF_VOID_P == 8
    MBlockMap *map = findMBlockMap(p);
    if(map == NULL)
    {
    	mblock_map_count++;
    	mblock_maps = realloc(mblock_maps,
			      sizeof(MBlockMap*) * mblock_map_count);
	map = mblock_maps[mblock_map_count-1] = calloc(1,sizeof(MBlockMap));
	map->addrHigh32 = (StgWord32) (((StgWord)p) >> 32);
    }
    map->mblocks[MBLOCK_MAP_ENTRY(p)] = 1;
    mblock_cache = map;
#endif
}

/* -----------------------------------------------------------------------------
   Allocate new mblock(s)
   -------------------------------------------------------------------------- */

void *
getMBlock(void)
{
  return getMBlocks(1);
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

#if !defined(mingw32_HOST_OS) && !defined(cygwin32_HOST_OS)

// A wrapper around mmap(), to abstract away from OS differences in
// the mmap() interface.

static void *
my_mmap (void *addr, lnat size)
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
    
    kern_return_t err;
    ret = addr;
    if(addr)	// try to allocate at adress
	err = vm_allocate(mach_task_self(),(vm_address_t*) &ret, size, FALSE);
    if(!addr || err)	// try to allocate anywhere
	err = vm_allocate(mach_task_self(),(vm_address_t*) &ret, size, TRUE);
	
    if(err) {
	// don't know what the error codes mean exactly, assume it's
	// not our problem though.
	errorBelch("memory allocation failed (requested %lu bytes)", size);
	stg_exit(EXIT_FAILURE);
    } else {
	vm_protect(mach_task_self(),ret,size,FALSE,VM_PROT_READ|VM_PROT_WRITE);
    }
#else
    ret = mmap(addr, size, PROT_READ | PROT_WRITE | PROT_EXEC, 
	       MAP_ANON | MAP_PRIVATE, -1, 0);
#endif

    if (ret == (void *)-1) {
	if (errno == ENOMEM || 
	    (errno == EINVAL && sizeof(void*)==4 && size >= 0xc0000000)) {
	    // If we request more than 3Gig, then we get EINVAL
	    // instead of ENOMEM (at least on Linux).
	    errorBelch("out of memory (requested %lu bytes)", size);
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
gen_map_mblocks (lnat size)
{
    int slop;
    void *ret;

    // Try to map a larger block, and take the aligned portion from
    // it (unmap the rest).
    size += MBLOCK_SIZE;
    ret = my_mmap(0, size);
    
    // unmap the slop bits around the chunk we allocated
    slop = (W_)ret & MBLOCK_MASK;
    
    if (munmap(ret, MBLOCK_SIZE - slop) == -1) {
      barf("gen_map_mblocks: munmap failed");
    }
    if (slop > 0 && munmap(ret+size-slop, slop) == -1) {
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


// The external interface: allocate 'n' mblocks, and return the
// address.

void *
getMBlocks(nat n)
{
  static caddr_t next_request = (caddr_t)HEAP_BASE;
  caddr_t ret;
  lnat size = MBLOCK_SIZE * n;
  nat i;
 
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

  debugTrace(DEBUG_gc, "allocated %d megablock(s) at %p",n,ret);

  // fill in the table
  for (i = 0; i < n; i++) {
      markHeapAlloced( ret + i * MBLOCK_SIZE );
  }

  mblocks_allocated += n;

  return ret;
}

void
freeAllMBlocks(void)
{
  /* XXX Do something here */
}

#else /* defined(mingw32_HOST_OS) || defined(cygwin32_HOST_OS) */

/*
 On Win32 platforms we make use of the two-phased virtual memory API
 to allocate mega blocks. We proceed as follows:

 Reserve a large chunk of VM (256M at the time, or what the user asked
 for via the -M option), but don't supply a base address that's aligned on
 a MB boundary. Instead we round up to the nearest mblock from the chunk of
 VM we're handed back from the OS (at the moment we just leave the 'slop' at
 the beginning of the reserved chunk unused - ToDo: reuse it .)

 Reserving memory doesn't allocate physical storage (not even in the
 page file), this is done later on by committing pages (or mega-blocks in
 our case).
*/

static char* base_non_committed = (char*)0;
static char* end_non_committed = (char*)0;

static void *membase;

/* Default is to reserve 256M of VM to minimise the slop cost. */
#define SIZE_RESERVED_POOL  ( 256 * 1024 * 1024 )

/* Number of bytes reserved */
static unsigned long size_reserved_pool = SIZE_RESERVED_POOL;

void *
getMBlocks(nat n)
{
  static char* base_mblocks       = (char*)0;
  static char* next_request       = (char*)0;
  void* ret                       = (void*)0;
  nat i;

  lnat size = MBLOCK_SIZE * n;
  
  if ( (base_non_committed == 0) || (next_request + size > end_non_committed) ) {
    if (base_non_committed) {
	/* Tacky, but if no user-provided -M option is in effect,
	 * set it to the default (==256M) in time for the heap overflow PSA.
	 */
	if (RtsFlags.GcFlags.maxHeapSize == 0) {
	    RtsFlags.GcFlags.maxHeapSize = size_reserved_pool / BLOCK_SIZE;
	}
	heapOverflow();
    }
    if (RtsFlags.GcFlags.maxHeapSize != 0) {
      size_reserved_pool = BLOCK_SIZE * RtsFlags.GcFlags.maxHeapSize;
      if (size_reserved_pool < MBLOCK_SIZE) {
	size_reserved_pool = 2*MBLOCK_SIZE;
      }
    }
    base_non_committed = VirtualAlloc ( NULL
                                      , size_reserved_pool
				      , MEM_RESERVE
				      , PAGE_READWRITE
				      );
    membase = base_non_committed;
    if ( base_non_committed == 0 ) {
         errorBelch("getMBlocks: VirtualAlloc MEM_RESERVE %lu failed with: %ld\n", size_reserved_pool, GetLastError());
       ret=(void*)-1;
    } else {
      end_non_committed = (char*)base_non_committed + (unsigned long)size_reserved_pool;
      /* The returned pointer is not aligned on a mega-block boundary. Make it. */
      base_mblocks = (char*)((unsigned long)base_non_committed & (unsigned long)~MBLOCK_MASK) + MBLOCK_SIZE;
#      if 0
       debugBelch("getMBlocks: Dropping %d bytes off of 256M chunk\n", 
		  (unsigned)base_mblocks - (unsigned)base_non_committed);
#      endif

       if ( ((char*)base_mblocks + size) > end_non_committed ) {
          debugBelch("getMBlocks: oops, committed too small a region to start with.");
	  ret=(void*)-1;
       } else {
          next_request = base_mblocks;
       }
    }
  }
  /* Commit the mega block(s) to phys mem */
  if ( ret != (void*)-1 ) {
     ret = VirtualAlloc(next_request, size, MEM_COMMIT, PAGE_READWRITE);
     if (ret == NULL) {
        debugBelch("getMBlocks: VirtualAlloc MEM_COMMIT %lu failed with: %ld\n", size, GetLastError());
        ret=(void*)-1;
     }
  }

  if (((W_)ret & MBLOCK_MASK) != 0) {
    barf("getMBlocks: misaligned block returned");
  }

  if (ret == (void*)-1) {
     barf("getMBlocks: unknown memory allocation failure on Win32.");
  }

  debugTrace(DEBUG_gc, "allocated %d megablock(s) at 0x%x",n,(nat)ret);
  next_request = (char*)next_request + size;

  mblocks_allocated += n;
  
  // fill in the table
  for (i = 0; i < n; i++) {
      markHeapAlloced( ret + i * MBLOCK_SIZE );
  }

  return ret;
}

void
freeAllMBlocks(void)
{
  BOOL rc;

  rc = VirtualFree(membase, 0, MEM_RELEASE);
  
  if (rc == FALSE) {
     debugBelch("freeAllMBlocks: VirtualFree failed with: %ld\n", GetLastError());
  }
}

/* Hand back the physical memory that is allocated to a mega-block. 
   ToDo: chain the released mega block onto some list so that
         getMBlocks() can get at it.

   Currently unused.
*/
#if 0
void
freeMBlock(void* p, nat n)
{
  BOOL rc;

  rc = VirtualFree(p, n * MBLOCK_SIZE , MEM_DECOMMIT );
  
  if (rc == FALSE) {
#    ifdef DEBUG
     debugBelch("freeMBlocks: VirtualFree failed with: %d\n", GetLastError());
#    endif
  }

}
#endif

#endif
