/* -----------------------------------------------------------------------------
 * $Id: MBlock.c,v 1.21 2001/01/16 11:54:25 simonmar Exp $
 *
 * (c) The GHC Team 1998-1999
 *
 * MegaBlock Allocator Interface.  This file contains all the dirty
 * architecture-dependent hackery required to get a chunk of aligned
 * memory from the operating system.
 *
 * ---------------------------------------------------------------------------*/

#define NON_POSIX_SOURCE

#include "Rts.h"
#include "RtsUtils.h"
#include "RtsFlags.h"
#include "MBlock.h"
#include "BlockAlloc.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifndef mingw32_TARGET_OS
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

lnat mblocks_allocated = 0;

void *
getMBlock(void)
{
  return getMBlocks(1);
}

#ifndef _WIN32
void *
getMBlocks(nat n)
{
  static caddr_t next_request = (caddr_t)HEAP_BASE;
  caddr_t ret;
  lnat size = MBLOCK_SIZE * n;
 
#ifdef solaris2_TARGET_OS
  { 
      int fd = open("/dev/zero",O_RDONLY);
      ret = mmap(next_request, size, PROT_READ | PROT_WRITE, 
		 MAP_FIXED | MAP_PRIVATE, fd, 0);
      close(fd);
  }
#elif hpux_TARGET_OS
 ret = mmap(next_request, size, PROT_READ | PROT_WRITE, 
	     MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
#elif macosx_TARGET_OS
 ret = mmap(next_request, size, PROT_READ | PROT_WRITE, 
            MAP_FIXED | MAP_ANON | MAP_PRIVATE, -1, 0);
#else
  ret = mmap(next_request, size, PROT_READ | PROT_WRITE, 
	     MAP_ANON | MAP_PRIVATE, -1, 0);
#endif
  
  if (ret == (void *)-1) {
    if (errno == ENOMEM) {
      barf("getMBlock: out of memory");
    } else {
      barf("GetMBlock: mmap failed");
    }
  }

  if (((W_)ret & MBLOCK_MASK) != 0) {
    barf("GetMBlock: misaligned block %p returned when allocating %d megablock(s) at %p", ret, n, next_request);
  }

  IF_DEBUG(gc,fprintf(stderr,"Allocated %d megablock(s) at %x\n",n,(nat)ret));

  next_request += size;

  mblocks_allocated += n;
  
  return ret;
}

#else /* _WIN32 */

/*
 On Win32 platforms we make use of the two-phased virtual memory API
 to allocate mega blocks. We proceed as follows:

 Reserve a large chunk of VM (128M at the time), but don't supply a 
 base address that's aligned on a MB boundary. Instead we round up to the
 nearest from the chunk of VM we're given back from the OS (at the
 moment we just leave the 'slop' at the beginning of the reserved
 chunk unused - ToDo: reuse it .)

 Reserving memory doesn't allocate physical storage (not even in the
 page file), this is done by committing pages (or mega-blocks in
 our case).

*/

char* base_non_committed = (char*)0;

/* Reserve VM 128M at the time to try to minimise the slop cost. */
#define SIZE_RESERVED_POOL  ( 128 * 1024 * 1024 )

/* This predicate should be inlined, really. */
/* TODO: this only works for a single chunk */
int
is_heap_alloced(const void* x)
{
  return (((char*)(x) >= base_non_committed) && 
          ((char*)(x) <= (base_non_committed + SIZE_RESERVED_POOL)));
}

void *
getMBlocks(nat n)
{
  static char* base_mblocks       = (char*)0;
  static char* next_request       = (char*)0;
  void* ret                       = (void*)0;

  lnat size = MBLOCK_SIZE * n;

  if ( (base_non_committed == 0) || 
       (next_request + size > base_non_committed + SIZE_RESERVED_POOL) ) {
#ifdef ENABLE_WIN32_DLL_SUPPORT
    if (base_non_committed)
        barf("Windows programs can only use 128Mb of heap; sorry!");
#endif
    base_non_committed = VirtualAlloc ( NULL
                                      , SIZE_RESERVED_POOL
				      , MEM_RESERVE
				      , PAGE_READWRITE
				      );
    if ( base_non_committed == 0 ) {
# if 1 /*def DEBUG*/
         fprintf(stderr, "getMBlocks: VirtualAlloc failed with: %d\n", GetLastError());
# endif
         ret=(void*)-1;
    } else {
    /* The returned pointer is not aligned on a mega-block boundary. Make it. */
       base_mblocks = (char*)((unsigned long)base_non_committed & (unsigned long)0xfff00000) + MBLOCK_SIZE;
# if 0
       fprintf(stderr, "Dropping %d bytes off of 128M chunk\n", 
	               (unsigned)base_mblocks - (unsigned)base_non_committed);
# endif

       if ( ((char*)base_mblocks + size) > ((char*)base_non_committed + SIZE_RESERVED_POOL) ) {
# if 1 /*def DEBUG*/
          fprintf(stderr, "oops, committed too small a region to start with.");
# endif
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
# if 1 /*def DEBUG*/
        fprintf(stderr, "getMBlocks: VirtualAlloc failed with: %d\n", GetLastError());
# endif
        ret=(void*)-1;
     }
  }

  if (((W_)ret & MBLOCK_MASK) != 0) {
    barf("GetMBlock: misaligned block returned");
  }

  IF_DEBUG(gc,fprintf(stderr,"Allocated %d megablock(s) at %x\n",n,(nat)ret));

  next_request = (char*)next_request + size;

  mblocks_allocated += n;
  
  return ret;
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
# ifdef DEBUG
     fprintf(stderr, "freeMBlocks: VirtualFree failed with: %d\n", GetLastError());
# endif
  }

}
#endif

#endif
