/* -----------------------------------------------------------------------------
 * $Id: MBlock.c,v 1.5 1999/01/18 09:20:08 sof Exp $
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

#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#if cygwin32_TARGET_OS
#include <windows.h>
#endif

#if freebsd2_TARGET_OS || freebsd3_TARGET_OS
/* Executable is loaded from      0x0
 * Shared libraries are loaded at 0x2000000
 * Stack is at the top of the address space.  The kernel probably owns
 * 0x8000000 onwards, so we'll pick 0x5000000.
 */
#define ASK_FOR_MEM_AT 0x50000000

#elif linux_TARGET_OS
/* Any ideas?
 */
#define ASK_FOR_MEM_AT 0x50000000

#elif solaris2_TARGET_OS
/* guess */
#define ASK_FOR_MEM_AT 0x50000000

#else
#error Dont know where to get memory from on this architecture
/* ToDo: memory locations on other architectures */
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
  static caddr_t next_request = (caddr_t)ASK_FOR_MEM_AT;
  caddr_t ret;
  lnat size = MBLOCK_SIZE * n;
 
#ifdef solaris2_TARGET_OS
  { 
      int fd = open("/dev/zero",O_RDONLY);
      ret = mmap(next_request, size, PROT_READ | PROT_WRITE, 
		 MAP_FIXED | MAP_PRIVATE, fd, 0);
      close(fd);
  }
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
    barf("GetMBlock: misaligned block returned");
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

void *
getMBlocks(nat n)
{
  static char* base_non_committed = (char*)0;
  static char* base_mblocks       = (char*)0;
  static char* next_request       = (char*)0;
  void* ret                       = (void*)0;

  lnat size = MBLOCK_SIZE * n;

  /* Reserve VM 128M at the time to try to minimise the slop cost. */
#define SIZE_RESERVED_POOL  ( 128 * 1024 * 1024 )

  if ( (base_non_committed == 0) || 
       (next_request + size > base_non_committed + SIZE_RESERVED_POOL) ) {
    base_non_committed = VirtualAlloc ( NULL
                                      , SIZE_RESERVED_POOL
				      , MEM_RESERVE
				      , PAGE_READWRITE
				      );
    if ( base_non_committed == 0 ) {
# ifdef DEBUG
         fprintf(stderr, "getMBlocks: VirtualAlloc failed with: %d\n", GetLastError());
# endif
         ret=(void*)-1;
    } else {
    /* The returned pointer is not aligned on a mega-block boundary. Make it. */
       base_mblocks = (char*)((unsigned)base_non_committed & (unsigned)0xfff00000) + 0x100000;
# if 0
       fprintf(stderr, "Dropping %d bytes off of 128M chunk\n", 
	               (unsigned)base_mblocks - (unsigned)base_non_committed);
# endif

       if ( ((char*)base_mblocks + size) > ((char*)base_non_committed + SIZE_RESERVED_POOL) ) {
# ifdef DEBUG
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
# ifdef DEBUG
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
