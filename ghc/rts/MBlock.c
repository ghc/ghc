/* -----------------------------------------------------------------------------
 * $Id: MBlock.c,v 1.4 1999/01/14 18:31:17 sof Exp $
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

#elif cygwin32_TARGET_OS
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
# ifdef _WIN32
  {
    /* Note: on 95, the legal range for next_request is: [0x00400000, 0x7fffffff]
             under NT it is: [0x00010000, 0x7fffffff]

       We start allocating at 0x50000000, hopefully that's not conflicting with
       others.. (ToDo: have the allocator try to gracefully rebase itself in
       case our initial guess is conflicting with others.)
    */
    ret = VirtualAlloc(next_request, size, MEM_RESERVE | MEM_COMMIT , PAGE_READWRITE);
    if (!ret) {
#  ifdef DEBUG
         fprintf(stderr, "getMBlocks: VirtualAlloc failed with: %d\n", GetLastError());
#  endif
         ret =(void*)-1;

    }
    return ret;
  }
# else
  ret = mmap(next_request, size, PROT_READ | PROT_WRITE, 
	     MAP_ANON | MAP_PRIVATE, -1, 0);
# endif
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
