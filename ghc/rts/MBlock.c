/* -----------------------------------------------------------------------------
 * $Id: MBlock.c,v 1.2 1998/12/02 13:28:28 simonm Exp $
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
    /* avoid using cygwin32's mmap implementation, it's buggy and
       it's just as easy to do what we want to do directly.
    */
   HANDLE hFile = (HANDLE)0xFFFFFFFF;
   SECURITY_ATTRIBUTES sa;
   HANDLE h;

   sa.nLength = sizeof (SECURITY_ATTRIBUTES);
   sa.bInheritHandle = TRUE;
   sa.lpSecurityDescriptor = 0;

   h = CreateFileMapping(hFile, &sa, PAGE_READWRITE, 0, size, NULL);
   if ( h == 0 ) {
#  ifdef DEBUG
      fprintf(stderr, "getMBlocks: CreateFileMapping failed with: %d\n", GetLastError());
#  endif
      ret=(void*)-1;
   } else {
      ret = MapViewOfFileEx (h, FILE_MAP_WRITE, 0, 0, size, next_request);
      if ( ret != next_request ) {
#  ifdef DEBUG
         fprintf(stderr, "getMBlocks: MapViewOfFileEx failed with: %d\n", GetLastError());
#  endif
         ret =(void*)-1;
      }
   }
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

  return ret;
}
