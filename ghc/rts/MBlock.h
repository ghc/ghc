/* -----------------------------------------------------------------------------
 * $Id: MBlock.h,v 1.9 2001/06/29 16:58:06 sewardj Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * MegaBlock Allocator interface.
 *
 * ---------------------------------------------------------------------------*/

extern lnat mblocks_allocated;

#ifdef ENABLE_WIN32_DLL_SUPPORT
extern int is_heap_alloced(const void* p);
#endif

extern void * getMBlock(void);
extern void * getMBlocks(nat n);

#if freebsd2_TARGET_OS || freebsd_TARGET_OS
/* Executable is loaded from      0x0
 * Shared libraries are loaded at 0x2000000
 * Stack is at the top of the address space.  The kernel probably owns
 * 0x8000000 onwards, so we'll pick 0x5000000.
 */
#define HEAP_BASE 0x50000000

#elif netbsd_TARGET_OS
/* NetBSD i386 shared libs are at 0x40000000
 */
#define HEAP_BASE 0x50000000
#elif openbsd_TARGET_OS
#define HEAP_BASE 0x50000000
#elif linux_TARGET_OS
/* Any ideas?
 */
#define HEAP_BASE 0x50000000

#elif solaris2_TARGET_OS
/* guess */
#define HEAP_BASE 0x50000000

#elif osf3_TARGET_OS
/* guess */
#define HEAP_BASE 0x50000000

#elif hpux_TARGET_OS
/* guess */
#define HEAP_BASE 0x50000000

#elif macosx_TARGET_OS
/* guess */
#define HEAP_BASE 0x50000000

#elif defined(mingw32_TARGET_OS) || defined(cygwin32_TARGET_OS)
/* doesn't matter, we use a reserve/commit algorithm */

#else
#error Dont know where to get memory from on this architecture
/* ToDo: memory locations on other architectures */
#endif
