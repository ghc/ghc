/* -----------------------------------------------------------------------------
 * $Id: MBlock.h,v 1.10 2001/07/26 03:26:28 ken Exp $
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
/* ToDo: Perhaps by adjusting this value we can make linking without
 * -static work (i.e., not generate a core-dumping executable)? */
#if SIZEOF_VOID_P == 8
#define HEAP_BASE 0x180000000L
#else
#error I have no idea where to begin the heap on a non-64-bit osf3 machine.
#endif

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
