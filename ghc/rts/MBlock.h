/* -----------------------------------------------------------------------------
 * $Id: MBlock.h,v 1.5 1999/03/03 19:04:57 sof Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * MegaBlock Allocator interface.
 *
 * ---------------------------------------------------------------------------*/

extern lnat mblocks_allocated;

#ifdef HAVE_WIN32_DLL_SUPPORT
extern int is_heap_alloced(const void* p);
#endif

extern void * getMBlock(void);
extern void * getMBlocks(nat n);
