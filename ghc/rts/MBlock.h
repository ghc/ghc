/* -----------------------------------------------------------------------------
 * $Id: MBlock.h,v 1.6 1999/05/04 10:19:16 sof Exp $
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
