/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2009
 *
 * TICKY_TICKY types
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
 *
 * ---------------------------------------------------------------------------*/

#ifndef RTS_TICKY_H
#define RTS_TICKY_H

/* -----------------------------------------------------------------------------
   The StgEntCounter type - needed regardless of TICKY_TICKY
   -------------------------------------------------------------------------- */

typedef struct _StgEntCounter {
  /* Using StgWord for everything, because both the C and asm code
     generators make trouble if you try to pack things tighter */
    StgWord	registeredp;	/* 0 == no, 1 == yes */
    StgInt	arity;		/* arity (static info) */
    StgInt	allocd; 	/* # allocation of this closure */
				/* (rest of args are in registers) */
    char   	*str;		/* name of the thing */
    char   	*arg_kinds;	/* info about the args types */
    StgInt	entry_count;	/* Trips to fast entry code */
    StgInt      allocs;         /* number of allocations by this fun */
    struct _StgEntCounter *link;/* link to chain them all together */
} StgEntCounter;

#endif /* RTS_TICKY_H */

