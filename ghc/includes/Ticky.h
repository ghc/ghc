/* ----------------------------------------------------------------------------
 * $Id: Ticky.h,v 1.2 1998/12/02 13:21:45 simonm Exp $
 *
 * Closures
 *
 * -------------------------------------------------------------------------- */

#ifndef TICKY_H
#define TICKY_H


#ifndef TICKY
/* just stubs if no ticky-ticky profiling*/
#define TICK_ENT_CAF_ENTERED(Node)	/* enter CAF */
#define TICK_ENT_IND(Node)	/* enter indirection */
#define TICK_ENT_VIA_NODE()	/* enter node */
#define TICK_UPD_EXISTING()	/* entering an update frame */
#define TICK_UPD_SQUEEZED()     /* squeezed an update frame */
#define TICK_UPDATED_SET_UPDATED(updclosure) /* updating a closure w/ ind */
#define TICK_ALLOC_HEAP(words)  /* allocate some words on the heap */
#define TICK_UNALLOC_HEAP(words)  /* unallocate some words on the heap */
#define TICK_ALLOC_UPD_PAP(DYN_HS,NArgWords,N,PapSize)
#define TICK_ALLOC_PRIM(hdr,args,n,size)
#define TICK_ENT_PAP(pap)	/* entering a PAP */
#define TICK_UPD_PAP_IN_NEW(NArgWords)
#define TICK_UPD_PAP_IN_PLACE()
#define TICK_UPDF_PUSHED()
#define TICK_SEQF_PUSHED()
#else
#error ticky-ticky not implemented!
#endif

#endif /* TICKY_H */
