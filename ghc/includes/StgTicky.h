/* ----------------------------------------------------------------------------
 * $Id: StgTicky.h,v 1.1 1999/01/21 10:31:44 simonm Exp $
 *
 * Ticky-ticky profiling macros.
 *
 * -------------------------------------------------------------------------- */

#ifndef TICKY_H
#define TICKY_H

#ifdef TICKY_TICKY

/* -----------------------------------------------------------------------------
   Allocations
   -------------------------------------------------------------------------- */

/* How many times we do a heap check and move Hp; comparing this with
 * the allocations gives an indication of how many things we get per trip
 * to the well:
 */
#define TICK_ALLOC_HEAP(n)	ALLOC_HEAP_ctr++; ALLOC_HEAP_tot += (n)

/* We count things every time we allocate something in the dynamic heap.
 * For each, we count the number of words of (1) ``admin'' (header),
 * (2) good stuff (useful pointers and data), and (3) ``slop'' (extra
 * space, to leave room for an old generation indirection for example).
 * 
 * The first five macros are inserted when the compiler generates code
 * to allocate something; the categories correspond to the @ClosureClass@
 * datatype (manifest functions, thunks, constructors, big tuples, and
 * partial applications).
 */

#define _HS  sizeofW(StgHeader)

#define TICK_ALLOC_FUN(g,s) 				\
	ALLOC_FUN_ctr++;	ALLOC_FUN_adm += _HS;	\
	ALLOC_FUN_gds += (g);	ALLOC_FUN_slp += (s);	\
	TICK_ALLOC_HISTO(FUN,_HS,g,s)

#define TICK_ALLOC_THK(g,s) 				\
     	ALLOC_THK_ctr++;	ALLOC_THK_adm += _HS;	\
	ALLOC_THK_gds += (g);	ALLOC_THK_slp += (s);	\
	TICK_ALLOC_HISTO(THK,_HS,g,s)

#define TICK_ALLOC_CON(g,s)				\
	ALLOC_CON_ctr++;	ALLOC_CON_adm += _HS;	\
	ALLOC_CON_gds += (g);	ALLOC_CON_slp += (s);	\
	TICK_ALLOC_HISTO(CON,_HS,g,s)

#define TICK_ALLOC_TUP(g,s)				\
	ALLOC_TUP_ctr++;	ALLOC_TUP_adm += _HS;	\
	ALLOC_TUP_gds += (g);	ALLOC_TUP_slp += (s);	\
	TICK_ALLOC_HISTO(TUP,_HS,g,s)

#define TICK_ALLOC_BH(g,s)				\
	ALLOC_BH_ctr++;		ALLOC_BH_adm += _HS;	\
	ALLOC_BH_gds += (g);	ALLOC_BH_slp += (s);	\
	TICK_ALLOC_HISTO(BH,_HS,g,s)

#define TICK_ALLOC_UPD_PAP(g,s)					\
	ALLOC_UPD_PAP_ctr++;  	ALLOC_UPD_PAP_adm += sizeofW(StgPAP)-1; \
	ALLOC_UPD_PAP_gds += (g); ALLOC_UPD_PAP_slp += (s);	\
	TICK_ALLOC_HISTO(UPD_PAP,sizeofW(StgPAP)-1,g,s)

#define TICK_ALLOC_TSO(g,s)						\
	ALLOC_TSO_ctr++;	ALLOC_TSO_adm += sizeofW(StgTSO);	\
	ALLOC_TSO_gds += (g);	ALLOC_TSO_slp += (s);			\
	TICK_ALLOC_HISTO(TSO,sizeofW(StgTSO),g,s)
     
#ifdef PAR
#define TICK_ALLOC_FMBQ(a,g,s) 				\
	ALLOC_FMBQ_ctr++;	ALLOC_FMBQ_adm += (a);	\
	ALLOC_FMBQ_gds += (g);	ALLOC_FMBQ_slp += (s);	\
	TICK_ALLOC_HISTO(FMBQ,a,g,s)

#define TICK_ALLOC_FME(a,g,s) 				\
	ALLOC_FME_ctr++;	ALLOC_FME_adm += (a);	\
	ALLOC_FME_gds += (g);	ALLOC_FME_slp += (s);	\
	TICK_ALLOC_HISTO(FME,a,g,s)

#define TICK_ALLOC_BF(a,g,s)  				\
	ALLOC_BF_ctr++;	ALLOC_BF_adm += (a);		\
	ALLOC_BF_gds += (g);	ALLOC_BF_slp += (s);	\
	TICK_ALLOC_HISTO(BF,a,g,s)
#endif
     
/* The histogrammy bit is fairly straightforward; the -2 is: one for
 * 0-origin C arrays; the other one because we do no one-word
 * allocations, so we would never inc that histogram slot; so we shift
 * everything over by one.
 */
#define TICK_ALLOC_HISTO(categ,a,g,s)				\
	{ I_ __idx;						\
	  __idx = (a) + (g) + (s) - 2;				\
	 ALLOC_##categ##_hst[((__idx > 4) ? 4 : __idx)] += 1;} 

/* Some hard-to-account-for words are allocated by/for primitives,
 * includes Integer support.  ALLOC_PRIM2 tells us about these.  We
 * count everything as ``goods'', which is not strictly correct.
 * (ALLOC_PRIM is the same sort of stuff, but we know the
 * admin/goods/slop breakdown.)
 */
#define TICK_ALLOC_PRIM(a,g,s)				\
	ALLOC_PRIM_ctr++;	ALLOC_PRIM_adm += (a);	\
	ALLOC_PRIM_gds += (g); 	ALLOC_PRIM_slp += (s);	\
	TICK_ALLOC_HISTO(PRIM,a,g,s)

#define TICK_ALLOC_PRIM2(w) ALLOC_PRIM_ctr++; ALLOC_PRIM_gds +=(w); \
		       TICK_ALLOC_HISTO(PRIM,0,w,0)


/* -----------------------------------------------------------------------------
   Enters
   -------------------------------------------------------------------------- */

#define TICK_ENT_VIA_NODE()	ENT_VIA_NODE_ctr++

#define TICK_ENT_THK()		ENT_THK_ctr++         /* thunk */
#define TICK_ENT_FUN_STD()	ENT_FUN_STD_ctr++     /* std entry pt */
#define TICK_ENT_FUN_DIRECT(n) 	ENT_FUN_DIRECT_ctr++  /* fast entry pt */

#define TICK_ENT_CON(n)		ENT_CON_ctr++	      /* enter constructor */
#define TICK_ENT_IND(n)		ENT_IND_ctr++	      /* enter indirection */
#define TICK_ENT_PAP(n)		ENT_PAP_ctr++	      /* enter PAP */
#define TICK_ENT_AP_UPD(n) 	ENT_AP_UPD_ctr++      /* enter AP_UPD */
#define TICK_ENT_BH()		ENT_BH_ctr++          /* enter BLACKHOLE */

/* -----------------------------------------------------------------------------
   Returns
   -------------------------------------------------------------------------- */

/* Whenever a ``return'' occurs, it is returning the constituent parts of
 * a data constructor.  The parts can be returned either in registers, or
 * by allocating some heap to put it in (the TICK_ALLOC_* macros account for
 * the allocation).  The constructor can either be an existing one
 * *OLD* or we could have {\em just} figured out this stuff
 * *NEW*.
 */

#define TICK_RET_HISTO(categ,n)					\
	{ I_ __idx;						\
	  __idx = (n);						\
	 RET_##categ##_hst[((__idx > 8) ? 8 : __idx)] += 1;} 

#define TICK_RET_NEW(n)	RET_NEW_ctr++; \
			TICK_RET_HISTO(NEW,n)

#define TICK_RET_OLD(n)	RET_OLD_ctr++; \
			TICK_RET_HISTO(OLD,n)

#define TICK_RET_UNBOXED_TUP(n)  RET_UNBOXED_TUP_ctr++; \
                         TICK_RET_HISTO(UNBOXED_TUP,n)

#define TICK_RET_SEMI(n) RET_SEMI_IN_HEAP_ctr++; \
			 TICK_RET_HISTO(SEMI_IN_HEAP,n)

#define TICK_RET_SEMI_BY_DEFAULT()/*???*/ RET_SEMI_BY_DEFAULT_ctr++

#define TICK_RET_SEMI_FAILED(tag)	do {				\
				if ((tag) == INFO_IND_TAG)		\
				    RET_SEMI_FAILED_IND_ctr++;		\
				else					\
				    RET_SEMI_FAILED_UNEVAL_ctr++;	\
				} while (0)

#define TICK_VEC_RETURN(n)	VEC_RETURN_ctr++;	    \
				TICK_RET_HISTO(VEC_RETURN,n)

/* -----------------------------------------------------------------------------
   Stack Frames

   Macro			   Counts
   ------------------              -------------------------------------------
   TICK_UPDF_PUSHED	 	   Update frame pushed
   TICK_SEQF_PUSHED	 	   Seq frame pushed
   TICK_CATCHF_PUSHED	 	   Catch frame pushed
   TICK_UPDF_OMITTED		   A thunk decided not to push an update frame
   TICK_UPDF_RCC_PUSHED		   Cost Centre restore frame pushed
   TICK_UPDF_RCC_OMITTED	   Cost Centres not required -- not pushed

   -------------------------------------------------------------------------- */

#define TICK_UPDF_OMITTED()	UPDF_OMITTED_ctr++
#define TICK_UPDF_PUSHED()	UPDF_PUSHED_ctr++
#define TICK_SEQF_PUSHED()      SEQF_PUSHED_ctr++
#define TICK_CATCHF_PUSHED()    CATCHF_PUSHED_ctr++
#define TICK_UPDF_RCC_PUSHED()	UPDF_RCC_PUSHED_ctr++
#define TICK_UPDF_RCC_OMITTED()	UPDF_RCC_OMITTED_ctr++

/* -----------------------------------------------------------------------------
   Updates

   These macros record information when we do an update.  We always
   update either with a data constructor (CON) or a partial application
   (PAP).
   
   
   Macro				Where
   -----------------------  	--------------------------------------------
   TICK_UPD_EXISTING		Updating with an indirection to something
   				already in the heap
   TICK_UPD_SQUEEZED		Same as UPD_EXISTING but because
   				of stack-squeezing
   TICK_UPD_CON_IN_NEW		Allocating a new CON
   TICK_UPD_PAP_IN_NEW		Allocating a new PAP
   TICK_UPD_PAP_IN_PLACE	Updating with a PAP in place

   -------------------------------------------------------------------------- */

#define TICK_UPD_HISTO(categ,n) \
	{ I_ __idx;						 \
	  __idx = (n);						 \
	 UPD_##categ##_hst[((__idx > 8) ? 8 : __idx)] += 1;} 

#define TICK_UPD_EXISTING()		UPD_EXISTING_ctr++
#define TICK_UPD_SQUEEZED()		UPD_SQUEEZED_ctr++

#define TICK_UPD_CON_IN_NEW(n)		UPD_CON_IN_NEW_ctr++ ; \
					TICK_UPD_HISTO(CON_IN_NEW,n)

#define TICK_UPD_PAP_IN_NEW(n)		UPD_PAP_IN_NEW_ctr++ ; \
					TICK_UPD_HISTO(PAP_IN_NEW,n)

#define TICK_UPD_PAP_IN_PLACE()		UPD_PAP_IN_PLACE_ctr++

/* For the generational collector: 
 */
#define TICK_UPD_NEW_IND()		UPD_NEW_IND_ctr++
#define TICK_UPD_OLD_IND()		UPD_OLD_IND_ctr++			

/* -----------------------------------------------------------------------------
   Garbage collection counters
   -------------------------------------------------------------------------- */

/* Selectors:
 *
 * GC_SEL_ABANDONED: we could've done the selection, but we gave up
 * (e.g., to avoid overflowing the C stack); GC_SEL_MINOR: did a
 * selection in a minor GC; GC_SEL_MAJOR: ditto, but major GC.
 */
#define TICK_GC_SEL_ABANDONED()		GC_SEL_ABANDONED_ctr++
#define TICK_GC_SEL_MINOR()		GC_SEL_MINOR_ctr++
#define TICK_GC_SEL_MAJOR()		GC_SEL_MAJOR_ctr++

/* Failed promotion: we wanted to promote an object early, but
 * it had already been evacuated to (or resided in) a younger
 * generation.
 */
#define TICK_GC_FAILED_PROMOTION()	GC_FAILED_PROMOTION_ctr++

/* -----------------------------------------------------------------------------
   The accumulators (extern decls)
   -------------------------------------------------------------------------- */

#ifdef TICKY_C
#define INIT(ializer) = ializer
#define EXTERN
#else
#define INIT(ializer)
#define EXTERN extern
#endif

EXTERN unsigned long ALLOC_HEAP_ctr INIT(0);
EXTERN unsigned long ALLOC_HEAP_tot INIT(0);

EXTERN unsigned long ALLOC_FUN_ctr INIT(0);
EXTERN unsigned long ALLOC_FUN_adm INIT(0);
EXTERN unsigned long ALLOC_FUN_gds INIT(0);
EXTERN unsigned long ALLOC_FUN_slp INIT(0);
EXTERN unsigned long ALLOC_FUN_hst[5] 
#ifdef TICKY_C
   = {0,0,0,0,0}  /* urk, can't use INIT macro 'cause of the commas */
#endif
;

EXTERN unsigned long ALLOC_THK_ctr INIT(0);
EXTERN unsigned long ALLOC_THK_adm INIT(0);
EXTERN unsigned long ALLOC_THK_gds INIT(0);
EXTERN unsigned long ALLOC_THK_slp INIT(0);
EXTERN unsigned long ALLOC_THK_hst[5]
#ifdef TICKY_C
   = {0,0,0,0,0}
#endif
;

EXTERN unsigned long ALLOC_CON_ctr INIT(0);
EXTERN unsigned long ALLOC_CON_adm INIT(0);
EXTERN unsigned long ALLOC_CON_gds INIT(0);
EXTERN unsigned long ALLOC_CON_slp INIT(0);
EXTERN unsigned long ALLOC_CON_hst[5]
#ifdef TICKY_C
   = {0,0,0,0,0}
#endif
;

EXTERN unsigned long ALLOC_TUP_ctr INIT(0);
EXTERN unsigned long ALLOC_TUP_adm INIT(0);
EXTERN unsigned long ALLOC_TUP_gds INIT(0);
EXTERN unsigned long ALLOC_TUP_slp INIT(0);
EXTERN unsigned long ALLOC_TUP_hst[5]
#ifdef TICKY_C
   = {0,0,0,0,0}
#endif
;

EXTERN unsigned long ALLOC_BH_ctr INIT(0);
EXTERN unsigned long ALLOC_BH_adm INIT(0);
EXTERN unsigned long ALLOC_BH_gds INIT(0);
EXTERN unsigned long ALLOC_BH_slp INIT(0);
EXTERN unsigned long ALLOC_BH_hst[5]
#ifdef TICKY_C
   = {0,0,0,0,0}
#endif
;

EXTERN unsigned long ALLOC_PRIM_ctr INIT(0);
EXTERN unsigned long ALLOC_PRIM_adm INIT(0);
EXTERN unsigned long ALLOC_PRIM_gds INIT(0);
EXTERN unsigned long ALLOC_PRIM_slp INIT(0);
EXTERN unsigned long ALLOC_PRIM_hst[5]
#ifdef TICKY_C
   = {0,0,0,0,0}
#endif
;

EXTERN unsigned long ALLOC_UPD_PAP_ctr INIT(0);
EXTERN unsigned long ALLOC_UPD_PAP_adm INIT(0);
EXTERN unsigned long ALLOC_UPD_PAP_gds INIT(0);
EXTERN unsigned long ALLOC_UPD_PAP_slp INIT(0);
EXTERN unsigned long ALLOC_UPD_PAP_hst[5]
#ifdef TICKY_C
   = {0,0,0,0,0}
#endif
;

EXTERN unsigned long ALLOC_TSO_ctr INIT(0);
EXTERN unsigned long ALLOC_TSO_adm INIT(0);
EXTERN unsigned long ALLOC_TSO_gds INIT(0);
EXTERN unsigned long ALLOC_TSO_slp INIT(0);
EXTERN unsigned long ALLOC_TSO_hst[5]
#ifdef TICKY_C
   = {0,0,0,0,0}
#endif
;

# ifdef PAR
EXTERN unsigned long ALLOC_FMBQ_ctr INIT(0);
EXTERN unsigned long ALLOC_FMBQ_adm INIT(0);
EXTERN unsigned long ALLOC_FMBQ_gds INIT(0);
EXTERN unsigned long ALLOC_FMBQ_slp INIT(0);
EXTERN unsigned long ALLOC_FMBQ_hst[5]
#ifdef TICKY_C
   = {0,0,0,0,0}
#endif
;

EXTERN unsigned long ALLOC_FME_ctr INIT(0);
EXTERN unsigned long ALLOC_FME_adm INIT(0);
EXTERN unsigned long ALLOC_FME_gds INIT(0);
EXTERN unsigned long ALLOC_FME_slp INIT(0);
EXTERN unsigned long ALLOC_FME_hst[5]
#ifdef TICKY_C
   = {0,0,0,0,0}
#endif
;

EXTERN unsigned long ALLOC_BF_ctr INIT(0);
EXTERN unsigned long ALLOC_BF_adm INIT(0);
EXTERN unsigned long ALLOC_BF_gds INIT(0);
EXTERN unsigned long ALLOC_BF_slp INIT(0);
EXTERN unsigned long ALLOC_BF_hst[5]
#ifdef TICKY_C
   = {0,0,0,0,0}
#endif
;
#endif

EXTERN unsigned long ENT_VIA_NODE_ctr INIT(0);
EXTERN unsigned long ENT_THK_ctr INIT(0);
EXTERN unsigned long ENT_FUN_STD_ctr INIT(0);
EXTERN unsigned long ENT_FUN_DIRECT_ctr INIT(0);
EXTERN unsigned long ENT_CON_ctr INIT(0);
EXTERN unsigned long ENT_IND_ctr INIT(0);
EXTERN unsigned long ENT_PAP_ctr INIT(0);
EXTERN unsigned long ENT_AP_UPD_ctr INIT(0);
EXTERN unsigned long ENT_BH_ctr INIT(0);

EXTERN unsigned long RET_NEW_ctr INIT(0);
EXTERN unsigned long RET_OLD_ctr INIT(0);
EXTERN unsigned long RET_UNBOXED_TUP_ctr INIT(0);
EXTERN unsigned long RET_SEMI_BY_DEFAULT_ctr INIT(0);
EXTERN unsigned long RET_SEMI_IN_HEAP_ctr INIT(0);
EXTERN unsigned long RET_SEMI_FAILED_IND_ctr INIT(0);
EXTERN unsigned long RET_SEMI_FAILED_UNEVAL_ctr INIT(0);

EXTERN unsigned long VEC_RETURN_ctr INIT(0);

EXTERN unsigned long RET_NEW_hst[9]
#ifdef TICKY_C
   = {0,0,0,0,0,0,0,0,0}
#endif
;
EXTERN unsigned long RET_OLD_hst[9]
#ifdef TICKY_C
   = {0,0,0,0,0,0,0,0,0}
#endif
;
EXTERN unsigned long RET_UNBOXED_TUP_hst[9]
#ifdef TICKY_C
   = {0,0,0,0,0,0,0,0,0}
#endif
;
EXTERN unsigned long RET_SEMI_IN_HEAP_hst[9]
#ifdef TICKY_C
   = {0,0,0,0,0,0,0,0,0}
#endif
;
EXTERN unsigned long RET_VEC_RETURN_hst[9]
#ifdef TICKY_C
   = {0,0,0,0,0,0,0,0,0}
#endif
;

EXTERN unsigned long RET_SEMI_loads_avoided INIT(0);

EXTERN unsigned long UPDF_OMITTED_ctr INIT(0);
EXTERN unsigned long UPDF_PUSHED_ctr INIT(0);
EXTERN unsigned long SEQF_PUSHED_ctr INIT(0);
EXTERN unsigned long CATCHF_PUSHED_ctr INIT(0);
EXTERN unsigned long UPDF_RCC_PUSHED_ctr INIT(0);
EXTERN unsigned long UPDF_RCC_OMITTED_ctr INIT(0);

EXTERN unsigned long UPD_EXISTING_ctr INIT(0);
EXTERN unsigned long UPD_SQUEEZED_ctr INIT(0);
EXTERN unsigned long UPD_CON_IN_NEW_ctr INIT(0);
EXTERN unsigned long UPD_PAP_IN_NEW_ctr INIT(0);
EXTERN unsigned long UPD_PAP_IN_PLACE_ctr INIT(0);

EXTERN unsigned long UPD_CON_IN_NEW_hst[9]
#ifdef TICKY_C
   = {0,0,0,0,0,0,0,0,0}
#endif
;
EXTERN unsigned long UPD_PAP_IN_NEW_hst[9]
#ifdef TICKY_C
   = {0,0,0,0,0,0,0,0,0}
#endif
;

EXTERN unsigned long UPD_NEW_IND_ctr INIT(0);
EXTERN unsigned long UPD_OLD_IND_ctr INIT(0);

EXTERN unsigned long GC_SEL_ABANDONED_ctr INIT(0);
EXTERN unsigned long GC_SEL_MINOR_ctr INIT(0);
EXTERN unsigned long GC_SEL_MAJOR_ctr INIT(0);

EXTERN unsigned long GC_FAILED_PROMOTION_ctr INIT(0);

#undef INIT
#undef EXTERN

/* -----------------------------------------------------------------------------
   Just stubs if no ticky-ticky profiling
   -------------------------------------------------------------------------- */

#else /* !TICKY_TICKY */

#define TICK_ALLOC_HEAP(words)

#define TICK_ALLOC_FUN(g,s)
#define TICK_ALLOC_THK(g,s)
#define TICK_ALLOC_CON(g,s)
#define TICK_ALLOC_TUP(g,s)
#define TICK_ALLOC_BH(g,s)
#define TICK_ALLOC_UPD_PAP(g,s)
#define TICK_ALLOC_TSO(g,s)
#define TICK_ALLOC_FMBQ(a,g,s)
#define TICK_ALLOC_FME(a,g,s)
#define TICK_ALLOC_BF(a,g,s)
#define TICK_ALLOC_PRIM(a,g,s)
#define TICK_ALLOC_PRIM2(w)

#define TICK_ENT_VIA_NODE()	
				
#define TICK_ENT_THK()
#define TICK_ENT_FUN_STD()
#define TICK_ENT_FUN_DIRECT(n)
				
#define TICK_ENT_CON(n)
#define TICK_ENT_IND(n)
#define TICK_ENT_PAP(n)
#define TICK_ENT_AP_UPD(n)
#define TICK_ENT_BH()

#define TICK_RET_NEW(n)
#define TICK_RET_OLD(n)
#define TICK_RET_UNBOXED_TUP(n)
#define TICK_RET_SEMI(n)
#define TICK_RET_SEMI_BY_DEFAULT()
#define TICK_RET_SEMI_FAILED(tag)
#define TICK_VEC_RETURN(n)

#define TICK_UPDF_OMITTED()
#define TICK_UPDF_PUSHED()
#define TICK_SEQF_PUSHED()
#define TICK_CATCHF_PUSHED()
#define TICK_UPDF_RCC_PUSHED()
#define TICK_UPDF_RCC_OMITTED()

#define TICK_UPD_EXISTING()
#define TICK_UPD_SQUEEZED()
#define TICK_UPD_CON_IN_NEW(n)
#define TICK_UPD_PAP_IN_NEW(n)
#define TICK_UPD_PAP_IN_PLACE()

#define TICK_UPD_NEW_IND()
#define TICK_UPD_OLD_IND()

#define TICK_GC_SEL_ABANDONED()
#define TICK_GC_SEL_MINOR()
#define TICK_GC_SEL_MAJOR()

#define TICK_GC_FAILED_PROMOTION()

#endif /* !TICKY_TICKY */

#endif /* TICKY_H */
