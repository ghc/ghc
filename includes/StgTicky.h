/* ----------------------------------------------------------------------------
 *
 * (c) The AQUA project, Glasgow University, 1994-1997
 * (c) The GHC Team, 1998-1999
 *
 * Ticky-ticky profiling macros.
 *
 * -------------------------------------------------------------------------- */

#ifndef TICKY_H
#define TICKY_H

/* -----------------------------------------------------------------------------
   The StgEntCounter type - needed regardless of TICKY_TICKY
   -------------------------------------------------------------------------- */

typedef struct _StgEntCounter {
    StgWord16	registeredp;	/* 0 == no, 1 == yes */
    StgWord16	arity;		/* arity (static info) */
    StgWord16	stk_args;	/* # of args off stack */
				/* (rest of args are in registers) */
    char   	*str;		/* name of the thing */
    char   	*arg_kinds;	/* info about the args types */
    StgInt	entry_count;	/* Trips to fast entry code */
    StgInt      allocs;         /* number of allocations by this fun */
    struct _StgEntCounter *link;/* link to chain them all together */
} StgEntCounter;


#ifdef TICKY_TICKY

/* -----------------------------------------------------------------------------
   Allocations
   -------------------------------------------------------------------------- */

/* How many times we do a heap check and move Hp; comparing this with
 * the allocations gives an indication of how many things we get per trip
 * to the well:
 */
#define TICK_ALLOC_HEAP(n, f_ct)		\
  {						\
    f_ct.allocs += (n);				\
    ALLOC_HEAP_ctr++;				\
    ALLOC_HEAP_tot += (n);			\
  }

#define TICK_ALLOC_HEAP_NOCTR(n)		\
  {						\
    ALLOC_HEAP_ctr++;				\
    ALLOC_HEAP_tot += (n);			\
  }

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

#define TICK_ALLOC_UP_THK(g,s)                          \
     	ALLOC_UP_THK_ctr++;	ALLOC_THK_adm += _HS;	\
	ALLOC_THK_gds += (g);	ALLOC_THK_slp += (s);	\
	TICK_ALLOC_HISTO(THK,_HS,g,s)

#define TICK_ALLOC_SE_THK(g,s)                          \
     	ALLOC_SE_THK_ctr++;	ALLOC_THK_adm += _HS;	\
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

/*
 * admin size doesn't take into account the FUN, that is accounted for
 * in the "goods".
 */
#define TICK_ALLOC_PAP(g,s)					\
	ALLOC_PAP_ctr++;      ALLOC_PAP_adm += sizeofW(StgPAP)-1; \
	ALLOC_PAP_gds += (g); ALLOC_PAP_slp += (s);	\
	TICK_ALLOC_HISTO(PAP,sizeofW(StgPAP)-1,g,s)

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

#define TICK_ENT_STATIC_THK()	ENT_STATIC_THK_ctr++ 
#define TICK_ENT_DYN_THK()	ENT_DYN_THK_ctr++

#define TICK_CTR(f_ct, str, arity, args, arg_kinds)	\
   static StgEntCounter f_ct			\
	= { 0, arity, args,			\
	    str, arg_kinds, 			\
	    0, 0, NULL };

#define TICK_ENT_FUN_DIRECT_BODY(f_ct)                          \
	{							\
	  if ( ! f_ct.registeredp ) {				\
	    /* hook this one onto the front of the list */	\
	    f_ct.link = ticky_entry_ctrs;			\
	    ticky_entry_ctrs = & (f_ct);			\
	    /* mark it as "registered" */			\
	    f_ct.registeredp = 1;				\
	  }							\
	  f_ct.entry_count += 1;				\
	}

#define TICK_ENT_STATIC_FUN_DIRECT(f_ct)			\
        TICK_ENT_FUN_DIRECT_BODY(f_ct)                          \
	ENT_STATIC_FUN_DIRECT_ctr++ /* The static total one */

#define TICK_ENT_DYN_FUN_DIRECT(f_ct)				\
        TICK_ENT_FUN_DIRECT_BODY(f_ct)                          \
	ENT_DYN_FUN_DIRECT_ctr++ /* The dynamic total one */

extern StgEntCounter top_ct;
extern StgEntCounter *ticky_entry_ctrs;

#define TICK_ENT_STATIC_CON(n)	ENT_STATIC_CON_ctr++  /* enter static constructor */
#define TICK_ENT_DYN_CON(n)	ENT_DYN_CON_ctr++     /* enter dynamic constructor */
#define TICK_ENT_STATIC_IND(n)	ENT_STATIC_IND_ctr++  /* enter static indirection */
#define TICK_ENT_DYN_IND(n)	ENT_DYN_IND_ctr++     /* enter dynamic indirection */
#define TICK_ENT_PERM_IND(n)    ENT_PERM_IND_ctr++    /* enter permanent indirection */
#define TICK_ENT_PAP(n)		ENT_PAP_ctr++	      /* enter PAP */
#define TICK_ENT_AP(n) 	        ENT_AP_ctr++          /* enter AP_UPD */
#define TICK_ENT_AP_STACK(n)    ENT_AP_STACK_ctr++    /* enter AP_STACK_UPD */
#define TICK_ENT_BH()		ENT_BH_ctr++          /* enter BLACKHOLE */


#define TICK_SLOW_HISTO(n)				\
 { unsigned __idx;					\
   __idx = (n);						\
   SLOW_CALL_hst[((__idx > 8) ? 8 : __idx)] += 1;	\
 }

#define UNDO_TICK_SLOW_HISTO(n)				\
 { unsigned __idx;					\
   __idx = (n);						\
   SLOW_CALL_hst[((__idx > 8) ? 8 : __idx)] -= 1;	\
 }

/*
 * A slow call with n arguments.  In the unevald case, this call has
 * already been counted once, so don't count it again.
 */
#define TICK_SLOW_CALL(n) \
  SLOW_CALL_ctr++; \
  TICK_SLOW_HISTO(n)

/*
 * This slow call was found to be to an unevaluated function; undo the
 * ticks we did in TICK_SLOW_CALL.
 */
#define TICK_SLOW_CALL_UNEVALD(n) \
  SLOW_CALL_UNEVALD_ctr++; \
  SLOW_CALL_ctr--; \
  UNDO_TICK_SLOW_HISTO(n)

#define TICK_MULTI_CHUNK_SLOW_CALL(pattern, chunks) \
  fprintf(stderr, "Multi-chunk slow call: %s\n", pattern); \
  MULTI_CHUNK_SLOW_CALL_ctr++; \
  MULTI_CHUNK_SLOW_CALL_CHUNKS_ctr += chunks;

/* A completely unknown tail-call */
#define TICK_UNKNOWN_CALL()               UNKNOWN_CALL_ctr++

/*
 * slow call patterns (includes "extra" args to known calls,
 * so the total of these will be greater than UNKNOWN_CALL_ctr).
 */
#define TICK_SLOW_CALL_v()             SLOW_CALL_v_ctr++
#define TICK_SLOW_CALL_f()             SLOW_CALL_f_ctr++
#define TICK_SLOW_CALL_d()             SLOW_CALL_d_ctr++
#define TICK_SLOW_CALL_l()             SLOW_CALL_l_ctr++
#define TICK_SLOW_CALL_n()             SLOW_CALL_n_ctr++
#define TICK_SLOW_CALL_p()             SLOW_CALL_p_ctr++
#define TICK_SLOW_CALL_pv()            SLOW_CALL_pv_ctr++
#define TICK_SLOW_CALL_pp()            SLOW_CALL_pp_ctr++
#define TICK_SLOW_CALL_ppv()           SLOW_CALL_ppv_ctr++
#define TICK_SLOW_CALL_ppp()           SLOW_CALL_ppp_ctr++
#define TICK_SLOW_CALL_pppv()          SLOW_CALL_pppv_ctr++
#define TICK_SLOW_CALL_pppp()          SLOW_CALL_pppp_ctr++
#define TICK_SLOW_CALL_ppppp()         SLOW_CALL_ppppp_ctr++
#define TICK_SLOW_CALL_pppppp()        SLOW_CALL_pppppp_ctr++
#define TICK_SLOW_CALL_OTHER(pattern) \
     fprintf(stderr,"slow call: %s\n", pattern); \
     SLOW_CALL_OTHER_ctr++

#define TICK_KNOWN_CALL()               KNOWN_CALL_ctr++
#define TICK_KNOWN_CALL_TOO_FEW_ARGS()  KNOWN_CALL_TOO_FEW_ARGS_ctr++
#define TICK_KNOWN_CALL_EXTRA_ARGS()    KNOWN_CALL_EXTRA_ARGS_ctr++

/* A slow call to a FUN found insufficient arguments, and built a PAP */
#define TICK_SLOW_CALL_FUN_TOO_FEW()	    SLOW_CALL_FUN_TOO_FEW_ctr++
#define TICK_SLOW_CALL_FUN_CORRECT()	    SLOW_CALL_FUN_CORRECT_ctr++
#define TICK_SLOW_CALL_FUN_TOO_MANY()	    SLOW_CALL_FUN_TOO_MANY_ctr++
#define TICK_SLOW_CALL_PAP_TOO_FEW()	    SLOW_CALL_PAP_TOO_FEW_ctr++
#define TICK_SLOW_CALL_PAP_CORRECT()	    SLOW_CALL_PAP_CORRECT_ctr++
#define TICK_SLOW_CALL_PAP_TOO_MANY()	    SLOW_CALL_PAP_TOO_MANY_ctr++
  
/* -----------------------------------------------------------------------------
   Returns
   -------------------------------------------------------------------------- */

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

#define TICK_VEC_RETURN(n)	VEC_RETURN_ctr++;	    \
				TICK_RET_HISTO(VEC_RETURN,n)

/* -----------------------------------------------------------------------------
   Stack Frames

   Macro			   Counts
   ------------------              -------------------------------------------
   TICK_UPDF_PUSHED	 	   Update frame pushed
   TICK_CATCHF_PUSHED	 	   Catch frame pushed
   TICK_UPDF_OMITTED		   A thunk decided not to push an update frame
   TICK_UPDF_RCC_PUSHED		   Cost Centre restore frame pushed
   TICK_UPDF_RCC_OMITTED	   Cost Centres not required -- not pushed

   -------------------------------------------------------------------------- */

#define TICK_UPDF_OMITTED()	UPDF_OMITTED_ctr++
#define TICK_UPDF_PUSHED(tgt,inf)	UPDF_PUSHED_ctr++ \
/*                              ; fprintf(stderr,"UPDF_PUSHED:%p:%p\n",tgt,inf) */
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
   TICK_UPD_SQUEEZED		Same as UPD_EXISTING but because
   				of stack-squeezing

   TICK_UPD_CON_IN_NEW		Allocating a new CON
   TICK_UPD_CON_IN_PLACE	Updating with a PAP in place
   TICK_UPD_PAP_IN_NEW		Allocating a new PAP
   TICK_UPD_PAP_IN_PLACE	Updating with a PAP in place

   ToDo: the IN_PLACE versions are not relevant any more.
   -------------------------------------------------------------------------- */

#define TICK_UPD_HISTO(categ,n) \
	{ I_ __idx;						 \
	  __idx = (n);						 \
	 UPD_##categ##_hst[((__idx > 8) ? 8 : __idx)] += 1;} 

#define TICK_UPD_SQUEEZED()		UPD_SQUEEZED_ctr++

#define TICK_UPD_CON_IN_NEW(n)		UPD_CON_IN_NEW_ctr++ ; \
					TICK_UPD_HISTO(CON_IN_NEW,n)

#define TICK_UPD_CON_IN_PLACE(n)	UPD_CON_IN_PLACE_ctr++; \
					TICK_UPD_HISTO(CON_IN_PLACE,n)

#define TICK_UPD_PAP_IN_NEW(n)		UPD_PAP_IN_NEW_ctr++ ; \
					TICK_UPD_HISTO(PAP_IN_NEW,n)

#define TICK_UPD_PAP_IN_PLACE()		UPD_PAP_IN_PLACE_ctr++

/* For the generational collector: 
 */
#define TICK_UPD_NEW_IND()		UPD_NEW_IND_ctr++
#define TICK_UPD_NEW_PERM_IND(tgt)	UPD_NEW_PERM_IND_ctr++ \
/*                                      ; fprintf(stderr,"UPD_NEW_PERM:%p\n",tgt) */
#define TICK_UPD_OLD_IND()		UPD_OLD_IND_ctr++			
#define TICK_UPD_OLD_PERM_IND()		UPD_OLD_PERM_IND_ctr++			

/* Count blackholes:
 */
#define TICK_UPD_BH_UPDATABLE()         UPD_BH_UPDATABLE_ctr++
#define TICK_UPD_BH_SINGLE_ENTRY()      UPD_BH_SINGLE_ENTRY_ctr++
#define TICK_UPD_CAF_BH_UPDATABLE(s)                          \
     UPD_CAF_BH_UPDATABLE_ctr++                               \
/*   ; fprintf(stderr,"TICK_UPD_CAF_BH_UPDATABLE(%s)\n",s) */
#define TICK_UPD_CAF_BH_SINGLE_ENTRY(s)                       \
     UPD_CAF_BH_SINGLE_ENTRY_ctr++                            \
/*   ; fprintf(stderr,"TICK_UPD_CAF_BH_SINGLE_ENTRY(%s)\n",s) */


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

/* Bytes copied: this is a fairly good measure of GC cost and depends
 * on all sorts of things like number of generations, aging, eager
 * promotion, generation sizing policy etc.
 */
#define TICK_GC_WORDS_COPIED(n)         GC_WORDS_COPIED_ctr+=(n)

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

EXTERN unsigned long ALLOC_UP_THK_ctr INIT(0);
EXTERN unsigned long ALLOC_SE_THK_ctr INIT(0);
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

EXTERN unsigned long ALLOC_PAP_ctr INIT(0);
EXTERN unsigned long ALLOC_PAP_adm INIT(0);
EXTERN unsigned long ALLOC_PAP_gds INIT(0);
EXTERN unsigned long ALLOC_PAP_slp INIT(0);
EXTERN unsigned long ALLOC_PAP_hst[5]
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
#endif /* PAR */

EXTERN unsigned long ENT_VIA_NODE_ctr INIT(0);
EXTERN unsigned long ENT_STATIC_THK_ctr INIT(0);
EXTERN unsigned long ENT_DYN_THK_ctr INIT(0);
EXTERN unsigned long ENT_STATIC_FUN_DIRECT_ctr INIT(0);
EXTERN unsigned long ENT_DYN_FUN_DIRECT_ctr INIT(0);
EXTERN unsigned long ENT_STATIC_CON_ctr INIT(0);
EXTERN unsigned long ENT_DYN_CON_ctr INIT(0);
EXTERN unsigned long ENT_STATIC_IND_ctr INIT(0);
EXTERN unsigned long ENT_DYN_IND_ctr INIT(0);
EXTERN unsigned long ENT_PERM_IND_ctr INIT(0);
EXTERN unsigned long ENT_PAP_ctr INIT(0);
EXTERN unsigned long ENT_AP_ctr INIT(0);
EXTERN unsigned long ENT_AP_STACK_ctr INIT(0);
EXTERN unsigned long ENT_BH_ctr INIT(0);

EXTERN unsigned long UNKNOWN_CALL_ctr INIT(0);

EXTERN unsigned long SLOW_CALL_v_ctr INIT(0);
EXTERN unsigned long SLOW_CALL_f_ctr INIT(0);
EXTERN unsigned long SLOW_CALL_d_ctr INIT(0);
EXTERN unsigned long SLOW_CALL_l_ctr INIT(0);
EXTERN unsigned long SLOW_CALL_n_ctr INIT(0);
EXTERN unsigned long SLOW_CALL_p_ctr INIT(0);
EXTERN unsigned long SLOW_CALL_pv_ctr INIT(0);
EXTERN unsigned long SLOW_CALL_pp_ctr INIT(0);
EXTERN unsigned long SLOW_CALL_ppv_ctr INIT(0);
EXTERN unsigned long SLOW_CALL_ppp_ctr INIT(0);
EXTERN unsigned long SLOW_CALL_pppv_ctr INIT(0);
EXTERN unsigned long SLOW_CALL_pppp_ctr INIT(0);
EXTERN unsigned long SLOW_CALL_ppppp_ctr INIT(0);
EXTERN unsigned long SLOW_CALL_pppppp_ctr INIT(0);
EXTERN unsigned long SLOW_CALL_OTHER_ctr INIT(0);

EXTERN unsigned long ticky_slow_call_unevald INIT(0);
EXTERN unsigned long SLOW_CALL_ctr INIT(0);
EXTERN unsigned long MULTI_CHUNK_SLOW_CALL_ctr INIT(0);
EXTERN unsigned long MULTI_CHUNK_SLOW_CALL_CHUNKS_ctr INIT(0);
EXTERN unsigned long KNOWN_CALL_ctr INIT(0);
EXTERN unsigned long KNOWN_CALL_TOO_FEW_ARGS_ctr INIT(0);
EXTERN unsigned long KNOWN_CALL_EXTRA_ARGS_ctr INIT(0);
EXTERN unsigned long SLOW_CALL_FUN_TOO_FEW_ctr INIT(0);
EXTERN unsigned long SLOW_CALL_FUN_CORRECT_ctr INIT(0);
EXTERN unsigned long SLOW_CALL_FUN_TOO_MANY_ctr INIT(0);
EXTERN unsigned long SLOW_CALL_PAP_TOO_FEW_ctr INIT(0);
EXTERN unsigned long SLOW_CALL_PAP_CORRECT_ctr INIT(0);
EXTERN unsigned long SLOW_CALL_PAP_TOO_MANY_ctr INIT(0);
EXTERN unsigned long SLOW_CALL_UNEVALD_ctr INIT(0);

EXTERN unsigned long SLOW_CALL_hst[8]
#ifdef TICKY_C
   = {0,0,0,0,0,0,0,0}
#endif
;

EXTERN unsigned long RET_NEW_ctr INIT(0);
EXTERN unsigned long RET_OLD_ctr INIT(0);
EXTERN unsigned long RET_UNBOXED_TUP_ctr INIT(0);

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
EXTERN unsigned long CATCHF_PUSHED_ctr INIT(0);
EXTERN unsigned long UPDF_RCC_PUSHED_ctr INIT(0);
EXTERN unsigned long UPDF_RCC_OMITTED_ctr INIT(0);

EXTERN unsigned long UPD_SQUEEZED_ctr INIT(0);
EXTERN unsigned long UPD_CON_IN_NEW_ctr INIT(0);
EXTERN unsigned long UPD_CON_IN_PLACE_ctr INIT(0);
EXTERN unsigned long UPD_PAP_IN_NEW_ctr INIT(0);
EXTERN unsigned long UPD_PAP_IN_PLACE_ctr INIT(0);

EXTERN unsigned long UPD_CON_IN_NEW_hst[9]
#ifdef TICKY_C
   = {0,0,0,0,0,0,0,0,0}
#endif
;
EXTERN unsigned long UPD_CON_IN_PLACE_hst[9]
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
EXTERN unsigned long UPD_NEW_PERM_IND_ctr INIT(0);
EXTERN unsigned long UPD_OLD_IND_ctr INIT(0);
EXTERN unsigned long UPD_OLD_PERM_IND_ctr INIT(0);

EXTERN unsigned long UPD_BH_UPDATABLE_ctr INIT(0);
EXTERN unsigned long UPD_BH_SINGLE_ENTRY_ctr INIT(0);
EXTERN unsigned long UPD_CAF_BH_UPDATABLE_ctr INIT(0);
EXTERN unsigned long UPD_CAF_BH_SINGLE_ENTRY_ctr INIT(0);

EXTERN unsigned long GC_SEL_ABANDONED_ctr INIT(0);
EXTERN unsigned long GC_SEL_MINOR_ctr INIT(0);
EXTERN unsigned long GC_SEL_MAJOR_ctr INIT(0);

EXTERN unsigned long GC_FAILED_PROMOTION_ctr INIT(0);

EXTERN unsigned long GC_WORDS_COPIED_ctr INIT(0);

#undef INIT
#undef EXTERN

/* -----------------------------------------------------------------------------
   Just stubs if no ticky-ticky profiling
   -------------------------------------------------------------------------- */

#else /* !TICKY_TICKY */

#define TICK_ALLOC_HEAP(words, f_ct)
#define TICK_ALLOC_HEAP_NOCTR(words)

#define TICK_ALLOC_FUN(g,s)
#define TICK_ALLOC_UP_THK(g,s)
#define TICK_ALLOC_SE_THK(g,s)
#define TICK_ALLOC_CON(g,s)
#define TICK_ALLOC_TUP(g,s)
#define TICK_ALLOC_BH(g,s)
#define TICK_ALLOC_PAP(g,s)
#define TICK_ALLOC_TSO(g,s)
#define TICK_ALLOC_FMBQ(a,g,s)
#define TICK_ALLOC_FME(a,g,s)
#define TICK_ALLOC_BF(a,g,s)
#define TICK_ALLOC_PRIM(a,g,s)
#define TICK_ALLOC_PRIM2(w)

#define TICK_ENT_VIA_NODE()	
				
#define TICK_ENT_STATIC_THK()
#define TICK_ENT_DYN_THK()
#define TICK_ENT_STATIC_FUN_DIRECT(n)
#define TICK_ENT_DYN_FUN_DIRECT(n)
#define TICK_ENT_STATIC_CON(n)
#define TICK_ENT_DYN_CON(n)
#define TICK_ENT_STATIC_IND(n)
#define TICK_ENT_DYN_IND(n)
#define TICK_ENT_PERM_IND(n)
#define TICK_ENT_PAP(n)
#define TICK_ENT_AP(n)
#define TICK_ENT_AP_STACK(n)
#define TICK_ENT_BH()

#define TICK_SLOW_CALL(n)
#define TICK_SLOW_CALL_UNEVALD(n)
#define TICK_SLOW_CALL_FUN_TOO_FEW()
#define TICK_SLOW_CALL_FUN_CORRECT()
#define TICK_SLOW_CALL_FUN_TOO_MANY()
#define TICK_SLOW_CALL_PAP_TOO_FEW()
#define TICK_SLOW_CALL_PAP_CORRECT()
#define TICK_SLOW_CALL_PAP_TOO_MANY()

#define TICK_SLOW_CALL_v()
#define TICK_SLOW_CALL_f()
#define TICK_SLOW_CALL_d()
#define TICK_SLOW_CALL_l()
#define TICK_SLOW_CALL_n()
#define TICK_SLOW_CALL_p()
#define TICK_SLOW_CALL_pv()
#define TICK_SLOW_CALL_pp()
#define TICK_SLOW_CALL_ppv()
#define TICK_SLOW_CALL_ppp()
#define TICK_SLOW_CALL_pppv()
#define TICK_SLOW_CALL_pppp()
#define TICK_SLOW_CALL_ppppp()
#define TICK_SLOW_CALL_pppppp()
#define TICK_SLOW_CALL_OTHER(pattern)

#define TICK_KNOWN_CALL()
#define TICK_KNOWN_CALL_TOO_FEW_ARGS()
#define TICK_KNOWN_CALL_EXTRA_ARGS()
#define TICK_UNKNOWN_CALL()

#define TICK_RET_NEW(n)
#define TICK_RET_OLD(n)
#define TICK_RET_UNBOXED_TUP(n)
#define TICK_RET_SEMI(n)
#define TICK_RET_SEMI_BY_DEFAULT()
#define TICK_RET_SEMI_FAILED(tag)
#define TICK_VEC_RETURN(n)

#define TICK_UPDF_OMITTED()
#define TICK_UPDF_PUSHED(tgt,inf)
#define TICK_CATCHF_PUSHED()
#define TICK_UPDF_RCC_PUSHED()
#define TICK_UPDF_RCC_OMITTED()

#define TICK_UPD_SQUEEZED()
#define TICK_UPD_CON_IN_NEW(n)
#define TICK_UPD_CON_IN_PLACE(n)
#define TICK_UPD_PAP_IN_NEW(n)
#define TICK_UPD_PAP_IN_PLACE()

#define TICK_UPD_NEW_IND()
#define TICK_UPD_NEW_PERM_IND(tgt)
#define TICK_UPD_OLD_IND()
#define TICK_UPD_OLD_PERM_IND()

#define TICK_UPD_BH_UPDATABLE()
#define TICK_UPD_BH_SINGLE_ENTRY()
#define TICK_UPD_CAF_BH_UPDATABLE()
#define TICK_UPD_CAF_BH_SINGLE_ENTRY()

#define TICK_GC_SEL_ABANDONED()
#define TICK_GC_SEL_MINOR()
#define TICK_GC_SEL_MAJOR()

#define TICK_GC_FAILED_PROMOTION()
#define TICK_GC_WORDS_COPIED(n)

#endif /* !TICKY_TICKY */

#endif /* TICKY_H */
