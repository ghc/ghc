/* ----------------------------------------------------------------------------
 * $Id: ClosureMacros.h,v 1.16 1999/05/13 17:31:06 simonm Exp $
 *
 * (c) The GHC Team, 1998-1999
 *
 * Macros for building and manipulating closures
 *
 * -------------------------------------------------------------------------- */

#ifndef CLOSUREMACROS_H
#define CLOSUREMACROS_H

/* -----------------------------------------------------------------------------
   Fixed Header Size

   The compiler tries to abstract away from the actual value of this
   constant.
   -------------------------------------------------------------------------- */

#define _FHS  sizeof(StgHeader)

/* -----------------------------------------------------------------------------
   Info tables are slammed up against the entry code, and the label
   for the info table is at the *end* of the table itself.  This
   inline function adjusts an info pointer to point to the beginning
   of the table, so we can use standard C structure indexing on it.

   Note: this works for SRT info tables as long as you don't want to
   access the SRT, since they are laid out the same with the SRT
   pointer as the first word in the table.

   NOTES ABOUT MANGLED C VS. MINI-INTERPRETER:

   A couple of definitions:

       "info pointer"    The first word of the closure.  Might point
                         to either the end or the beginning of the
			 info table, depending on whether we're using
			 the mini interpretter or not.  GET_INFO(c)
			 retrieves the info pointer of a closure.

       "info table"      The info table structure associated with a
                         closure.  This is always a pointer to the
			 beginning of the structure, so we can
			 use standard C structure indexing to pull out
			 the fields.  get_itbl(c) returns a pointer to
			 the info table for closure c.

   An address of the form xxxx_info points to the end of the info
   table or the beginning of the info table depending on whether we're
   mangling or not respectively.  So, 

         c->header.info = xxx_info 

   makes absolute sense, whether mangling or not.
 
   -------------------------------------------------------------------------- */

#define INIT_INFO(i)  info : &(i)
#define SET_INFO(c,i) ((c)->header.info = (i))
#define GET_INFO(c)   ((c)->header.info)

#if USE_MINIINTERPRETER
#define INIT_ENTRY(e)    entry : (F_)(e)
#define GET_ENTRY(c)     ((c)->header.info->entry)
#define ENTRY_CODE(info) (((StgInfoTable *)info)->entry)
#define INFO_PTR_TO_STRUCT(info) ((StgInfoTable *)info)
#define get_itbl(c)      ((c)->header.info)
static __inline__ StgFunPtr get_entry(const StgInfoTable *itbl) {
    return itbl->entry;
}
#else
#define INIT_ENTRY(e)    code : {}
#define GET_ENTRY(c)     ((StgFunPtr)((c)->header.info))
#define ENTRY_CODE(info) (info)
#define INFO_PTR_TO_STRUCT(info) ((StgInfoTable *)(info) - 1)
#define get_itbl(c)      (((c)->header.info) - 1)
static __inline__ StgFunPtr get_entry(const StgInfoTable *itbl) {
    return (StgFunPtr)(itbl+1);
}
#endif

/* -----------------------------------------------------------------------------
   Macros for distinguishing data pointers from code pointers
   -------------------------------------------------------------------------- */
/*
 * We use some symbols inserted automatically by the linker to decide
 * whether a pointer points to text, data, or user space.  These tests
 * assume that text is lower in the address space than data, which in
 * turn is lower than user allocated memory.  
 *
 * If this assumption is false (say on some strange architecture) then
 * the tests IS_CODE_PTR and IS_DATA_PTR below will need to be
 * modified (and that should be all that's necessary).
 *
 * _start      } start of read-only text space
 * _etext      } end   of read-only text space
 * _end } end of read-write data space 
 */
extern StgFun start;

extern void* TEXT_SECTION_END_MARKER_DECL;
extern void* DATA_SECTION_END_MARKER_DECL;

#define IS_CODE_PTR(p) ((P_)(p) < (P_)&TEXT_SECTION_END_MARKER)
#define IS_DATA_PTR(p) ((P_)(p) >= (P_)&TEXT_SECTION_END_MARKER && (P_)(p) < (P_)&DATA_SECTION_END_MARKER)
#define IS_USER_PTR(p) ((P_)(p) >= (P_)&DATA_SECTION_END_MARKER)

#ifdef HAVE_WIN32_DLL_SUPPORT
extern int is_heap_alloced(const void* x);
# define HEAP_ALLOCED(x)  (is_heap_alloced(x))
#else
# define HEAP_ALLOCED(x)  IS_USER_PTR(x)
#endif

/* When working with Win32 DLLs, static closures are identified by
   being prefixed with a zero word. This is needed so that we can
   distinguish between pointers to static closures and (reversed!)
   info tables.

   This 'scheme' breaks down for closure tables such as CHARLIKE,
   so we catch these separately.
   
   LOOKS_LIKE_STATIC_CLOSURE() 
       - discriminates between static closures and info tbls
         (needed by LOOKS_LIKE_GHC_INFO() below - [Win32 DLLs only.])
   LOOKS_LIKE_STATIC() 
       - distinguishes between static and heap allocated data.
 */
#ifdef HAVE_WIN32_DLL_SUPPORT
#define LOOKS_LIKE_STATIC(r) (!(HEAP_ALLOCED(r)))

/* Tiresome predicates needed to check for pointers into the closure tables */
#define IS_CHARLIKE_CLOSURE(p)  ( (P_)(p) >= (P_)CHARLIKE_closure && (char*)(p) <= ((char*)CHARLIKE_closure + 255 * sizeof(StgIntCharlikeClosure)) )
#define IS_INTLIKE_CLOSURE(p)  ( (P_)(p) >= (P_)INTLIKE_closure && (char*)(p) <= ((char*)INTLIKE_closure + 32 * sizeof(StgIntCharlikeClosure)) )

#define LOOKS_LIKE_STATIC_CLOSURE(r) (((*(((unsigned long *)(r))-1)) == 0) || IS_CHARLIKE_CLOSURE(r) || IS_INTLIKE_CLOSURE(r))
#else
#define LOOKS_LIKE_STATIC(r) IS_DATA_PTR(r)
#define LOOKS_LIKE_STATIC_CLOSURE(r) IS_DATA_PTR(r)
#endif


/* -----------------------------------------------------------------------------
   Macros for distinguishing infotables from closures.
   
   You'd think it'd be easy to tell an info pointer from a closure pointer:
   closures live on the heap and infotables are in read only memory.  Right?
   Wrong!  Static closures live in read only memory and Hugs allocates
   infotables for constructors on the (writable) C heap.

   ToDo: in the combined Hugs-GHC system, the following are but crude
   approximations.  This absolutely has to be fixed.
   -------------------------------------------------------------------------- */

#ifdef INTERPRETER
#ifdef USE_MINIINTERPRETER
/* yoiks: one of the dreaded pointer equality tests */
#define IS_HUGS_CONSTR_INFO(info) (((StgInfoTable *)(info))->entry == (StgFunPtr)&Hugs_CONSTR_entry)
#else
#define IS_HUGS_CONSTR_INFO(info) 0 /* ToDo: more than mildly bogus */
#endif
#else
#define IS_HUGS_CONSTR_INFO(info) 0 /* ToDo: more than mildly bogus */
#endif

#ifdef USE_MINIINTERPRETER
/* in the mininterpreter, we put infotables on closures */
#define LOOKS_LIKE_GHC_INFO(info) IS_CODE_PTR(info)
#else
/* otherwise we have entry pointers on closures */
# ifdef HAVE_WIN32_DLL_SUPPORT
#  define LOOKS_LIKE_GHC_INFO(info) (!HEAP_ALLOCED(info) && !LOOKS_LIKE_STATIC_CLOSURE(info))
# else
#  define LOOKS_LIKE_GHC_INFO(info) IS_CODE_PTR(info)
# endif
#endif

/* -----------------------------------------------------------------------------
   Macros for calculating how big a closure will be (used during allocation)
   -------------------------------------------------------------------------- */

/* ToDo: replace unsigned int by nat.  The only fly in the ointment is that
 * nat comes from Rts.h which many folk dont include.  Sigh!
 */
static __inline__ StgOffset AP_sizeW    ( unsigned int n_args )              
{ return sizeofW(StgAP_UPD) + n_args; }

static __inline__ StgOffset PAP_sizeW   ( unsigned int n_args )              
{ return sizeofW(StgPAP)    + n_args; }

static __inline__ StgOffset CONSTR_sizeW( unsigned int p, unsigned int np )  
{ return sizeofW(StgHeader) + p + np; }

static __inline__ StgOffset BCO_sizeW   ( unsigned int p, unsigned int np, unsigned int is ) 
{ return sizeofW(StgBCO) + p + np + (is+sizeof(StgWord)-1)/sizeof(StgWord); }

static __inline__ StgOffset THUNK_SELECTOR_sizeW ( void )                    
{ return sizeofW(StgHeader) + MIN_UPD_SIZE; }

static __inline__ StgOffset BLACKHOLE_sizeW ( void )                    
{ return sizeofW(StgHeader) + MIN_UPD_SIZE; }

static __inline__ StgOffset CAF_sizeW ( void )                    
{ return sizeofW(StgCAF); }

/* --------------------------------------------------------------------------
 * Sizes of closures
 * ------------------------------------------------------------------------*/

static __inline__ StgOffset size_fromITBL( const StgInfoTable* itbl ) 
{ return sizeof(StgClosure) 
       + sizeof(StgPtr)  * itbl->layout.payload.ptrs 
       + sizeof(StgWord) * itbl->layout.payload.nptrs; }

static __inline__ StgOffset sizeW_fromITBL( const StgInfoTable* itbl ) 
{ return sizeofW(StgClosure) 
       + sizeofW(StgPtr)  * itbl->layout.payload.ptrs 
       + sizeofW(StgWord) * itbl->layout.payload.nptrs; }

static __inline__ StgOffset pap_size( StgPAP* x )
{ return sizeof(StgPAP) 
       + sizeof(StgWord)  * x->n_args; }

static __inline__ StgOffset pap_sizeW( StgPAP* x )
{ return PAP_sizeW(x->n_args); }

/* These two functions give the same result - but have slightly
 * different types. 
 */
static __inline__ StgOffset arr_words_sizeW( StgArrWords* x )
{ return sizeofW(StgArrWords) + x->words; }
static __inline__ StgOffset mut_arr_ptrs_sizeW( StgMutArrPtrs* x )
{ return sizeofW(StgMutArrPtrs) + x->ptrs; }

static __inline__ StgWord bco_sizeW( StgBCO* bco )
{ return BCO_sizeW(bco->n_ptrs,bco->n_words,bco->n_instrs); }

static __inline__ StgWord tso_sizeW ( StgTSO *tso )
{ return TSO_STRUCT_SIZEW + tso->stack_size; }

/* -----------------------------------------------------------------------------
   Macros for building closures
   -------------------------------------------------------------------------- */

#ifdef PROFILING
#define SET_PROF_HDR(c,ccs_) 		(c)->header.prof.ccs = ccs_
#define SET_STATIC_PROF_HDR(ccs_)	prof : { ccs : ccs_ },
#else
#define SET_PROF_HDR(c,ccs)
#define SET_STATIC_PROF_HDR(ccs)
#endif

#ifdef GRAN
#define SET_GRAN_HDR(c,pe)		(c)->header.gran.procs = pe
#define SET_STATIC_GRAN_HDR		gran : { procs : Everywhere },
#else
#define SET_GRAN_HDR(c,pe)
#define SET_STATIC_GRAN_HDR
#endif

/* there is no PAR header, as far as I can tell -- SDM */

#ifdef PAR
#define SET_PAR_HDR(c,stuff)
#define SET_STATIC_PAR_HDR(stuff)
#else
#define SET_PAR_HDR(c,stuff)
#define SET_STATIC_PAR_HDR(stuff)
#endif

#ifdef TICKY_TICKY
#define SET_TICKY_HDR(c,stuff)		/* old: (c)->header.ticky.updated = stuff */
#define SET_STATIC_TICKY_HDR(stuff)	/* old: ticky : { updated : stuff } */
#else
#define SET_TICKY_HDR(c,stuff)
#define SET_STATIC_TICKY_HDR(stuff)
#endif
#define SET_HDR(c,info,ccs) \
   {					\
	SET_INFO(c,info); 	                        \
	SET_GRAN_HDR((StgClosure *)(c),ThisPE);		\
	SET_PAR_HDR((StgClosure *)(c),LOCAL_GA);	\
	SET_PROF_HDR((StgClosure *)(c),ccs);		\
	SET_TICKY_HDR((StgClosure *)(c),0);		\
   }

#define SET_ARR_HDR(c,info,costCentreStack,n_words) \
   SET_HDR(c,info,costCentreStack); \
   (c)->words = n_words;

/* -----------------------------------------------------------------------------
   Static closures are defined as follows:


SET_STATIC_HDR(PrelBase_CZh_closure,PrelBase_CZh_info,costCentreStack,const);

   The info argument must have type 'StgInfoTable' or
   'StgSRTInfoTable', since we use '&' to get its address in the macro.
   -------------------------------------------------------------------------- */

#define SET_STATIC_HDR(label,info,costCentreStack,closure_class,info_class) \
   info_class info;                        \
   closure_class StgClosure label = {                   \
   STATIC_HDR(info,costCentreStack)

#define STATIC_HDR(info,ccs) \
	header : { 			      \
		INIT_INFO(info),              \
		SET_STATIC_GRAN_HDR	      \
		SET_STATIC_PAR_HDR(LOCAL_GA)  \
		SET_STATIC_PROF_HDR(ccs)       \
		SET_STATIC_TICKY_HDR(0)       \
	}

/* how to get hold of the static link field for a static closure.
 *
 * Note that we have to use (*cast(T*,&e)) instead of cast(T,e)
 * because C won't let us take the address of a casted expression. Huh?
 */
#define STATIC_LINK(info,p) \
   (*stgCast(StgClosure**,&((p)->payload[info->layout.payload.ptrs + \
					info->layout.payload.nptrs])))
/* These macros are optimised versions of the above for certain
 * closure types.  They *must* be equivalent to the generic
 * STATIC_LINK.
 */
#define FUN_STATIC_LINK(p)   ((p)->payload[0])
#define THUNK_STATIC_LINK(p) ((p)->payload[2])
#define IND_STATIC_LINK(p)   ((p)->payload[1])

#define STATIC_LINK2(info,p) \
   (*stgCast(StgClosure**,&((p)->payload[info->layout.payload.ptrs + \
					info->layout.payload.nptrs + 1])))

/* -----------------------------------------------------------------------------
   INTLIKE and CHARLIKE closures.
   -------------------------------------------------------------------------- */

#define CHARLIKE_CLOSURE(n) ((P_)&CHARLIKE_closure[n])
#define INTLIKE_CLOSURE(n)  ((P_)&INTLIKE_closure[(n)-MIN_INTLIKE])

/* -----------------------------------------------------------------------------
   Closure Tables (for enumerated data types)
   -------------------------------------------------------------------------- */

#define CLOSURE_TBL(lbl) const StgClosure *lbl[] = {

/* -----------------------------------------------------------------------------
   Payload access
   -------------------------------------------------------------------------- */

#define payloadPtr( c, i )    (*stgCast(StgPtr*,       ((c)->payload+(i))))
#define payloadCPtr( c, i )   (*stgCast(StgClosure**,  ((c)->payload+(i))))
#define payloadWord( c, i )   (*stgCast(StgWord*,      ((c)->payload+(i))))

/* -----------------------------------------------------------------------------
   CONSTRs.
   -------------------------------------------------------------------------- */

/* constructors don't have SRTs */
#define GET_TAG(info) (INFO_PTR_TO_STRUCT(info)->srt_len)

/* -----------------------------------------------------------------------------
   BCOs.
   -------------------------------------------------------------------------- */

#define bcoConstPtr( bco, i )    (*stgCast(StgPtr*,       ((bco)->payload+(i))))
#define bcoConstCPtr( bco, i )   (*stgCast(StgClosurePtr*,((bco)->payload+(i))))
#define bcoConstInfoPtr( bco, i )(*stgCast(StgInfoTable**,((bco)->payload+(bco)->n_ptrs+i)))
#define bcoConstInt( bco, i )    (*stgCast(StgInt*,       ((bco)->payload+(bco)->n_ptrs+i)))
#define bcoConstInt64( bco, i )  (PK_Int64(stgCast(StgWord*,(bco)->payload+(bco)->n_ptrs+i)))
#define bcoConstWord( bco, i )   (*stgCast(StgWord*,      ((bco)->payload+(bco)->n_ptrs+i)))
#define bcoConstAddr( bco, i )   (*stgCast(StgAddr*,      ((bco)->payload+(bco)->n_ptrs+i)))
#define bcoConstChar( bco, i )   (*stgCast(StgChar*,      ((bco)->payload+(bco)->n_ptrs+i)))
#define bcoConstFloat( bco, i )  (PK_FLT(stgCast(StgWord*,(bco)->payload+(bco)->n_ptrs+i)))
#define bcoConstDouble( bco, i ) (PK_DBL(stgCast(StgWord*,(bco)->payload+(bco)->n_ptrs+i)))
#define bcoInstr( bco, i )       (stgCast(StgWord8*,      ((bco)->payload+(bco)->n_ptrs+(bco)->n_words))[i])
static __inline__ StgInt bcoInstr16 ( StgBCO* bco, unsigned int i )
{ StgInt x = (bcoInstr(bco,i) << 8) + bcoInstr(bco,i+1); return x; }

#endif /* CLOSUREMACROS_H */
