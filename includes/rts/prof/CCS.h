/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2009-2012
 *
 * Macros for profiling operations in STG code
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
 *
 * ---------------------------------------------------------------------------*/

#ifndef RTS_PROF_CCS_H
#define RTS_PROF_CCS_H

// Returns non-zero if the RTS is a profiling version
int rts_isProfiled(void);

/* -----------------------------------------------------------------------------
 * Data Structures 
 * ---------------------------------------------------------------------------*/  
/*
 * Note [struct alignment]
 * NB. be careful to avoid unwanted padding between fields, by
 * putting the 8-byte fields on an 8-byte boundary.  Padding can
 * vary between C compilers, and we don't take into account any
 * possible padding when generating CCS and CC decls in the code
 * generator (compiler/codeGen/StgCmmProf.hs).
 */

typedef struct CostCentre_ {
    StgInt ccID;              // Unique Id, allocated by the RTS

    char * label;
    char * module;
    char * srcloc;

    // used for accumulating costs at the end of the run...
    StgWord64 mem_alloc;      // align 8 (Note [struct alignment])
    StgWord   time_ticks;

    StgInt is_caf;            // non-zero for a CAF cost centre

    struct CostCentre_ *link;
} CostCentre;

typedef struct CostCentreStack_ {
    StgInt ccsID;               // unique ID, allocated by the RTS

    CostCentre *cc;             // Cost centre at the top of the stack

    struct CostCentreStack_ *prevStack;   // parent
    struct IndexTable_      *indexTable;  // children
    struct CostCentreStack_ *root;        // root of stack
    StgWord    depth;           // number of items in the stack

    StgWord64  scc_count;       // Count of times this CCS is entered
                                // align 8 (Note [struct alignment])

    StgWord    selected;        // is this CCS shown in the heap
                                // profile? (zero if excluded via -hc
                                // -hm etc.)

    StgWord    time_ticks;      // number of time ticks accumulated by
                                // this CCS

    StgWord64  mem_alloc;       // mem allocated by this CCS
                                // align 8 (Note [struct alignment])

    StgWord64  inherited_alloc; // sum of mem_alloc over all children
                                // (calculated at the end)
                                // align 8 (Note [struct alignment])

    StgWord    inherited_ticks; // sum of time_ticks over all children
                                // (calculated at the end)
} CostCentreStack;


/* -----------------------------------------------------------------------------
 * Start and stop the profiling timer.  These can be called from
 * Haskell to restrict the profile to portion(s) of the execution.
 * See the module GHC.Profiling.
 * ---------------------------------------------------------------------------*/

void stopProfTimer      ( void );
void startProfTimer     ( void );

/* -----------------------------------------------------------------------------
 * The rest is PROFILING only...
 * ---------------------------------------------------------------------------*/

#if defined(PROFILING)
  
/* -----------------------------------------------------------------------------
 * Constants
 * ---------------------------------------------------------------------------*/

#define EMPTY_STACK NULL
#define EMPTY_TABLE NULL

/* Constants used to set is_caf flag on CostCentres */
#define CC_IS_CAF      'c'            /* 'c'  => *is* a CAF cc           */
#define CC_NOT_CAF     0

/* -----------------------------------------------------------------------------
 * Data Structures
 * ---------------------------------------------------------------------------*/

// IndexTable is the list of children of a CCS. (Alternatively it is a
// cache of the results of pushing onto a CCS, so that the second and
// subsequent times we push a certain CC on a CCS we get the same
// result).

typedef struct IndexTable_ {
    CostCentre *cc;
    CostCentreStack *ccs;
    struct IndexTable_ *next;
    nat back_edge;
} IndexTable;

     
/* -----------------------------------------------------------------------------
   Pre-defined cost centres and cost centre stacks
   -------------------------------------------------------------------------- */

#if IN_STG_CODE

extern StgWord CC_MAIN[];	
extern StgWord CCS_MAIN[];      // Top CCS

extern StgWord CC_SYSTEM[];	
extern StgWord CCS_SYSTEM[];    // RTS costs

extern StgWord CC_GC[];
extern StgWord CCS_GC[];         // Garbage collector costs

extern StgWord CC_OVERHEAD[];
extern StgWord CCS_OVERHEAD[];   // Profiling overhead

extern StgWord CC_DONT_CARE[];
extern StgWord CCS_DONT_CARE[];  // CCS attached to static constructors

#else

extern CostCentre      CC_MAIN[];	
extern CostCentreStack CCS_MAIN[];      // Top CCS

extern CostCentre      CC_SYSTEM[];	
extern CostCentreStack CCS_SYSTEM[];    // RTS costs

extern CostCentre      CC_GC[];
extern CostCentreStack CCS_GC[];         // Garbage collector costs

extern CostCentre      CC_OVERHEAD[];
extern CostCentreStack CCS_OVERHEAD[];   // Profiling overhead

extern CostCentre      CC_DONT_CARE[];
extern CostCentreStack CCS_DONT_CARE[];  // shouldn't ever get set

extern CostCentre      CC_PINNED[];
extern CostCentreStack CCS_PINNED[];     // pinned memory

extern CostCentre      CC_IDLE[];
extern CostCentreStack CCS_IDLE[];       // capability is idle

#endif /* IN_STG_CODE */

extern unsigned int RTS_VAR(CC_ID);     // global ids
extern unsigned int RTS_VAR(CCS_ID);

extern unsigned int RTS_VAR(era);

/* -----------------------------------------------------------------------------
 * Functions 
 * ---------------------------------------------------------------------------*/

CostCentreStack * pushCostCentre (CostCentreStack *, CostCentre *);
void              enterFunCCS    (StgRegTable *reg, CostCentreStack *);

/* -----------------------------------------------------------------------------
   Registering CCs and CCSs
 
   Registering a CC or CCS consists of
     - assigning it a unique ID
     - linking it onto the list of registered CCs/CCSs

   Cost centres are registered at startup by a C constructor function
   generated by the compiler in the _stub.c file for each module.  The
   macros below are invoked by that C code to register CCs and CCSs.
 -------------------------------------------------------------------------- */

extern CostCentre * RTS_VAR(CC_LIST);               // registered CC list
extern CostCentreStack * RTS_VAR(CCS_LIST);         // registered CCS list

#define REGISTER_CC(cc)					\
	do {						\
	if ((cc)->link == (CostCentre *)0) {		\
	    (cc)->link = CC_LIST;			\
	    CC_LIST = (cc);				\
	    (cc)->ccID = CC_ID++; 			\
	}} while(0)

#define REGISTER_CCS(ccs)				\
	do {						\
        if ((ccs)->prevStack == (CostCentreStack *)0) {	\
	  (ccs)->prevStack = CCS_LIST;			\
	  CCS_LIST = (ccs);				\
	  (ccs)->ccsID = CCS_ID++;			\
	}} while(0)

/* -----------------------------------------------------------------------------
 * Declaring Cost Centres & Cost Centre Stacks.
 * -------------------------------------------------------------------------- */

# define CC_DECLARE(cc_ident,name,mod,loc,caf,is_local) \
     is_local CostCentre cc_ident[1]                    \
       = {{ ccID       : 0,                             \
            label      : name,                          \
            module     : mod,                           \
            srcloc     : loc,                           \
            time_ticks : 0,                             \
            mem_alloc  : 0,                             \
            link       : 0,                             \
            is_caf     : caf                            \
         }};

# define CCS_DECLARE(ccs_ident,cc_ident,is_local)       \
     is_local CostCentreStack ccs_ident[1]              \
       = {{ ccsID 		: 0,                    \
	    cc 			: cc_ident,             \
	    prevStack 		: NULL,                 \
	    indexTable 		: NULL,                 \
            root                : NULL,                 \
            depth               : 0,                    \
            selected            : 0,                    \
	    scc_count 		: 0,                    \
	    time_ticks 		: 0,                    \
	    mem_alloc 		: 0,                    \
	    inherited_ticks 	: 0,                    \
            inherited_alloc     : 0                     \
       }};

/* -----------------------------------------------------------------------------
 * Time / Allocation Macros
 * ---------------------------------------------------------------------------*/

/* eliminate profiling overhead from allocation costs */
#define CCS_ALLOC(ccs, size) (ccs)->mem_alloc += ((size)-sizeofW(StgProfHeader))

#else /* !PROFILING */

#define CCS_ALLOC(ccs, amount) doNothing()
 
#endif /* PROFILING */

#endif /* RTS_PROF_CCS_H */

