/*
  Time-stamp: <Tue Mar 13 2001 19:07:13 Stardate: [-30]6323.98 hwloidl>

  Revertible Black Hole Manipulation.
  Used in GUM and GranSim during the packing of closures. These black holes
  must be revertible because a GC might occur while the packet is being 
  transmitted. In this case all RBHs have to be reverted.
  */

#if defined(PAR) || defined(GRAN) /* whole file */

#include "Rts.h"
#include "RtsFlags.h"
#include "RtsUtils.h"
#include "GranSimRts.h"
#include "ParallelRts.h"
# if defined(DEBUG)
# include "ParallelDebug.h"
# endif
#include "Storage.h"  // for recordMutable
#include "StgMacros.h" // inlined IS_... fcts

/*
   Turn a closure into a revertible black hole.  After the conversion, the
   first two words of the closure (after the fixed header, of course) will
   be a link to the mutables list (if appropriate for the garbage
   collector), and a pointer to the blocking queue.  The blocking queue is
   terminated by a 2-word SPEC closure which holds the original contents of
   the first two words of the closure.  
*/

//@menu
//* Externs and prototypes::	
//* Conversion Functions::	
//* Index::			
//@end menu

//@node Externs and prototypes, Conversion Functions
//@section Externs and prototypes

EXTFUN(stg_RBH_Save_0_info);
EXTFUN(stg_RBH_Save_1_info);
EXTFUN(stg_RBH_Save_2_info);

//@node Conversion Functions, Index, Externs and prototypes
//@section Conversion Functions

/*
  A closure is turned into an RBH upon packing it (see PackClosure in Pack.c).
  This is needed in case we have to do a GC before the packet is turned
  into a graph on the PE receiving the packet. 
*/
//@cindex convertToRBH
StgClosure *
convertToRBH(closure)
StgClosure *closure;
{
  StgRBHSave *rbh_save;
  StgInfoTable *info_ptr, *rbh_info_ptr, *old_info;
  nat size, ptrs, nonptrs, vhs;
  char str[80];

  /*
     Closure layout before this routine runs amuck:
       +-------------------
       |   HEADER   | DATA ...
       +-------------------
       | FIXED_HS   |
  */
  /* 
     Turn closure into an RBH.  This is done by modifying the info_ptr,
     grabbing the info_ptr of the RBH for this closure out of its
     ITBL. Additionally, we have to save the words from the closure, which
     will hold the link to the blocking queue.  For this purpose we use the
     RBH_Save_N closures, with N being the number of pointers for this
     closure.  */
  IF_GRAN_DEBUG(pack,
		belch("*>::   %p (%s): Converting closure into an RBH",
		      closure, info_type(closure))); 
  IF_PAR_DEBUG(pack,
		belch("*>::   %p (%s): Converting closure into an RBH",
		      closure, info_type(closure))); 

  ASSERT(closure_THUNK(closure));

  IF_GRAN_DEBUG(pack,
		old_info = get_itbl(closure));

  /* Allocate a new closure for the holding data ripped out of closure */
  if ((rbh_save = (StgRBHSave *)allocate(_HS + 2)) == NULL)
    return NULL;  /* have to Garbage Collect; check that in the caller! */

  info_ptr = get_closure_info(closure, &size, &ptrs, &nonptrs, &vhs, str);
  ASSERT(size >= _HS+MIN_UPD_SIZE);

  /* Fill in the RBH_Save closure with the original data from closure */
  rbh_save->payload[0] = (StgPtr) ((StgRBH *)closure)->blocking_queue;
  rbh_save->payload[1] = (StgPtr) ((StgRBH *)closure)->mut_link;

  /* Set the info_ptr for the rbh_Save closure according to the number of
     pointers in the original */

  rbh_info_ptr = (StgInfoTable *) (ptrs == 0 ? &stg_RBH_Save_0_info :
				   ptrs == 1 ? &stg_RBH_Save_1_info :
				   &stg_RBH_Save_2_info);
  SET_INFO(rbh_save, rbh_info_ptr);
  /* same bitmask as the original closure */
  SET_GRAN_HDR(rbh_save, PROCS(closure));

  /* Init the blocking queue of the RBH and have it point to the saved data */
  ((StgRBH *)closure)->blocking_queue = (StgBlockingQueueElement *)rbh_save;

  ASSERT(LOOKS_LIKE_GHC_INFO(RBH_INFOPTR(get_itbl(closure))));
  /* Turn the closure into a RBH;  a great system, indeed! */
  SET_INFO(closure, RBH_INFOPTR(get_itbl(closure)));

  /*
    add closure to the mutable list!
    do this after having turned the closure into an RBH, because an
    RBH is mutable but the closure it was before wasn't mutable
  */
  recordMutable((StgMutClosure *)closure);

  //IF_GRAN_DEBUG(pack,
		/* sanity check; make sure that reverting the RBH yields the 
		   orig closure, again */
  //ASSERT(REVERT_INFOPTR(get_itbl(closure))==old_info));

  /*
     Closure layout after this routine has run amuck:
       +---------------------
       | RBH-HEADER | |   |  ...
       +--------------|---|--
       | FIXED_HS   | |   v
                      |   Mutable-list ie another StgMutClosure
		      v
		      +---------
		      | RBH_SAVE with 0-2 words of DATA
		      +---------
  */

  return closure;
}

/*
  An RBH closure is turned into a FETCH_ME when reveiving an ACK message
  indicating that the transferred closure has been unpacked on the other PE
  (see processAck in HLComms.c). The ACK also contains the new GA of the
  closure to which the FETCH_ME closure has to point.

  Converting a closure to a FetchMe is trivial, unless the closure has
  acquired a blocking queue.  If that has happened, we first have to awaken
  the blocking queue.  What a nuisance!  Fortunately, @AwakenBlockingQueue@
  should now know what to do.

  A note on GrAnSim: In GrAnSim we don't have FetchMe closures. However,
  we have to turn a RBH back to its original form when the simulated
  transfer of the closure has been finished. Therefore we need the
  @convertFromRBH@ routine below. After converting the RBH back to its
  original form and awakening all TSOs, the first TSO will reenter the
  closure which is now local and carry on merrily reducing it (the other
  TSO will be less merrily blocked on the now local closure; we're costing
  the difference between local and global blocks in the BQ code).  -- HWL 
*/

# if defined(PAR)

EXTFUN(stg_FETCH_ME_info);

//@cindex convertToFetchMe
void
convertToFetchMe(rbh, ga)
StgRBH *rbh;
globalAddr *ga;
{
  // StgInfoTable *ip = get_itbl(rbh);
  StgBlockingQueueElement *bqe = rbh->blocking_queue;

  ASSERT(get_itbl(rbh)->type==RBH);

  IF_PAR_DEBUG(pack,
	       belch("**:: Converting RBH %p (%s) into a FETCH_ME for GA ((%x, %d, %x))",
		     rbh, info_type(rbh), 
	             ga->payload.gc.gtid, ga->payload.gc.slot, ga->weight)); 

  /* put closure on mutables list, while it is still a RBH */
  recordMutable((StgMutClosure *)rbh);

  /* actually turn it into a FETCH_ME */
  SET_INFO((StgClosure *)rbh, &stg_FETCH_ME_info);

  /* set the global pointer in the FETCH_ME closure to the given value */
  ((StgFetchMe *)rbh)->ga = ga;

  IF_PAR_DEBUG(pack,
	       if (get_itbl(bqe)->type==TSO || get_itbl(bqe)->type==BLOCKED_FETCH)
	         belch("**:: Awakening non-empty BQ of RBH closure %p (first TSO is %d (%p)",
		      rbh, ((StgTSO *)bqe)->id, ((StgTSO *)bqe))); 

  /* awaken all TSOs and BLOCKED_FETCHES on the blocking queue */
  if (get_itbl(bqe)->type==TSO || get_itbl(bqe)->type==BLOCKED_FETCH)
    awakenBlockedQueue(bqe, (StgClosure *)rbh);
}
# else  /* GRAN */
/* Prototype */
// void UnlinkFromMUT(StgPtr closure); 

/*
  This routine in fact reverts the RBH into its original form; this code 
  should be of interest for GUM, too, but is not needed in the current version.
  convertFromRBH is called where GUM uses convertToFetchMe.
*/
void
convertFromRBH(closure)
StgClosure *closure;
{
  StgBlockingQueueElement *bqe = ((StgRBH*)closure)->blocking_queue;
  char str[NODE_STR_LEN]; // debugging only
  StgInfoTable *rip = REVERT_INFOPTR(get_itbl(closure));  // debugging only

  IF_GRAN_DEBUG(pack,
		if (get_itbl(bqe)->type==TSO)
		  sprintf(str, "%d (%p)", 
			  ((StgTSO *)bqe)->id, ((StgTSO *)bqe));
		else 
		  strcpy(str, "empty");
		belch("*<:: Reverting RBH %p (%s) into a ??? closure again; BQ start: %s",
		      closure, info_type(closure), str));

  ASSERT(get_itbl(closure)->type==RBH);

  /* awakenBlockedQueue also restores the RBH_Save closure
     (have to call it even if there are no TSOs in the queue!) */
  awakenBlockedQueue(bqe, closure);

  /* Put back old info pointer (grabbed from the RBH's info table).
     We do that *after* awakening the BQ to be sure node is an RBH when
     calling awakenBlockedQueue (different in GUM!)
  */
  SET_INFO(closure, REVERT_INFOPTR(get_itbl(closure)));

  /* put closure on mutables list */
  recordMutable((StgMutClosure *)closure);

# if 0 /* rest of this fct */
    /* ngoq ngo' */
    /* FETCHME_GA(closure) = ga; */
    if (IS_MUTABLE(INFO_PTR(bqe))) {
      PROC old_proc = CurrentProc,        /* NB: For AwakenBlockingQueue, */
           new_proc = where_is(closure);  /*     CurentProc must be where */
					  /*     closure lives. */
      CurrentProc = new_proc;

#  if defined(GRAN_CHECK)
      if (RTSflags.GranFlags.debug & 0x100)
        fprintf(stderr,"===== AwBQ of node 0x%lx (%s) [PE %2u]\n",
	               closure, (isSpec ? "SPEC_RBH" : "GEN_RBH"), new_proc);
#  endif

      rbh_save = AwakenBlockingQueue(bqe);     /* AwakenBlockingQueue(bqe); */
      CurrentProc = old_proc;
    } else {
        rbh_save = bqe;
    }

    /* Put data from special RBH save closures back into the closure */
    if ( rbh_save == NULL ) {
      fprintf(stderr,"convertFromRBH: No RBH_Save_? closure found at end of BQ!\n");
      EXIT(EXIT_FAILURE);
    } else {
      closure[isSpec ? SPEC_HS : GEN_HS] = rbh_save[SPEC_HS];
      closure[(isSpec ? SPEC_HS : GEN_HS) + 1] = rbh_save[SPEC_HS + 1];
    }
# endif /* 0 */

# if 0 && (defined(GCap) || defined(GCgn))
    /* ngoq ngo' */
    /* If we convert from an RBH in the old generation,
       we have to make sure it goes on the mutables list */

    if(closure <= StorageMgrInfo.OldLim) {
	if (IS_MUTABLE(INFO_PTR(closure)) && MUT_LINK(closure) == MUT_NOT_LINKED) {
	    MUT_LINK(closure) = (StgWord) StorageMgrInfo.OldMutables;
            StorageMgrInfo.OldMutables = closure;
	}
    }
# endif /* 0 */
}
#endif /* PAR */

/* Remove closure from the mutables list */
#if 0
/* ngoq ngo' */
void
UnlinkFromMUT(StgPtr closure) 
{
  StgPtr curr = StorageMgrInfo.OldMutables, prev = NULL;

  while (curr != NULL && curr != closure) {
    ASSERT(MUT_LINK(curr)!=MUT_NOT_LINKED);
    prev=curr;
    curr=MUT_LINK(curr); 
  }
  if (curr==closure) {   
   if (prev==NULL) 
     StorageMgrInfo.OldMutables = MUT_LINK(curr);
   else   
     MUT_LINK(prev) = MUT_LINK(curr);
   MUT_LINK(curr) = MUT_NOT_LINKED;
  }

#  if 0 && (defined(GCap) || defined(GCgn))
  {
    closq newclos;
    extern closq ex_RBH_q;

    newclos = (closq) stgMallocBytes(sizeof(struct clos), "UnlinkFromMUT");
    CLOS_CLOSURE(newclos) = closure;
    CLOS_PREV(newclos) = NULL;
    CLOS_NEXT(newclos) = ex_RBH_q;
    if (ex_RBH_q!=NULL)
      CLOS_PREV(ex_RBH_q) = newclos;
    ex_RBH_q = newclos;
  }
#  endif
}
#endif /* PAR */

#endif /* PAR || GRAN -- whole file */

//@node Index,  , Conversion Functions
//@section Index

//@index
//* convertToFetchMe::  @cindex\s-+convertToFetchMe
//* convertToRBH::  @cindex\s-+convertToRBH
//@end index
