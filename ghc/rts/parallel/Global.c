/* ---------------------------------------------------------------------------
   Time-stamp: <Wed Mar 21 2001 16:32:23 Stardate: [-30]6363.44 hwloidl>

   (c) The AQUA/Parade Projects, Glasgow University, 1995
       The GdH/APART 624 Projects, Heriot-Watt University, Edinburgh, 1999

   Global Address Manipulation.
   
   The GALA and LAGA tables for mapping global addresses to local addresses 
   (i.e. heap pointers) are defined here. We use the generic hash tables
   defined in Hash.c.
   ------------------------------------------------------------------------- */

#ifdef PAR /* whole file */

//@menu
//* Includes::			
//* Global tables and lists::	
//* Fcts on GALA tables::	
//* Interface to taskId-PE table::  
//* Interface to LAGA table::	
//* Interface to GALA table::	
//* GC functions for GALA tables::  
//* Index::			
//@end menu
//*/

//@node Includes, Global tables and lists, Global Address Manipulation, Global Address Manipulation
//@subsection Includes

#include "Rts.h"
#include "RtsFlags.h"
#include "RtsUtils.h"
#include "Storage.h"
#include "Hash.h"
#include "HLC.h"
#include "ParallelRts.h"
#if defined(DEBUG)
# include "Sanity.h"
#include "ParallelDebug.h"
#endif
#if defined(DIST)
# include "Dist.h"
#endif

/*
  @globalAddr@ structures are allocated in chunks to reduce malloc overhead.
*/

//@node Global tables and lists, Fcts on GALA tables, Includes, Global Address Manipulation
//@subsection Global tables and lists

//@cindex thisPE
nat thisPE;

//@menu
//* Free lists::		
//* Hash tables::		
//@end menu

//@node Free lists, Hash tables, Global tables and lists, Global tables and lists
//@subsubsection Free lists

/* Free list of GALA entries */
GALA *freeGALAList = NULL;

/* Number of globalAddr cells to allocate in one go */
#define GCHUNK	    (1024 * sizeof(StgWord) / sizeof(GALA))

/* Free list of indirections */

//@cindex nextIndirection
static StgInt nextIndirection = 0;
//@cindex freeIndirections
GALA *freeIndirections = NULL;

/* The list of live indirections has to be marked for GC (see makeGlobal) */
//@cindex liveIndirections
GALA *liveIndirections = NULL;

/* The list of remote indirections has to be marked for GC (see setRemoteGA) */
//@cindex liveRemoteGAs
GALA *liveRemoteGAs = NULL;

//@node Hash tables,  , Free lists, Global tables and lists
//@subsubsection Hash tables

/* Mapping global task ids PEs */
//@cindex taskIDtoPEtable
HashTable *taskIDtoPEtable = NULL;

static int nextPE = 0;

/* LAGA table: StgClosure* -> globalAddr*
               (Remember: globalAddr = (GlobalTaskId, Slot, Weight))
   Mapping local to global addresses (see interface below) 
*/

//@cindex LAtoGALAtable
HashTable *LAtoGALAtable = NULL;

/* GALA table: globalAddr* -> StgClosure*
               (Remember: globalAddr = (GlobalTaskId, Slot, Weight))
   Mapping global to local addresses (see interface below) 
*/

//@cindex pGAtoGALAtable
HashTable *pGAtoGALAtable = NULL;

//@node Fcts on GALA tables, Interface to taskId-PE table, Global tables and lists, Global Address Manipulation
//@subsection Fcts on GALA tables

//@cindex allocGALA
static GALA *
allocGALA(void)
{
  GALA *gl, *p;

  if ((gl = freeGALAList) != NULL) {
    IF_DEBUG(sanity,
	     ASSERT(gl->ga.weight==0xdead0add);
             ASSERT(gl->la==(StgPtr)0xdead00aa));
    freeGALAList = gl->next;
  } else {
    gl = (GALA *) stgMallocBytes(GCHUNK * sizeof(GALA), "allocGALA");

    freeGALAList = gl + 1;
    for (p = freeGALAList; p < gl + GCHUNK - 1; p++) {
      p->next = p + 1;
      IF_DEBUG(sanity,
	       p->ga.weight=0xdead0add;
               p->la=(StgPtr)0xdead00aa);
    }
    /* last elem in the new block has NULL pointer in link field */
    p->next = NULL;
    IF_DEBUG(sanity,
	     p->ga.weight=0xdead0add;
	     p->la=(StgPtr)0xdead00aa);
  }
  IF_DEBUG(sanity,
	   gl->ga.weight=0xdead0add;
           gl->la=(StgPtr)0xdead00aa);
  return gl;
}

//@node Interface to taskId-PE table, Interface to LAGA table, Fcts on GALA tables, Global Address Manipulation
//@subsection Interface to taskId-PE table

/*
  We don't really like GLOBAL_TASK_ID, so we keep a table of TASK_ID to
  PE mappings.  The idea is that a PE identifier will fit in 16 bits, whereas 
  a TASK_ID may not.
*/

//@cindex taskIDtoPE
PEs
taskIDtoPE(GlobalTaskId gtid)
{
  return ((PEs) lookupHashTable(taskIDtoPEtable, gtid));
}

//@cindex registerTask
void 
registerTask(GlobalTaskId gtid) { 
  nextPE++;               //start counting from 1
  if (gtid == mytid)
    thisPE = nextPE;

  insertHashTable(taskIDtoPEtable, gtid, (void *) (StgWord) nextPE);
}

//@node Interface to LAGA table, Interface to GALA table, Interface to taskId-PE table, Global Address Manipulation
//@subsection Interface to LAGA table

/*
  The local address to global address mapping returns a globalAddr structure
  (pe task id, slot, weight) for any closure in the local heap which has a
  global identity.  Such closures may be copies of normal form objects with
  a remote `master' location, @FetchMe@ nodes referencing remote objects, or
  globally visible objects in the local heap (for which we are the master).
*/

//@cindex LAGAlookup
globalAddr *
LAGAlookup(addr)
StgClosure *addr;
{
  GALA *gala;

  /* We never look for GA's on indirections. -- unknown hacker
     Well, in fact at the moment we do in the new RTS. -- HWL
     ToDo: unwind INDs when entering them into the hash table

  ASSERT(IS_INDIRECTION(addr) == NULL);
  */
  if ((gala = lookupHashTable(LAtoGALAtable, (StgWord) addr)) == NULL)
    return NULL;
  else
    return &(gala->ga);
}

//@node Interface to GALA table, GC functions for GALA tables, Interface to LAGA table, Global Address Manipulation
//@subsection Interface to GALA table

/*
  We also manage a mapping of global addresses to local addresses, so that
  we can ``common up'' multiple references to the same object as they arrive
  in data packets from remote PEs.

  The global address to local address mapping is actually managed via a
  ``packed global address'' to GALA hash table.  The packed global
  address takes the interesting part of the @globalAddr@ structure
  (i.e. the pe and slot fields) and packs them into a single word
  suitable for hashing.
*/

//@cindex GALAlookup
StgClosure *
GALAlookup(ga)
globalAddr *ga;
{
  StgWord pga = PackGA(taskIDtoPE(ga->payload.gc.gtid), ga->payload.gc.slot);
  GALA *gala;

  if ((gala = (GALA *) lookupHashTable(pGAtoGALAtable, pga)) == NULL)
    return NULL;
  else {
    /* 
     * Bypass any indirections when returning a local closure to
     * the caller.  Note that we do not short-circuit the entry in
     * the GALA tables right now, because we would have to do a
     * hash table delete and insert in the LAtoGALAtable to keep
     * that table up-to-date for preferred GALA pairs.  That's
     * probably a bit expensive.
     */
    return UNWIND_IND((StgClosure *)(gala->la));
  }
}

/* ga becomes non-preferred (e.g. due to CommonUp) */
void
GALAdeprecate(ga)
globalAddr *ga;
{
  StgWord pga = PackGA(taskIDtoPE(ga->payload.gc.gtid), ga->payload.gc.slot);
  GALA *gala;

  gala = (GALA *) lookupHashTable(pGAtoGALAtable, pga);
  ASSERT(gala!=NULL);
  ASSERT(gala->preferred==rtsTrue);
  gala->preferred = rtsFalse;
}

/*
  External references to our globally-visible closures are managed through an
  indirection table.  The idea is that the closure may move about as the result
  of local garbage collections, but its global identity is determined by its
  slot in the indirection table, which never changes.

  The indirection table is maintained implicitly as part of the global
  address to local address table.  We need only keep track of the
  highest numbered indirection index allocated so far, along with a free
  list of lower numbered indices no longer in use.
*/

/* 
   Allocate an indirection slot for the closure currently at address @addr@.
*/

//@cindex allocIndirection
static GALA *
allocIndirection(StgClosure *closure)
{
  GALA *gala;
  
  if ((gala = freeIndirections) != NULL) {
    IF_DEBUG(sanity,
	     ASSERT(gala->ga.weight==0xdead0add);
             ASSERT(gala->la==(StgPtr)0xdead00aa));
    freeIndirections = gala->next;
  } else {
    gala = allocGALA();
    IF_DEBUG(sanity,
	     ASSERT(gala->ga.weight==0xdead0add);
             ASSERT(gala->la==(StgPtr)0xdead00aa));
    gala->ga.payload.gc.gtid = mytid;
    gala->ga.payload.gc.slot = nextIndirection++;
    IF_DEBUG(sanity,
	     if (nextIndirection>=MAX_SLOTS)
	       barf("Cannot handle more than %d slots for GA in a sanity-checking setup (this is no error)"));
  }
  gala->ga.weight = MAX_GA_WEIGHT;
  gala->la = (StgPtr)closure;
  IF_DEBUG(sanity,
	   gala->next=(struct gala *)0xcccccccc);
  return gala;
}

/* 
   This is only used for sanity checking (see LOOKS_LIKE_SLOT)
*/
StgInt
highest_slot (void) { return nextIndirection; }

/*
  Make a local closure globally visible.  

  Called from: GlobaliseAndPackGA
  Args: 
   closure ... closure to be made visible
   preferred ... should the new GA become the preferred one (normalle=y true)

  Allocate a GALA structure and add it to the (logical) Indirections table,
  by inserting it into the LAtoGALAtable hash table and putting it onto the
  liveIndirections list (only if it is preferred).
   
  We have to allocate an indirection slot for it, and update both the local
  address to global address and global address to local address maps.  
*/

//@cindex makeGlobal
globalAddr *
makeGlobal(closure, preferred)
StgClosure *closure;
rtsBool preferred;
{
  /* check whether we already have a GA for this local closure */
  GALA *oldGALA = lookupHashTable(LAtoGALAtable, (StgWord) closure);
  /* create an entry in the LAGA table */
  GALA *newGALA = allocIndirection(closure);
  StgWord pga = PackGA(thisPE, newGALA->ga.payload.gc.slot);

  IF_DEBUG(sanity,
	   ASSERT(newGALA->next==(struct gala *)0xcccccccc););
  // ASSERT(HEAP_ALLOCED(closure)); // check that closure might point into the heap; might be static, though
  ASSERT(GALAlookup(&(newGALA->ga)) == NULL);
  
  /* global statistics gathering */
  if (RtsFlags.ParFlags.ParStats.Global &&
      RtsFlags.GcFlags.giveStats > NO_GC_STATS) {
    globalParStats.local_alloc_GA++;
  }

  newGALA->la = (StgPtr)closure;
  newGALA->preferred = preferred;

  if (preferred) {
    /* The new GA is now the preferred GA for the LA */
    if (oldGALA != NULL) {
      oldGALA->preferred = rtsFalse;
      (void) removeHashTable(LAtoGALAtable, (StgWord) closure, (void *) oldGALA);
    }
    insertHashTable(LAtoGALAtable, (StgWord) closure, (void *) newGALA);
  }

  ASSERT(!isOnLiveIndTable(&(newGALA->ga)));
  /* put the new GALA entry on the list of live indirections */
  newGALA->next = liveIndirections;
  liveIndirections = newGALA;
  
  insertHashTable(pGAtoGALAtable, pga, (void *) newGALA);
  
  return &(newGALA->ga);
}

/*
  Assign an existing remote global address to an existing closure.

  Called from: Unpack in Pack.c
  Args:
   local_closure ... a closure that has just been unpacked 
   remote_ga ... the GA that came with it, ie. the name under which the 
                 closure is known while being transferred
   preferred ... should the new GA become the preferred one (normalle=y true)

  Allocate a GALA structure and add it to the (logical) RemoteGA table,
  by inserting it into the LAtoGALAtable hash table and putting it onto the
  liveRemoteGAs list (only if it is preferred).

  We do not retain the @globalAddr@ structure that's passed in as an argument,
  so it can be a static in the calling routine.
*/

//@cindex setRemoteGA
globalAddr *
setRemoteGA(local_closure, remote_ga, preferred)
StgClosure *local_closure;
globalAddr *remote_ga;
rtsBool preferred;
{
  /* old entry ie the one with the GA generated when sending off the closure */
  GALA *oldGALA = lookupHashTable(LAtoGALAtable, (StgWord) local_closure);
  /* alloc new entry and fill it with contents of the newly arrives GA */
  GALA *newGALA = allocGALA();
  StgWord pga = PackGA(taskIDtoPE(remote_ga->payload.gc.gtid), 
		       remote_ga->payload.gc.slot);

  ASSERT(remote_ga->payload.gc.gtid != mytid);
  ASSERT(remote_ga->weight > 0);
  ASSERT(GALAlookup(remote_ga) == NULL);

  newGALA->ga = *remote_ga;
  newGALA->la = (StgPtr)local_closure;
  newGALA->preferred = preferred;

  if (preferred) {
    /* The new GA is now the preferred GA for the LA */
    if (oldGALA != NULL) {
      oldGALA->preferred = rtsFalse;
      (void) removeHashTable(LAtoGALAtable, (StgWord) local_closure, (void *) oldGALA);
    }
    insertHashTable(LAtoGALAtable, (StgWord) local_closure, (void *) newGALA);
  }

  ASSERT(!isOnRemoteGATable(&(newGALA->ga)));
  /* add new entry to the (logical) RemoteGA table */
  newGALA->next = liveRemoteGAs;
  liveRemoteGAs = newGALA;
  
  insertHashTable(pGAtoGALAtable, pga, (void *) newGALA);
  
  /*
    The weight carried by the incoming closure is transferred to the newGALA
    entry (via the structure assign above). Therefore, we have to give back
    the weight to the GA on the other processor, because that indirection is
    no longer needed. 
  */
  remote_ga->weight = 0;
  return &(newGALA->ga);
}

/*
  Give me a bit of weight to give away on a new reference to a particular
  global address.  If we run down to nothing, we have to assign a new GA.  
*/

//@cindex splitWeight
#if 0
void
splitWeight(to, from)
globalAddr *to, *from;
{
  /* Make sure we have enough weight to split */
  if (from->weight!=MAX_GA_WEIGHT && from->weight<=3)  // fixed by UK in Eden implementation
    from = makeGlobal(GALAlookup(from), rtsTrue);
  
  to->payload = from->payload;

  if (from->weight == MAX_GA_WEIGHT)
    to->weight = 1L << (BITS_IN(unsigned) - 1);
  else
    to->weight = from->weight / 2;

  from->weight -= to->weight;
}
#else
void
splitWeight(to, from)
globalAddr *to, *from;
{
  /* Make sure we have enough weight to split */
  /* Splitting at 2 needed, as weight 1 is not legal in packets (UK+KH) */
  
  if (from->weight / 2 <= 2) /* old: weight== 1 (UK) */
      from = makeGlobal(GALAlookup(from), rtsTrue);
  
  to->payload = from->payload;
  
  if (from->weight <= 1) /* old == 0 (UK) */
      to->weight = 1L << (BITS_IN(unsigned) - 1);
  else
      to->weight = from->weight / 2;
  
  from->weight -= to->weight;
}
#endif
/*
  Here, I am returning a bit of weight that a remote PE no longer needs.
*/

//@cindex addWeight
globalAddr *
addWeight(ga)
globalAddr *ga;
{
  StgWord pga;
  GALA *gala;

  ASSERT(LOOKS_LIKE_GA(ga));

  pga = PackGA(taskIDtoPE(ga->payload.gc.gtid), ga->payload.gc.slot);
  gala = (GALA *) lookupHashTable(pGAtoGALAtable, pga);

  IF_PAR_DEBUG(weight,
	       fprintf(stderr, "@* Adding weight %x to ", ga->weight);
	       printGA(&(gala->ga));
	       fputc('\n', stderr));

  gala->ga.weight += ga->weight;    
  ga->weight = 0;

  return &(gala->ga);
}

/*
  Initialize all of the global address structures: the task ID to PE id
  map, the local address to global address map, the global address to
  local address map, and the indirection table.
*/

//@cindex initGAtables
void
initGAtables(void)
{
  taskIDtoPEtable = allocHashTable();
  LAtoGALAtable = allocHashTable();
  pGAtoGALAtable = allocHashTable();
}

//@cindex PackGA
StgWord
PackGA (pe, slot)
StgWord pe;
int slot;
{
  int pe_shift = (BITS_IN(StgWord)*3)/4;
  int pe_bits  = BITS_IN(StgWord) - pe_shift;

  if ( pe_bits < 8 || slot >= (1L << pe_shift) ) { /* big trouble */
    fflush(stdout);
    fprintf(stderr, "PackGA: slot# too big (%d) or not enough pe_bits (%d)\n",
	    slot,pe_bits);
    stg_exit(EXIT_FAILURE);
  }

  return((((StgWord)(pe)) << pe_shift) | ((StgWord)(slot)));
	
    /* the idea is to use 3/4 of the bits (e.g., 24) for indirection-
       table "slot", and 1/4 for the pe# (e.g., 8).
       
       We check for too many bits in "slot", and double-check (at
       compile-time?) that we have enough bits for "pe".  We *don't*
       check for too many bits in "pe", because SysMan enforces a
       MAX_PEs limit at the very very beginning.

       Phil & Will 95/08
    */
}

//@node GC functions for GALA tables, Debugging routines, Interface to GALA table, Global Address Manipulation
//@subsection GC functions for GALA tables

/*
  When we do a copying collection, we want to evacuate all of the local
  entries in the GALA table for which there are outstanding remote
  pointers (i.e. for which the weight is not MAX_GA_WEIGHT.)
  This routine has to be run BEFORE doing the GC proper (it's a 
  ``mark roots'' thing).
*/
//@cindex markLocalGAs
void
markLocalGAs(rtsBool full)
{
  GALA *gala, *next, *prev = NULL;
  StgPtr old_la, new_la;
  nat n=0, m=0; // debugging only
  double start_time_GA; // stats only

  IF_PAR_DEBUG(tables,
	   belch("@@%%%% markLocalGAs (full=%d): Marking LIVE INDIRECTIONS in GALA table starting with GALA at %p\n",
		 full, liveIndirections);
	   printLAGAtable());

  PAR_TICKY_MARK_LOCAL_GAS_START();

  for (gala = liveIndirections, m=0; gala != NULL; gala = next, m++) {
    IF_PAR_DEBUG(tables,
		 fputs("@@ ",stderr);
		 printGA(&(gala->ga));
		 fprintf(stderr, ";@ %d: LA: %p (%s) ",
			 m, (void*)gala->la, info_type((StgClosure*)gala->la)));
    next = gala->next;
    old_la = gala->la;
    ASSERT(gala->ga.payload.gc.gtid == mytid); /* it's supposed to be local */
    if (gala->ga.weight != MAX_GA_WEIGHT) {
      /* Remote references exist, so we must evacuate the local closure */
      if (get_itbl((StgClosure *)old_la)->type == EVACUATED) {
	/* somebody else already evacuated this closure */
	new_la = (StgPtr)((StgEvacuated *)old_la)->evacuee;
	IF_PAR_DEBUG(tables,
		 belch(" already evacuated to %p", new_la));
      } else {
#if 1
	/* unwind any indirections we find */
	StgClosure *foo = UNWIND_IND((StgClosure *)old_la) ; // debugging only
	//ASSERT(HEAP_ALLOCED(foo));
	n++;

	new_la = (StgPtr) MarkRoot(foo);
	IF_PAR_DEBUG(tables,
		     belch(" evacuated %p to %p", foo, new_la));
	/* ToDo: is this the right assertion to check that new_la is in to-space?
	ASSERT(!HEAP_ALLOCED(new_la) || Bdescr(new_la)->evacuated);
	*/
#else
	new_la = MarkRoot(old_la); // or just evacuate(old_ga)
	IF_PAR_DEBUG(tables,
		     belch(" evacuated %p to %p", old_la, new_la));
#endif
      }

      gala->la = new_la;
      /* remove old LA and replace with new LA */
      if (/* !full && */ gala->preferred && new_la != old_la) {
	GALA *q;
	ASSERT(lookupHashTable(LAtoGALAtable, (StgWord)old_la));
	(void) removeHashTable(LAtoGALAtable, (StgWord) old_la, (void *) gala);
	if ((q = lookupHashTable(LAtoGALAtable, (StgWord) new_la))!=NULL) {
	  if (q->preferred && gala->preferred) {
	    q->preferred = rtsFalse;
	    IF_PAR_DEBUG(tables,
			 fprintf(stderr, "@@## found hash entry for closure %p (%s): deprecated GA ",
			   new_la, info_type((StgClosure*)new_la));
			 printGA(&(q->ga));
			 fputc('\n', stderr)); 
	  }
	} else {
	  insertHashTable(LAtoGALAtable, (StgWord) new_la, (void *) gala);
	}
	IF_PAR_DEBUG(tables,
		 belch("__## Hash table update (%p --> %p): ",
		       old_la, new_la));
      }

      gala->next = prev;
      prev = gala;
    } else if(LOOKS_LIKE_STATIC_CLOSURE(gala->la)) {
      /* to handle the CAFs, is this all?*/
      MarkRoot(gala->la);
      IF_PAR_DEBUG(tables,
		   belch(" processed static closure"));
      n++;
      gala->next = prev;
      prev = gala;   
    } else {
      /* Since we have all of the weight, this GA is no longer needed */
      StgWord pga = PackGA(thisPE, gala->ga.payload.gc.slot);
      
      IF_PAR_DEBUG(free,
		   belch("@@!! Freeing slot %d", 
			 gala->ga.payload.gc.slot));
      /* put gala on free indirections list */
      gala->next = freeIndirections;
      freeIndirections = gala;
      (void) removeHashTable(pGAtoGALAtable, pga, (void *) gala);
      if (/* !full && */ gala->preferred)
	(void) removeHashTable(LAtoGALAtable, (W_) gala->la, (void *) gala);

      IF_DEBUG(sanity,
	       gala->ga.weight = 0xdead0add;
	       gala->la = (StgPtr) 0xdead00aa);
    }
  } /* for gala ... */
  liveIndirections = prev;  /* list has been reversed during the marking */


  PAR_TICKY_MARK_LOCAL_GAS_END(n);

  IF_PAR_DEBUG(tables,
	       belch("@@%%%% markLocalGAs: %d of %d GALAs marked on PE %x",
		     n, m, mytid));
}

/*
  Traverse the GALA table: for every live remote GA check whether it has been
  touched during GC; if not it is not needed locally and we can free the 
  closure (i.e. let go of its heap space and send a free message to the
  PE holding its GA).
  This routine has to be run AFTER doing the GC proper.
*/
void
rebuildGAtables(rtsBool full)
{
  GALA *gala, *next, *prev;
  StgClosure *closure;
  nat n = 0, size_GA = 0; // stats only (no. of GAs, and their heap size in bytes)

  IF_PAR_DEBUG(tables,
	   belch("@@%%%% rebuildGAtables (full=%d): rebuilding LIVE REMOTE GAs in GALA table starting with GALA at %p\n",
		 full, liveRemoteGAs));

  PAR_TICKY_REBUILD_GA_TABLES_START();

  prepareFreeMsgBuffers();

  for (gala = liveRemoteGAs, prev = NULL; gala != NULL; gala = next) {
    IF_PAR_DEBUG(tables,
		 printGA(&(gala->ga)));
    next = gala->next;
    ASSERT(gala->ga.payload.gc.gtid != mytid); /* it's supposed to be remote */

    closure = (StgClosure *) (gala->la);
    IF_PAR_DEBUG(tables,
		 fprintf(stderr, " %p (%s) ",
			 (StgClosure *)closure, info_type(closure)));

    if (/* !full && */ gala->preferred)
      (void) removeHashTable(LAtoGALAtable, (StgWord) gala->la, (void *) gala);

    /* Follow indirection chains to the end, just in case */
    // should conform with unwinding in markLocalGAs
    closure = UNWIND_IND(closure);

    /*
       If closure has been evacuated it is live; otherwise it's dead and we
       can nuke the GA attached to it in the LAGA table.
       This approach also drops global aliases for PLCs.
    */

    //ASSERT(!HEAP_ALLOCED(closure) || !(Bdescr((StgPtr)closure)->evacuated));
    if (get_itbl(closure)->type == EVACUATED) {
      closure = ((StgEvacuated *)closure)->evacuee;
      IF_PAR_DEBUG(tables,
		   fprintf(stderr, " EVAC %p (%s)\n",
			   closure, info_type(closure)));
    } else {
      /* closure is not alive any more, thus remove GA and send free msg */
      int pe = taskIDtoPE(gala->ga.payload.gc.gtid);
      StgWord pga = PackGA(pe, gala->ga.payload.gc.slot);

      /* check that the block containing this closure is not in to-space */
      IF_PAR_DEBUG(tables,
		   fprintf(stderr, " !EVAC %p (%s); sending free to PE %d\n",
			   closure, info_type(closure), pe));

      (void) removeHashTable(pGAtoGALAtable, pga, (void *) gala);
      freeRemoteGA(pe-1, &(gala->ga)); //-1 cause ids start at 1... not 0
      gala->next = freeGALAList;
      freeGALAList = gala;
      IF_DEBUG(sanity,
	       gala->ga.weight = 0xdead0add;
	       gala->la = (StgPtr)0xdead00aa);
      continue;
    }
    gala->la = (StgPtr)closure;
    if (/* !full && */ gala->preferred) {
      GALA *q;
      if ((q = lookupHashTable(LAtoGALAtable, (StgWord) gala->la))!=NULL) {
	if (q->preferred && gala->preferred) {
	    q->preferred = rtsFalse;
	    IF_PAR_DEBUG(tables,
			 fprintf(stderr, "@@## found hash entry for closure %p (%s): deprecated GA ",
			   gala->la, info_type((StgClosure*)gala->la));
			 printGA(&(q->ga));
			 fputc('\n', stderr)); 
	}
      } else {
	insertHashTable(LAtoGALAtable, (StgWord) gala->la, (void *) gala);
      }
    }
    gala->next = prev;
    prev = gala;
    /* Global statistics: count GAs and total size
    if (RtsFlags.ParFlags.ParStats.Global &&
	RtsFlags.GcFlags.giveStats > NO_GC_STATS) {
      StgInfoTable *info;
      nat size, ptrs, nonptrs, vhs, i;
      char str[80];

      info = get_closure_info(closure, &size, &ptrs, &nonptrs, &vhs, str);

      size_GA += size ;
      n++; // stats: count number of GAs we add to the new table
    }
    */
  }
  liveRemoteGAs = prev; /* list is reversed during marking */

  /* If we have any remaining FREE messages to send off, do so now */
  sendFreeMessages();

  PAR_TICKY_CNT_FREE_GA();

  IF_DEBUG(sanity,
	   checkFreeGALAList();
	   checkFreeIndirectionsList());

  rebuildLAGAtable();

#if defined(PAR_TICKY)
  getLAGAtableSize(&n, &size_GA);        // determine no of GAs and global heap
  PAR_TICKY_REBUILD_GA_TABLES_END(n, size_GA); // record these values
#endif

  IF_PAR_DEBUG(tables,
	   belch("@#%%%% rebuildGAtables: After ReBuilding GALA table starting with GALA at %p",
		 liveRemoteGAs);
	   printLAGAtable());
}

/*
  Rebuild the LA->GA table, assuming that the addresses in the GALAs are
  correct.  
  A word on the lookupHashTable check in both loops:
  After GC we may end up with 2 preferred GAs for the same LA! For example,
  if we received a closure whose GA already exists on this PE we CommonUp
  both closures, making one an indirection to the other. Before GC everything
  is fine: one preferred GA refers to the IND, the other preferred GA refers
  to the closure it points to. After GC, however, we have short cutted the 
  IND and suddenly we have 2 preferred GAs for the same closure. We detect
  this case in the loop below and deprecate one GA, so that we always just
  have one preferred GA per LA.
*/

//@cindex rebuildLAGAtable
void
rebuildLAGAtable(void)
{
  GALA *gala;
  nat n=0, m=0; // debugging

  /* The old LA->GA table is worthless */
  freeHashTable(LAtoGALAtable, NULL);
  LAtoGALAtable = allocHashTable();

  IF_PAR_DEBUG(tables,
	   belch("@@%%%% rebuildLAGAtable: new LAGA table at %p",
		 LAtoGALAtable)); 
  
  for (gala = liveIndirections; gala != NULL; gala = gala->next) {
    n++;
    if (gala->preferred) {
      GALA *q;
      if ((q = lookupHashTable(LAtoGALAtable, (StgWord) gala->la))!=NULL) {
	if (q->preferred && gala->preferred) {
	  /* this deprecates q (see also GALAdeprecate) */
	  q->preferred = rtsFalse;
	  (void) removeHashTable(LAtoGALAtable, (StgWord) gala->la, (void *)q);
	  IF_PAR_DEBUG(tables,
		       fprintf(stderr, "@@## found hash entry for closure %p (%s): deprecated GA ",
			       gala->la, info_type((StgClosure*)gala->la));
		       printGA(&(q->ga));
		       fputc('\n', stderr)); 
	}
      }
      insertHashTable(LAtoGALAtable, (StgWord) gala->la, (void *) gala);
    }
  }

  for (gala = liveRemoteGAs; gala != NULL; gala = gala->next) {
    m++;
    if (gala->preferred) {
      GALA *q;
      if ((q = lookupHashTable(LAtoGALAtable, (StgWord) gala->la))!=NULL) {
	if (q->preferred && gala->preferred) {
	  /* this deprecates q (see also GALAdeprecate) */
	  q->preferred = rtsFalse;
	  (void) removeHashTable(LAtoGALAtable, (StgWord) gala->la, (void *)q);
	  IF_PAR_DEBUG(tables,
		       fprintf(stderr, "@@## found hash entry for closure %p (%s): deprecated GA ",
			       (StgClosure*)gala->la, info_type((StgClosure*)gala->la));
		       printGA(&(q->ga));
		       fputc('\n', stderr)); 
	}
      }
      insertHashTable(LAtoGALAtable, (StgWord) gala->la, (void *) gala);
    }
  }

  IF_PAR_DEBUG(tables,
	   belch("@@%%%% rebuildLAGAtable: inserted %d entries from liveIndirections and %d entries from liveRemoteGAs",
		 n,m)); 
}

/*
  Determine the size of the LAGA and GALA tables.
  Has to be done after rebuilding the tables. 
  Only used for global statistics gathering.
*/

//@cindex getLAGAtableSize
void
getLAGAtableSize(nat *nP, nat *sizeP)
{
  GALA *gala;
  // nat n=0, tot_size=0;
  StgClosure *closure;
  StgInfoTable *info;
  nat size, ptrs, nonptrs, vhs, i;
  char str[80];
  /* IN order to avoid counting closures twice we maintain a hash table
     of all closures seen so far.
     ToDo: collect this data while rebuilding the GALA table and make use
           of the existing hash tables;
  */
  HashTable *closureTable;  // hash table for closures encountered already

  closureTable = allocHashTable();

  (*nP) = (*sizeP) = 0;
  for (gala = liveIndirections; gala != NULL; gala = gala->next) {
    closure = (StgClosure*) gala->la;
    if (lookupHashTable(closureTable, (StgWord)closure)==NULL) { // not seen yet
      insertHashTable(closureTable, (StgWord)closure, (void *)1);
      info = get_closure_info(closure, &size, &ptrs, &nonptrs, &vhs, str);
      (*sizeP) += size ;   // stats: measure total heap size of global closures
      (*nP)++;             // stats: count number of GAs
    }
  }

  for (gala = liveRemoteGAs; gala != NULL; gala = gala->next) {
    closure = (StgClosure*) gala->la;
    if (lookupHashTable(closureTable, (StgWord)closure)==NULL) { // not seen yet
      insertHashTable(closureTable, (StgWord)closure, (void *)1);
      info = get_closure_info(closure, &size, &ptrs, &nonptrs, &vhs, str);
      (*sizeP) += size ;   // stats: measure total heap size of global closures
      (*nP)++;             // stats: count number of GAs
    }
  }

  freeHashTable(closureTable, NULL);
}

//@node Debugging routines, Index, GC functions for GALA tables, Global Address Manipulation
//@subsection Debugging routines

//@cindex printGA
void
printGA (globalAddr *ga)
{
  fprintf(stderr, "((%x, %d, %x))", 
	  ga->payload.gc.gtid,
	  ga->payload.gc.slot,
	  ga->weight);
}

//@cindex printGALA
void 
printGALA (GALA *gala)
{
  printGA(&(gala->ga));
  fprintf(stderr, " -> %p (%s)",
	  (StgClosure*)gala->la, info_type((StgClosure*)gala->la));
  fprintf(stderr, " %s",
	  (gala->preferred) ? "PREF" : "____");
}

/*
  Printing the LA->GA table.
*/

//@cindex printLiveIndTable
void
printLiveIndTable(void)
{
  GALA *gala, *q;
  nat n=0; // debugging

  belch("@@%%%%:: logical LiveIndTable (%p) (liveIndirections=%p):",
	LAtoGALAtable, liveIndirections); 
  
  for (gala = liveIndirections; gala != NULL; gala = gala->next) {
    n++;
    printGALA(gala);
    /* check whether this gala->la is hashed into the LAGA table */
    q = lookupHashTable(LAtoGALAtable, (StgWord)(gala->la));
    fprintf(stderr, "\t%s\n", (q==NULL) ? "...." : (q==gala) ?  "====" : "####");
    //ASSERT(lookupHashTable(LAtoGALAtable, (StgWord)(gala->la)));
  }
  belch("@@%%%%:: %d live indirections",
	n);
}

void
printRemoteGATable(void)
{
  GALA *gala, *q;
  nat m=0; // debugging

  belch("@@%%%%:: logical RemoteGATable (%p) (liveRemoteGAs=%p):",
	LAtoGALAtable, liveRemoteGAs);

  for (gala = liveRemoteGAs; gala != NULL; gala = gala->next) {
    m++;
    printGALA(gala);
    /* check whether this gala->la is hashed into the LAGA table */
    q = lookupHashTable(LAtoGALAtable, (StgWord)(gala->la));
    fprintf(stderr, "\t%s\n", (q==NULL) ? "...." : (q==gala) ? "====" : "####");
    // ASSERT(lookupHashTable(LAtoGALAtable, (StgWord)(gala->la)));
  }
  belch("@@%%%%:: %d remote GAs",
	m);
}

//@cindex printLAGAtable
void
printLAGAtable(void)
{
  belch("@@%%: LAGAtable (%p) with liveIndirections=%p, liveRemoteGAs=%p:",
	LAtoGALAtable, liveIndirections, liveRemoteGAs); 

  printLiveIndTable();
  printRemoteGATable();
}

/*
  Check whether a GA is already in a list.
*/
rtsBool
isOnLiveIndTable(globalAddr *ga)
{
  GALA *gala;

  for (gala = liveIndirections; gala != NULL; gala = gala->next) 
    if (gala->ga.weight==ga->weight &&
	gala->ga.payload.gc.slot==ga->payload.gc.slot &&
	gala->ga.payload.gc.gtid==ga->payload.gc.gtid)
      return rtsTrue;

  return rtsFalse;
}

rtsBool
isOnRemoteGATable(globalAddr *ga)
{
  GALA *gala;

  for (gala = liveRemoteGAs; gala != NULL; gala = gala->next) 
    if (gala->ga.weight==ga->weight &&
	gala->ga.payload.gc.slot==ga->payload.gc.slot &&
	gala->ga.payload.gc.gtid==ga->payload.gc.gtid)
      return rtsTrue;

  return rtsFalse;
}

/* 
   Sanity check for free lists.
*/
void
checkFreeGALAList(void) {
  GALA *gl;

  for (gl=freeGALAList; gl != NULL; gl=gl->next) {
    ASSERT(gl->ga.weight==0xdead0add);
    ASSERT(gl->la==(StgPtr)0xdead00aa);
  }
}

void
checkFreeIndirectionsList(void) {
  GALA *gl;

  for (gl=freeIndirections; gl != NULL; gl=gl->next) {
    ASSERT(gl->ga.weight==0xdead0add);
    ASSERT(gl->la==(StgPtr)0xdead00aa);
  }
}
#endif /* PAR -- whole file */

//@node Index,  , Debugging routines, Global Address Manipulation
//@subsection Index

//@index
//* DebugPrintLAGAtable::  @cindex\s-+DebugPrintLAGAtable
//* GALAlookup::  @cindex\s-+GALAlookup
//* LAGAlookup::  @cindex\s-+LAGAlookup
//* LAtoGALAtable::  @cindex\s-+LAtoGALAtable
//* PackGA::  @cindex\s-+PackGA
//* addWeight::  @cindex\s-+addWeight
//* allocGALA::  @cindex\s-+allocGALA
//* allocIndirection::  @cindex\s-+allocIndirection
//* freeIndirections::  @cindex\s-+freeIndirections
//* initGAtables::  @cindex\s-+initGAtables
//* liveIndirections::  @cindex\s-+liveIndirections
//* liveRemoteGAs::  @cindex\s-+liveRemoteGAs
//* makeGlobal::  @cindex\s-+makeGlobal
//* markLocalGAs::  @cindex\s-+markLocalGAs
//* nextIndirection::  @cindex\s-+nextIndirection
//* pGAtoGALAtable::  @cindex\s-+pGAtoGALAtable
//* printGA::  @cindex\s-+printGA
//* printGALA::  @cindex\s-+printGALA
//* rebuildLAGAtable::  @cindex\s-+rebuildLAGAtable
//* registerTask::  @cindex\s-+registerTask
//* setRemoteGA::  @cindex\s-+setRemoteGA
//* splitWeight::  @cindex\s-+splitWeight
//* taskIDtoPE::  @cindex\s-+taskIDtoPE
//* taskIDtoPEtable::  @cindex\s-+taskIDtoPEtable
//* thisPE::  @cindex\s-+thisPE
//@end index
