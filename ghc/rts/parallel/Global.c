/* ---------------------------------------------------------------------------
   Time-stamp: <Sat Dec 04 1999 21:28:56 Stardate: [-30]3999.47 hwloidl>
   $Id: Global.c,v 1.2 2000/01/13 14:34:06 hwloidl Exp $

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

//@node Includes, Global tables and lists, Global Address Manipulation, Global Address Manipulation
//@subsection Includes

#include "Rts.h"
#include "RtsFlags.h"
#include "RtsUtils.h"
#include "Storage.h"
#include "Hash.h"
#include "ParallelRts.h"

/*
  @globalAddr@ structures are allocated in chunks to reduce malloc overhead.
*/

//@node Global tables and lists, Fcts on GALA tables, Includes, Global Address Manipulation
//@subsection Global tables and lists

//@cindex thisPE
int thisPE;

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
    freeGALAList = gl->next;
  } else {
    gl = (GALA *) stgMallocBytes(GCHUNK * sizeof(GALA), "allocGALA");

    freeGALAList = gl + 1;
    for (p = freeGALAList; p < gl + GCHUNK - 1; p++)
      p->next = p + 1;
    p->next = NULL;
  }
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
  return (PEs) lookupHashTable(taskIDtoPEtable, gtid);
}

//@cindex registerTask
void 
registerTask(gtid)
GlobalTaskId gtid;
{
  if (gtid == mytid)
    thisPE = nextPE;

  insertHashTable(taskIDtoPEtable, gtid, (void *) (StgWord) nextPE++);
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

  /* We never look for GA's on indirections */
  ASSERT(IS_INDIRECTION(addr) == NULL);
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
allocIndirection(StgPtr addr)
{
  GALA *gala;
  
  if ((gala = freeIndirections) != NULL) {
    freeIndirections = gala->next;
  } else {
    gala = allocGALA();
    gala->ga.payload.gc.gtid = mytid;
    gala->ga.payload.gc.slot = nextIndirection++;
  }
  gala->ga.weight = MAX_GA_WEIGHT;
  gala->la = addr;
  return gala;
}

/*
  Make a local closure at @addr@ globally visible.  We have to allocate an
  indirection slot for it, and update both the local address to global address
  and global address to local address maps.
*/

//@cindex makeGlobal
globalAddr *
makeGlobal(addr, preferred)
StgClosure *addr;
rtsBool preferred;
{
  GALA *oldGALA = lookupHashTable(LAtoGALAtable, (StgWord) addr);
  GALA *newGALA = allocIndirection((StgPtr)addr);
  StgWord pga = PackGA(thisPE, newGALA->ga.payload.gc.slot);

  ASSERT(HEAP_ALLOCED(addr)); // check that addr might point into the heap 
  ASSERT(GALAlookup(&(newGALA->ga)) == NULL);
  
  newGALA->la = addr;
  newGALA->preferred = preferred;

  if (preferred) {
    /* The new GA is now the preferred GA for the LA */
    if (oldGALA != NULL) {
      oldGALA->preferred = rtsFalse;
      (void) removeHashTable(LAtoGALAtable, (StgWord) addr, (void *) oldGALA);
    }
    insertHashTable(LAtoGALAtable, (StgWord) addr, (void *) newGALA);
  }

  /* put the new GALA entry on the list of live indirections */
  newGALA->next = liveIndirections;
  liveIndirections = newGALA;
  
  insertHashTable(pGAtoGALAtable, pga, (void *) newGALA);
  
  return &(newGALA->ga);
}

/*
  Assign an existing remote global address to an existing closure.
  We do not retain the @globalAddr@ structure that's passed in as an argument,
  so it can be a static in the calling routine.
*/

//@cindex setRemoteGA
globalAddr *
setRemoteGA(addr, ga, preferred)
StgClosure *addr;
globalAddr *ga;
rtsBool preferred;
{
  GALA *oldGALA = lookupHashTable(LAtoGALAtable, (StgWord) addr);
  GALA *newGALA = allocGALA();
  StgWord pga = PackGA(taskIDtoPE(ga->payload.gc.gtid), ga->payload.gc.slot);

  ASSERT(ga->payload.gc.gtid != mytid);
  ASSERT(ga->weight > 0);
  ASSERT(GALAlookup(ga) == NULL);

  newGALA->ga = *ga;
  newGALA->la = addr;
  newGALA->preferred = preferred;

  if (preferred) {
    /* The new GA is now the preferred GA for the LA */
    if (oldGALA != NULL) {
      oldGALA->preferred = rtsFalse;
      (void) removeHashTable(LAtoGALAtable, (StgWord) addr, (void *) oldGALA);
    }
    insertHashTable(LAtoGALAtable, (StgWord) addr, (void *) newGALA);
  }
  newGALA->next = liveRemoteGAs;
  liveRemoteGAs = newGALA;
  
  insertHashTable(pGAtoGALAtable, pga, (void *) newGALA);
  
  ga->weight = 0;

  return &(newGALA->ga);
}

/*
  Give me a bit of weight to give away on a new reference to a particular
  global address.  If we run down to nothing, we have to assign a new GA.  
*/

//@cindex splitWeight
void
splitWeight(to, from)
globalAddr *to, *from;
{
  /* Make sure we have enough weight to split */
  if (from->weight == 1)
    from = makeGlobal(GALAlookup(from), rtsTrue);
  
  to->payload = from->payload;

  if (from->weight == 0)
    to->weight = 1L << (BITS_IN(unsigned) - 1);
  else
    to->weight = from->weight / 2;

  from->weight -= to->weight;
}

/*
  Here, I am returning a bit of weight that a remote PE no longer needs.
*/

//@cindex addWeight
globalAddr *
addWeight(ga)
globalAddr *ga;
{
  StgWord pga = PackGA(taskIDtoPE(ga->payload.gc.gtid), ga->payload.gc.slot);
  GALA *gala = (GALA *) lookupHashTable(pGAtoGALAtable, pga);

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
*/
//@cindex markLocalGAs
void
markLocalGAs(rtsBool full)
{
  GALA *gala;
  GALA *next;
  GALA *prev = NULL;
  StgPtr old_la, new_la;
  nat n=0, m=0; // debugging only
  
  IF_DEBUG(gc,
	   belch("@@ markLocalGAs: Marking LIVE INDIRECTIONS in GALA table starting with GALA at %p\n",
		 liveIndirections);
	   printLAGAtable());

  for (gala = liveIndirections, m=0; gala != NULL; gala = next, m++) {
    IF_DEBUG(gc,
 	     printGA(&(gala->ga));
	     fprintf(stderr, ";@ %d: LA: %p (%s) ",
		     m, gala->la, info_type(gala->la)));
    next = gala->next;
    old_la = gala->la;
    ASSERT(gala->ga.payload.gc.gtid == mytid); /* it's supposed to be local */
    if (get_itbl((StgClosure *)old_la)->type == EVACUATED) {
      /* somebody else already evacuated this closure */
      new_la = ((StgEvacuated *)old_la)->evacuee;
      IF_DEBUG(gc,
	       belch(" already evacuated to %p\n", new_la));
    } else {
      StgClosure *foo ; // debugging only
      n++;
      IF_PAR_DEBUG(verbose,
		   if (IS_INDIRECTION((StgClosure *)old_la))
		       belch("{markLocalGAs}Daq ghuH: trying to mark an indirection %p (%s) -> %p (%s); [closure=%p]",
			     old_la, info_type(old_la), 
			     (foo = UNWIND_IND((StgClosure *)old_la)), info_type(foo), 
			     old_la));
      new_la = MarkRoot(UNWIND_IND((StgClosure *)old_la)); // or just evacuate(old_ga)
      IF_DEBUG(gc,
	       belch(" evacuated %p to %p\n", old_la, new_la));
    }

    gala->la = new_la;
    /* remove old LA and replace with new LA */
    //(void) removeHashTable(LAtoGALAtable, (StgWord) old_la, (void *) gala);
    //insertHashTable(LAtoGALAtable, (StgWord) new_la, (void *) gala);

    gala->next = prev;
    prev = gala;
  }
  liveIndirections = prev;  /* list has been reversed during the marking */

  IF_PAR_DEBUG(verbose,
	       belch("@@ markLocalGAs: %d of %d GALAs marked on PE %x",
		     n, m, mytid));

  /* -------------------------------------------------------------------- */

  n=0; m=0; // debugging only
  
  IF_DEBUG(gc,
	   belch("@@ markLocalGAs: Marking LIVE REMOTE GAs in GALA table starting with GALA at %p\n",
		 liveRemoteGAs));

  for (gala = liveRemoteGAs, prev = NULL; gala != NULL; gala = next) {
    IF_DEBUG(gc,
	     printGA(&(gala->ga)));
    next = gala->next;
    old_la = gala->la;
    ASSERT(gala->ga.payload.gc.gtid != mytid); /* it's supposed to be remote */
    if (get_itbl((StgClosure *)old_la)->type == EVACUATED) {
      /* somebody else already evacuated this closure */
      new_la = ((StgEvacuated *)old_la)->evacuee;
    } else {
      n++;
      new_la = MarkRoot((StgClosure *)old_la); // or just evacuate(old_ga)
    }

    gala->la = new_la;
    /* remove old LA and replace with new LA */
    //(void) removeHashTable(LAtoGALAtable, (StgWord) old_la, (void *) gala);
    //insertHashTable(LAtoGALAtable, (StgWord) new_la, (void *) gala);

    gala->next = prev;
    prev = gala;
  }
  liveRemoteGAs = prev; /* list is reversed during marking */

  /* If we have any remaining FREE messages to send off, do so now */
  // sendFreeMessages();

  IF_DEBUG(gc,
	   belch("@@ markLocalGAs: GALA after marking");
	   printLAGAtable();
	   belch("--------------------------------------"));
  
}

void
OLDmarkLocalGAs(rtsBool full)
{
  extern StgClosure *MarkRootHWL(StgClosure *root);

  GALA *gala;
  GALA *next;
  GALA *prev = NULL;
  StgPtr new_la;
  nat n=0, m=0; // debugging only
  
  IF_DEBUG(gc,
	   belch("@@ markLocalGAs: Marking entries in GALA table starting with GALA at %p",
		 liveIndirections);
	   printLAGAtable());

  for (gala = liveIndirections; gala != NULL; gala = next) {
    IF_DEBUG(gc,
 	     printGA(&(gala->ga));
	     fprintf(stderr, " LA: %p (%s) ",
		     gala->la, info_type(gala->la)));
    next = gala->next;
    ASSERT(gala->ga.payload.gc.gtid == mytid); /* it's supposed to be local */
    if (gala->ga.weight != MAX_GA_WEIGHT) {
      /* Remote references exist, so we must evacuate the local closure */
      StgPtr old_la = gala->la;

      if (get_itbl((StgClosure *)old_la)->type != EVACUATED) { // track evacuee!??
	n++;
	IF_DEBUG(gc,
		 fprintf(stderr, " marking as root\n"));
	new_la = MarkRoot((StgClosure *)old_la); // or just evacuate(old_ga)
	//IF_DEBUG(gc,
	//	 fprintf(stderr, " new LA is %p ", new_la));
	if (!full && gala->preferred && new_la != old_la) {
	  IF_DEBUG(gc,
		   fprintf(stderr, " replacing %p with %p in LAGA table\n",
			   old_la, new_la));
	  (void) removeHashTable(LAtoGALAtable, (StgWord) old_la, (void *) gala);
	  insertHashTable(LAtoGALAtable, (StgWord) new_la, (void *) gala);
	}
      } else {
  	IF_DEBUG(gc,
  		 fprintf(stderr, " EVAC "));
	new_la = ((StgEvacuated *)old_la)->evacuee;
	IF_DEBUG(gc,
		 fprintf(stderr, " replacing %p with %p in LAGA table\n",
			   old_la, new_la));
	(void) removeHashTable(LAtoGALAtable, (StgWord) old_la, (void *) gala);
	insertHashTable(LAtoGALAtable, (StgWord) new_la, (void *) gala);
      } 
      gala->next = prev;
      prev = gala;
    } else {
      /* Since we have all of the weight, this GA is no longer needed */
      StgWord pga = PackGA(thisPE, gala->ga.payload.gc.slot);

      m++;
      IF_DEBUG(gc,
	       fprintf(stderr, " freeing slot %d", 
		       gala->ga.payload.gc.slot));

      /* put the now redundant GALA onto the free list */
      gala->next = freeIndirections;
      freeIndirections = gala;
      /* remove the GALA from the GALA table; now it's just local */
      (void) removeHashTable(pGAtoGALAtable, pga, (void *) gala);
      if (!full && gala->preferred)
	(void) removeHashTable(LAtoGALAtable, (StgWord) gala->la, (void *) gala);

#ifdef DEBUG
      gala->ga.weight = 0x0d0d0d0d;
      gala->la = (StgWord) 0x0bad0bad;
#endif
    }
  }
  liveIndirections = prev;  /* list has been reversed during the marking */

  IF_PAR_DEBUG(verbose,
	       belch("@@ markLocalGAs: %d GALAs marked, %d GALAs nuked on PE %x",
		     n, m, mytid));

}

//@cindex RebuildGAtables
void
RebuildGAtables(rtsBool full)
{
  GALA *gala;
  GALA *next;
  GALA *prev;
  StgClosure *closure, *last, *new_closure;

  //prepareFreeMsgBuffers();

  if (full)
    RebuildLAGAtable();

  IF_DEBUG(gc,
	   belch("@@ RebuildGAtables: After ReBuilding GALA table starting with GALA at %p",
		 liveRemoteGAs);
	   printLAGAtable());
}

void
OLDRebuildGAtables(rtsBool full)
{
  GALA *gala;
  GALA *next;
  GALA *prev;
  StgClosure *closure, *last, *new_closure;

  prepareFreeMsgBuffers();

  for (gala = liveRemoteGAs, prev = NULL; gala != NULL; gala = next) {
    IF_DEBUG(gc,
	     printGA(&(gala->ga)));
    next = gala->next;
    ASSERT(gala->ga.payload.gc.gtid != mytid); /* it's supposed to be remote */

    closure = (StgClosure *) (gala->la);

    /*
     * If the old closure has not been forwarded, we let go.  Note that this
     * approach also drops global aliases for PLCs.
     */

    if (!full && gala->preferred)
      (void) removeHashTable(LAtoGALAtable, (StgWord) gala->la, (void *) gala);

    /* Follow indirection chains to the end, just in case */
    closure = UNWIND_IND(closure);

    /*
    if (get_itbl(closure)->type != EVACUATED) { // (new_closure = isAlive(closure)) == NULL) { // (W_) Forward_Ref_info)
      // closure is not alive any more, thus remove GA 
      int pe = taskIDtoPE(gala->ga.payload.gc.gtid);
      StgWord pga = PackGA(pe, gala->ga.payload.gc.slot);

      IF_DEBUG(gc,
	       fprintf(stderr, " (LA: %p (%s)) is unused on this PE -> sending free\n",
		       closure, info_type(closure)));

      (void) removeHashTable(pGAtoGALAtable, pga, (void *) gala);
      freeRemoteGA(pe, &(gala->ga));
      gala->next = freeGALAList;
      freeGALAList = gala;
    } else {
    */
    if (get_itbl(closure)->type == EVACUATED) {
      IF_DEBUG(gc,
	       fprintf(stderr, " EVAC %p (%s)\n",
		       closure, info_type(closure)));
      closure = ((StgEvacuated *)closure)->evacuee;
    } else {
      IF_DEBUG(gc,
	       fprintf(stderr, " !EVAC %p (%s)\n",
		       closure, info_type(closure)));
    }
    gala->la = closure;
    if (!full && gala->preferred)
      insertHashTable(LAtoGALAtable, (StgWord) gala->la, (void *) gala);
    gala->next = prev;
    prev = gala;
  }
  //}
  liveRemoteGAs = prev; /* list is reversed during marking */

  /* If we have any remaining FREE messages to send off, do so now */
  sendFreeMessages();

  if (full)
    RebuildLAGAtable();

  IF_DEBUG(gc,
	   belch("@@ RebuildGAtables: After ReBuilding GALA table starting with GALA at %p",
		 liveRemoteGAs);
	   printLAGAtable());
}

/*
  Rebuild the LA->GA table, assuming that the addresses in the GALAs are
  correct.  
*/

//@cindex RebuildLAGAtable
void
RebuildLAGAtable(void)
{
  GALA *gala;
  nat n=0, m=0; // debugging

  /* The old LA->GA table is worthless */
  freeHashTable(LAtoGALAtable, NULL);
  LAtoGALAtable = allocHashTable();

  IF_DEBUG(gc,
	   belch("@@ RebuildLAGAtable: new LAGA table at %p",
		 LAtoGALAtable)); 
  
  for (gala = liveIndirections; gala != NULL; gala = gala->next) {
    n++;
    if (gala->preferred)
      insertHashTable(LAtoGALAtable, (StgWord) gala->la, (void *) gala);
  }

  for (gala = liveRemoteGAs; gala != NULL; gala = gala->next) {
    m++;
    if (gala->preferred)
      insertHashTable(LAtoGALAtable, (StgWord) gala->la, (void *) gala);
  }

  IF_DEBUG(gc,
	   belch("@@ RebuildLAGAtable: inserted %d entries from liveIndirections and %d entries from liveRemoteGAs",
		 n,m)); 
  
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
  fprintf(stderr, " -> %p (%s)", (StgPtr)gala->la, info_type(gala->la));
  fprintf(stderr, " %s", (gala->preferred) ? "PREF" : "____");
}

/*
  Printing the LA->GA table.
*/

//@cindex DebugPrintLAGAtable
void
printLAGAtable(void)
{
  GALA *gala;
  nat n=0, m=0; // debugging

  belch("@@ LAGAtable (%p) with liveIndirections=%p, liveRemoteGAs=%p:",
	LAtoGALAtable, liveIndirections, liveRemoteGAs); 
  
  for (gala = liveIndirections; gala != NULL; gala = gala->next) {
    n++;
    printGALA(gala);
    fputc('\n', stderr);
  }

  for (gala = liveRemoteGAs; gala != NULL; gala = gala->next) {
    m++;
    printGALA(gala);
    fputc('\n', stderr);
  }
  belch("@@ LAGAtable has %d liveIndirections entries and %d liveRemoteGAs entries",
	n, m);
}

#endif /* PAR -- whole file */

//@node Index,  , Debugging routines, Global Address Manipulation
//@subsection Index

//@index
//* GALAlookup::  @cindex\s-+GALAlookup
//* LAGAlookup::  @cindex\s-+LAGAlookup
//* LAtoGALAtable::  @cindex\s-+LAtoGALAtable
//* PackGA::  @cindex\s-+PackGA
//* RebuildGAtables::  @cindex\s-+RebuildGAtables
//* RebuildLAGAtable::  @cindex\s-+RebuildLAGAtable
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
//* registerTask::  @cindex\s-+registerTask
//* setRemoteGA::  @cindex\s-+setRemoteGA
//* splitWeight::  @cindex\s-+splitWeight
//* taskIDtoPE::  @cindex\s-+taskIDtoPE
//* taskIDtoPEtable::  @cindex\s-+taskIDtoPEtable
//* thisPE::  @cindex\s-+thisPE
//@end index
