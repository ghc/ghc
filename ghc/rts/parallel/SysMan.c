/* ----------------------------------------------------------------------------
   Time-stamp: <Tue Mar 21 2000 20:25:55 Stardate: [-30]4539.25 hwloidl>
   $Id: SysMan.c,v 1.3 2000/03/31 03:09:37 hwloidl Exp $

   GUM System Manager Program
   Handles startup, shutdown and global synchronisation of the parallel system.

   The Parade/AQUA Projects, Glasgow University, 1994-1995.
   GdH/APART Projects, Heriot-Watt University, Edinburgh, 1997-1999.
   P. Trinder, November 30th. 1994.
   Adapted for new RTS
   P. Trinder, July 1997.
   H-W. Loidl, November 1999.  
 
   ------------------------------------------------------------------------- */

//@node GUM System Manager Program, , ,
//@section GUM System Manager Program

//@menu
//* General docu::		
//* Includes::			
//* Macros etc::		
//* Variables::			
//* Main fct::			
//* Auxiliary fcts::		
//* Index::			
//@end menu

//@node General docu, Includes, GUM System Manager Program, GUM System Manager Program
//@subsection General docu

/*

The Sysman task currently controls initiation, termination, of a
parallel Haskell program running under GUM. In the future it may
control global GC synchronisation and statistics gathering. Based on
K. Hammond's SysMan.lc in Graph for PVM. SysMan is unusual in that it
is not part of the executable produced by ghc: it is a free-standing
program that spawns PVM tasks (logical PEs) to evaluate the
program. After initialisation it runs in parallel with the PE tasks,
awaiting messages.

OK children, buckle down for some serious weirdness, it works like this ...


o The argument vector (argv) for SysMan has one the following 2 shapes:

-------------------------------------------------------------------------------
| SysMan path | debug flag | pvm-executable path | Num. PEs | Program Args ...|
-------------------------------------------------------------------------------

-------------------------------------------------------------------
| SysMan path | pvm-executable path | Num. PEs | Program Args ... |
-------------------------------------------------------------------

The "pvm-executable path" is an absolute path of where PVM stashes the
code for each PE. The arguments passed on to each PE-executable
spawned by PVM are:

-------------------------------
| Num. PEs | Program Args ... |
-------------------------------

The arguments passed to the Main-thread PE-executable are

-------------------------------------------------------------------
| main flag | pvm-executable path | Num. PEs | Program Args ... |
-------------------------------------------------------------------

o SysMan's algorithm is as follows.

o use PVM to spawn (nPE-1) PVM tasks 
o fork SysMan to create the main-thread PE. This permits the main-thread to 
read and write to stdin and stdout. 
o Barrier-synchronise waiting for all of the PE-tasks to start.
o Broadcast the SysMan task-id, so that the main thread knows it.
o Wait for the Main-thread PE to send it's task-id.
o Broadcast an array of the PE task-ids to all of the PE-tasks.
o Enter a loop awaiting incoming messages, e.g. failure, Garbage-collection, 
termination.

The forked Main-thread algorithm, in SysMan, is as follows.

o disconnects from PVM.
o sets a flag in argv to indicate that it is the main thread.
o `exec's a copy of the pvm-executable (i.e. the program being run)


The pvm-executable run by each PE-task, is initialised as follows.

o Registers with PVM, obtaining a task-id.
o Joins the barrier synchronisation awaiting the other PEs.
o Receives and records the task-id of SysMan, for future use.
o If the PE is the main thread it sends its task-id to SysMan.
o Receives and records the array of task-ids of the other PEs.
o Begins execution.

*/

//@node Includes, Macros etc, General docu, GUM System Manager Program
//@subsection Includes

#include "Rts.h"
#include "ParTypes.h"
#include "LLC.h"
#include "Parallel.h"

//@node Macros etc, Variables, Includes, GUM System Manager Program
//@subsection Macros etc

#define NON_POSIX_SOURCE /* so says Solaris */

#define checkerr(c)	do { \
                          if ((c)<0) { \
                            pvm_perror("Sysman"); \
                            fprintf(stderr,"Sysman"); \
                            stg_exit(EXIT_FAILURE); \
                          } \
                        } while(0)

/* SysMan is put on top of the GHC routine that does the RtsFlags handling.
   So, we cannot use the standard macros. For the time being we use a macro
   that is fixed at compile time.
*/
/* debugging enabled */
#define IF_PAR_DEBUG(c,s)  { s; }
/* debugging disabled */
// #define IF_PAR_DEBUG(c,s)  /* nothing */

//@node Variables, Main fct, Macros etc, GUM System Manager Program
//@subsection Variables

/*
   The following definitions included so that SysMan can be linked with Low
   Level Communications module (LLComms). They are not used in SysMan.  */

GlobalTaskId  mytid, SysManTask;
rtsBool       IAmMainThread;
rtsBool       GlobalStopPending = rtsFalse;
              /* Handle unexpected messages correctly */

static           GlobalTaskId gtids[MAX_PES];
static           GlobalTaskId sysman_id, sender_id, mainThread_id;
static unsigned  PEsTerminated = 0;
static rtsBool   Finishing = rtsFalse;
static long      PEbuffer[MAX_PES];
nat              nPEs = 0;

//@node Main fct, Auxiliary fcts, Variables, GUM System Manager Program
//@subsection Main fct

//@cindex main
main (int argc, char **argv) {
  int rbufid;
  int opcode, nbytes;
  char **pargv;
  int i, cc, spawn_flag = PvmTaskDefault;
  char *petask, *pvmExecutable;
  rtsPacket addr;
  
  setbuf(stdout, NULL);  // disable buffering of stdout
  setbuf(stderr, NULL);  // disable buffering of stderr
  
  if (argc > 1) {
    if (*argv[1] == '-') {
      spawn_flag = PvmTaskDebug;
      argv[1] = argv[0];
      argv++; argc--;
    }
    sysman_id = pvm_mytid();  /* This must be the first PVM call */
    
    checkerr(sysman_id);
    
    /* 
       Get the full path and filename of the pvm executable (stashed in some
       PVM directory), and the number of PEs from the command line.
    */
    pvmExecutable = argv[1];
    nPEs = atoi(argv[2]);
    
    if ((petask = getenv(PETASK)) == NULL)  // PETASK set by driver
      petask = PETASK;

    IF_PAR_DEBUG(verbose,
		 fprintf(stderr,"== [%x] nPEs (%s) = %d\n", 
			 sysman_id, petask, nPEs));
    
    /* Check that we can create the number of PE and IMU tasks requested */
    if (nPEs > MAX_PES) {
      fprintf(stderr,"SysMan: No more than %d PEs allowed (%d requested)\n", 
	   MAX_PES, nPEs);
      stg_exit(EXIT_FAILURE);
    }
    /* 
       Now create the PE Tasks. We spawn (nPEs-1) pvm threads: the Main Thread 
       (which starts execution and performs IO) is created by forking SysMan 
    */
    nPEs--;
    if (nPEs > 0) {
      /* Initialise the PE task arguments from Sysman's arguments */
      pargv = argv + 2;

      IF_PAR_DEBUG(verbose,
		   fprintf(stderr, "== [%x] Spawning %d PEs(%s) ...\n", 
			   sysman_id, nPEs, petask);
		   fprintf(stderr, "  args: ");
		   for (i = 0; pargv[i]; ++i)
		     fprintf(stderr, "%s, ", pargv[i]);
		   fprintf(stderr, "\n"));

      checkerr(pvm_spawn(petask, pargv, spawn_flag, "", nPEs, gtids));
      /*
       * Stash the task-ids of the PEs away in a buffer, once we know 
       * the Main Thread's task-id, we'll broadcast them all.
       */	    
      for (i = 0; i < nPEs; i++)
	PEbuffer[i+1] = (long) gtids[i];

      IF_PAR_DEBUG(verbose,
		   fprintf(stderr,"== [%x] Spawned\n", sysman_id));
    }
    
    /* 
       Create the MainThread PE by forking SysMan. This arcane coding 
       is required to allow MainThread to read stdin and write to stdout.
       PWT 18/1/96 
    */
    nPEs++;                /* Record that the number of PEs is increasing */
    if ((cc = fork())) {
      checkerr(cc);        /* Parent continues as SysMan */
      IF_PAR_DEBUG(verbose,
		   fprintf(stderr,"== [%x] SysMan Task is [t%x]\n", sysman_id));

      /*
	SysMan joins PECTLGROUP, so that it can wait (at the
	barrier sysnchronisation a few instructions later) for the
	other PE-tasks to start.
	
	The manager group (MGRGROUP) is vestigial at the moment. It
	may eventually include a statistics manager, and a (global) 
	garbage collector manager.
      */
      checkerr(pvm_joingroup(PECTLGROUP));
      IF_PAR_DEBUG(verbose,
		   fprintf(stderr,"== [%x] Joined PECTLGROUP \n", sysman_id));

      /* Wait for all the PEs to arrive */
      checkerr(pvm_barrier(PECTLGROUP, nPEs + 1));

      IF_PAR_DEBUG(verbose,
		   fprintf(stderr,"== [%x] PECTLGROUP  barrier passed \n", 
			   sysman_id));

      /* Broadcast SysMan's ID, so Main Thread PE knows it */
      pvm_initsend(PvmDataDefault);
      pvm_bcast(PEGROUP, PP_SYSMAN_TID);
      
      /* Wait for Main Thread to identify itself*/
      addr = waitForPEOp(PP_MAIN_TASK, ANY_GLOBAL_TASK);
      pvm_bufinfo(addr, &nbytes, &opcode, &mainThread_id);
      PEbuffer[0] = mainThread_id;

      IF_PAR_DEBUG(verbose,
		   fprintf(stderr,"== [%x] SysMan received Main Task = %x\n", 
			   sysman_id, mainThread_id));

      /* Now that we have them all, broadcast Global Task Ids of all PEs */
      pvm_initsend(PvmDataDefault);
      PutArgs(PEbuffer, nPEs);
      pvm_bcast(PEGROUP, PP_PETIDS);

      IF_PAR_DEBUG(verbose,
		   fprintf(stderr,"== [%x] Sysman successfully initialized!\n",
			   sysman_id));

//@cindex message handling loop
      /* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
      /* Main message handling loop                                         */
      /* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
      /* Process incoming messages */
      while (1) {
	if ((rbufid = pvm_recv(ANY_TASK, ANY_OPCODE)) < 0)
	  pvm_perror("Sysman: Receiving Message");
	else {
	  pvm_bufinfo(rbufid, &nbytes, &opcode, &sender_id);

	  /* 
	  IF_PAR_DEBUG(trace,
		       fprintf(stderr,"== [%x] SysMan: Message received by SysMan: rbufid=%x, nbytes = %d, opcode = %x, sender_id = %x\n",
			     sysman_id, rbufid, nbytes, opcode, sender_id));
	  */
	  switch (opcode) {
	  case PP_GC_INIT:
	    /* This Function not yet implemented for GUM */
	    fprintf(stderr,"Global GC from %x Not yet implemented for GUM!\n", 
		  sender_id);
	    sync(PECTLGROUP, PP_FULL_SYSTEM);
	    broadcast(PEGROUP, PP_GC_INIT);
	    /*                DoGlobalGC();                */
	    /*		      broadcast(PEGROUP, PP_INIT); */
	    break;
	    
	  case PP_STATS_ON:
	    fprintf(stderr,"PP_STATS_ON (from %x) not yet implemented for GUM!\n", 
		  sender_id);
	    break;

	  case PP_STATS_OFF:
	    fprintf(stderr,"PP_STATS_OFF (from %x) not yet implemented for GUM!\n", 
		  sender_id);
	    break;
	    
	  case PP_FINISH:
	    IF_PAR_DEBUG(verbose,
			 fprintf(stderr,"== [%x] Finish from %x\n", 
				 sysman_id, sender_id));
	    if (!Finishing) {
	      Finishing = rtsTrue;
	      PEsTerminated = 1;
	      pvm_initsend(PvmDataDefault);
	      pvm_bcast(PEGROUP, PP_FINISH);
	    } else {
	      ++PEsTerminated;
	    }
	    if (PEsTerminated >= nPEs) {
	      IF_PAR_DEBUG(verbose,
			   fprintf(stderr,"== [%x] Global Shutdown, Goodbye!! (SysMan has received FINISHes from all PEs)\n", 
				   sysman_id));
	      broadcast(PEGROUP, PP_FINISH);
	      broadcast(MGRGROUP, PP_FINISH);
	      pvm_lvgroup(PECTLGROUP);
	      pvm_lvgroup(MGRGROUP);
	      pvm_exit();
	      exit(EXIT_SUCCESS);
	      /* Qapla'! */
	    }
	    break;
	    
	  case PP_FAIL:
	    IF_PAR_DEBUG(verbose,
			 fprintf(stderr,"== [%x] Fail from %x\n", 
				 sysman_id, sender_id));
	    if (!Finishing) {
	      Finishing = rtsTrue;
	      broadcast(PEGROUP, PP_FAIL);
	    }
	    break;
	    
	  default:
	    {
	     /*		          
	      char *opname = GetOpName(opcode);
	      fprintf(stderr,"Sysman: Unrecognised opcode %s (%x)\n",
	                      opname,opcode);	*/
	      fprintf(stderr,"Qagh: Sysman: Unrecognised opcode (%x)\n",
		    opcode);
	    }
	    break;
	  } 	/* switch */
	}		/* else */
      }		/* while 1 */
    }      		/* forked Sysman Process */
    else {
      fprintf(stderr, "Main Thread PE has been forked; doing an execv(%s,...)\n", 
	      pvmExecutable);
      pvmendtask();		 /* Disconnect from PVM to avoid confusion: */
      /* executable reconnects  */
      *argv[0] = '-';		 /* Flag that this is the Main Thread PE */
      execv(pvmExecutable,argv); /* Parent task becomes Main Thread PE */
    }
  }			/* argc > 1 */  
}			/* main */

//@node Auxiliary fcts, Index, Main fct, GUM System Manager Program
//@subsection Auxiliary fcts

/*
 * This reproduced from RtsUtlis to save linking with a whole ball of wax
 */
/* result-checking malloc wrappers. */

//@cindex stgMallocBytes

void *
stgMallocBytes (int n, char *msg)
{
    char *space;

    if ((space = (char *) malloc((size_t) n)) == NULL) {
	fflush(stdout);
	fprintf(stderr, msg);
	// MallocFailHook((W_) n, msg); /*msg*/
	stg_exit(EXIT_FAILURE);
    }
    return space;
}

/* Needed here because its used in loads of places like LLComms etc */

//@cindex stg_exit

void stg_exit(n)
I_ n;
{
    exit(n);
}

//@node Index,  , Auxiliary fcts, GUM System Manager Program
//@subsection Index

//@index
//* main::  @cindex\s-+main
//* message handling loop::  @cindex\s-+message handling loop
//* stgMallocBytes::  @cindex\s-+stgMallocBytes
//* stg_exit::  @cindex\s-+stg_exit
//@end index
