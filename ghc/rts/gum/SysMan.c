/****************************************************************************

   GUM System Manager Program

   The Parade/AQUA Projects, Glasgow University, 1994-1995.
   P. Trinder, November 30th. 1994.
   Adapted for new RTS
   P. Trinder, July 1997.
  
 
****************************************************************************

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

***************************************************************************/

#define NON_POSIX_SOURCE /* so says Solaris */

#include "Rts.h"
#include "ParTypes.h"
#include "LLC.h"
#include "Parallel.h"

/*
 *The following definitions included so that SysMan can be linked with
 *Low Level Communications module (LLComms). They are not used in
 *SysMan.
 */

GLOBAL_TASK_ID mytid, SysManTask;
rtsBool IAmMainThread;
rtsBool GlobalStopPending =   	rtsFalse;	/* Handle Unexpexted messages correctly */

static GLOBAL_TASK_ID gtids[MAX_PES];
static long PEbuffer[MAX_PES];
int nPEs = 0;
static GLOBAL_TASK_ID sysman_id, sender_id, mainThread_id;
static unsigned PEsTerminated = 0;
static rtsBool Finishing = rtsFalse;

/*
 * This reproduced from RtsUtlis to save linking with a whole ball of wax
 */
stgMallocBytes (int n, char *msg)
{
    char *space;

    if ((space = (char *) malloc((size_t) n)) == NULL) {
	fflush(stdout);
        fprintf(stderr,"stgMallocBytes failed: ", msg);
	stg_exit(EXIT_FAILURE);
    }
    return space;
}

#define checkerr(c)	do {if((c)<0) { pvm_perror("Sysman"); exit(EXIT_FAILURE); }} while(0)

main(int argc, char **argv)
{
    int rbufid;
    int opcode, nbytes;
    char **pargv;
    int i, cc;
    int spawn_flag = PvmTaskDefault;
    PACKET addr;

    char *petask, *pvmExecutable;

    setbuf(stdout, NULL);
    setbuf(stderr, NULL);

    if (argc > 1) {
	if (*argv[1] == '-') {
	    spawn_flag = PvmTaskDebug;
	    argv[1] = argv[0];
	    argv++; argc--;
	}
	sysman_id = pvm_mytid();/* This must be the first PVM call */

	checkerr(sysman_id);

	/* 
	Get the full path and filename of the pvm executable (stashed in some
	PVM directory.
	*/
	pvmExecutable = argv[1];

	nPEs = atoi(argv[2]);

	if ((petask = getenv(PETASK)) == NULL)
	    petask = PETASK;

#if 1
	fprintf(stderr, "nPEs (%s) = %d\n", petask, nPEs);
#endif

	/* Check that we can create the number of PE and IMU tasks requested */
	if (nPEs > MAX_PES) {
	    fprintf(stderr, "No more than %d PEs allowed (%d requested)\n", MAX_PES, nPEs);
	    exit(EXIT_FAILURE);
	}
        
	/* 
	Now create the PE Tasks. We spawn (nPEs-1) pvm threads: the Main Thread 
	(which starts execution and performs IO) is created by forking SysMan 
        */
	nPEs--;
	if (nPEs > 0) {
	    /* Initialise the PE task arguments from Sysman's arguments */
	    pargv = argv + 2;
#if 1
	    fprintf(stderr, "Spawning %d PEs(%s) ...\n", nPEs, petask);
	    fprintf(stderr, "  args: ");
	    for (i = 0; pargv[i]; ++i)
		fprintf(stderr, "%s, ", pargv[i]);
	    fprintf(stderr, "\n");
#endif
	    checkerr(pvm_spawn(petask, pargv, spawn_flag, "", nPEs, gtids));
	    /*
	     * Stash the task-ids of the PEs away in a buffer, once we know 
	     * the Main Thread's task-id, we'll broadcast them all.
	     */	    
	    for (i = 0; i < nPEs; i++)
		PEbuffer[i+1] = (long) gtids[i];
#if 1
	    fprintf(stderr, "Spawned /* PWT */\n");
#endif
	}

	/* 
	Create the MainThread PE by forking SysMan. This arcane coding 
	is required to allow MainThread to read stdin and write to stdout.
	PWT 18/1/96 
	*/
	nPEs++;				/* Record that the number of PEs is increasing */
	if ((cc = fork())) {
            checkerr(cc);		/* Parent continues as SysMan */
#if 1
	    fprintf(stderr, "SysMan Task is [t%x]\n", sysman_id);
#endif
	    /*
	    SysMan joins PECTLGROUP, so that it can wait (at the
	    barrier sysnchronisation a few instructions later) for the
	    other PE-tasks to start.
	   
	    The manager group (MGRGROUP) is vestigial at the moment. It
	    may eventually include a statistics manager, and a (global) 
	    garbage collector manager.
  	    */
	    checkerr(pvm_joingroup(PECTLGROUP));
#if 1
	    fprintf(stderr, "Joined PECTLGROUP /* PWT */\n");
#endif
	    /* Wait for all the PEs to arrive */
	    checkerr(pvm_barrier(PECTLGROUP, nPEs + 1));
#if 1
	    fprintf(stderr, "PECTLGROUP  barrier passed /* HWL */\n");
#endif
	    /* Broadcast SysMan's ID, so Main Thread PE knows it */
	    pvm_initsend(PvmDataDefault);
	    pvm_bcast(PEGROUP, PP_SYSMAN_TID);

	    /* Wait for Main Thread to identify itself*/
	    addr = WaitForPEOp(PP_MAIN_TASK, ANY_GLOBAL_TASK);
            pvm_bufinfo(addr, &nbytes, &opcode, &mainThread_id );
	    PEbuffer[0] = mainThread_id;
#if 1
    	    fprintf(stderr,"SysMan received Main Task = %x\n",mainThread_id); 
#endif	    
  	    /* Now that we have them all, broadcast Global Task Ids of all PEs */
  	    pvm_initsend(PvmDataDefault);
	    PutArgs(PEbuffer, nPEs);
	    pvm_bcast(PEGROUP, PP_PETIDS);
#if 1
 	    fprintf(stderr, "Sysman successfully initialized!\n");
#endif
 	    /* Process incoming messages */
	    while (1) {
	        if ((rbufid = pvm_recv(ANY_TASK, ANY_OPCODE)) < 0)
	    	    pvm_perror("Sysman: Receiving Message");
	        else {
	  	    pvm_bufinfo(rbufid, &nbytes, &opcode, &sender_id);
#if 1
	  	  fprintf(stderr, "HWL-DBG(SysMan; main loop): rbufid=%x, nbytes = %d, opcode = %x, sender_id = %x\n",
	  	      rbufid, nbytes, opcode, sender_id);
#endif
	  	  switch (opcode) {
		    case PP_GC_INIT:
		      /* This Function not yet implemented for GUM */
		      fprintf(stderr, "Global GC from %x Not yet implemented for GUM!\n", sender_id);
		      sync(PECTLGROUP, PP_FULL_SYSTEM);
		      broadcast(PEGROUP, PP_GC_INIT);
/*                    DoGlobalGC();                */
/*		      broadcast(PEGROUP, PP_INIT); */
		      break;

		    case PP_STATS_ON:
		    case PP_STATS_OFF:
		        /* This Function not yet implemented for GUM */
		        break;

		    case PP_FINISH:
		        if (!Finishing) {
		          fprintf(stderr, "\nFinish from %x\n", sender_id);
		    	  Finishing = rtsTrue;
		  	  pvm_initsend(PvmDataDefault);
		  	  pvm_bcast(PEGROUP, PP_FINISH);
		      } else {
		  	  ++PEsTerminated;
		      }
		      if (PEsTerminated >= nPEs) {
		    	  broadcast(PEGROUP, PP_FINISH);
		  	  broadcast(MGRGROUP, PP_FINISH);
		  	  pvm_lvgroup(PECTLGROUP);
		  	  pvm_lvgroup(MGRGROUP);
		  	  pvm_exit();
		  	  exit(EXIT_SUCCESS);
		      }
		      break;

		  case PP_FAIL:
		      fprintf(stderr, "Fail from %x\n", sender_id);
		      if (!Finishing) {
		  	  Finishing = rtsTrue;
		  	  broadcast(PEGROUP, PP_FAIL);
		      }
		      break;

		  default:
		      {
/*		          char *opname = GetOpName(opcode);
		          fprintf(stderr,"Sysman: Unrecognised opcode %s (%x)\n",
                                opname,opcode);	*/
		  	  fprintf(stderr, "Sysman: Unrecognised opcode (%x)\n",
		  	  	opcode);
		      }
		      break;
		  } 	/* switch */
	      }		/* else */
	  }		/* while 1 */
      }      		/* forked Sysman Process */
      else {
            pvmendtask();		/* Disconnect from PVM to avoid confusion: */
					/* executable reconnects  */
   	    *argv[0] = '-';		/* Flag that this is the Main Thread PE */
	    execv(pvmExecutable,argv);	/* Parent task becomes Main Thread PE */
      }
  }			/* argc > 1 */  
}			/* main */

/* Needed here because its used in loads of places like LLComms etc */

void stg_exit(n)
I_ n;
{
    exit(n);
}
