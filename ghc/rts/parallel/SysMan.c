/* ----------------------------------------------------------------------------
   Time-stamp: <Wed Mar 21 2001 17:16:28 Stardate: [-30]6363.59 hwloidl>

   GUM System Manager Program
   Handles startup, shutdown and global synchronisation of the parallel system.

   The Parade/AQUA Projects, Glasgow University, 1994-1995.
   GdH/APART Projects, Heriot-Watt University, Edinburgh, 1997-2000.
 
   ------------------------------------------------------------------------- */

//@node GUM System Manager Program, , , 
//@section GUM System Manager Program

//@menu
//* General docu::		
//* Includes::			
//* Macros etc::		
//* Variables::			
//* Prototypes::		
//* Aux startup and shutdown fcts::  
//* Main fct::			
//* Message handlers::		
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
o  Wait for all the PE-tasks to reply back saying they are ready and if they were the
  main thread or not.
o Broadcast an array of the PE task-ids out to all of the PE-tasks.
o Enter a loop awaiting incoming messages, e.g. failure, Garbage-collection, 
  termination.

The forked Main-thread algorithm, in SysMan, is as follows.

o disconnects from PVM.
o sets a flag in argv to indicate that it is the main thread.
o `exec's a copy of the pvm-executable (i.e. the program being run)


The pvm-executable run by each PE-task, is initialised as follows.

o Registers with PVM, obtaining a task-id.
o If it was main it gets SysMan's task-id from argv otherwise it can use pvm_parent.
oSends a ready message to SysMan together with a flag indicating if it was main or not.
o Receives from SysMan the array of task-ids of the other PEs.
o If the number of task-ids sent was larger than expected then it must have been a task
  generated after the rest of the program had started, so it sends its own task-id message
  to all the tasks it was told about.
o Begins execution.

*/

//@node Includes, Macros etc, General docu, GUM System Manager Program
//@subsection Includes

/* Evidently not Posix */
/* #include "PosixSource.h" */

#include "Rts.h"
#include "ParTypes.h"
#include "LLC.h"
#include "Parallel.h"
#include "ParallelRts.h" // stats only

//@node Macros etc, Variables, Includes, GUM System Manager Program
//@subsection Macros etc

/* SysMan is put on top of the GHC routine that does the RtsFlags handling.
   So, we cannot use the standard macros. For the time being we use a macro
   that is fixed at compile time.
*/

#ifdef IF_PAR_DEBUG
#undef IF_PAR_DEBUG
#endif
    
/* debugging enabled */
//#define IF_PAR_DEBUG(c,s)  { s; } 
/* debugging disabled */
#define IF_PAR_DEBUG(c,s)  /* nothing */

void *stgMallocBytes (int n, char *msg);

//@node Variables, Prototypes, Macros etc, GUM System Manager Program
//@subsection Variables

/*
   The following definitions included so that SysMan can be linked with Low
   Level Communications module (LLComms). They are not used in SysMan.  
*/
GlobalTaskId         mytid; 

static unsigned      PEsArrived = 0;
static GlobalTaskId  gtids[MAX_PES];
static GlobalTaskId  sysman_id, sender_id;
static unsigned      PEsTerminated = 0;
static rtsBool       Finishing = rtsFalse;
static long          PEbuffer[MAX_PES];
nat                  nSpawn = 0;    // current no. of spawned tasks (see gtids)
nat                  nPEs = 0;      // number of PEs specified on startup
nat                  nextPE;
/* PVM-ish variables */
char                 *petask, *pvmExecutable;
char                 **pargv;
int                  cc, spawn_flag = PvmTaskDefault;

#if 0 && defined(PAR_TICKY)
/* ToDo: use allGlobalParStats to collect stats of all PEs */
GlobalParStats *allGlobalParStats[MAX_PES];
#endif

//@node Prototypes, Aux startup and shutdown fcts, Variables, GUM System Manager Program
//@subsection Prototypes

/* prototypes for message handlers called from the main loop of SysMan */
void newPE(int nbytes, int opcode, int sender_id);
void readyPE(int nbytes, int opcode, int sender_id);
void finishPE(int nbytes, int opcode, int sender_id, int exit_code);

//@node Aux startup and shutdown fcts, Main fct, Prototypes, GUM System Manager Program
//@subsection Aux startup and shutdown fcts

/* 
   Create the PE Tasks. We spawn (nPEs-1) pvm threads: the Main Thread 
   (which starts execution and performs IO) is created by forking SysMan 
*/
static int
createPEs(int total_nPEs) {
  int i, spawn_nPEs, iSpawn = 0, nArch, nHost;
  struct pvmhostinfo *hostp; 
  int sysman_host;

  spawn_nPEs = total_nPEs-1;
  if (spawn_nPEs > 0) {
    IF_PAR_DEBUG(verbose,
		 fprintf(stderr, "==== [%x] Spawning %d PEs(%s) ...\n", 
			 sysman_id, spawn_nPEs, petask);
		 fprintf(stderr, "  args: ");
		 for (i = 0; pargv[i]; ++i)
		   fprintf(stderr, "%s, ", pargv[i]);
		 fprintf(stderr, "\n"));

    pvm_config(&nHost,&nArch,&hostp);
    sysman_host=pvm_tidtohost(sysman_id);
	
    /* create PEs on the specific machines in the specified order! */
    for (i=0; (iSpawn<spawn_nPEs) && (i<nHost); i++)
      if (hostp[i].hi_tid != sysman_host) { 
	checkComms(pvm_spawn(petask, pargv, spawn_flag+PvmTaskHost, 
			     hostp[i].hi_name, 1, gtids+iSpawn),
		   "SysMan startup");
	IF_PAR_DEBUG(verbose,
		     fprintf(stderr, "==== [%x] Spawned PE %d onto %s\n",
			     sysman_id, i, hostp[i].hi_name));
	iSpawn++;
      }
      
    /* create additional PEs anywhere you like */
    if (iSpawn<spawn_nPEs) { 
      checkComms(pvm_spawn(petask, pargv, spawn_flag, "", 
			   spawn_nPEs-iSpawn, gtids+iSpawn),
		 "SysMan startup");
	IF_PAR_DEBUG(verbose,
		     fprintf(stderr,"==== [%x] Spawned %d additional PEs anywhere\n",
			     sysman_id, spawn_nPEs-iSpawn));
      }   
    }

#if 0
  /* old code with random placement of PEs; make that a variant? */
# error "Broken startup in SysMan"
  { /* let pvm place the PEs anywhere; not used anymore */
    checkComms(pvm_spawn(petask, pargv, spawn_flag, "", spawn_nPEs, gtids),"SysMan startup");
    IF_PAR_DEBUG(verbose,
		 fprintf(stderr,"==== [%x] Spawned\n", sysman_id));
    
  }
#endif    

  // iSpawn=spawn_nPEs; 

  return iSpawn;
}

/* 
   Check if this pvm task is in the list of tasks we spawned and are waiting 
   on, if so then remove it.
*/

static rtsBool 
alreadySpawned (GlobalTaskId g) { 
  unsigned int i;

  for (i=0; i<nSpawn; i++)
    if (g==gtids[i]) { 
      nSpawn--;
      gtids[i] = gtids[nSpawn];  //the last takes its place
      return rtsTrue;
    }
  return rtsFalse;
}

static void 
broadcastFinish(void) { 
  int i,j;
  int tids[MAX_PES];  /* local buffer of all surviving PEs */

  for (i=0, j=0; i<nPEs; i++) 
    if (PEbuffer[i]) 
      tids[j++]=PEbuffer[i]; //extract valid tids

  IF_PAR_DEBUG(verbose,
    fprintf(stderr,"==== [%x] Broadcasting Finish to %d PEs; initiating shutdown\n", 
	sysman_id, j));

  /* ToDo: move into LLComms.c */	    	    	    
  pvm_initsend(PvmDataDefault);
  pvm_mcast(tids,j,PP_FINISH);
}

static void 
broadcastPEtids (void) { 
  nat i; 

  IF_PAR_DEBUG(verbose,
    fprintf(stderr,"==== [%x] SysMan sending PE table to all PEs\n", sysman_id);
    /* debugging */
    fprintf(stderr,"++++ [%x] PE table as seen by SysMan:\n", mytid);
    for (i = 0; i < nPEs; i++) { 
      fprintf(stderr,"++++ PEbuffer[%d] = %x\n", i, PEbuffer[i]);
    }      	
  )

  broadcastOpN(PP_PETIDS, PEGROUP, nPEs, &PEbuffer);
}

//@node Main fct, Message handlers, Aux startup and shutdown fcts, GUM System Manager Program
//@subsection Main fct

//@cindex main
int 
main (int argc, char **argv) {
  int rbufid;
  int opcode, nbytes, nSpawn;
  unsigned int i;
  
  setbuf(stdout, NULL);  // disable buffering of stdout
  setbuf(stderr, NULL);  // disable buffering of stderr

  IF_PAR_DEBUG(verbose,
	       fprintf(stderr,
	               "==== RFP: GdH enabled SysMan reporting for duty\n"));
  
  if (argc > 1) {
    if (*argv[1] == '-') {
      spawn_flag = PvmTaskDebug;
      argv[1] = argv[0];
      argv++; argc--;
    }
    sysman_id = pvm_mytid();  /* This must be the first PVM call */
    
    if (sysman_id<0) { 
	fprintf(stderr, "==== PVM initialisation failure\n");  
      	exit(EXIT_FAILURE);  
    }
    
    /* 
       Get the full path and filename of the pvm executable (stashed in some
       PVM directory), and the number of PEs from the command line.
    */
    pvmExecutable = argv[1];
    nPEs = atoi(argv[2]);
    
    if (nPEs==0) { 
      /* as usual 0 means infinity: use all PEs specified in PVM config */
      int nArch, nHost;
      struct pvmhostinfo *hostp; 

      /* get info on PVM config */
      pvm_config(&nHost,&nArch,&hostp);
      nPEs=nHost;
      sprintf(argv[2],"%d",nPEs); /* ToCheck: does this work on all archs */
    }	

    /* get the name of the binary to execute */
    if ((petask = getenv(PETASK)) == NULL)  // PETASK set by driver
      petask = PETASK;

    IF_PAR_DEBUG(verbose,
		 fprintf(stderr,"==== [%x] nPEs: %d; executable: |%s|\n", 
			sysman_id, nPEs, petask));
    
    /* Check that we can create the number of PE and IMU tasks requested.
                                                     ^^^
       This comment is most entertaining since we haven't been using IMUs 
       for the last 10 years or so -- HWL */
    if ((nPEs > MAX_PES) || (nPEs<1)) {
      fprintf(stderr,"==** SysMan: No more than %d PEs allowed (%d requested)\n     Reconfigure GUM setting MAX_PE in ghc/includes/Parallel.h to a higher value\n", 
	   MAX_PES, nPEs);
      exit(EXIT_FAILURE);
    }

    IF_PAR_DEBUG(verbose,
		   fprintf(stderr,"==== [%x] is SysMan Task\n", sysman_id));

    /* Initialise the PE task arguments from Sysman's arguments */
    pargv = argv + 2;

    /* Initialise list of all PE identifiers */
    PEsArrived=0;  
    nextPE=1;
    for (i=0; i<nPEs; i++)
      PEbuffer[i]=0;
    
    /* start up the required number of PEs */
    nSpawn = createPEs(nPEs);
    
    /* 
       Create the MainThread PE by forking SysMan. This arcane coding 
       is required to allow MainThread to read stdin and write to stdout.
       PWT 18/1/96 
    */
    //nPEs++;                /* Record that the number of PEs is increasing */
    if ((cc = fork())) {
      checkComms(cc,"SysMan fork");         /* Parent continues as SysMan */
                  
      PEbuffer[0]=0;    /* we accept the first main and assume its valid. */
      PEsArrived=1;     /* assume you've got main                         */

      IF_PAR_DEBUG(verbose,
		   fprintf(stderr,"==== [%x] Sysman successfully initialized!\n",
			   sysman_id));

//@cindex message handling loop
      /* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
      /* Main message handling loop                                         */
      /* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
      /* Process incoming messages */
      while (1) {
	if ((rbufid = pvm_recv(ANY_TASK, ANY_OPCODE)) < 0) {
	  pvm_perror("==** Sysman: Receiving Message (pvm_recv)");
          /* never reached */
        }

	pvm_bufinfo(rbufid, &nbytes, &opcode, &sender_id);

        /* very low level debugging
	IF_PAR_DEBUG(verbose,
	             fprintf(stderr,"== [%x] SysMan: Message received by SysMan: rbufid=%x, nbytes = %d, opcode = %x, sender_id = %x\n",
                     sysman_id, rbufid, nbytes, opcode, sender_id));
	*/

	switch (opcode) {
	    
	  case PP_NEWPE: /* a new PE is registering for work */
	    newPE(nbytes, opcode, sender_id);
	    break;

          case PP_READY: /* startup complete; let PEs start working */
	    readyPE(nbytes, opcode, sender_id);
	    break;

	      
	  case PP_GC_INIT: /* start global GC */
	    /* This Function not yet implemented for GUM */
	    fprintf(stderr,"==** Global GC requested by PE %x. Not yet implemented for GUM!\n", 
		    sender_id);
	    break;
	    
	  case PP_STATS_ON: /* enable statistics gathering */
	    fprintf(stderr,"==** PP_STATS_ON requested by %x. Not yet implemented for GUM!\n", 
		  sender_id);
	    break;

	  case PP_STATS_OFF: /* disable statistics gathering */
	    fprintf(stderr,"==** PP_STATS_OFF requested by %x. Not yet implemented for GUM!\n", 
		  sender_id);
	    break;
	    
	  case PP_FINISH:
	    { 
              int exit_code = getExitCode(nbytes, &sender_id);
	      finishPE(nbytes, opcode, sender_id, exit_code);
	      break;

	  default:
	    {
	     /*		          
	      char *opname = GetOpName(opcode);
	      fprintf(stderr,"Sysman: Unrecognised opcode %s (%x)\n",
	                      opname,opcode);	*/
	      fprintf(stderr,"==** Qagh: Sysman: Unrecognised opcode (%x)\n",
		      opcode);
	    }
	    break;
	  } 	/* switch */
	}	/* else */
      }		/* while 1 */
      /* end of SysMan!! */
    } else {	
      /* forked main thread begins here */
      IF_PAR_DEBUG(verbose,
	           fprintf(stderr, "==== Main Thread PE has been forked; doing an execv(%s,...)\n", 
	           pvmExecutable));
      pvmendtask();		 // Disconnect from PVM to avoid confusion:
                                 // executable reconnects 
      
      // RFP: assumes that length(arvv[0])>=9 !!!
      sprintf(argv[0],"-%08X",sysman_id);  /*flag that its the Main Thread PE and include sysman's id*/
      execv(pvmExecutable,argv); /* Parent task becomes Main Thread PE */
    }           /* else */
  }		/* argc > 1 */  
}		/* main */

//@node Message handlers, Auxiliary fcts, Main fct, GUM System Manager Program
//@subsection Message handlers

/*
   Received PP_NEWPE:
   A new PE has been added to the configuration.
*/
void
newPE(int nbytes, int opcode, int sender_id) { 
  IF_PAR_DEBUG(verbose,
	       fprintf(stderr,"==== [%x] SysMan detected a new host\n",
		       sysman_id));

  /* Determine the new machine... assume its the last on the config list? */
  if (nSpawn < MAX_PES) { 
    int nArch,nHost;
    struct pvmhostinfo *hostp; 

    /* get conmfiguration of PVM machine */
    pvm_config(&nHost,&nArch,&hostp);	      
    nHost--;
    checkComms(pvm_spawn(petask, pargv, spawn_flag+PvmTaskHost, 
			 hostp[nHost].hi_name, 1, gtids+nSpawn),
	       "SysMan loop");
    nSpawn++;
    IF_PAR_DEBUG(verbose,
		 fprintf(stderr, "==== [%x] Spawned onto %s\n",
			 sysman_id, hostp[nHost].hi_name));
  }
}
	  
/* 
   Received PP_READY:
   Let it be known that PE @sender_id@ participates in the computation.
*/
void
readyPE(int nbytes, int opcode, int sender_id) { 
  int i = 0, flag = 1;
  long isMain;
  int nArch, nHost;
  struct pvmhostinfo *hostp; 

  //ASSERT(opcode==PP_READY);

  IF_PAR_DEBUG(verbose,
	       fprintf(stderr,"==== [%x] SysMan received PP_READY message from %x\n",
		       sysman_id, sender_id));

    pvm_config(&nHost,&nArch,&hostp);

  GetArg1(isMain);
	      
  //if ((isMain && (PEbuffer[0]==0)) || alreadySpawned(sender_id)) { 
    if (nPEs >= MAX_PES) { 
      fprintf(stderr,"==== [%x] SysMan doesn't need PE %d (max %d PEs allowed)\n",
	      sysman_id, sender_id, MAX_PES);
      pvm_kill(sender_id); 
    } else { 
      if (isMain) { 
	IF_PAR_DEBUG(verbose,
		     fprintf(stderr,"==== [%x] SysMan found Main PE %x\n", 
			     sysman_id, sender_id));
	PEbuffer[0]=sender_id;
      } else { 
	/* search for PE in list of PEs */
	for(i=1; i<nPEs; i++)
	  if (PEbuffer[i]==sender_id) { 
	    flag=0;
	    break;
	  }
	/* it's a new PE: add it to the list of PEs */
	if (flag)  
	  PEbuffer[nextPE++] = sender_id; 
	
	IF_PAR_DEBUG(verbose,
		     fprintf(stderr,"==== [%x] SysMan: found PE %d as [%x] on host %s\n", 
			     sysman_id, PEsArrived, sender_id, hostp[PEsArrived].hi_name));

	PEbuffer[PEsArrived++] = sender_id;
      }

		
      /* enable better handling of unexpected terminations */
      checkComms( pvm_notify(PvmTaskExit, PP_FINISH, 1, &sender_id),
	          "SysMan loop");

      /* finished registration of all PEs => enable notification */
      if ((PEsArrived==nPEs) && PEbuffer[0]) { 
	checkComms( pvm_notify(PvmHostAdd, PP_NEWPE, -1, 0),
                    "SysMan startup");
	IF_PAR_DEBUG(verbose,
		     fprintf(stderr,"==== [%x] SysMan initialising notificaton for new hosts\n", sysman_id));
      }
		
      /* finished notification => send off the PE ids */
      if ((PEsArrived>=nPEs) && PEbuffer[0]) { 
        if (PEsArrived>nPEs) {
	IF_PAR_DEBUG(verbose,	
		     fprintf(stderr,"==== [%x] Weird: %d PEs registered, but we only asked for %d\n", sysman_id, PEsArrived, nPEs));
	// nPEs=PEsArrived;
        }
	broadcastPEtids();
      }
    }
}

/* 
   Received PP_FINISH:
   Shut down the corresponding PE. Check whether it is a regular shutdown
   or an uncontrolled termination.
*/
void
finishPE(int nbytes, int opcode, int sender_id, int exitCode) { 
  int i;

  IF_PAR_DEBUG(verbose,
	       fprintf(stderr,"==== [%x] SysMan received PP_FINISH message from %x (exit code: %d)\n",
		       sysman_id, sender_id, exitCode));

  /* Is it relevant to us? Count the first message */
  for (i=0; i<nPEs; i++) 
    if (PEbuffer[i] == sender_id) { 
      PEsTerminated++;
      PEbuffer[i]=0; 
	
      /* handle exit code */
      if (exitCode<0) {           /* a task exit before a controlled finish? */
	fprintf(stderr,"==== [%x] Termination at %x with exit(%d)\n", 
		sysman_id, sender_id, exitCode);
      } else if (exitCode>0) {		           /* an abnormal exit code? */
	fprintf(stderr,"==== [%x] Uncontrolled termination at %x with exit(%d)\n", 
		sysman_id, sender_id, exitCode);	
      } else if (!Finishing) {             /* exitCode==0 which is good news */
        if (i!=0) {          /* someone other than main PE terminated first? */
	 fprintf(stderr,"==== [%x] Unexpected early termination at %x\n", 
		 sysman_id, sender_id);	
	} else {
         /* start shutdown by broadcasting FINISH to other PEs */
	 IF_PAR_DEBUG(verbose,
	              fprintf(stderr,"==== [%x] Initiating shutdown (requested by [%x] RIP) (exit code: %d)\n", sysman_id, sender_id, exitCode));
         Finishing = rtsTrue;
         broadcastFinish();
        }
      }	else {
         /* we are in a shutdown already */
	IF_PAR_DEBUG(verbose,
		     fprintf(stderr,"==== [%x] Finish from %x during shutdown (%d PEs terminated so far; %d total)\n", 
			     sysman_id, sender_id, PEsTerminated, nPEs));
      }

      if (PEsTerminated >= nPEs) { 
        IF_PAR_DEBUG(verbose,
          fprintf(stderr,"==== [%x] Global Shutdown, Goodbye!! (SysMan has received FINISHes from all PEs)\n", sysman_id));
        //broadcastFinish();
	/* received finish from everybody; now, we can exit, too */
        exit(EXIT_SUCCESS); /* Qapla'! */
      }
    }
}	
	    
//@node Auxiliary fcts, Index, Message handlers, GUM System Manager Program
//@subsection Auxiliary fcts

/* Needed here because its used in loads of places like LLComms etc */

//@cindex stg_exit

/* 
 * called from STG-land to exit the program
 */

void  
stg_exit(I_ n)
{
  fprintf(stderr, "==// [%x] %s in SysMan code; sending PP_FINISH to all PEs ...\n", 
	    mytid,(n!=0)?"FAILURE":"FINISH");
  broadcastFinish();
  //broadcastFinish();
  pvm_exit();
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
