/****************************************************************************

[ParInit.c] Initialising the parallel RTS

     P. Trinder, January 17th 1995.
     An extension based on Kevin Hammond's GRAPH for PVM version
     P. Trinder, July 1997.
     Adapted for the new RTS

****************************************************************************/

#ifdef PAR /* whole file */

#define NON_POSIX_SOURCE /* so says Solaris */

#include "Rts.h"
#include <setjmp.h>
#include "LLC.h"
#include "HLC.h"

/* Global conditions defined here. */

rtsBool
	IAmMainThread = 	rtsFalse,	/* Set for the main thread	*/
	GlobalStopPending =   	rtsFalse;	/* Terminating			*/

/* Task identifiers for various interesting global tasks. */

GLOBAL_TASK_ID IOTask = 0, 		/* The IO Task Id		*/
	       SysManTask = 0, 		/* The System Manager Task Id	*/
	       mytid = 0;		/* This PE's Task Id		*/

REAL_TIME 	main_start_time;	/* When the program started	*/
REAL_TIME   	main_stop_time;	    	/* When the program finished    */
jmp_buf		exit_parallel_system;	/* How to abort from the RTS	*/


/* Flag handling. */

#if 0
rtsBool TraceSparks =    rtsFalse;		/* Enable the spark trace mode 		*/
rtsBool SparkLocally =   rtsFalse;		/* Use local threads if possible 	*/
rtsBool DelaySparks =    rtsFalse;		/* Use delayed sparking 		*/
rtsBool LocalSparkStrategy =   rtsFalse;	/* Either delayed threads or local threads*/
rtsBool GlobalSparkStrategy =   rtsFalse;	/* Export all threads	    	     	*/

rtsBool DeferGlobalUpdates =	 rtsFalse;	/* Defer updating of global nodes	*/
#endif

rtsBool fishing = rtsFalse;                     /* We have no fish out in the stream    */

/* Initialisation Routines */

/*
par_exit defines how to terminate the program.  If the exit code is
non-zero (i.e. an error has occurred), the PE should not halt until
outstanding error messages have been processed.  Otherwise, messages
might be sent to non-existent Task Ids.  The infinite loop will actually
terminate, since STG_Exception will call myexit\tr{(0)} when
it received a PP_FINISH from the system manager task.
*/

void
par_exit(I_ n)			/* NB: "EXIT" is set to "myexit" for parallel world */
{
    GlobalStopPending = rtsTrue;
    SendOp(PP_FINISH, SysManTask);
    if (n != 0) 
      WaitForTermination();
    else
      WaitForPEOp(PP_FINISH, SysManTask);
    PEShutDown();
/*    fprintf(stderr,"PE %lx shutting down, %ld Threads run, %ld Sparks Ignored\n", (W_) mytid, threadId, sparksIgnored); */
   fprintf(stderr,"PE %lx shutting down, %ld Threads run\n", (W_) mytid, threadId); 

    exit(n);
}

void srand48 (long);
time_t time (time_t *);

void
initParallelSystem(void)
{
    /* Don't buffer standard channels... */
    setbuf(stdout,NULL);
    setbuf(stderr,NULL);

    srand48(time(NULL) * getpid());	/*Initialise Random-number generator seed*/
                                        /* Used to select target of FISH message*/
    InitPackBuffer();
    InitMoreBuffers();
}

/* 
 *SynchroniseSystem synchronises the reduction task with the system
 *manager, and initialises the Global address tables (LAGA & GALA)
 */

GLOBAL_TASK_ID *PEs;

void
SynchroniseSystem(void)
{
    int i;

    PEs = PEStartUp(nPEs);

    /* Initialize global address tables */
    initGAtables();

    /* Record the shortened the PE identifiers for LAGA etc. tables */
    for (i = 0; i < nPEs; ++i)
	registerTask(PEs[i]);

}

#endif /* PAR -- whole file */

