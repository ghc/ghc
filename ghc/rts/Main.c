/* -----------------------------------------------------------------------------
 * $Id: Main.c,v 1.2 1998/12/02 13:28:30 simonm Exp $
 *
 * Main function for a standalone Haskell program.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "RtsAPI.h"
#include "Schedule.h"  /* for MainTSO */
#include "RtsUtils.h"

#ifdef DEBUG
#include "RtsFlags.h"  /* for debugging flags */
#include "Printer.h"   /* for printing        */
#endif

#ifdef INTERPRETER
#include "Assembler.h"
#endif

#ifdef PAR
#include "ParInit.h"
#include "Parallel.h"
#include "LLC.h"
#endif

/* Hack: we assume that we're building a batch-mode system unless 
 * INTERPRETER is set
 */
#ifndef INTERPRETER /* Hack */
int main(int argc, char *argv[])
{
    SchedulerStatus status;
    startupHaskell(argc,argv);

#ifndef PAR
    MainTSO = createIOThread(BLOCK_SIZE_W,(StgClosure *)&mainIO_closure);
    status = schedule(MainTSO,NULL);
#else
    if (IAmMainThread == rtsTrue) {
    /*Just to show we're alive */
      fprintf(stderr, "Main Thread Started ...\n");
     
      MainTSO = createIOThread(BLOCK_SIZE_W,(StgClosure *)&mainIO_closure);
      status = schedule(MainTSO,NULL);
    } else {
      WaitForPEOp(PP_FINISH,SysManTask);
      exit(EXIT_SUCCESS);
    }
#endif /* PAR */
    switch (status) {
    case AllBlocked:
      barf("Scheduler stopped, all threads blocked");
    case Deadlock:
      shutdownHaskell();
      barf("No threads to run!  Deadlock?");
    case Killed:
      belch("%s: warning: main thread killed", prog_argv[0]);
    case Success:
    case Interrupted:
      /* carry on */
    }
    shutdownHaskell();
    stg_exit(EXIT_SUCCESS);
}
#endif /* BATCH_MODE */
