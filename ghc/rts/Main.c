/* -----------------------------------------------------------------------------
 * $Id: Main.c,v 1.12 1999/11/02 15:05:58 simonmar Exp $
 *
 * (c) The GHC Team 1998-1999
 *
 * Main function for a standalone Haskell program.
 *
 * ---------------------------------------------------------------------------*/

#define COMPILING_RTS_MAIN

#include "Rts.h"
#include "RtsAPI.h"
#include "SchedAPI.h"
#include "RtsFlags.h"
#include "RtsUtils.h"

#ifdef DEBUG
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

#ifdef HAVE_WINDOWS_H
#include <windows.h>
#endif


/* Hack: we assume that we're building a batch-mode system unless 
 * INTERPRETER is set
 */
# ifndef INTERPRETER /* Hack */
int main(int argc, char *argv[])
{
    SchedulerStatus status;
    startupHaskell(argc,argv);

#  ifndef PAR
    /* ToDo: want to start with a larger stack size */
    status = rts_evalIO((StgClosure *)&mainIO_closure, NULL);
#  else
    if (IAmMainThread == rtsTrue) {
    /*Just to show we're alive */
      fprintf(stderr, "Main Thread Started ...\n");
     
      status = rts_evalIO((StgClosure *)&mainIO_closure, NULL);
    } else {
      WaitForPEOp(PP_FINISH,SysManTask);
      exit(EXIT_SUCCESS);
    }
#  endif /* PAR */
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
    shutdownHaskellAndExit(EXIT_SUCCESS);
}
# endif /* BATCH_MODE */
