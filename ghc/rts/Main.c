/* -----------------------------------------------------------------------------
 * $Id: Main.c,v 1.13 2000/01/13 12:40:15 simonmar Exp $
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
    int exit_status;

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
    case Deadlock:
      prog_belch("no threads to run:  infinite loop or deadlock?");
      exit_status = EXIT_DEADLOCK;
      break;
    case Killed:
      prog_belch("main thread killed");
      exit_status = EXIT_KILLED;
      break;
    case Interrupted:
      prog_belch("interrupted");
      exit_status = EXIT_INTERRUPTED;
      break;
    case Success:
      exit_status = EXIT_SUCCESS;
      break;
    case NoStatus:
      barf("main thread completed with no status");
    }
    shutdownHaskellAndExit(exit_status);
}
# endif /* BATCH_MODE */
