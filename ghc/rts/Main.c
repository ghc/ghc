/* -----------------------------------------------------------------------------
 * $Id: Main.c,v 1.7 1999/05/10 08:23:55 sof Exp $
 *
 * (c) The GHC Team 1998-1999
 *
 * Main function for a standalone Haskell program.
 *
 * ---------------------------------------------------------------------------*/

#define COMPILING_RTS_MAIN

#include "Rts.h"
#include "RtsAPI.h"
#include "RtsFlags.h"
#include "Schedule.h"  /* for MainTSO */
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


#ifndef ENABLE_WIN32_DLL_SUPPORT

/* Hack: we assume that we're building a batch-mode system unless 
 * INTERPRETER is set
 */
# ifndef INTERPRETER /* Hack */
int main(int argc, char *argv[])
{
    SchedulerStatus status;
    startupHaskell(argc,argv);

#  ifndef PAR
    MainTSO = createIOThread(stg_max(BLOCK_SIZE_W,
				     RtsFlags.GcFlags.initialStkSize),
			     (StgClosure *)&mainIO_closure);
    status = schedule(MainTSO,NULL);
#  else
    if (IAmMainThread == rtsTrue) {
    /*Just to show we're alive */
      fprintf(stderr, "Main Thread Started ...\n");
     
      MainTSO = createIOThread(stg_max(BLOCK_SIZE_W,
				       RtsFlags.GcFlags.initialStkSize),
			       (StgClosure *)&mainIO_closure);
      status = schedule(MainTSO,NULL);
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
    shutdownHaskell();
    stg_exit(EXIT_SUCCESS);
}
# endif /* BATCH_MODE */

#else   /* !ENABLE_WIN32_DLL_SUPPORT */

static char* args[] = { "ghcRts" };

BOOL
WINAPI
DllMain ( HINSTANCE hInstance
        , DWORD reason
	, LPVOID reserved
	)
{
  /*
    ToDo: let the user configure RTS options to use
          via the registry.
   */
  switch (reason) {
  case DLL_PROCESS_ATTACH:
    startupHaskell(args,1);
    /* ToDo: gracefully handle startupHaskell() failures.. */
    return TRUE;
  case DLL_PROCESS_DETACH:
    shutdownHaskell();
  }
  return TRUE;
}

#endif /* !ENABLE_WIN32_DLL_SUPPORT */
