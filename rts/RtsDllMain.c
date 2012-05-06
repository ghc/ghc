/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 1999-2000
 *
 * Entry point for RTS-in-a-DLL
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"
#include "RtsAPI.h"
#include "RtsDllMain.h"

#ifdef HAVE_WINDOWS_H
#include <windows.h>
#endif

/* I'd be mildly surprised if this wasn't defined, but still. */
#if defined(COMPILING_WINDOWS_DLL)
BOOL
WINAPI
DllMain ( HINSTANCE hInstance STG_UNUSED
        , DWORD reason
	, LPVOID reserved STG_UNUSED
	)
{
  /*
   * Note: the DllMain() doesn't call startupHaskell() for you,
   *       that is the task of users of the RTS. The reason is
   *       that *you* want to be able to control the arguments
   *       you pass to the RTS.
   */
  switch (reason) {
  
  // shutdownHaskelAndExit() is already being called,
  //	so I don't think we need this. BL 2009/11/17
 
  //case DLL_PROCESS_DETACH: shutdownHaskell();
  }
  return TRUE;
}

#endif
