/*
 *
 * $Id: ghci.c,v 1.2 2001/08/01 21:55:04 sof Exp $
 *
 * ghci wrapper - invokes ghc.exe with the added command-line
 *                option "--interactive".
 *
 * (c) The GHC Team 2001
 *
 * ghc.exe is searched for using the 'normal' search rules
 * for DLLs / EXEs (i.e., first in the same dir as this wrapper,
 * then system dirs, then PATH).
 *
 * To compile:
 *
 *   MSVC:    cl /o ghci.exe /c ghciwrap.c
 *   mingw:   gcc -mno-cygwin -o ghci.exe ghciwrap.c
 *
 * If you want to associate your own icon with the wrapper,
 * here's how to do it:
 *
 *   * Create a one-line .rc file, ghci.rc (say), containing
 *          0 ICON "hsicon.ico"
 *     (subst the string literal for the name of your icon file).
 *   * Compile it up (assuming the .ico file is in the same dir
 *     as the .rc file):
 *
 *         MSVC:    rc /I. ghci.rc /o ghci.res
 *         mingw:   windres -o ghci.res -o ghci.rc -O coff
 *
 *   * Add the resulting .res file to the link line of the wrapper:
 *
 *     MSVC:    cl /o ghci.exe /c ghciwrap.c ghci.res
 *     mingw:   gcc -mno-cygwin -o ghci.exe ghciwrap.c ghci.res
 *
 */

#include <windows.h>
#include <stdio.h>
#include <process.h>
#include <malloc.h>
#include <stdlib.h>

#define BINARY_NAME "ghc.exe"
#define IACTIVE_OPTION "--interactive"

#define errmsg(msg) fprintf(stderr, msg "\n"); fflush(stderr)

int
main(int argc, char** argv)
{
  TCHAR  binPath[FILENAME_MAX+1];
  TCHAR  binPathShort[MAX_PATH+1];
  DWORD  dwSize = FILENAME_MAX;
  DWORD  dwRes;
  TCHAR* szEnd;
  char** new_argv;
  int    i;
  
  /* Locate the binary we want to start up */
  dwRes = 
    SearchPath(NULL,
	       BINARY_NAME,
	       NULL,
	       dwSize,
	       (char*)binPath,
	       &szEnd);
	       
  if (dwRes == 0) {	       
    errmsg("Unable to locate ghc.exe");
    return 1;
  }
  
  dwSize = MAX_PATH;
  /* Turn the path into short form - LFN form causes problems
     when passed in argv[0]. */
  if ( !(GetShortPathName(binPath, binPathShort, dwSize)) ) {
    errmsg("Unable to locate ghc.exe");
    return 1;
  }
  
  new_argv = (char**)malloc(sizeof(char) * (argc + 1 + 1));
  if (new_argv == NULL) {
    errmsg("failed to start up ghc.exe");
    return 1;
  }
  new_argv[0] = binPathShort;

  new_argv[1] = (char*)malloc(sizeof(char) * (strlen(IACTIVE_OPTION) + 1));
  if (new_argv[1]) {
    strcpy(new_argv[1], IACTIVE_OPTION);
  } else {
    errmsg("failed to start up ghc.exe");
    return 1;
  }

  for ( i=1; i < argc; i++ ) {
    new_argv[i+1] = (char*)malloc(sizeof(char) * (strlen(argv[i] + 1)));
    if (new_argv[i+1] == NULL) {
      errmsg("failed to start up ghc.exe");
      return 1;
    } else {
      strcpy(new_argv[i+1], argv[i]);
    }
  }
  new_argv[i+1] = NULL;
  
  /* I was hoping to be able to use execv() here, but
     the MS implementation of said function doesn't appear to
     be quite right (the 'parent' app seems to exit without
     waiting, which is not a spec-fulfilling thing to do).
     
     Cygwin gives me the right behaviour, but does it by
     implementing it in terms of spawnv(), so you pay
     the cost of having to create an extra process.
     
     ==> Just use spawnv().
  */
  return _spawnv(_P_WAIT, binPath, new_argv);
}
