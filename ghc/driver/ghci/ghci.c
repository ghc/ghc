/*
 *
 * $Id: ghci.c,v 1.8 2003/06/12 09:48:17 simonpj Exp $
 *
 * ghci wrapper for Win32 only
 * 
 * This wrapper invokes ghc.exe with the added command-line
 *                option "--interactive".
 * (On Unix this is done by the ghci.sh shell script, but
 *  that does not work so well on Win32.)
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
 *         MSVC:    rc /i. /fo ghci.res ghci.rc 
 *         mingw:   windres -o ghci.res -i ghci.rc -O coff
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
  
  new_argv = (char**)malloc(sizeof(char*) * (argc + 1 + 1));
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
    int len = strlen(argv[i]);
    /* to avoid quoting issues, surround each option in double quotes */
    new_argv[i+1] = (char*)malloc(sizeof(char) * (len + 3));
    if (new_argv[i+1] == NULL) {
      errmsg("failed to start up ghc.exe");
      return 1;
    } else {
      new_argv[i+1][0] = '"';
      strcpy(1 + new_argv[i+1], argv[i]);
      new_argv[i+1][len+1] = '"';
      new_argv[i+1][len+2] = '\0';
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
     Plus, of course, we aren't allowed to use Cygwin here, because
     GHC does not assume Cygwin.
     
     ==> Just use spawnv(), which is provided by msvcrt.dll, the
         Microsoft C runtime to which mingw delegates almost all
	 system calls

	 [Sigbjorn adds 12 Jun 03]
     We probably ought to use CreateProcess() in ghci.c -- or better still an exec()-like
     that didn't have to create a separate process from the wrapper (which is what that
     code comment in there is driving at.) 
      
     CreateProcess() is a more wieldy function to invoke, which is probably why
     I opted for spawnv(). spawnv() performs the equivalent of Prelude.unwords
     (to look at the code itself, or at least an older version, see dospawn.c in the
     vc98/crt/src/ directory of an MSVC6 installation.)
      
     CreateProcess() is a native Win32 API though, which has the merit that it is
     guaranteed to work the same with both the mingw and cygwin ports.
  */
#if 0
  fprintf(stderr, "Invoking ghc: ");
  i=0;
  while (new_argv[i] != NULL) {
    fprintf(stderr, "%s ", new_argv[i++]);
  }
  fprintf(stderr, "\n"); fflush(stderr);
#endif
  return _spawnv(_P_WAIT, binPath, new_argv);
}
