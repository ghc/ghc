/* ----------------------------------------------------------------------------
   (c) The University of Glasgow 2004

   Interface for code in runProcess.c (providing support for System.Process)
   ------------------------------------------------------------------------- */

#include "HsProcessConfig.h"
// Otherwise these clash with similar definitions from other packages:
#undef PACKAGE_BUGREPORT
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION

#if defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32)
#define UNICODE
#include <windows.h>
#include <stdlib.h>
#include <stdbool.h>
#endif

#include <unistd.h>
#include <sys/types.h>

#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif

#ifdef HAVE_VFORK_H
#include <vfork.h>
#endif

#if defined(HAVE_WORKING_VFORK)
#define myfork vfork
#elif defined(HAVE_WORKING_FORK)
#define myfork fork
// We don't need a fork command on Windows
#elif !(defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32))
#error Cannot find a working fork command
#endif

#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif

#if !(defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32))
typedef pid_t ProcHandle;
#else
// Should really be intptr_t, but we don't have that type on the Haskell side
typedef PHANDLE ProcHandle;
#endif

#include "processFlags.h"

#if !(defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32))

extern ProcHandle runInteractiveProcess( char *const args[],
                                         char *workingDirectory,
                                         char **environment,
                                         int fdStdIn,
                                         int fdStdOut,
                                         int fdStdErr,
                                         int *pfdStdInput,
                                         int *pfdStdOutput,
                                         int *pfdStdError,
                                         gid_t *childGroup,
                                         uid_t *childUser,
                                         int reset_int_quit_handlers,
                                         int flags,
                                         char **failed_doing);

#else

extern ProcHandle runInteractiveProcess( wchar_t *cmd,
                                         wchar_t *workingDirectory,
                                         wchar_t *environment,
                                         int fdStdIn,
                                         int fdStdOut,
                                         int fdStdErr,
                                         int *pfdStdInput,
                                         int *pfdStdOutput,
                                         int *pfdStdError,
                                         int flags,
                                         bool useJobObject,
                                         HANDLE *hJob,
                                         HANDLE *hIOcpPort );

typedef void(*setterDef)(DWORD, HANDLE);
typedef HANDLE(*getterDef)(DWORD);

extern int terminateJob( ProcHandle handle );
extern int waitForJobCompletion( HANDLE hJob, HANDLE ioPort, DWORD timeout, int *pExitCode, setterDef set, getterDef get );

#endif

extern int terminateProcess( ProcHandle handle );
extern int getProcessExitCode( ProcHandle handle, int *pExitCode );
extern int waitForProcess( ProcHandle handle, int *ret );
