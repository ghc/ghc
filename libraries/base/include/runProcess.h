/* ----------------------------------------------------------------------------
   (c) The University of Glasgow 2004

   Interface for code in runProcess.c (providing support for System.Process)
   ------------------------------------------------------------------------- */

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
typedef pid_t ProcHandle;
#else
// Should really be intptr_t, but we don't have that type on the Haskell side
typedef long ProcHandle;
#endif

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)

extern ProcHandle runProcess( char *const args[], 
			      char *workingDirectory, char **environment, 
			      int fdStdInput, int fdStdOutput, int fdStdError);

extern ProcHandle runInteractiveProcess( char *const args[], 
					 char *workingDirectory, 
					 char **environment, 
					 int *pfdStdInput, 
					 int *pfdStdOutput, 
					 int *pfdStdError);

#else

extern ProcHandle runProcess( char *cmd, 
			      char *workingDirectory, void *environment, 
			      int fdStdInput, int fdStdOutput, int fdStdError);

extern ProcHandle runInteractiveProcess( char *cmd, 
					 char *workingDirectory, 
					 void *environment,
					 int *pfdStdInput,
					 int *pfdStdOutput,
					 int *pfdStdError);

#endif

extern int terminateProcess( ProcHandle handle );
extern int getProcessExitCode( ProcHandle handle, int *pExitCode );
extern int waitForProcess( ProcHandle handle );
