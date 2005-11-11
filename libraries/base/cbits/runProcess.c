/* ----------------------------------------------------------------------------
   (c) The University of Glasgow 2004
   
   Support for System.Process
   ------------------------------------------------------------------------- */

#include "HsBase.h"

#if defined(mingw32_HOST_OS)
#include <windows.h>
#include <stdlib.h>
#endif

#ifdef HAVE_VFORK_H
#include <vfork.h>
#endif

#ifdef HAVE_VFORK
#define fork vfork
#endif

#ifdef HAVE_SIGNAL_H
#include <signal.h>
#endif

#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
/* ----------------------------------------------------------------------------
   UNIX versions
   ------------------------------------------------------------------------- */

ProcHandle
runProcess (char *const args[], char *workingDirectory, char **environment, 
	    int fdStdInput, int fdStdOutput, int fdStdError,
	    int set_inthandler, long inthandler, 
	    int set_quithandler, long quithandler)
{
    int pid;
    struct sigaction dfl;

    switch(pid = fork())
    {
    case -1:
	return -1;
	
    case 0:
    {
	pPrPr_disableITimers();
	
	if (workingDirectory) {
	    if (chdir (workingDirectory) < 0) {
		return -1;
	    }
	}
	
	/* Set the SIGINT/SIGQUIT signal handlers in the child, if requested 
	 */
        (void)sigemptyset(&dfl.sa_mask);
        dfl.sa_flags = 0;
	if (set_inthandler) {
	    dfl.sa_handler = (void *)inthandler;
	    (void)sigaction(SIGINT, &dfl, NULL);
	}
	if (set_quithandler) {
	    dfl.sa_handler = (void *)quithandler;
	    (void)sigaction(SIGQUIT,  &dfl, NULL);
	}

	dup2 (fdStdInput,  STDIN_FILENO);
	dup2 (fdStdOutput, STDOUT_FILENO);
	dup2 (fdStdError,  STDERR_FILENO);
	
	if (environment) {
	    execvpe(args[0], args, environment);
	} else {
	    execvp(args[0], args);
	}
    }
    _exit(127);
    }
    
    return pid;
}

ProcHandle
runInteractiveProcess (char *const args[], 
		       char *workingDirectory, char **environment,
		       int *pfdStdInput, int *pfdStdOutput, int *pfdStdError)
{
    int pid;
    int fdStdInput[2], fdStdOutput[2], fdStdError[2];

    pipe(fdStdInput);
    pipe(fdStdOutput);
    pipe(fdStdError);

    switch(pid = fork())
    {
    case -1:
	close(fdStdInput[0]);
	close(fdStdInput[1]);
	close(fdStdOutput[0]);
	close(fdStdOutput[1]);
	close(fdStdError[0]);
	close(fdStdError[1]);
	return -1;
	
    case 0:
    {
	pPrPr_disableITimers();
	
	if (workingDirectory) {
	    if (chdir (workingDirectory) < 0) {
		return -1;
	    }
	}
	
	if (fdStdInput[0] != STDIN_FILENO) {
	    dup2 (fdStdInput[0], STDIN_FILENO);
	    close(fdStdInput[0]);
	}

	if (fdStdOutput[1] != STDOUT_FILENO) {
	    dup2 (fdStdOutput[1], STDOUT_FILENO);
	    close(fdStdOutput[1]);
	}

	if (fdStdError[1] != STDERR_FILENO) {
	    dup2 (fdStdError[1], STDERR_FILENO);
	    close(fdStdError[1]);
	}
	
	close(fdStdInput[1]);
	close(fdStdOutput[0]);
	close(fdStdError[0]);
	
	/* the child */
	if (environment) {
	    execvpe(args[0], args, environment);
	} else {
	    execvp(args[0], args);
	}
    }
    _exit(127);
    
    default:
	close(fdStdInput[0]);
	close(fdStdOutput[1]);
	close(fdStdError[1]);
	
	*pfdStdInput  = fdStdInput[1];
	*pfdStdOutput = fdStdOutput[0];
	*pfdStdError  = fdStdError[0];
	break;
    }
    
    return pid;
}

int
terminateProcess (ProcHandle handle)
{
    return (kill(handle, SIGTERM) == 0);
}

int
getProcessExitCode (ProcHandle handle, int *pExitCode)
{
    int wstat, res;
    
    *pExitCode = 0;
    
    if ((res = waitpid(handle, &wstat, WNOHANG)) > 0)
    {
	if (WIFEXITED(wstat))
	{
	    *pExitCode = WEXITSTATUS(wstat);
	    return 1;
	}
	else
	    if (WIFSIGNALED(wstat))
	    {
		errno = EINTR;
		return -1;
	    }
	    else
	    {
		/* This should never happen */
	    }
    }
    
    if (res == 0) return 0;

    if (errno == ECHILD) 
    {
	    *pExitCode = 0;
	    return 1;
    }

    return -1;
}

int waitForProcess (ProcHandle handle)
{
    int wstat;
    
    while (waitpid(handle, &wstat, 0) < 0)
    {
	if (errno != EINTR)
	{
	    return -1;
	}
    }
    
    if (WIFEXITED(wstat))
	return WEXITSTATUS(wstat);
    else
	if (WIFSIGNALED(wstat))
	{
	    return wstat;
	}
	else
	{
	    /* This should never happen */
	}
    
    return -1;
}

#else
/* ----------------------------------------------------------------------------
   Win32 versions
   ------------------------------------------------------------------------- */

/* -------------------- WINDOWS VERSION --------------------- */

/* This is the error table that defines the mapping between OS error
   codes and errno values */

struct errentry {
        unsigned long oscode;           /* OS return value */
        int errnocode;  /* System V error code */
};

static struct errentry errtable[] = {
        {  ERROR_INVALID_FUNCTION,       EINVAL    },  /* 1 */
        {  ERROR_FILE_NOT_FOUND,         ENOENT    },  /* 2 */
        {  ERROR_PATH_NOT_FOUND,         ENOENT    },  /* 3 */
        {  ERROR_TOO_MANY_OPEN_FILES,    EMFILE    },  /* 4 */
        {  ERROR_ACCESS_DENIED,          EACCES    },  /* 5 */
        {  ERROR_INVALID_HANDLE,         EBADF     },  /* 6 */
        {  ERROR_ARENA_TRASHED,          ENOMEM    },  /* 7 */
        {  ERROR_NOT_ENOUGH_MEMORY,      ENOMEM    },  /* 8 */
        {  ERROR_INVALID_BLOCK,          ENOMEM    },  /* 9 */
        {  ERROR_BAD_ENVIRONMENT,        E2BIG     },  /* 10 */
        {  ERROR_BAD_FORMAT,             ENOEXEC   },  /* 11 */
        {  ERROR_INVALID_ACCESS,         EINVAL    },  /* 12 */
        {  ERROR_INVALID_DATA,           EINVAL    },  /* 13 */
        {  ERROR_INVALID_DRIVE,          ENOENT    },  /* 15 */
        {  ERROR_CURRENT_DIRECTORY,      EACCES    },  /* 16 */
        {  ERROR_NOT_SAME_DEVICE,        EXDEV     },  /* 17 */
        {  ERROR_NO_MORE_FILES,          ENOENT    },  /* 18 */
        {  ERROR_LOCK_VIOLATION,         EACCES    },  /* 33 */
        {  ERROR_BAD_NETPATH,            ENOENT    },  /* 53 */
        {  ERROR_NETWORK_ACCESS_DENIED,  EACCES    },  /* 65 */
        {  ERROR_BAD_NET_NAME,           ENOENT    },  /* 67 */
        {  ERROR_FILE_EXISTS,            EEXIST    },  /* 80 */
        {  ERROR_CANNOT_MAKE,            EACCES    },  /* 82 */
        {  ERROR_FAIL_I24,               EACCES    },  /* 83 */
        {  ERROR_INVALID_PARAMETER,      EINVAL    },  /* 87 */
        {  ERROR_NO_PROC_SLOTS,          EAGAIN    },  /* 89 */
        {  ERROR_DRIVE_LOCKED,           EACCES    },  /* 108 */
        {  ERROR_BROKEN_PIPE,            EPIPE     },  /* 109 */
        {  ERROR_DISK_FULL,              ENOSPC    },  /* 112 */
        {  ERROR_INVALID_TARGET_HANDLE,  EBADF     },  /* 114 */
        {  ERROR_INVALID_HANDLE,         EINVAL    },  /* 124 */
        {  ERROR_WAIT_NO_CHILDREN,       ECHILD    },  /* 128 */
        {  ERROR_CHILD_NOT_COMPLETE,     ECHILD    },  /* 129 */
        {  ERROR_DIRECT_ACCESS_HANDLE,   EBADF     },  /* 130 */
        {  ERROR_NEGATIVE_SEEK,          EINVAL    },  /* 131 */
        {  ERROR_SEEK_ON_DEVICE,         EACCES    },  /* 132 */
        {  ERROR_DIR_NOT_EMPTY,          ENOTEMPTY },  /* 145 */
        {  ERROR_NOT_LOCKED,             EACCES    },  /* 158 */
        {  ERROR_BAD_PATHNAME,           ENOENT    },  /* 161 */
        {  ERROR_MAX_THRDS_REACHED,      EAGAIN    },  /* 164 */
        {  ERROR_LOCK_FAILED,            EACCES    },  /* 167 */
        {  ERROR_ALREADY_EXISTS,         EEXIST    },  /* 183 */
        {  ERROR_FILENAME_EXCED_RANGE,   ENOENT    },  /* 206 */
        {  ERROR_NESTING_NOT_ALLOWED,    EAGAIN    },  /* 215 */
        {  ERROR_NOT_ENOUGH_QUOTA,       ENOMEM    }    /* 1816 */
};

/* size of the table */
#define ERRTABLESIZE (sizeof(errtable)/sizeof(errtable[0]))

/* The following two constants must be the minimum and maximum
   values in the (contiguous) range of Exec Failure errors. */
#define MIN_EXEC_ERROR ERROR_INVALID_STARTING_CODESEG
#define MAX_EXEC_ERROR ERROR_INFLOOP_IN_RELOC_CHAIN

/* These are the low and high value in the range of errors that are
   access violations */
#define MIN_EACCES_RANGE ERROR_WRITE_PROTECT
#define MAX_EACCES_RANGE ERROR_SHARING_BUFFER_EXCEEDED

static void maperrno (void)
{
	int i;
	DWORD dwErrorCode;

	dwErrorCode = GetLastError();

	/* check the table for the OS error code */
	for (i = 0; i < ERRTABLESIZE; ++i)
	{
		if (dwErrorCode == errtable[i].oscode)
		{
			errno = errtable[i].errnocode;
			return;
		}
	}

	/* The error code wasn't in the table.  We check for a range of */
	/* EACCES errors or exec failure errors (ENOEXEC).  Otherwise   */
	/* EINVAL is returned.                                          */

	if (dwErrorCode >= MIN_EACCES_RANGE && dwErrorCode <= MAX_EACCES_RANGE)
		errno = EACCES;
	else
		if (dwErrorCode >= MIN_EXEC_ERROR && dwErrorCode <= MAX_EXEC_ERROR)
			errno = ENOEXEC;
		else
			errno = EINVAL;
}

/*
 * Function: mkAnonPipe
 *
 * Purpose:  create an anonymous pipe with read and write ends being
 *           optionally (non-)inheritable.
 */
static BOOL
mkAnonPipe (HANDLE* pHandleIn, BOOL isInheritableIn, 
	    HANDLE* pHandleOut, BOOL isInheritableOut)
{
	HANDLE hTemporaryIn  = NULL;
	HANDLE hTemporaryOut = NULL;
	BOOL status;
	SECURITY_ATTRIBUTES sec_attrs;

	/* Create inheritable security attributes */
	sec_attrs.nLength = sizeof(SECURITY_ATTRIBUTES);
	sec_attrs.lpSecurityDescriptor = NULL;
	sec_attrs.bInheritHandle = TRUE;

	/* Create the anon pipe with both ends inheritable */
	if (!CreatePipe(&hTemporaryIn, &hTemporaryOut, &sec_attrs, 0))
	{
		maperrno();
		*pHandleIn  = NULL;
		*pHandleOut = NULL;
		return FALSE;
	}

	if (isInheritableIn)
		*pHandleIn = hTemporaryIn;
	else
	{
		/* Make the read end non-inheritable */
		status = DuplicateHandle(GetCurrentProcess(), hTemporaryIn,
			      GetCurrentProcess(), pHandleIn,
			      0,
			      FALSE, /* non-inheritable */
			      DUPLICATE_SAME_ACCESS);
		CloseHandle(hTemporaryIn);
		if (!status)
		{
			maperrno();
			*pHandleIn  = NULL;
			*pHandleOut = NULL;
			CloseHandle(hTemporaryOut);
			return FALSE;
		}
	}

	if (isInheritableOut)
		*pHandleOut = hTemporaryOut;
	else
	{
		/* Make the write end non-inheritable */
		status = DuplicateHandle(GetCurrentProcess(), hTemporaryOut,
			      GetCurrentProcess(), pHandleOut,
			      0,
			      FALSE, /* non-inheritable */
			      DUPLICATE_SAME_ACCESS);
		CloseHandle(hTemporaryOut);
		if (!status)
		{
			maperrno();
			*pHandleIn  = NULL;
			*pHandleOut = NULL;
			CloseHandle(*pHandleIn);
      		return FALSE;
    	}
	}

	return TRUE;
}

ProcHandle
runProcess (char *cmd, char *workingDirectory, void *environment,
	    int fdStdInput, int fdStdOutput, int fdStdError)
{
	STARTUPINFO sInfo;
	PROCESS_INFORMATION pInfo;
	DWORD flags;
	char buffer[256];

	ZeroMemory(&sInfo, sizeof(sInfo));
	sInfo.cb = sizeof(sInfo);	
	sInfo.hStdInput = (HANDLE) _get_osfhandle(fdStdInput);
	sInfo.hStdOutput= (HANDLE) _get_osfhandle(fdStdOutput);
	sInfo.hStdError = (HANDLE) _get_osfhandle(fdStdError);

	if (sInfo.hStdInput == INVALID_HANDLE_VALUE)
		sInfo.hStdInput = NULL;
	if (sInfo.hStdOutput == INVALID_HANDLE_VALUE)
		sInfo.hStdOutput = NULL;
	if (sInfo.hStdError == INVALID_HANDLE_VALUE)
		sInfo.hStdError = NULL;

	if (sInfo.hStdInput || sInfo.hStdOutput || sInfo.hStdError)
		sInfo.dwFlags = STARTF_USESTDHANDLES;

	if (sInfo.hStdInput  != GetStdHandle(STD_INPUT_HANDLE)  &&
	    sInfo.hStdOutput != GetStdHandle(STD_OUTPUT_HANDLE) &&
	    sInfo.hStdError  != GetStdHandle(STD_ERROR_HANDLE))
		flags = CREATE_NO_WINDOW;   // Run without console window only when both output and error are redirected
	else
		flags = 0;

	if (!CreateProcess(NULL, cmd, NULL, NULL, TRUE, flags, environment, workingDirectory, &sInfo, &pInfo))
	{
		maperrno();
		return -1;
	}

	CloseHandle(pInfo.hThread);
	return (ProcHandle)pInfo.hProcess;
}

ProcHandle
runInteractiveProcess (char *cmd, char *workingDirectory, void *environment,
		       int *pfdStdInput, int *pfdStdOutput, int *pfdStdError)
{
	STARTUPINFO sInfo;
	PROCESS_INFORMATION pInfo;
	HANDLE hStdInputRead,  hStdInputWrite;
	HANDLE hStdOutputRead, hStdOutputWrite;
	HANDLE hStdErrorRead,  hStdErrorWrite;

	if (!mkAnonPipe(&hStdInputRead,  TRUE, &hStdInputWrite,  FALSE))
		return -1;

	if (!mkAnonPipe(&hStdOutputRead, FALSE, &hStdOutputWrite, TRUE))
	{
		CloseHandle(hStdInputRead);
		CloseHandle(hStdInputWrite);
		return -1;
	}

	if (!mkAnonPipe(&hStdErrorRead,  FALSE, &hStdErrorWrite,  TRUE))
	{
		CloseHandle(hStdInputRead);
		CloseHandle(hStdInputWrite);
		CloseHandle(hStdOutputRead);
		CloseHandle(hStdOutputWrite);
		return -1;
	}

	ZeroMemory(&sInfo, sizeof(sInfo));
	sInfo.cb = sizeof(sInfo);
	sInfo.dwFlags = STARTF_USESTDHANDLES;
	sInfo.hStdInput = hStdInputRead;
	sInfo.hStdOutput= hStdOutputWrite;
	sInfo.hStdError = hStdErrorWrite;

	if (!CreateProcess(NULL, cmd, NULL, NULL, TRUE, CREATE_NO_WINDOW, environment, workingDirectory, &sInfo, &pInfo))
	{
		maperrno();
		CloseHandle(hStdInputRead);
		CloseHandle(hStdInputWrite);
		CloseHandle(hStdOutputRead);
		CloseHandle(hStdOutputWrite);
		CloseHandle(hStdErrorRead);
		CloseHandle(hStdErrorWrite);
		return -1;
	}
	CloseHandle(pInfo.hThread);

	// Close the ends of the pipes that were inherited by the
	// child process.  This is important, otherwise we won't see
	// EOF on these pipes when the child process exits.
	CloseHandle(hStdInputRead);
	CloseHandle(hStdOutputWrite);
	CloseHandle(hStdErrorWrite);

	*pfdStdInput  = _open_osfhandle((intptr_t) hStdInputWrite, _O_WRONLY);
	*pfdStdOutput = _open_osfhandle((intptr_t) hStdOutputRead, _O_RDONLY);
  	*pfdStdError  = _open_osfhandle((intptr_t) hStdErrorRead, _O_RDONLY);

  	return (int) pInfo.hProcess;
}

int
terminateProcess (ProcHandle handle)
{
    if (!TerminateProcess((HANDLE) handle, 1)) {
	maperrno();
	return -1;
    }
    return 0;
}

int
getProcessExitCode (ProcHandle handle, int *pExitCode)
{
    *pExitCode = 0;

    if (WaitForSingleObject((HANDLE) handle, 1) == WAIT_OBJECT_0)
    {
	if (GetExitCodeProcess((HANDLE) handle, (DWORD *) pExitCode) == 0)
	{
	    maperrno();
	    return -1;
	}
	return 1;
    }
    
    return 0;
}

int
waitForProcess (ProcHandle handle)
{
    DWORD retCode;

    if (WaitForSingleObject((HANDLE) handle, INFINITE) == WAIT_OBJECT_0)
    {
	if (GetExitCodeProcess((HANDLE) handle, &retCode) == 0)
	{
	    maperrno();
	    return -1;
	}
	return retCode;
    }
    
    maperrno();
    return -1;
}

#endif /* Win32 */
