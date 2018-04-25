/* ----------------------------------------------------------------------------
   (c) The University of Glasgow 2004

   Support for System.Process
   ------------------------------------------------------------------------- */

#if defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32)
#define UNICODE
#endif

/* XXX This is a nasty hack; should put everything necessary in this package */
#include "HsBase.h"
#include "Rts.h"

#include "runProcess.h"

#if !(defined(_MSC_VER) || defined(__MINGW32__) || defined(_WIN32))

#include "execvpe.h"

/* ----------------------------------------------------------------------------
   UNIX versions
   ------------------------------------------------------------------------- */

// If a process was terminated by a signal, the exit status we return
// via the System.Process API is (-signum). This encoding avoids collision with
// normal process termination status codes. See also #7229.
#define TERMSIG_EXITSTATUS(s) (-(WTERMSIG(s)))

static long max_fd = 0;

// Rts internal API, not exposed in a public header file:
extern void blockUserSignals(void);
extern void unblockUserSignals(void);

// See #1593.  The convention for the exit code when
// exec() fails seems to be 127 (gleened from C's
// system()), but there's no equivalent convention for
// chdir(), so I'm picking 126 --SimonM.
#define forkChdirFailed 126
#define forkExecFailed  127

// These are arbitrarily chosen -- JP
#define forkSetgidFailed 124
#define forkSetuidFailed 125

__attribute__((__noreturn__))
static void childFailed(int pipe, int failCode) {
    int err;
    ssize_t unused __attribute__((unused));

    err = errno;
    unused = write(pipe, &failCode, sizeof(failCode));
    unused = write(pipe, &err,      sizeof(err));
    // As a fallback, exit with the failCode
    _exit(failCode);
}

ProcHandle
runInteractiveProcess (char *const args[],
                       char *workingDirectory, char **environment,
                       int fdStdIn, int fdStdOut, int fdStdErr,
                       int *pfdStdInput, int *pfdStdOutput, int *pfdStdError,
                       gid_t *childGroup, uid_t *childUser,
                       int reset_int_quit_handlers,
                       int flags,
                       char **failed_doing)
{
    int close_fds = ((flags & RUN_PROCESS_IN_CLOSE_FDS) != 0);
    int pid;
    int fdStdInput[2], fdStdOutput[2], fdStdError[2];
    int forkCommunicationFds[2];
    int r;
    int failCode, err;

    // Ordering matters here, see below [Note #431].
    if (fdStdIn == -1) {
        r = pipe(fdStdInput);
        if (r == -1) {
            *failed_doing = "runInteractiveProcess: pipe";
            return -1;
        }
    }
    if (fdStdOut == -1) {
        r = pipe(fdStdOutput);
        if (r == -1) {
            if (fdStdIn == -1) {
                close(fdStdInput[0]);
                close(fdStdInput[1]);
            }
            *failed_doing = "runInteractiveProcess: pipe";
            return -1;
        }
    }
    if (fdStdErr == -1) {
        r = pipe(fdStdError);
        if (r == -1) {
            *failed_doing = "runInteractiveProcess: pipe";
            if (fdStdIn == -1) {
                close(fdStdInput[0]);
                close(fdStdInput[1]);
            }
            if (fdStdOut == -1) {
                close(fdStdOutput[0]);
                close(fdStdOutput[1]);
            }
            return -1;
        }
    }

    r = pipe(forkCommunicationFds);
    if (r == -1) {
        *failed_doing = "runInteractiveProcess: pipe";
        return -1;
    }

    // Block signals with Haskell handlers.  The danger here is that
    // with the threaded RTS, a signal arrives in the child process,
    // the RTS writes the signal information into the pipe (which is
    // shared between parent and child), and the parent behaves as if
    // the signal had been raised.
    blockUserSignals();

    // See #4074.  Sometimes fork() gets interrupted by the timer
    // signal and keeps restarting indefinitely.
    stopTimer();

    switch(pid = myfork())
    {
    case -1:
        unblockUserSignals();
        startTimer();
        if (fdStdIn == -1) {
            close(fdStdInput[0]);
            close(fdStdInput[1]);
        }
        if (fdStdOut == -1) {
            close(fdStdOutput[0]);
            close(fdStdOutput[1]);
        }
        if (fdStdErr == -1) {
            close(fdStdError[0]);
            close(fdStdError[1]);
        }
        close(forkCommunicationFds[0]);
        close(forkCommunicationFds[1]);
        *failed_doing = "fork";
        return -1;

    case 0:
        // WARNING! We may now be in the child of vfork(), and any
        // memory we modify below may also be seen in the parent
        // process.

        close(forkCommunicationFds[0]);
        fcntl(forkCommunicationFds[1], F_SETFD, FD_CLOEXEC);

        if ((flags & RUN_PROCESS_NEW_SESSION) != 0) {
            setsid();
        }
        if ((flags & RUN_PROCESS_IN_NEW_GROUP) != 0) {
            setpgid(0, 0);
        }

        if ( childGroup) {
            if ( setgid( *childGroup) != 0) {
                // ERROR
                childFailed(forkCommunicationFds[1], forkSetgidFailed);
            }
        }

        if ( childUser) {
            if ( setuid( *childUser) != 0) {
                // ERROR
                childFailed(forkCommunicationFds[1], forkSetuidFailed);
            }
        }

        unblockUserSignals();

        if (workingDirectory) {
            if (chdir (workingDirectory) < 0) {
                childFailed(forkCommunicationFds[1], forkChdirFailed);
            }
        }

        // [Note #431]: Ordering matters here.  If any of the FDs
        // 0,1,2 were initially closed, then our pipes may have used
        // these FDs.  So when we dup2 the pipe FDs down to 0,1,2, we
        // must do it in that order, otherwise we could overwrite an
        // FD that we need later.

        if (fdStdIn == -1) {
            if (fdStdInput[0] != STDIN_FILENO) {
                dup2 (fdStdInput[0], STDIN_FILENO);
                close(fdStdInput[0]);
            }
            close(fdStdInput[1]);
        } else if (fdStdIn == -2) {
            close(STDIN_FILENO);
        } else {
            dup2(fdStdIn,  STDIN_FILENO);
        }

        if (fdStdOut == -1) {
            if (fdStdOutput[1] != STDOUT_FILENO) {
                dup2 (fdStdOutput[1], STDOUT_FILENO);
                close(fdStdOutput[1]);
            }
            close(fdStdOutput[0]);
        } else if (fdStdOut == -2) {
            close(STDOUT_FILENO);
        } else {
            dup2(fdStdOut,  STDOUT_FILENO);
        }

        if (fdStdErr == -1) {
            if (fdStdError[1] != STDERR_FILENO) {
                dup2 (fdStdError[1], STDERR_FILENO);
                close(fdStdError[1]);
            }
            close(fdStdError[0]);
        } else if (fdStdErr == -2) {
            close(STDERR_FILENO);
        } else {
            dup2(fdStdErr,  STDERR_FILENO);
        }

        if (close_fds) {
            int i;
            if (max_fd == 0) {
#if HAVE_SYSCONF
                max_fd = sysconf(_SC_OPEN_MAX);
                if (max_fd == -1) {
                    max_fd = 256;
                }
#else
                max_fd = 256;
#endif
            }
            // XXX Not the pipe
            for (i = 3; i < max_fd; i++) {
                if (i != forkCommunicationFds[1]) {
                    close(i);
                }
            }
        }

        /* Reset the SIGINT/SIGQUIT signal handlers in the child, if requested
         */
        if (reset_int_quit_handlers) {
            struct sigaction dfl;
            (void)sigemptyset(&dfl.sa_mask);
            dfl.sa_flags = 0;
            dfl.sa_handler = SIG_DFL;
            (void)sigaction(SIGINT,  &dfl, NULL);
            (void)sigaction(SIGQUIT, &dfl, NULL);
        }

        /* the child */
        if (environment) {
            // XXX Check result
            execvpe(args[0], args, environment);
        } else {
            // XXX Check result
            execvp(args[0], args);
        }

        childFailed(forkCommunicationFds[1], forkExecFailed);

    default:
        if ((flags & RUN_PROCESS_IN_NEW_GROUP) != 0) {
            setpgid(pid, pid);
        }
        if (fdStdIn  == -1) {
            close(fdStdInput[0]);
            fcntl(fdStdInput[1], F_SETFD, FD_CLOEXEC);
            *pfdStdInput  = fdStdInput[1];
        }
        if (fdStdOut == -1) {
            close(fdStdOutput[1]);
            fcntl(fdStdOutput[0], F_SETFD, FD_CLOEXEC);
            *pfdStdOutput = fdStdOutput[0];
        }
        if (fdStdErr == -1) {
            close(fdStdError[1]);
            fcntl(fdStdError[0], F_SETFD, FD_CLOEXEC);
            *pfdStdError  = fdStdError[0];
        }
        close(forkCommunicationFds[1]);
        fcntl(forkCommunicationFds[0], F_SETFD, FD_CLOEXEC);

        break;
    }

    // If the child process had a problem, then it will tell us via the
    // forkCommunicationFds pipe. First we try to read what the problem
    // was. Note that if none of these conditionals match then we fall
    // through and just return pid.
    r = read(forkCommunicationFds[0], &failCode, sizeof(failCode));
    if (r == -1) {
        *failed_doing = "runInteractiveProcess: read pipe";
        pid = -1;
    }
    else if (r == sizeof(failCode)) {
        // This is the case where we successfully managed to read
        // the problem
        switch (failCode) {
        case forkChdirFailed:
            *failed_doing = "runInteractiveProcess: chdir";
            break;
        case forkExecFailed:
            *failed_doing = "runInteractiveProcess: exec";
            break;
        case forkSetgidFailed:
            *failed_doing = "runInteractiveProcess: setgid";
            break;
        case forkSetuidFailed:
            *failed_doing = "runInteractiveProcess: setuid";
        default:
            *failed_doing = "runInteractiveProcess: unknown";
            break;
        }
        // Now we try to get the errno from the child
        r = read(forkCommunicationFds[0], &err, sizeof(err));
        if (r == -1) {
            *failed_doing = "runInteractiveProcess: read pipe";
        }
        else if (r != sizeof(failCode)) {
            *failed_doing = "runInteractiveProcess: read pipe bad length";
        }
        else {
            // If we succeed then we set errno. It'll be saved and
            // restored again below. Note that in any other case we'll
            // get the errno of whatever else went wrong instead.
            errno = err;
        }

        // We forked the child, but the child had a problem and stopped so it's
        // our responsibility to reap here as nobody else can.
        waitpid(pid, NULL, 0);

        if (fdStdIn == -1) {
            // Already closed fdStdInput[0] above
            close(fdStdInput[1]);
        }
        if (fdStdOut == -1) {
            close(fdStdOutput[0]);
            // Already closed fdStdOutput[1] above
        }
        if (fdStdErr == -1) {
            close(fdStdError[0]);
            // Already closed fdStdError[1] above
        }

        pid = -1;
    }
    else if (r != 0) {
        *failed_doing = "runInteractiveProcess: read pipe bad length";
        pid = -1;
    }

    if (pid == -1) {
        err = errno;
    }

    close(forkCommunicationFds[0]);

    unblockUserSignals();
    startTimer();

    if (pid == -1) {
        errno = err;
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
                *pExitCode = TERMSIG_EXITSTATUS(wstat);
                return 1;
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

int waitForProcess (ProcHandle handle, int *pret)
{
    int wstat;

    if (waitpid(handle, &wstat, 0) < 0)
    {
        return -1;
    }

    if (WIFEXITED(wstat)) {
        *pret = WEXITSTATUS(wstat);
        return 0;
    }
    else {
        if (WIFSIGNALED(wstat))
        {
            *pret = TERMSIG_EXITSTATUS(wstat);
            return 0;
        }
        else
        {
            /* This should never happen */
        }
    }

    return -1;
}

#else
/* ----------------------------------------------------------------------------
   Win32 versions
   ------------------------------------------------------------------------- */

/* -------------------- WINDOWS VERSION --------------------- */

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

    /* Create the anon pipe with both ends inheritable */
    if (!CreatePipe(&hTemporaryIn, &hTemporaryOut, NULL, 0))
    {
        maperrno();
        *pHandleIn  = NULL;
        *pHandleOut = NULL;
        return FALSE;
    }

    if (isInheritableIn) {
        // SetHandleInformation requires at least Win2k
        if (!SetHandleInformation(hTemporaryIn,
                                  HANDLE_FLAG_INHERIT,
                                  HANDLE_FLAG_INHERIT))
        {
            maperrno();
            *pHandleIn  = NULL;
            *pHandleOut = NULL;
            CloseHandle(hTemporaryIn);
            CloseHandle(hTemporaryOut);
            return FALSE;
        }
    }
    *pHandleIn = hTemporaryIn;

    if (isInheritableOut) {
        if (!SetHandleInformation(hTemporaryOut,
                                  HANDLE_FLAG_INHERIT,
                                  HANDLE_FLAG_INHERIT))
        {
            maperrno();
            *pHandleIn  = NULL;
            *pHandleOut = NULL;
            CloseHandle(hTemporaryIn);
            CloseHandle(hTemporaryOut);
            return FALSE;
        }
    }
    *pHandleOut = hTemporaryOut;

    return TRUE;
}

static HANDLE
createJob ()
{
    HANDLE hJob = CreateJobObject (NULL, NULL);
    JOBOBJECT_EXTENDED_LIMIT_INFORMATION jeli;
    ZeroMemory(&jeli, sizeof(JOBOBJECT_EXTENDED_LIMIT_INFORMATION));
    // Configure all child processes associated with the job to terminate when the
    // Last process in the job terminates. This prevent half dead processes.
    jeli.BasicLimitInformation.LimitFlags = JOB_OBJECT_LIMIT_KILL_ON_JOB_CLOSE;

    if (SetInformationJobObject (hJob, JobObjectExtendedLimitInformation,
                                 &jeli, sizeof(JOBOBJECT_EXTENDED_LIMIT_INFORMATION)))
    {
        return hJob;
    }

    maperrno();
    return NULL;
}

static HANDLE
createCompletionPort (HANDLE hJob)
{
    HANDLE ioPort = CreateIoCompletionPort (INVALID_HANDLE_VALUE, NULL, 0, 1);
    if (!ioPort)
    {
        // Something failed. Error is in GetLastError, let caller handler it.
        return NULL;
    }

    JOBOBJECT_ASSOCIATE_COMPLETION_PORT Port;
    Port.CompletionKey = hJob;
    Port.CompletionPort = ioPort;
    if (!SetInformationJobObject(hJob,
        JobObjectAssociateCompletionPortInformation,
        &Port, sizeof(Port))) {
        // Something failed. Error is in GetLastError, let caller handler it.
        return NULL;
    }

    return ioPort;
}

/* Note [Windows exec interaction]

   The basic issue that process jobs tried to solve is this:

   Say you have two programs A and B. Now A calls B. There are two ways to do this.

   1) You can use the normal CreateProcess API, which is what normal Windows code do.
      Using this approach, the current waitForProcess works absolutely fine.
   2) You can call the emulated POSIX function _exec, which of course is supposed to
      allow the child process to replace the parent.

    With approach 2) waitForProcess falls apart because the Win32's process model does
    not allow this the same way as linux. _exec is emulated by first making a call to
    CreateProcess to spawn B and then immediately exiting from A. So you have two
    different processes.

    waitForProcess is waiting on the termination of A. Because A is immediately killed,
    waitForProcess will return even though B is still running. This is why for instance
    the GHC testsuite on Windows had lots of file locked errors.

    This approach creates a new Job and assigned A to the job, but also all future
    processes spawned by A. This allows us to listen in on events, such as, when all
    processes in the job are finished, but also allows us to propagate exit codes from
    _exec calls.

    The only reason we need this at all is because we don't interact with just actual
    native code on Windows, and instead have a lot of ported POSIX code.

    The Job handle is returned to the user because Jobs have additional benefits as well,
    such as allowing you to specify resource limits on the to be spawned process.
 */

ProcHandle
runInteractiveProcess (wchar_t *cmd, wchar_t *workingDirectory,
                       wchar_t *environment,
                       int fdStdIn, int fdStdOut, int fdStdErr,
                       int *pfdStdInput, int *pfdStdOutput, int *pfdStdError,
                       int flags, bool useJobObject, HANDLE *hJob, HANDLE *hIOcpPort)
{
    STARTUPINFO sInfo;
    PROCESS_INFORMATION pInfo;
    HANDLE hStdInputRead   = INVALID_HANDLE_VALUE;
    HANDLE hStdInputWrite  = INVALID_HANDLE_VALUE;
    HANDLE hStdOutputRead  = INVALID_HANDLE_VALUE;
    HANDLE hStdOutputWrite = INVALID_HANDLE_VALUE;
    HANDLE hStdErrorRead   = INVALID_HANDLE_VALUE;
    HANDLE hStdErrorWrite  = INVALID_HANDLE_VALUE;
    BOOL close_fds = ((flags & RUN_PROCESS_IN_CLOSE_FDS) != 0);
    // We always pass a wide environment block, so we MUST set this flag
    DWORD dwFlags = CREATE_UNICODE_ENVIRONMENT;
    BOOL status;
    BOOL inherit;

    ZeroMemory(&sInfo, sizeof(sInfo));
    sInfo.cb = sizeof(sInfo);
    sInfo.dwFlags = STARTF_USESTDHANDLES;
    ZeroMemory(&pInfo, sizeof(pInfo));

    if (fdStdIn == -1) {
        if (!mkAnonPipe(&hStdInputRead,  TRUE, &hStdInputWrite,  FALSE))
            goto cleanup_err;
        sInfo.hStdInput = hStdInputRead;
    } else if (fdStdIn == -2) {
        sInfo.hStdInput = NULL;
    } else if (fdStdIn == 0) {
        // Don't duplicate stdin, as console handles cannot be
        // duplicated and inherited. urg.
        sInfo.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
    } else {
        // The handle might not be inheritable, so duplicate it
        status = DuplicateHandle(GetCurrentProcess(),
                                 (HANDLE) _get_osfhandle(fdStdIn),
                                 GetCurrentProcess(), &hStdInputRead,
                                 0,
                                 TRUE, /* inheritable */
                                 DUPLICATE_SAME_ACCESS);
        if (!status) goto cleanup_err;
        sInfo.hStdInput = hStdInputRead;
    }

    if (fdStdOut == -1) {
        if (!mkAnonPipe(&hStdOutputRead,  FALSE, &hStdOutputWrite,  TRUE))
            goto cleanup_err;
        sInfo.hStdOutput = hStdOutputWrite;
    } else if (fdStdOut == -2) {
        sInfo.hStdOutput = NULL;
    } else if (fdStdOut == 1) {
        // Don't duplicate stdout, as console handles cannot be
        // duplicated and inherited. urg.
        sInfo.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
    } else {
        // The handle might not be inheritable, so duplicate it
        status = DuplicateHandle(GetCurrentProcess(),
                                 (HANDLE) _get_osfhandle(fdStdOut),
                                 GetCurrentProcess(), &hStdOutputWrite,
                                 0,
                                 TRUE, /* inheritable */
                                 DUPLICATE_SAME_ACCESS);
        if (!status) goto cleanup_err;
        sInfo.hStdOutput = hStdOutputWrite;
    }

    if (fdStdErr == -1) {
        if (!mkAnonPipe(&hStdErrorRead,  TRUE, &hStdErrorWrite,  TRUE))
            goto cleanup_err;
        sInfo.hStdError = hStdErrorWrite;
    } else if (fdStdErr == -2) {
        sInfo.hStdError = NULL;
    } else if (fdStdErr == 2) {
        // Don't duplicate stderr, as console handles cannot be
        // duplicated and inherited. urg.
        sInfo.hStdError = GetStdHandle(STD_ERROR_HANDLE);
    } else {
        /* The handle might not be inheritable, so duplicate it */
        status = DuplicateHandle(GetCurrentProcess(),
                                 (HANDLE) _get_osfhandle(fdStdErr),
                                 GetCurrentProcess(), &hStdErrorWrite,
                                 0,
                                 TRUE, /* inheritable */
                                 DUPLICATE_SAME_ACCESS);
        if (!status) goto cleanup_err;
        sInfo.hStdError = hStdErrorWrite;
    }

    if (sInfo.hStdInput  != GetStdHandle(STD_INPUT_HANDLE)  &&
        sInfo.hStdOutput != GetStdHandle(STD_OUTPUT_HANDLE) &&
        sInfo.hStdError  != GetStdHandle(STD_ERROR_HANDLE)  &&
        (flags & RUN_PROCESS_IN_NEW_GROUP) == 0)
            dwFlags |= CREATE_NO_WINDOW;   // Run without console window only when both output and error are redirected

    // See #3231
    if (close_fds && fdStdIn == 0 && fdStdOut == 1 && fdStdErr == 2) {
        inherit = FALSE;
    } else {
        inherit = TRUE;
    }

    if ((flags & RUN_PROCESS_IN_NEW_GROUP) != 0) {
        dwFlags |= CREATE_NEW_PROCESS_GROUP;
    }
    if ((flags & RUN_PROCESS_DETACHED) != 0) {
        dwFlags |= DETACHED_PROCESS;
    }
    if ((flags & RUN_PROCESS_NEW_CONSOLE) != 0) {
        dwFlags |= CREATE_NEW_CONSOLE;
    }

    /* If we're going to use a job object, then we have to create
       the thread suspended.
       See Note [Windows exec interaction].  */
    if (useJobObject)
    {
        dwFlags |= CREATE_SUSPENDED;
        *hJob = createJob();
        if (!*hJob)
        {
            goto cleanup_err;
        }

        // Create the completion port and attach it to the job
        *hIOcpPort = createCompletionPort(*hJob);
        if (!*hIOcpPort)
        {
            goto cleanup_err;
        }
    } else {
        *hJob      = NULL;
        *hIOcpPort = NULL;
    }

    if (!CreateProcess(NULL, cmd, NULL, NULL, inherit, dwFlags, environment, workingDirectory, &sInfo, &pInfo))
    {
            goto cleanup_err;
    }

    if (useJobObject && hJob && *hJob)
    {
        // Then associate the process and the job;
        if (!AssignProcessToJobObject (*hJob, pInfo.hProcess))
        {
            goto cleanup_err;
        }

        // And now that we've associated the new process with the job
        // we can actively resume it.
        ResumeThread (pInfo.hThread);
    }

    CloseHandle(pInfo.hThread);

    // Close the ends of the pipes that were inherited by the
    // child process.  This is important, otherwise we won't see
    // EOF on these pipes when the child process exits.
    if (hStdInputRead   != INVALID_HANDLE_VALUE) CloseHandle(hStdInputRead);
    if (hStdOutputWrite != INVALID_HANDLE_VALUE) CloseHandle(hStdOutputWrite);
    if (hStdErrorWrite  != INVALID_HANDLE_VALUE) CloseHandle(hStdErrorWrite);

    *pfdStdInput  = _open_osfhandle((intptr_t) hStdInputWrite, _O_WRONLY);
    *pfdStdOutput = _open_osfhandle((intptr_t) hStdOutputRead, _O_RDONLY);
    *pfdStdError  = _open_osfhandle((intptr_t) hStdErrorRead,  _O_RDONLY);

    return pInfo.hProcess;

cleanup_err:
    if (hStdInputRead   != INVALID_HANDLE_VALUE) CloseHandle(hStdInputRead);
    if (hStdInputWrite  != INVALID_HANDLE_VALUE) CloseHandle(hStdInputWrite);
    if (hStdOutputRead  != INVALID_HANDLE_VALUE) CloseHandle(hStdOutputRead);
    if (hStdOutputWrite != INVALID_HANDLE_VALUE) CloseHandle(hStdOutputWrite);
    if (hStdErrorRead   != INVALID_HANDLE_VALUE) CloseHandle(hStdErrorRead);
    if (hStdErrorWrite  != INVALID_HANDLE_VALUE) CloseHandle(hStdErrorWrite);
    if (useJobObject && hJob      && *hJob     ) CloseHandle(*hJob);
    if (useJobObject && hIOcpPort && *hIOcpPort) CloseHandle(*hIOcpPort);

    maperrno();
    return NULL;
}

int
terminateProcess (ProcHandle handle)
{
    if (!TerminateProcess ((HANDLE) handle, 1)) {
        maperrno();
        return -1;
    }
    return 0;
}

int
terminateJob (ProcHandle handle)
{
    if (!TerminateJobObject ((HANDLE)handle, 1)) {
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
waitForProcess (ProcHandle handle, int *pret)
{
    DWORD retCode;

    if (WaitForSingleObject((HANDLE) handle, INFINITE) == WAIT_OBJECT_0)
    {
        if (GetExitCodeProcess((HANDLE) handle, &retCode) == 0)
        {
            maperrno();
            return -1;
        }
        *pret = retCode;
        return 0;
    }

    maperrno();
    return -1;
}


int
waitForJobCompletion ( HANDLE hJob, HANDLE ioPort, DWORD timeout, int *pExitCode, setterDef set, getterDef get )
{
    DWORD CompletionCode;
    ULONG_PTR CompletionKey;
    LPOVERLAPPED Overlapped;
    *pExitCode = 0;

    // We have to loop here. It's a blocking call, but
    // we get notified on each completion event. So if it's
    // not one we care for we should just block again.
    // If all processes are finished before this call is made
    // then the initial call will return false.
    // List of events we can listen to:
    // https://msdn.microsoft.com/en-us/library/windows/desktop/ms684141(v=vs.85).aspx
    while (GetQueuedCompletionStatus (ioPort, &CompletionCode,
           &CompletionKey, &Overlapped, timeout)) {

        switch (CompletionCode)
        {
            case JOB_OBJECT_MSG_NEW_PROCESS:
            {
                // A new child process is born.
                // Retrieve and save the process handle from the process id.
                // We'll need it for later but we can't retrieve it after the
                // process has exited.
                DWORD pid    = (DWORD)(uintptr_t)Overlapped;
                HANDLE pHwnd = OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, TRUE, pid);
                set(pid, pHwnd);
            }
            break;
            case JOB_OBJECT_MSG_ABNORMAL_EXIT_PROCESS:
            case JOB_OBJECT_MSG_EXIT_PROCESS:
            {
                // A child process has just exited.
                // Read exit code, We assume the last process to exit
                // is the process whose exit code we're interested in.
                HANDLE pHwnd = get((DWORD)(uintptr_t)Overlapped);
                if (GetExitCodeProcess(pHwnd, (DWORD *)pExitCode) == 0)
                {
                    maperrno();
                    return 1;
                }
            }
            break;
            case JOB_OBJECT_MSG_ACTIVE_PROCESS_ZERO:
                // All processes in the tree are done.
                return 0;
            default:
                break;
        }
    }

    // Check to see if a timeout has occurred or that the
    // all processes in the job were finished by the time we
    // got to the loop.
    if (Overlapped == NULL && (HANDLE)CompletionKey != hJob)
    {
        // Timeout occurred.
        return -1;
    }

    return 0;
}

#endif /* Win32 */
