
/* gcc on mingw is hardcoded to use /mingw (which is c:/mingw) to
   find various files. If this is a different version of mingw to the
   one that we have in the GHC tree then things can go wrong. We
   therefore need to add various -B flags to the gcc commandline,
   so that it uses our in-tree mingw. Hence this wrapper. */

#include "cwrapper.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <windows.h>

void die(const char *fmt, ...) {
    va_list argp;

    va_start(argp, fmt);
    vfprintf(stderr, fmt, argp);
    va_end(argp);
    exit(1);
}

char *mkString(const char *fmt, ...) {
    char *p;
    int i, j;
    va_list argp;

    va_start(argp, fmt);
    i = vsnprintf(NULL, 0, fmt, argp);
    va_end(argp);

    if (i < 0) {
        die("vsnprintf 0 failed: errno %d: %s\n", errno, strerror(errno));
    }

    p = malloc(i + 1);
    if (p == NULL) {
        die("malloc failed: errno %d: %s\n", errno, strerror(errno));
    }

    va_start(argp, fmt);
    j = vsnprintf(p, i + 1, fmt, argp);
    va_end(argp);
    if (j < 0) {
        die("vsnprintf with %d failed: errno %d: %s\n",
            i + 1, errno, strerror(errno));
    }

    return p;
}

char *flattenAndQuoteArgs(char *ptr, int argc, char *argv[])
{
    int i;
    char *src;

    for (i = 0; i < argc; i++) {
        *ptr++ = '"';
        src = argv[i];
        while(*src) {
            if (*src == '"' || *src == '\\') {
                *ptr++ = '\\';
            }
            *ptr++ = *src++;
        }
        *ptr++ = '"';
        *ptr++ = ' ';
    }
    return ptr;
}

/* This function takes a callback to be called after the creation of the child
   process but before we block waiting for the child. Can be NULL.  */
__attribute__((noreturn)) int run (char *exePath,
                                   int numArgs1, char **args1,
                                   int numArgs2, char **args2,
                                   runCallback callback)
{
    int i, cmdline_len;
    char *new_cmdline, *ptr;

    STARTUPINFO si;
    PROCESS_INFORMATION pi;

    ZeroMemory(&pi, sizeof(PROCESS_INFORMATION));
    ZeroMemory(&si, sizeof(STARTUPINFO));
    si.cb = sizeof(STARTUPINFO);

    /* Compute length of the flattened 'argv'.  for each arg:
     *   + 1 for the space
     *   + chars * 2 (accounting for possible escaping)
     *   + 2 for quotes
     */
    cmdline_len = 1 + strlen(exePath)*2 + 2;
    for (i=0; i < numArgs1; i++) {
        cmdline_len += 1 + strlen(args1[i])*2 + 2;
    }
    for (i=0; i < numArgs2; i++) {
        cmdline_len += 1 + strlen(args2[i])*2 + 2;
    }

    new_cmdline = (char*)malloc(sizeof(char) * (cmdline_len + 1));
    if (!new_cmdline) {
        die("failed to start up %s; insufficient memory", exePath);
    }

    ptr = flattenAndQuoteArgs(new_cmdline, 1, &exePath);
    ptr = flattenAndQuoteArgs(ptr, numArgs1, args1);
    ptr = flattenAndQuoteArgs(ptr, numArgs2, args2);
    *--ptr = '\0'; // replace the final space with \0

    /* Note: Used to use _spawnv(_P_WAIT, ...) here, but it suffered
       from the parent intercepting console events such as Ctrl-C,
       which it shouldn't. Installing an ignore-all console handler
       didn't do the trick either.

       Irrespective of this issue, using CreateProcess() is preferable,
       as it makes this wrapper work on both mingw and cygwin.
    */
#if 0
    fprintf(stderr, "Invoking %s\n", new_cmdline); fflush(stderr);
#endif
    if (!CreateProcess(exePath,
                       new_cmdline,
                       NULL,
                       NULL,
                       TRUE,
                       0, /* dwCreationFlags */
                       NULL, /* lpEnvironment */
                       NULL, /* lpCurrentDirectory */
                       &si,  /* lpStartupInfo */
                       &pi) ) {
        die("Unable to start %s (error code: %lu)\n", exePath, GetLastError());
    }

    /* Synchronize input and wait for target to be ready.  */
    WaitForInputIdle(pi.hProcess, INFINITE);

    /* If we have a registered callback then call it before we block.  */
    if (callback)
      callback();

    switch (WaitForSingleObject(pi.hProcess, INFINITE) ) {
    case WAIT_OBJECT_0:
    {
        DWORD pExitCode;
        if (GetExitCodeProcess(pi.hProcess, &pExitCode) == 0) {
            exit(1);
        }
        exit(pExitCode);
    }
    case WAIT_ABANDONED:
    case WAIT_FAILED:
        /* in the event we get any hard errors, bring the child to a halt. */
        TerminateProcess(pi.hProcess,1);
        exit(1);
    default:
        exit(1);
    }
}
