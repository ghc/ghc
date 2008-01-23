#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <ghcplatform.h>

#ifdef mingw32_HOST_OS
#include <windows.h>
#include <process.h>
#include <malloc.h>
#include <signal.h>
#include <io.h>
#endif

void error(const char *fmt, ...) {
    va_list argp;
    va_start(argp, fmt);
    vfprintf(stderr, fmt, argp);
    va_end(argp);
    fflush(stderr);
}

#ifndef mingw32_HOST_OS
int run(char *this, char *program, int argc, char** argv) {
    execv(program, argv);
    error("%s: Unable to start %s: ", this, program);
    perror("");
    return 1; /* Not reached */
}
#else
int run(char *this, char *program, int argc, char** argv) {
    TCHAR  programShort[MAX_PATH+1];
    DWORD  dwSize;
    DWORD  dwExitCode;
    int    i;
    char*  new_cmdline;
    char   *ptr;
    char   *src;
    unsigned int cmdline_len;

    STARTUPINFO si;
    PROCESS_INFORMATION pi;
  
    ZeroMemory(&si, sizeof(STARTUPINFO));
    ZeroMemory(&pi, sizeof(PROCESS_INFORMATION));

    si.cb = sizeof(STARTUPINFO);

    dwSize = MAX_PATH;
    /* Turn the path into short form - LFN form causes problems
       when passed in argv[0]. */
    if ( !(GetShortPathName(program, programShort, dwSize)) ) {
        error("%s: Unable to locate %s\n", this, program);
        return 1;
    }
  
    /* Compute length of the flattened 'argv', including spaces! */
    cmdline_len = 0;
    for(i = 0; i < argc; i++) {
        /* Note: play it safe and quote all argv strings */
        /* In the worst case we have to escape every character with a \ */
        cmdline_len += 1 + 2 * strlen(argv[i]) + 2;
    }
    new_cmdline = (char*)malloc(sizeof(char) * (cmdline_len + 1));
    if (!new_cmdline) {
        error("%s: failed to start up ghc.exe; insufficient memory\n", this);
        return 1;
    }

    ptr = new_cmdline;
    for(i = 0; i < argc; i++) {
        *ptr++ = ' ';
        *ptr++ = '"';
        src = argv[i];
        while(*src) {
            /* Escape any \ and " characters */
            if ((*src == '\\') || (*src == '"')) {
                *ptr++ = '\\';
            }
            *ptr++ = *src++;
        }
        *ptr++ = '"';
    }
    *ptr = '\0';
    new_cmdline = new_cmdline + 1; /* Skip the leading space */

    /* Note: Used to use _spawnv(_P_WAIT, ...) here, but it suffered
       from the parent intercepting console events such as Ctrl-C,
       which it shouldn't. Installing an ignore-all console handler
       didn't do the trick either.

       Irrespective of this issue, using CreateProcess() is preferable,
       as it makes this wrapper work on both mingw and cygwin.
    */
#if 0
    fprintf(stderr, "Invoking ghc: %s %s\n", programShort, new_cmdline);
    fflush(stderr);
#endif
    if (!CreateProcess(programShort,
                       new_cmdline,
                       NULL,
                       NULL,
                       TRUE,
                       0, /* dwCreationFlags */
                       NULL, /* lpEnvironment */
                       NULL, /* lpCurrentDirectory */
                       &si,  /* lpStartupInfo */
                       &pi) ) {
        error("%s: Unable to start ghc.exe (error code: %lu)\n",
              this, GetLastError());
        return 1;
    }
    /* Disable handling of console events in the parent by dropping its
     * connection to the console. This has the (minor) downside of not being
     * able to subsequently emit any error messages to the console.
     */
    FreeConsole();

    switch (WaitForSingleObject(pi.hProcess, INFINITE) ) {
        case WAIT_OBJECT_0:
            if (GetExitCodeProcess(pi.hProcess, &dwExitCode)) {
                return dwExitCode;
            }
            else {
                return 1;
            }
        case WAIT_ABANDONED:
        case WAIT_FAILED:
            /* in the event we get any hard errors, bring the child
               to a halt. */
            TerminateProcess(pi.hProcess, 1);
            return 1;
        default:
            return 1;
    }
}
#endif

