
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#ifdef WINDOWS
#include <windows.h>
#include <process.h>
#include <malloc.h>
#include <signal.h>
#include <io.h>
#endif

int run(char *this, char *program, int argc, char **argv);

void error(const char *fmt, ...) {
    va_list argp;
    va_start(argp, fmt);
    vfprintf(stderr, fmt, argp);
    va_end(argp);
    fprintf(stderr, "\n");
    fflush(stderr);
}

int main(int argc, char **argv) {
    char **args;
    args = malloc(sizeof(char *) * (argc + 3));
    if (args == NULL) {
        fprintf(stderr, "Malloc failed\n");
        exit(1);
    }
    args[0] = GHC_PATH;
    args[1] = "-B" TOP_ABS;
    args[2] = "-fhardwire-lib-paths";
    if ((argc >= 2) && (strcmp(argv[1], "-v") == 0)) {
        printf("Using %s %s %s\n", args[0], args[1], args[2]);
    }
    memcpy(args + 3, argv + 1, sizeof(char *) * (argc - 1));
    args[argc+2] = NULL;
    return run(argv[0], GHC_PATH, argc + 2, args);
}

#ifndef WINDOWS
int run(char *this, char *program, int argc, char** argv) {
    execv(program, argv);
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
        error("%s: Unable to locate %s", this, program);
        return 1;
    }
  
    /* Compute length of the flattened 'argv', including spaces! */
    cmdline_len = 0;
    for(i = 1; i < argc; i++) {
        /* Note: play it safe and quote all argv strings */
        cmdline_len += 1 + strlen(argv[i]) + 2;
    }
    new_cmdline = (char*)malloc(sizeof(char) * (cmdline_len + 1));
    if (!new_cmdline) {
        error("%s: failed to start up ghc.exe; insufficient memory", this);
        return 1;
    }
  
    ptr = new_cmdline;
    for(i = 1; i < argc; i++) {
        *ptr++ = ' ';
        *ptr++ = '"';
        src = argv[i];
        while(*src) {
            *ptr++ = *src++;
        }
        *ptr++ = '"';
    }
    *ptr = '\0';
  
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
        error("%s: Unable to start ghc.exe (error code: %lu)",
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

