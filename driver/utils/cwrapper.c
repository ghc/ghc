
/* gcc on mingw is hardcoded to use /mingw (which is c:/mingw) to
   find various files. If this is a different version of mingw to the
   one that we have in the GHC tree then things can go wrong. We
   therefore need to add various -B flags to the gcc commandline,
   so that it uses our in-tree mingw. Hence this wrapper. */

#include "cwrapper.h"
#include <errno.h>
#include <process.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

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
        die("snprintf 0 failed: errno %d: %s\n", errno, strerror(errno));
    }

    p = malloc(i + 1);
    if (p == NULL) {
        die("malloc failed: errno %d: %s\n", errno, strerror(errno));
    }

    va_start(argp, fmt);
    j = vsnprintf(p, i + 1, fmt, argp);
    va_end(argp);
    if (i < 0) {
        die("snprintf with %d failed: errno %d: %s\n",
            i + 1, errno, strerror(errno));
    }

    return p;
}

char *quote(char *str) {
    char *quotedStr;
    char *p;

    quotedStr = malloc(2 * strlen(str) + 2 + 1);
    if (quotedStr == NULL) {
        die("malloc failed: errno %d: %s\n", errno, strerror(errno));
    }
    p = quotedStr;
    *p++ = '"';
    while (*str) {
        if (*str == '"') {
            *p++ = '\\';
        }
        *p++ = *str++;
    }
    *p++ = '"';
    *p = '\0';

    return quotedStr;
}

__attribute__((noreturn)) int run(char *exePath, int numArgs1, char **args1, int numArgs2, char **args2) {
    char **p;
    char **newArgv;
    int i, ret;

    newArgv = malloc(sizeof(char *) * (1 + numArgs1 + numArgs2 + 1));
    if (newArgv == NULL) {
        die("malloc failed: errno %d: %s\n", errno, strerror(errno));
    }
    p = newArgv;
    *p++ = quote(exePath);
    for (i = 0; i < numArgs1; i++) {
        *p++ = quote(args1[i]);
    }
    for (i = 0; i < numArgs2; i++) {
        *p++ = quote(args2[i]);
    }
    *p = NULL;
    ret = spawnv(_P_WAIT, exePath, (const char* const*)newArgv);
    if (errno) {
        die("spawnv failed: errno %d: %s\n", errno, strerror(errno));
    }
    exit(ret);
}
