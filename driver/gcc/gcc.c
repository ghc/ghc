
/* gcc on mingw is hardcoded to use /mingw (which is c:/mingw) to
   find various files. If this is a different version of mingw to the
   one that we have in the GHC tree then things can go wrong. We
   therefore need to add various -B flags to the gcc commandline,
   so that it uses our in-tree mingw. Hence this wrapper. */

#include "getLocation.h"
#include <errno.h>
#include <process.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>

static void die(const char *fmt, ...) {
    va_list argp;

    va_start(argp, fmt);
    vfprintf(stderr, fmt, argp);
    va_end(argp);
    exit(1);
}

static char *mkString(const char *fmt, ...) {
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
    int i;

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

int main(int argc, char** argv) {
    char *p;
    char *binDir;
    char *exePath;
    char *bArg;
    char **newArgv;
    int i, j, ret;

    binDir = getExecutablePath();
    exePath = mkString("%s/realgcc.exe", binDir);

    /* Without these -B args, gcc will still work. However, if you
       have a mingw installation in c:/mingw then it will use files
       from that in preference to the in-tree files. */

    newArgv = malloc(sizeof(char *) * (argc + 4 + 1));
    newArgv[0] = quote(exePath);
    newArgv[1] = quote(mkString("-B%s", binDir));
    newArgv[2] = quote(mkString("-B%s/../lib", binDir));
    newArgv[3] = quote(mkString("-B%s/../lib/gcc/mingw32/3.4.5", binDir));
    newArgv[4] = quote(mkString("-B%s/../libexec/gcc/mingw32/3.4.5", binDir));
    for (i = 1; i < argc; i++) {
        newArgv[4 + i] = quote(argv[i]);
    }
    newArgv[4 + argc] = NULL;
    // execv(exePath, argv);
    ret = spawnv(_P_WAIT, exePath, (const char* const*)newArgv);
    if (errno) {
        die("spawnv failed: errno %d: %s\n", errno, strerror(errno));
    }
    exit(ret);
}
