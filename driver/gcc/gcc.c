
#include "getLocation.h"
#include <process.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

static void die(char *msg) {
    fprintf(stderr, "%s", msg);
    exit(1);
}

static char *mkString(const char *fmt, ...) {
    char *p;
    int i, j;
    va_list argp;

    va_start(argp, fmt);
    i = vsnprintf(p, 0, fmt, argp);
    va_end(argp);

    if (i < 0) {
        die("snprintf failed\n");
    }

    p = malloc(i + 1);
    if (p == NULL) {
        die("malloc failed\n");
    }

    va_start(argp, fmt);
    j = vsnprintf(p, i + 1, fmt, argp);
    va_end(argp);
    if (i < 0) {
        die("snprintf failed\n");
    }

    return p;
}

char *quote(char *str) {
    char *quotedStr;
    char *p;
    int i;

    quotedStr = malloc(2 * strlen(str) + 2 + 1);
    if (quotedStr == NULL) {
        die("malloc failed\n");
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
        die("Spawn failed\n");
    }
    exit(ret);
}
