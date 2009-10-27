
#include "getLocation.h"
#include <stdio.h>
#include <windows.h>

static void die(char *msg) {
    fprintf(stderr, "%s", msg);
    exit(1);
}

char *getExecutable(void) {
    char *p;
    int i;
    int r;

    i = 2048; /* plenty, PATH_MAX is 512 under Win32 */
    p = malloc(i);
    if (p == NULL) {
        die("Malloc failed\n");
    }
    r = GetModuleFileNameA(NULL, p, i);
    if (r == 0) {
        die("getModuleFileName failed\n");
    }
    return p;
}

char *getExecutablePath(void) {
    char *p;
    char *f;

    p = getExecutable();
    f = strrchr(p, '\\');
    if (f == NULL) {
        die("No '\\' in executable location\n");
    }
    f[0] = '\0';
    return p;
}

