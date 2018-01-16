
/* gcc on mingw is hardcoded to use /mingw (which is c:/mingw) to
   find various files. If this is a different version of mingw to the
   one that we have in the GHC tree then things can go wrong. We
   therefore need to add various -B flags to the gcc commandline,
   so that it uses our in-tree mingw. Hence this wrapper. */

#include "cwrapper.h"
#include "getLocation.h"

#include <stdio.h>
#include <stdlib.h>

int main(int argc, char** argv) {
    char *binDir;
    char *exePath;
    char *preArgv[4];
    char *oldPath;
    char *newPath;
    char *base;
    char *version;
    int n;

    binDir = getExecutablePath();
    exePath = mkString("%s/realgcc.exe", binDir);

    /* We need programs like
           inplace/mingw/libexec/gcc/mingw32/4.5.0/cc1.exe
       to be able to find the DLLs in inplace/mingw/bin, so we need to
       add it to $PATH */
    oldPath = getenv("PATH");
    if (!oldPath) {
        die("Couldn't read PATH\n");
    }
    n = snprintf(NULL, 0, "PATH=%s;%s", binDir, oldPath);
    n++;
    newPath = malloc(n);
    if (!newPath) {
        die("Couldn't allocate space for PATH\n");
    }
    snprintf(newPath, n, "PATH=%s;%s", binDir, oldPath);
    n = putenv(newPath);
    if (n) {
        die("putenv failed\n");
    }

    /* GCC Version. */
    version = mkString("%d.%d.%d", __GNUC__, __GNUC_MINOR__, __GNUC_PATCHLEVEL__);

    /* Without these -B args, gcc will still work. However, if you
       have a mingw installation in c:/mingw then it will use files
       from that in preference to the in-tree files. */
    preArgv[0] = mkString("-B%s", binDir);
    preArgv[1] = mkString("-B%s/../lib", binDir);
#if defined(__MINGW64__)
    base = mkString("x86_64-w64-mingw32");
#else
    base = mkString("i686-w64-mingw32");
#endif

    preArgv[2] = mkString("-B%s/../lib/gcc/%s/%s"    , binDir, base, version);
    preArgv[3] = mkString("-B%s/../libexec/gcc/%s/%s", binDir, base, version);

    run(exePath, 4, preArgv, argc - 1, argv + 1, NULL);
}

