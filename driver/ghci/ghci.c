
#include "cwrapper.h"
#include "getLocation.h"
#include <stdio.h>
#include <stdlib.h>
#include <windows.h>

BOOL fileExists(const char *path) {
    const DWORD r = GetFileAttributesA(path);
    return r != INVALID_FILE_ATTRIBUTES && !(r & FILE_ATTRIBUTE_DIRECTORY);
}

int main(int argc, char** argv) {
    char *binDir;
    char *exePath;
    char *preArgv[1];

    if (getenv("_")) {
        printf("WARNING: GHCi invoked via 'ghci.exe' in *nix-like shells (cygwin-bash, in particular)\n");
        printf("         doesn't handle Ctrl-C well; use the 'ghcii.sh' shell wrapper instead\n");
        fflush(stdout);
    }

    binDir = getExecutablePath();
    exePath = mkString("%s/ghc.exe", binDir);
    preArgv[0] = "--interactive";

    /* If ghc.exe can't be found, we assume that we're building ghc from
     * source, in which case we fall back on ghc-stage2.
     */
    if (!fileExists(exePath)) {
        exePath = mkString("%s/ghc-stage2.exe", binDir);
    }

    run(exePath, 1, preArgv, argc - 1, argv + 1);
}

