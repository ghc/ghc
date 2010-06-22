
#include "cwrapper.h"
#include "getLocation.h"
#include <stdio.h>
#include <stdlib.h>

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

    run(exePath, 1, preArgv, argc - 1, argv + 1);
}

