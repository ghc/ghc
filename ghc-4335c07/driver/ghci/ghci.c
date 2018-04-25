
#include "cwrapper.h"
#include "getLocation.h"
#include "isMinTTY.h"
#include <stdio.h>
#include <stdlib.h>
#include <windows.h>

BOOL fileExists(const char *path) {
    const DWORD r = GetFileAttributesA(path);
    return r != INVALID_FILE_ATTRIBUTES && !(r & FILE_ATTRIBUTE_DIRECTORY);
}

/* In order for this console program to pass on full event processing to called
   process we need to remove it from the current console. Since we want the
   child to inherit the handles so redirection etc all work we need to detach
   from the console after the child has been created. However we don't want to
   detach from the console in non-interactive scenarios otherwise we'll hit
   #13411 again. So we only detach when we're sure we need to, see #14150.  */
void ReleaseResource(void) {
    FreeConsole();
}

int main(int argc, char** argv) {
    char *binDir;
    char *exePath;
    char *preArgv[1];

    if (isMinTTY()) {
        printf("WARNING: GHCi invoked via 'ghci.exe' in MinTTY consoles (e.g., Cygwin or MSYS)\n");
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

    run(exePath, 1, preArgv, argc - 1, argv + 1, ReleaseResource);
}
