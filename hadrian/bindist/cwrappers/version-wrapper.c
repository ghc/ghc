
#include "cwrapper.h"
#include "getLocation.h"
#include <stddef.h>
#include <windows.h>

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

    binDir = getExecutablePath();
    exePath = mkString("%s/%s", binDir, EXE_PATH);

    run(exePath, 0, NULL, argc - 1, argv + 1,
#if INTERACTIVE_PROCESS
            ReleaseResource);
#else
            NULL);
#endif
}
