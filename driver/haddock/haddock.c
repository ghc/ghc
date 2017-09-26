
#include "cwrapper.h"
#include "getLocation.h"
#include <stddef.h>

int main(int argc, char** argv) {
    char *binDir;
    char *exePath;

    binDir = getExecutablePath();
    exePath = mkString("%s/haddock.exe", binDir);

    run(exePath, 0, NULL, argc - 1, argv + 1, NULL);
}
