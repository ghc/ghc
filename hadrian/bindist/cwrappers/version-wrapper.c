
#include "cwrapper.h"
#include "getLocation.h"
#include <stddef.h>

int main(int argc, char** argv) {
    char *binDir;
    char *exePath;

    binDir = getExecutablePath();
    exePath = mkString("%s/%s", binDir, EXE_PATH);

    run(exePath, 0, NULL, argc - 1, argv + 1, NULL);
}
