#include "HsFFI.h"
#include "conc059_stub.h"
#include <stdbool.h>
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>
#include <stdio.h>
// stdlib is needed for exit()
#include <stdlib.h>
#if mingw32_HOST_OS
#include <windows.h>
#else
#include <time.h> // nanosleep
#endif

void millisleep(int milliseconds);

int main(int argc, char *argv[])
{
    hs_init(&argc,&argv);
    f(5000); // this should be considerably longer than the delay on the next
             // line
    millisleep(100);

    printf("exiting...\n");
    fflush(stdout);
    hs_exit();
    printf("exited.\n");
    millisleep(1000);

    exit(0);
}

void millisleep(int milliseconds) {
#if defined(mingw32_HOST_OS)
    Sleep(milliseconds);
#else
    struct timespec ts = {
        .tv_sec = milliseconds / 1000,
        .tv_nsec = (milliseconds % 1000) * 1000000
    };

    while (true) {
        int ret = nanosleep(&ts, &ts);
        if (ret == -1) {
            if (errno != EINTR) {
                printf("nanosleep failed\n");
                exit(1);
            } else {
                continue;
            }
        } else {
            return;
        }
    }
#endif
}
