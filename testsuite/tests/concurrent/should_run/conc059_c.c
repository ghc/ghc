#include "HsFFI.h"
#include "conc059_stub.h"
#include <unistd.h>
#include <stdio.h>
#if mingw32_HOST_OS
#include <windows.h>
#endif

int main(int argc, char *argv[])
{
    hs_init(&argc,&argv);
    f(500000);
#if mingw32_HOST_OS
    Sleep(100);
#else
    usleep(100000);
#endif
    printf("exiting...\n");
    hs_exit();
    printf("exited.\n");
#if mingw32_HOST_OS
    Sleep(1000);
#else
    usleep(1000000);
#endif
    exit(0);
}
