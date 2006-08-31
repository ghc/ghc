#include "HsFFI.h"
#include "conc059_stub.h"
#include <unistd.h>
#include <stdio.h>

int main(int argc, char *argv[])
{
    hs_init(&argc,&argv);
    f(500000);
    usleep(100000);
    printf("exiting...\n");
    hs_exit();
    printf("exited.\n");
    usleep(1000000);
}
