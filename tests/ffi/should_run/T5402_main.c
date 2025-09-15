#include "HsFFI.h"
#include <stdio.h>
#include <stdlib.h>
#include "T5402_stub.h"

int main (int argc, char *argv[])
{
    hs_init(&argc,&argv);
    hs_init(&argc,&argv);
    hsmain();
    printf("I shouldn't be here\n");
    exit(1);
}
