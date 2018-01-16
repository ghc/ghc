#include <stdio.h>
#include "HsFFI.h"

#ifdef __GLASGOW_HASKELL__
#include "Bar_stub.h"
#endif

int main(int argc, char **argv) {
    hs_init(&argc, &argv);
    bar();
    printf("Hello world!");
    return 0;
}
