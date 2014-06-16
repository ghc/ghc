#include "HsFFI.h"
#include "T6006_stub.h"
#include <stdlib.h>
int main(int argc, char** argv) {
    hs_init(NULL,NULL);
    f();
    hs_exit();
    exit(0);
}
