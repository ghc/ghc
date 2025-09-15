/* verify that Rts.h can be parsed as a C++ header */
extern "C" {
#include "Rts.h"
}

extern "C"
int main(int argc, char *argv[]) {
    debugBelch("Hello world!");
    return 0;
}
