
#include "HsFFI.h"
#include "T4464H_stub.h"
#include <stdio.h>

void HsStart(void);
void HsEnd(void);

int main(void) {
    HsStart();
    printf("f 12 = %i\n", f(12));
    HsEnd();
    return 0;
}

