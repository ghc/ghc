#include "Rts.h"

StgWord i;

int main(int argc, char *argv[])
{
    StgWord j;

    i = 0;
    j = xchg(&i,42);
    CHECK(j == 0);
    CHECK(i == 42);

    return 0;
}
