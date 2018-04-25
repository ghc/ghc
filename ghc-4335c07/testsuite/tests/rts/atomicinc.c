#include "Rts.h"

StgWord i;

int main(int argc, char *argv[])
{
    StgWord j;

    i = 0;
    j = atomic_inc(&i,1);
    CHECK(j == 1);
    CHECK(i == 1);

    j = atomic_dec(&i);
    CHECK(j == 0);
    CHECK(i == 0);

    return 0;
}
