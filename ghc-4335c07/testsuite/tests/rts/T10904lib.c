#include <stdio.h>
#include <stdlib.h>


#define MAGIC 0x11223344

void
init_value(int * p)
{
    *p = MAGIC;
}


void
finalize_value(int * p)
{
    static long counter = 0;

    counter += 1;

    if (counter % 1000000 == 0) {
        fprintf(stderr, "finalize_value: %ld calls\n", counter);
    }

    if (*p != MAGIC) {
        fprintf(stderr, "finalize_value: %x != %x after %ld calls\n",
                *p, MAGIC, counter);
        abort();
    }
}
