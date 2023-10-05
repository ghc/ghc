#include <stdio.h>
#include <stdint.h>

int64_t func(int64_t a, uint32_t b, int64_t c, int64_t d) {
    printf("ffi call");
    if (a == 1) {
        printf(" with corrupted convention\n");
    }
    else {
        printf("\n");
    }
    return 0;
}

