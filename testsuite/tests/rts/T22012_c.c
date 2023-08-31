#include <stdio.h>
#include <stdint.h>
#include <stdatomic.h>
#include <stdbool.h>

void test (void) {
    _Atomic uint32_t x = 42;
    uint32_t y = 42;

    bool success = atomic_compare_exchange_strong(&x, &y, 43);
    printf("# CAS\n");
    printf("success=%u\n", (int) success);
    printf("old=%u\n", y);
    printf("x=%u\n", x);

    printf("# Swap\n");
    y = atomic_exchange(&x, 2);
    printf("x=%u\n", x);
    printf("y=%u\n", y);

    printf("# Fetch-Add\n");
    y = atomic_fetch_add(&x, 2);
    printf("x=%u\n", x);
    printf("y=%u\n", y);
    fflush(stdout);
}

