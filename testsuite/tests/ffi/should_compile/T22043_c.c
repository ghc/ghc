#include <stdlib.h>

const int * foo() {
    int *x = malloc(sizeof(int));
    *x = 42;
    return x;
}

const int *bar = 0;
