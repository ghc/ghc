// Copyright (c) 2016, Ryan Scott
// baz.c
#include "foo.h"
#include <stdio.h>

void baz(void) {
    printf("The value of foo is %d\n", foo); // Segfaults on this line
    fflush(stdout);
}
