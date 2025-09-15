// Copyright (c) 2016, Ryan Scott
// bar.c
#include "foo.h"

int foo = 0;

void bar(void) {
    foo = 1;

    baz();
}
