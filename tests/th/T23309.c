#define _GNU_SOURCE 1
#include <stdio.h>

const char* foo(int e) {
    static char s[256];
    sprintf(s, "The value of e is: %u", e);
    return s;
}
