#include <stdio.h>

void __attribute__ ((__visibility__ ("hidden"))) foo(void) {
    printf("HIDDEN FOO\n");
}

void bar(void) {
    printf("BAR\n");
    foo();
}
