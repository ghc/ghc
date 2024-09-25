#include <stdio.h>

extern void bar(void);

void foo(void) {
    printf("VISIBLE FOO\n");
    bar();
}
