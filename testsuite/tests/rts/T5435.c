#include <stdio.h>
static void initializer(void) __attribute__((constructor));

static void initializer(void)
{
    printf("initializer run\n");
    fflush(stdout);
}
