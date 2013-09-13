#include <stdio.h>
static void initializer1(void) __attribute__((constructor));
static void initializer2(void) __attribute__((constructor));

static void initializer1(void)
{
    printf("initializer1 run\n");
    fflush(stdout);
}

static void initializer2(void)
{
    printf("initializer2 run\n");
    fflush(stdout);
}
