#include <stdio.h>

// flush so that:
//      (1) if we segfault, we'll see the output
//      (2) we get ordered correctly with Haskell output, which uses
//          different buffers
static void initArray1(void) { printf("initArray1\n"); fflush(stdout); }
static void initArray2(void) { printf("initArray2\n"); fflush(stdout); }
static void ctors1(void)     { printf("ctors1\n");     fflush(stdout); }
static void ctors2(void)     { printf("ctors2\n");     fflush(stdout); }

#if defined(cygwin32_HOST_OS) || defined (mingw32_HOST_OS)

#error "Not implemented yet!"

#elif defined(darwin_HOST_OS)

#error "Not implemented yet!"

#else /* ELF */

static void (*const init_array[2])(void) __attribute__((
            section(".init_array"), // put it in the right section
            used,                   // prevent GCC from optimizing this away
            aligned(sizeof(void*))  // avoid slop between GCC's preloaded initializers and ours
            ))
    = {initArray1, initArray2};

static void (*ctors[2])(void) __attribute__((
            section(".ctors"),
            used,
            aligned(sizeof(void*))))
    = {ctors2, ctors1}; // ctors run in reverse

#endif
