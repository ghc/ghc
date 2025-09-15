#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>

int
functionNameThatDoesntClash (char *const * args,
                       char *workingDirectory, char **environment,
                       // handles to use for the standard handles. -1 indicates
                       // create pipe, -2 indicates close.
                       int fdStdIn, int fdStdOut, int fdStdErr,
                       // output arguments to return created pipe handle to caller
                       int *pfdStdInput, int *pfdStdOutput, int *pfdStdError,
                       int *childGroup, int *childUser,
                       int flags,
                       char **failed_doing)
{
        // N.B. We don't use %p here since the rendering of this varies across
        // libc implementations
        printf("%" PRIxPTR "\n", (uintptr_t) args);
        printf("%" PRIxPTR "\n", (uintptr_t) workingDirectory);
        printf("%" PRIxPTR "\n", (uintptr_t) environment);
        printf("%x\n", fdStdIn);
        printf("%x\n", fdStdOut);
        printf("%x\n", fdStdErr);
        printf("%" PRIxPTR "\n", (uintptr_t) pfdStdInput);
        printf("%" PRIxPTR "\n", (uintptr_t) pfdStdOutput);
        printf("%" PRIxPTR "\n", (uintptr_t) pfdStdError);
        printf("%" PRIxPTR "\n", (uintptr_t) childGroup);
        printf("%" PRIxPTR "\n", (uintptr_t) childUser);
        printf("%x\n", flags);
        printf("%" PRIxPTR "\n", (uintptr_t) failed_doing);
        return 0;
}

