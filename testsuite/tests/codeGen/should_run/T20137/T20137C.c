#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>

int
runInteractiveProcess (char *const * args,
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
        printf("%" PRIx64 "\n", (uint64_t) args);
        printf("%" PRIx64 "\n", (uint64_t) workingDirectory);
        printf("%" PRIx64 "\n", (uint64_t) environment);
        printf("%x\n", fdStdIn);
        printf("%x\n", fdStdOut);
        printf("%x\n", fdStdErr);
        printf("%" PRIx64 "\n", (uint64_t) pfdStdInput);
        printf("%" PRIx64 "\n", (uint64_t) pfdStdOutput);
        printf("%" PRIx64 "\n", (uint64_t) pfdStdError);
        printf("%" PRIx64 "\n", (uint64_t) childGroup);
        printf("%" PRIx64 "\n", (uint64_t) childUser);
        printf("%x\n", flags);
        printf("%" PRIx64 "\n", (uint64_t) failed_doing);
        return 0;
}

