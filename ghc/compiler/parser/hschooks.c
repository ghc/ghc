/*
These routines customise the error messages
for various bits of the RTS.  They are linked
in instead of the defaults.
*/
#include <stdio.h>

#define W_ unsigned long int
#define I_ long int

void
ErrorHdrHook (FILE *where)
{
    fprintf(where, "\n"); /* no "Fail: " */
}


void
OutOfHeapHook (W_ request_size, W_ heap_size)  /* both in bytes */
{
    fprintf(stderr, "GHC's heap exhausted;\nwhile trying to allocate %lu bytes in a %lu-byte heap;\nuse the `-H<size>' option to increase the total heap size.\n",
	request_size,
	heap_size);
}

void
StackOverflowHook (I_ stack_size)    /* in bytes */
{
    fprintf(stderr, "GHC stack-space overflow: current size %ld bytes.\nUse the `-K<size>' option to increase it.\n", stack_size);
}

void
PatErrorHdrHook (FILE *where)
{
    fprintf(where, "\n*** Pattern-matching error within GHC!\n\nThis is a compiler bug; please report it to glasgow-haskell-bugs@dcs.gla.ac.uk.\n\nFail: ");
}

void
PreTraceHook (FILE *where)
{
    fprintf(where, "\n"); /* not "Trace On" */
}

void
PostTraceHook (FILE *where)
{
    fprintf(where, "\n"); /* not "Trace Off" */
}
