/*
These routines customise the error messages
for various bits of the RTS.  They are linked
in instead of the defaults.
*/
#include <stdio.h>

#define W_ unsigned long int
#define I_ long int

void
ErrorHdrHook (where)
  FILE *where;
{
    fprintf(where, "\n"); /* no "Fail: " */
}


void
OutOfHeapHook (request_size, heap_size)
  W_ request_size; /* in bytes */
  W_ heap_size;    /* in bytes */
{
    fprintf(stderr, "GHC's heap exhausted;\nwhile trying to allocate %lu bytes in a %lu-byte heap;\nuse the `-H<size>' option to increase the total heap size.\n",
	request_size,
	heap_size);
}

void
StackOverflowHook (stack_size)
  I_ stack_size;    /* in bytes */
{
    fprintf(stderr, "GHC stack-space overflow: current size %ld bytes.\nUse the `-K<size>' option to increase it.\n", stack_size);
}

#if 0
/* nothing to add here, really */
void
MallocFailHook (request_size, msg)
  I_ request_size;    /* in bytes */
  char *msg;
{
    fprintf(stderr, "malloc: failed on request for %lu bytes\n", request_size);
}
#endif /* 0 */

void
PatErrorHdrHook (where)
  FILE *where;
{
    fprintf(where, "\n*** Pattern-matching error within GHC!\n\nThis is a compiler bug; please report it to glasgow-haskell-bugs@dcs.glasgow.ac.uk.\n\nFail: ");
}

void
PreTraceHook (where)
  FILE *where;
{
    fprintf(where, "\n"); /* not "Trace On" */
}

void
PostTraceHook (where)
  FILE *where;
{
    fprintf(where, "\n"); /* not "Trace Off" */
}
