/*
These routines customise the error messages
for various bits of the RTS.  They are linked
in instead of the defaults.
*/

#if __GLASGOW_HASKELL__ >= 400
#include "Rts.h"
#else
#include "rtsdefs.h"
#endif

#if __GLASGOW_HASKELL__ >= 303

void
ErrorHdrHook (long fd)
{
    char msg[]="\n";
    write(fd,msg,1);
}

void
PatErrorHdrHook (long fd)
{
    const char msg[]="\n*** Pattern-matching error within GHC!\n\nThis is a compiler bug; please report it to glasgow-haskell-bugs@haskell.org.\n\nFail:";
    write(fd,msg,sizeof(msg)-1);
}

void
PreTraceHook (long fd)
{
    const char msg[]="\n";
    write(fd,msg,sizeof(msg)-1);
}

void
PostTraceHook (long fd)
{
#if 0
    const char msg[]="\n";
    write(fd,msg,sizeof(msg)-1);
#endif
}

#else /* pre-3.03 GHC with old IO system */

void
ErrorHdrHook (FILE *where)
{
    fprintf(where, "\n"); /* no "Fail: " */
}

void
PatErrorHdrHook (FILE *where)
{
    fprintf(where, "\n*** Pattern-matching error within GHC!\n\nThis is a compiler bug; please report it to glasgow-haskell-bugs@haskell.org.\n\nFail: ");
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

#endif

#if __GLASGOW_HASKELL__ >= 400
void
OutOfHeapHook (unsigned long request_size, unsigned long heap_size)
  /* both in bytes */
{
    fprintf(stderr, "GHC's heap exhausted;\nwhile trying to allocate %lu bytes in a %lu-byte heap;\nuse the `-H<size>' option to increase the total heap size.\n",
	request_size,
	heap_size);
}

void
StackOverflowHook (unsigned long stack_size)    /* in bytes */
{
    fprintf(stderr, "GHC stack-space overflow: current size %ld bytes.\nUse the `-K<size>' option to increase it.\n", stack_size);
}

#else /* GHC < 4.00 */

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

#endif
