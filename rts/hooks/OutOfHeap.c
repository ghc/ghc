/* -----------------------------------------------------------------------------
 *
 * User-overridable RTS hooks.
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"
#include <stdio.h>

void
OutOfHeapHook (W_ request_size, W_ heap_size) /* both sizes in bytes */
{
  /*    fprintf(stderr, "Heap exhausted;\nwhile trying to allocate %lu bytes in a %lu-byte heap;\nuse `+RTS -H<size>' to increase the total heap size.\n", */

  (void)request_size;   /* keep gcc -Wall happy */
  if (heap_size > 0) {
      errorBelch("Heap exhausted;\nCurrent maximum heap size is %" FMT_Word " bytes (%" FMT_Word " MB);\nuse `+RTS -M<size>' to increase it.",
          heap_size, heap_size / (1024*1024));
  } else {
      errorBelch("out of memory");
  }
}

