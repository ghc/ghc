/* -----------------------------------------------------------------------------
 *
 * User-overridable RTS hooks.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"
#include <stdio.h>

void
OutOfHeapHook (lnat request_size, lnat heap_size) /* both sizes in bytes */
{
  /*    fprintf(stderr, "Heap exhausted;\nwhile trying to allocate %lu bytes in a %lu-byte heap;\nuse `+RTS -H<size>' to increase the total heap size.\n", */

  (void)request_size;   /* keep gcc -Wall happy */
  fprintf(stderr, "Heap exhausted;\nCurrent maximum heap size is %lu bytes (%lu Mb);\nuse `+RTS -M<size>' to increase it.\n",
	  heap_size, heap_size / (1024*1024));
}

