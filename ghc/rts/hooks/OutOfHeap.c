/* -----------------------------------------------------------------------------
 * $Id: OutOfHeap.c,v 1.3 1999/06/29 13:06:45 panne Exp $
 *
 * User-overridable RTS hooks.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"

void
OutOfHeapHook (lnat request_size, lnat heap_size) /* both sizes in bytes */
{
  /*    fprintf(stderr, "Heap exhausted;\nwhile trying to allocate %lu bytes in a %lu-byte heap;\nuse `+RTS -H<size>' to increase the total heap size.\n", */

  (void)request_size;   /* keep gcc -Wall happy */
  fprintf(stderr, "Heap exhausted;\nCurrent maximum heap size is %lu bytes;\nuse `+RTS -M<size>' to increase it.\n",
	  heap_size);
}

