/* -----------------------------------------------------------------------------
 * $Id: OutOfHeap.c,v 1.2 1998/12/02 13:29:14 simonm Exp $
 *
 * User-overridable RTS hooks.
 *
 * ---------------------------------------------------------------------------*/

#include "Rts.h"

void
OutOfHeapHook (lnat request_size, lnat heap_size) /* both sizes in bytes */
{
  /*    fprintf(stderr, "Heap exhausted;\nwhile trying to allocate %lu bytes in a %lu-byte heap;\nuse `+RTS -H<size>' to increase the total heap size.\n", */

  fprintf(stderr, "Heap exhausted;\nCurrent maximum heap size is %lu bytes;\nuse `+RTS -M<size>' to increase it.\n",
	  heap_size);
}

