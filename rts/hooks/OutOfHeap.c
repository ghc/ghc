/* -----------------------------------------------------------------------------
 *
 * User-overridable RTS hooks.
 *
 * ---------------------------------------------------------------------------*/

#include "PosixSource.h"
#include "Rts.h"
#include "Hooks.h"
#include "RtsFlags.h"
#include <stdio.h>

void
OutOfHeapHook (W_ request_size, W_ heap_size) /* both sizes in bytes */
{
  (void)request_size;   /* keep gcc -Wall happy */
  if (heap_size > 0) {
      errorBelch("Heap exhausted;\n"
                 "Current maximum heap size is %" FMT_Word
                 " bytes (%" FMT_Word " MB);\n"
                 "%s `+RTS -M<size>' to increase it.",
                 heap_size, heap_size / (1024*1024),
                 ((rtsConfig.rts_opts_enabled == RtsOptsAll)
                  ? "use"
                  : "relink with -rtsopts and use"));
  } else {
      errorBelch("out of memory");
  }
}
