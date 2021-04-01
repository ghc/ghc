/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2005
 *
 * Support for heap profiling
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include <rts/TraverseHeap.h>

typedef struct _Census Census;

void        heapCensus         (Time t);
Census*     performHeapCensus  (Time t, W_ n_blocks, bdescr **block_list);
void        endHeapCensus      (Census *census);

void        initHeapProfiling  (void);
void        endHeapProfiling   (void);
void        freeHeapProfiling  (void);
bool        strMatchesSelector (const char* str, const char* sel);

#if defined(PROFILING)
// doingRetainerProfiling: `-hr` or `-hr<cc> -h<x>`
bool doingRetainerProfiling(void);

#endif
