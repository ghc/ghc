/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 1998-2019
 *
 * Non-moving garbage collector and allocator:
 * Indirection short-cutting and the selector optimisation
 *
 * ---------------------------------------------------------------------------*/

#pragma once

void
nonmoving_eval_thunk_selector(MarkQueue *queue, StgSelector *p, StgClosure **origin);
