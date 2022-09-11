/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2020
 *
 * Continuations
 *
 * --------------------------------------------------------------------------*/

#pragma once

#include "BeginPrivate.h"

StgClosure *captureContinuationAndAbort(Capability *cap, StgTSO *tso, StgPromptTag prompt_tag);

#include "EndPrivate.h"
