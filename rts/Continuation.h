/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2020
 *
 * Continuations
 *
 * --------------------------------------------------------------------------*/

#pragma once

StgClosure *captureContinuationAndAbort(Capability *cap, StgTSO *tso, StgPromptTag prompt_tag);
