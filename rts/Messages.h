/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2010
 *
 * Inter-Capability message passing
 *
 * --------------------------------------------------------------------------*/

#pragma once

#include "BeginPrivate.h"

uint32_t messageBlackHole(Capability *cap, MessageBlackHole *msg);
StgTSO * blackHoleOwner (StgClosure *bh);

#if defined(THREADED_RTS)
void executeMessage (Capability *cap, Message *m);
void sendMessage    (Capability *from_cap, Capability *to_cap, Message *msg);
#endif

#include "Capability.h"
#include "Updates.h" // for DEBUG_FILL_SLOP
#include "SMPClosureOps.h"

INLINE_HEADER void
doneWithMsgThrowTo (MessageThrowTo *m)
{
    OVERWRITING_CLOSURE((StgClosure*)m);
    unlockClosure((StgClosure*)m, &stg_MSG_NULL_info);
    LDV_RECORD_CREATE(m);
}

#include "EndPrivate.h"

#if defined(THREADED_RTS) && defined(PROF_SPIN)
extern volatile StgWord64 whitehole_executeMessage_spin;
#endif
