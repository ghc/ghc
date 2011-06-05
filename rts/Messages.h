/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2010
 *
 * Inter-Capability message passing
 *
 * --------------------------------------------------------------------------*/

#include "BeginPrivate.h"

nat  messageBlackHole (Capability *cap, MessageBlackHole *msg);
nat  messageGlobalise (Capability *cap, StgTSO *tso, StgClosure *p, nat owner);
StgTSO * blackHoleOwner (StgClosure *bh);

#ifdef THREADED_RTS
void executeMessage (Capability *cap, Message *m);
void sendMessage    (Capability *from_cap, Capability *to_cap, Message *msg);
#endif

#include "Capability.h"
#include "Updates.h" // for DEBUG_FILL_SLOP

INLINE_HEADER void
doneWithMsgThrowTo (MessageThrowTo *m)
{
    overwritingPrimClosure((StgClosure*)m,
                           sizeofW(MessageThrowTo), sizeofW(Message));
    unlockClosure((StgClosure*)m, &stg_MSG_NULL_info);
    LDV_RECORD_CREATE(m);
}

#include "EndPrivate.h"
