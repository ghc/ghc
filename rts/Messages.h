/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2010
 *
 * Inter-Capability message passing
 *
 * --------------------------------------------------------------------------*/

#include "BeginPrivate.h"

nat messageBlackHole(Capability *cap, MessageBlackHole *msg);

#ifdef THREADED_RTS
void executeMessage (Capability *cap, Message *m);
void sendMessage    (Capability *from_cap, Capability *to_cap, Message *msg);
#endif

#include "EndPrivate.h"
